module Main where

{-# LANGUAGE OverloadedStrings #-}
-- The program will raise the error "Not enough values to operate
-- on" if you call it with less than 2 stack items to operate on.

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read as TR ( double )
import Data.Tuple as Tu ( swap )
import Data.List ( foldl' )
import qualified Data.Either as E
import qualified Data.List as DL
import qualified Data.Text.Lazy as TL
import qualified Control.Arrow as Arrow



-- ByteCode data type
newtype ByteCode = ByteCode {bytecode :: [T.Text]}
    deriving (Show)

-- A data type for the stack (wanted to try out some
-- things with custom data types and it makes it
-- easier to keep track of it as the stack and
-- not any old list), each stack item comprises
-- of a Text item and Double value, the text
-- item will either be empty or have a variable
-- value (I was going to use a Maybe monad to use
-- nothing instead of a blank string for non variables
-- but that introduced a ton of extra code to deal
-- with the bytecode lines value part in regards
-- to variables vs numeric values) and the double
-- value is for storing whatever value is given
-- to the stack (either from the original bytecode
-- or interpreted values).
newtype Stack = Stack {stack :: [(T.Text, Double)]}
    deriving (Show)


-- This will take the input text, split it into a series of
-- lines on '\n', calls a foldr on the items that will
-- first strip '\r' with stripEnd (Needed windows for
-- a program we used at my old work so the \r was causing
-- trouble, I generally use Pop!_OS as I love the tiling
-- management but I've been super keen to try out NixOS
-- for a while and you guys made it sound worth giving
-- a go so I'm planning to swap over after), then just pump
-- the non empty lines into an accumulator list.
getLinesOfByteCode :: T.Text -> ByteCode
getLinesOfByteCode input =
    ByteCode (
        foldr ((\x acc -> if not (T.null x) then x:acc else acc) . T.stripEnd) [] (T.lines input)
        )


-- This splits the command lines of byte code up into
-- command and value on the space character.
-- Note that I didn't drop line in regards
-- to not knowing if that is best practice
-- yet.
splitCMDAndValue :: T.Text -> (T.Text, T.Text)
splitCMDAndValue line = T.breakOnEnd " " line


-- was just messing around with a couple of way to get
-- convert the value part of the bytecode to a number
-- and put it in a stack element, this is quite cool
-- as when using Data.Text.Read.double (TR.double) it
-- it returns a Right tuple of the text value and remaining
-- text after (e.g. TR.double "123foo" -> Right (123.0,"foo"))
-- so given that it just passes a number, the second element
-- will be blank so I can just fromRight and swap around the
-- tuple's elements to get the result I would have built
-- with value anyway: ("", 123.0).
makeStackElement :: T.Text -> (T.Text, Double)
makeStackElement value = Tu.swap (E.fromRight (1,"") (TR.double value))


-- this takes an unpacked stack and an item to add, then
-- uses cons to put the item at the head and pack it into
-- a Stack type.
addToStack :: [(T.Text, Double)] -> (T.Text, Double) -> Stack
addToStack currentStack item  = Stack (item:currentStack)

addToBottomOfStack :: [(T.Text, Double)] -> (T.Text, Double) -> Stack
addToBottomOfStack currentStack item  = Stack (currentStack++[item])

-- Makes a stack element with makeStackElement and uses
-- addToStack to insert it at the head of the stack.
loadVal :: T.Text -> Stack -> Stack
loadVal line (Stack pStack) =
    let insertVal = makeStackElement (snd (splitCMDAndValue line))
    in addToStack pStack insertVal

-- Takes the line of bytecode to get the var name,
-- workingVal gets the value from the item at the
-- top of the stack,
-- removeHeadOfStack removes the current head of
-- the stack.
-- It uses splitCmdAndValue and snd to get the 
-- variable name from bytecode and uses the value from
-- workingVal to create a stack element, then calls
-- addToBottomOfStack on the current stack minus the
-- element which working val was gotten from at the head,
-- putting the value at the bottom of the stack (since
-- it's a variable it'll be called to the head with
-- read anyway).
writeVar :: T.Text -> Stack -> Stack
writeVar line (Stack pStack) =
    let workingVal = snd (head pStack)
        removeHeadOfStack pStack = drop 1 pStack
    in addToBottomOfStack (removeHeadOfStack pStack) (snd (splitCMDAndValue line), workingVal)

-- Looks for the index of the variable name in the stack,
-- returns the index if it finds it or -1 if it's not found.
-- an error will be represented in readVar if -1 is returned
-- as I thought it would be better to do error handling for it
-- in the function for reading the variable than a helper function
-- related to it.
findVarIndex :: T.Text -> [(T.Text, Double)] -> Integer -> Integer
findVarIndex varName [] _ = -1
findVarIndex varName (x:xs) n
    | varName == fst x = n
    | otherwise = findVarIndex varName xs (n+1)


-- This is called in readVar if findVarIndex returned something
-- other than -1, it will spit the stack elements at the index
-- returned by findVarIndex and create a new Stack where
-- with the head of the second item returned from the split
-- will be the new head of stack, first part of split second
-- and then third will be the second part of the split
-- minus the head.
putItemAtStackHead :: [(T.Text,Double)] -> Integer -> Stack
putItemAtStackHead xs n =
    let splitList = DL.genericSplitAt n xs
        secondHalf = DL.genericSplitAt 1 (snd splitList)
    in Stack (fst secondHalf ++ fst splitList ++ snd secondHalf)


-- Checks for the var name of the bytecode line in the stack
-- with findVarIndex and then if that returns it's index,
-- it'll use putItemAtStackHead to move the variable to the
-- head of stack for operation, else return an error that
-- the variable is not in the stack.
readVar :: T.Text -> Stack -> Stack
readVar line (Stack pStack) =
    let varName = snd (splitCMDAndValue line)
        varIndex = findVarIndex varName pStack 0
    in if varIndex == -1
        then error "Var does not exist in stack"
        else putItemAtStackHead pStack varIndex

 
-- Applies the arithmetic application defined below with head
-- of the stack as second operand and second item of stack
-- being the first operand (this makes it coincide nicely
-- with the example code) and then removes the 2 items used
-- in operation from the stack and inserts the result at the head.
-- It has quite a few different options for handling certain variable
-- positions in order to not delete them (based on shorter lengths
-- when variables are in play so they don't get removed when there
-- are only 2 or 3 items in the stack and some shuffling for
-- when the stack is bigger but there is 1 var in position 1 or
-- 2 or in both positions)
arithmeticApplication :: Stack -> (Double -> Double -> Double) -> Stack
arithmeticApplication (Stack pStack) operation =
    let workingVals = take 2 pStack
        execOpReturnVal 
            | length pStack < 2 = error "Not enough values to operate on"
            | otherwise = operation (snd (last workingVals)) (snd (head workingVals))
        removeOldStackVals [] = pStack
        removeOldStackVals [(_, _)] = pStack
        removeOldStackVals (x:y:xs)
            | length pStack == 2 && fst y /= "" && fst x /= "" = pStack
            | length pStack == 2 && fst y /= "" = [y]
            | length pStack == 2 && fst x /= "" = [x]
            | length pStack == 3 && fst y /= "" && fst x /= "" = xs ++ [x,y]
            | length pStack == 3 && fst y /= "" = xs ++ [y]
            | length pStack == 3 && fst x /= "" = xs ++ [x]
            | fst x /= "" && fst y /= ""  = xs ++ [x,y]
            | fst y /= ""  = xs ++ [y]
            | fst x /= ""  = xs ++ [x]
            | otherwise = xs
    in addToStack (removeOldStackVals pStack) ("", execOpReturnVal)


-- Commutative: adds first value to second value in the stack.
add :: Stack -> Stack
add pStack = arithmeticApplication pStack (+)

-- Commutative: multiplies second value by first value in the stack.
multiply :: Stack -> Stack
multiply pStack = arithmeticApplication pStack (*)

-- We were only shown commutative operations in the example byte code
-- so I made a bit of an assumption that the head would be the second
-- operand in math style operations (y in x / y or x - y), this works
-- nicely, given that parentheses and operation ordering would be
-- sorted out in bytecode generation.

-- Non-Commutative: divides second value by first value in stack.
divide :: Stack -> Stack
divide pStack = arithmeticApplication pStack (/)


-- Non-Commutative: subtracts first stack element from second.
subtract' :: Stack -> Stack
subtract' pStack = arithmeticApplication pStack (-)


-- sets up the tuple of n and operation type
getLoopNAndCommand :: T.Text -> (T.Text, T.Text)
getLoopNAndCommand line = (Arrow.***) T.strip T.strip $ T.breakOn " " $ T.strip $ snd $ T.breakOn " " line
    -- below is the old definition but it needed stripping on both sides in order to call LOAD_VAL so I modified
    -- it to do so.
    -- Arrow.first T.strip $ T.breakOnEnd " " $ snd $ T.breakOn " " line
   

-- loopVals will structure the command for input.
-- convert n to int converts the first element of the tuple into an int as well as doing some error handling,
-- I thought about extracting this functionality but I'd rather it fails before doing more processing if it's
-- going to have issues, I want to tackle performance on this code when I know more haskell, this code could
-- definitely be optimized a lot.
-- iteration is a basic recursive function that just does the n number of loops, using executeCommand
-- to fulfil the operation (it also does a check for trying to loop negative times and errors out).
-- Note that looping 0 times will just return the stack.
loop :: T.Text -> Stack -> Stack
loop line pStack =
    let loopVals = getLoopNAndCommand line
        convertNToInt :: (T.Text, T.Text) -> (Int, T.Text)
        convertNToInt (stringN, op')
            | not $ any (==True) $ zipWith T.isInfixOf ["LOAD_VAL", "SUBTRACT", "ADD", "DIVIDE", "MULTIPLY"] (repeat op')
                = error "You can only loop over loading values and arithmetic operations (Also double check casing)"
            | T.isInfixOf "LOAD_VAL" op' = (x, op')
            | x > length (stack pStack) - 1 = error "More loops than available items in stack, included variables"
            | otherwise = (x, op')
                where x = read $ T.unpack stringN :: Int
        iteration :: (Int, T.Text) -> Stack -> Stack
        iteration (n, op) pStack'
            | n < 0                          = error "You can't loop negative times"
            | n <= 0                         = pStack'
            | n > 0                          = iteration (n - 1, op) (executeCommand pStack' op)
            | otherwise                      = pStack'
    in  iteration (convertNToInt loopVals) pStack


-- at the moment this just returns the stack itself but I was
-- thinking of implementing just returning the resulting head
-- so that you could have a main stack and then use a separate
-- stack for functions but I was worried about causing issues
-- related to global vs local vars so left it for now, will
-- probably tackle it on a weekend sometime though.
return' :: Stack -> Stack
return' (Stack pStack) = Stack pStack


-- Calls the various commands based on the bytecode line.
-- will raise a "No such bytecode" error if invalid
-- bytecode for the interpreter is supplied.
-- It checks for which command to use with
-- commandNamePredicate.
-- In regards to looping, it's the first command because
-- commandNamePredicate will check for the command as an
-- infix.
executeCommand :: Stack -> T.Text -> Stack
executeCommand pStack line
    | cmd "LOOP" = loop line pStack
    | cmd "LOAD_VAL" = loadVal line pStack
    | cmd "WRITE_VAR" = writeVar line pStack
    | cmd "READ_VAR" = readVar line pStack
    | cmd "ADD" = add pStack
    | cmd "MULTIPLY" = multiply pStack
    | cmd "DIVIDE" = divide pStack
    | cmd "SUBTRACT" = subtract' pStack
    | cmd "RETURN_VALUE" = return' pStack
    | otherwise = error "No such bytecode command" -- pStack
    where cmd = commandNamePredicate line

-- will check if the given command is in the list of
-- possible commands by doing a substring search.
commandNamePredicate :: T.Text -> T.Text -> Bool
commandNamePredicate line command = T.isInfixOf command line


-- This runs the byte code by calling a foldl' with
-- executeCommand as the function, the stack as the
-- accumulator and the list of ByteCode instructions
-- as the list to operate on, then returns the stack
-- at the end of all operations.
runByteCode :: Stack -> ByteCode -> Stack
runByteCode programStack (ByteCode byteCodeList) = foldl' executeCommand programStack byteCodeList


printFinalStackResult :: Stack -> IO ()
printFinalStackResult (Stack pStack) = print $ TL.pack . show . snd . head $ pStack

main = do
    let programStack = Stack []
    contents <- TIO.getContents
    let x = getLinesOfByteCode contents
    -- putStrLn "\nThe ByteCode input after processing into list"
    -- print x
    -- putStrLn "\nThe empty stack"
    -- print programStack
    let answer = runByteCode programStack x
    -- putStrLn "\nThe stack after processing"
    -- print answer
    putStr "\n\nresult: "
    printFinalStackResult answer
