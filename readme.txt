A project I did during an internship, had to make a stack structure and be able to set and load variables, use literal values, perform arithmetic and loop.

ADDING LOOP FUNCTIONALITY (also includes the cabal2nix, how I did it will be listed at the bottom of this file):

I took out the web part of the initial program so we can just check the looping on a file for
ease of use, you can call it with:
nix-shell --command "cat bytecode/test_bytecode | cabal run"
replacing bytecode/test_bytecode with whatever file contains the
bytecode you want to run.

Please note that subtract is rather odd when using loops, if you chain subtractions, it will
keep minus-ing the head from second element, meaning once it's negative it will invert like so '- (-x)'
which can be kind of odd in the concept of looping, I tried out changing it using a flag and abs
to make it always lower the value but although that would be intuitive for looping in the traditional
sense, it doesn't really make sense for a repeating of a subtraction function as that's what's happening
here, so I decided to leave it as is.

I also allow looping for arithmetic and for loading new values only, the other commands will raise an
error that states such.

I thought about how I wanted looping to work and decided a nice way to go about it would be to do
LOOP <n> <command> and then it'll check if there are enough values in the stack to perform the operation
(I also thought about being able to pass a value with the loop like "LOOP 5 ADD 1" and then have it perform
the action multiple times but it would be pretty boring as it's just mapping).

So the main challenge is handling variables (do I let it get to variables in the stack if it doesn't explicitly
refer to them? given how the rest of it functions that would make sense as the bytecode just works from values and doesn't
have a separate stack for variables or anything), I just need to figure out how I'm going to work with moving the variables
to the bottom of the stack then after they've been used if they're called before the loop.


My initial plan is to rebuild the stack as:
tail:  
- no variables consumed and not fully looping through entire stack: this will pretty much just be the unconsumed elements.
- variables consumed and not fully looping through stack: right at the bottom will go variables, then other unconsumed elements,
    this is already handled by the normal variable use.
- variables consumed and fully loops: tail will just be variables consumed.
- no variables consumed, fully loops: tail will just be an empty stack if it was empty or just the variables if it fully looped until
    the variables but none were consumed.
- too many loops: handled by the not enough values error already technically but I'm going to introduce a length check so it can't just
    keep looping over variables infinitely since they don't get removed from the stack on use but rather pushed to the bottom.

head: will be the result of the looping.

Things went pretty well I think.

After some research on setting up cabal2nix, I decided to use the below command to generate a shell friendly default.nix as it
achieved everything I wanted:
cabal2nix --shell .  > default.nix 
