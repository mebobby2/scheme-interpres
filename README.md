# scheme-interpres

## Instructions

* Installing packages: stack install parsec
* Building project: stack build
* Run program: stack exec scheme-interpres-exe
* If running it requires arguments: stack exec scheme-interpres-exe John

## Notes

* In general, use >> if the actions don't return a value, >>= if you'll be immediately passing that value into the next action, and do-notation otherwise.

>>= passes the result of the expression on the left as an argument to the expression on the right, in a way that respects the context the argument and function use

>> is used to order the evaluation of expressions within some context; it makes evaluation of the right depend on the evaluation of the left

* Each line of a do-block must have the same type

* ```parseNumber = liftM (Number . read) $ many1 digit```
It's easiest to read this backwards, since both function application ($) and function composition (.) associate to the right. The parsec combinator many1 matches one or more of its argument, so here we're matching one or more digits. We'd like to construct a number LispVal from the resulting string, but we have a few type mismatches. First, we use the built-in function read to convert that string into a number. Then we pass the result to Number to get a LispVal. The function composition operator . creates a function that applies its right argument and then passes the result to the left argument, so we use that to combine the two function applications.

Unfortunately, the result of many1 digit is actually a Parser String, so our combined Number . read still can't operate on it. We need a way to tell it to just operate on the value inside the monad, giving us back a Parser LispVal. The standard function liftM does exactly that, so we apply liftM to our Number . read function, and then apply the result of that to our parser.

* This style of programming—relying heavily on function composition, function application, and passing functions to functions—is very common in Haskell code. It often lets you express very complicated algorithms in a single line, breaking down intermediate steps into other functions that can be combined in various ways. Unfortunately, it means that you often have to read Haskell code from right-to-left and keep careful track of the types.

*
```
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
```
Our definition of unwordsList doesn't include any arguments. This is an example of point-free style: writing definitions purely in terms of function composition and partial application, without regard to individual values or "points". Instead, we define it as the composition of a couple of built-in functions. First, we partially-apply map to showVal, which creates a function that takes a list of LispVals and returns a list of their string representations. Haskell functions are curried: this means that a function of two arguments, like map, is really a function that returns a function of one argument. As a result, if you supply only a single argument, you get back a function of one argument that you can pass around, compose, and apply later. In this case, we compose it with unwords: map showVal converts a list of LispVals to a list of their string representations, and then unwords joins the result together with spaces.

* ```eval (List (Atom func : args)) = apply func $ map eval args```
This is another nested pattern, but this time we match against the cons operator ":" instead of a literal list. Lists in Haskell are really syntactic sugar for a chain of cons applications and the empty list: [1, 2, 3, 4] = 1:(2:(3:(4:[]))). By pattern-matching against cons itself instead of a literal list, we're saying "give me the rest of the list" instead of "give me the second element of the list". For example, if we passed (+ 2 2) to eval, func would be bound to "+" and args would be bound to [Number 2, Number 2].


## Reference

https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

## Upto
https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Error_Checking_and_Exceptions

Then we define a type to represent functions that may throw a LispError or return a value. Remember how parse used an Either data type to represent exceptions? We take the same approach here: