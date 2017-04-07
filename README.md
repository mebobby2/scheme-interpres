# scheme-interpres

## Instructions

* Installing packages: stack install parsec
* Building project: stack build
* Run program: stack exec scheme-interpres-exe
* If running it requires arguments: stack exec scheme-interpres-exe John

## Notes

* In general, use >> if the actions don't return a value, >>= if you'll be immediately passing that value into the next action, and do-notation otherwise.

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


## Reference

https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

## Upto
https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Evaluation,_Part_1