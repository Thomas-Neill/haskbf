# lang-A--
A flavor of lisp I wrote that compiles to Brainfuck. Exciting, huh? This probably won't get too complicated, but I'll try to update it periodically

## Syntax

The syntax is literally Lisp's, so it should be easy to learn.
All programs are started and terminated by parentheses.
Functions are called like so: ```(func arg1 arg2 arg3)```

## Builtin Functions

```(let name value)``` binds a value to a name. Pretty simple.

```(alloc size type)``` allocates ```size``` slots of brainfuck memory. Right now, the only type is str.

```(set memory value)``` sets the allocated memory to the given value - WARNING: only for memory given out via ```alloc``` (otherwise, use ```let```)

```(macro (args) (code) (more code) (the result here gets returned))``` It's an anynomyous function that is evaluated at compile-time. Assign it to a name with ```let```

```(debug value)``` prints value to the terminal as the program is being compiled.

```(print value)``` does exactly what you think, but it does not terminate its output with a newline.

```(+ value1 value2)``` Adds/concatenates value1 and value2

## Example Code

```
(
(let puts (macro (string) 
	(print (+ string "\n"))))
(let hello (alloc 5 str))
(set hello "hello")
(let world (alloc 5 str))
(set world "world")
(let WORLD (+ " " world))
(puts (+ hello WORLD))
)
```
