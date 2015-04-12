#Common Lisp

###CSCI3055u

####Caleb Phillips 100518555

________________________________

###Introduction

In this paper I will explore the language Common Lisp, one of the oldest surviving dialects of the original Lisp programming language written by John McCarthy.

Due to Lisp's unique feature of being able to define itself (i.e. write a compiler) in Lisp, there are many variations of the original language. Common Lisp was an attempt to standardize the language as defined in the ANSI standard document. There are many compilers/interpreters for Common Lisp, and the one in particular I have used for all of the code and testing in this document is CLISP. CLISP follows the standard put forth by the ANSI standard document, and includes a few extra features unique to the compiler.

The CLISP compiler is available at [http://www.clisp.org/](http://www.clisp.org/).

Throughout this document I will provide as many code examples as possible to back up the claims being made about the language. It can be assumed that all of these code examples have been tested, and produce an output similar to that shown in each code block. The code examples are either performed directly in the CLISP REPL, or saved in a .lisp file and loaded into the repl with the `load` function.

Code examples in this document are generally written as:

```

(some code) ; => Output

```

Comments in Common Lisp are denoted with a `;`, and since the written output is commented out in all of the code, it allows copy and paste execution of any of the examples.

Common Lisp code appears in two basic syntactic forms, atoms or S-expressions. Atoms are single entities or values that can appear without any sorrounding brackets. For example, `10` is an atom. S-expressions are the general bracketed expression that is common lisp dialects, and generally appear in the form `(function and some arguments or (other S-expressions))`.

My work in this book attempted to follow some standards used by LISP programmers for writing code. Although these standards are not required, they are worth mentioning as apparently it is social suicide to do otherwise amongst the LISP community. Variables and functions all use lower case letters when being defined and reference, and if they require multiple words, the words are separated by a hyphen. For example:

```

(defun my-function (my-variable) (princ my-variable))

```

One of the main reasons for using the dash as opposed to traditional camelCasing is because of the way Lisp handles it's symbols. When a symbol (which includes, but is not limited to, a variable or function name) is read by the CLISP compiler, it is automatically forced to all capital letters by defualt for later use or storage. For this reason also symbols are case insensitive, but more on this later.

An additional convention is to denote global variables by sorrounding them with asterisks. Again this is not required, but important. Because of the importance that Lisp and the functional paradigm places on pure functions, it is important to know when a function is affecting something outside of it's scope, and is therefore not pure. Pure functions of course meaning that they have no side effects, and only alter data internally.

###Type System

Common Lisp is a strongly, and dynamically typed language. Although it offers some support for type declarations, it is not commonly used. The purpose of type declarations in Common Lisp is to provide extra information to a compiler for optimizations when it may be necessary, although for most of my sample programs, it will not be. Many Common Lisp compilers claim the potential for code that is able to run faster than the C programming language, given that these optimizations are used properly.

There are several data types in Common Lisp, many are familiar in respect to our usual object oriented tools, and some are far more mathematical in nature.

#####Lists

The backbone of any Lisp are it's heterogenous structures known as list's. To understand the underlying implementation and meaning of lists consider the `cons` function.

```

(cons 1 '(2 3 4)) 	; => (1 2 3 4)

```

`cons` joins two lists or values together to create a new list. But why `cons`? What if we just `cons` together two elements, in this case symbols:

```
(cons 'red 'blue) 	; => (RED . BLUE)

```

What is that dot in the middle? The dot represents that this paired piece of data is actually a cons cell. A cons cell is simply a 2 boxed cell each containing a pointer to a piece of data. When we call `cons` on red and blue, we create a cons cell with the the first pointer pointing to our symbol 'red', and the second to 'blue'.

So why does this allow us to join 2 lists together? Simply because we are able to have each pointer point to a list. But it goes deeper than that, why exactly do cons cells help us understand lists? Because lists are not actually *lists* in the sense that we see them, but just a linked series of cons cells that each contain a pointer to a piece of data, and then a pointer to the next cell.

The actual underlying lying implementation looks alot like what we know as 'linked lists' in OOP languages. Consider the following Common Lisp code that defines a list of 3 numbers in 2 different ways, and then uses `eq` to check their equality.

```
(eq
	'(3 7 13)
	(cons 1 (cons 2 (cons 3 nil))))   ; => T (true)

```

We can visualize the underlying implementation of this structure's cons cells in much the same way we would if it was made in java with a linked list.

![](images/cons.png)

Each cells contains 2 boxes, the first containing a pointer to the value, and the second containing a pointer to the next cons cell. The final value contains a `nil`, a close enough equivalent of `null` in java, that signals the termination of the list.

We can see how efficient cons is able to join lists and values by simply changing the pointers in cons cells. This is much more efficient than having to move an entire list in memory.

So why did out cons cell of symbols turn out like `(RED . BLUE)`, and our list did not contain that dot. This is because lists are generally terminated with nil, and when we consed together 2 symbols the dot is telling us that the Common Lisp compiler found `BLUE` as a terminator rather than nil. 

The dot `.` that was used to indicate the last unexpected element of the list, is essentially a separate notation for `cons` and can be used as such.

```

'(3 . (7 . (13 . nil)))

```


It should be reiterated that from an equality standpoint, a consed together list is the same as a freshly declared list, because again, the underlying implementation is the same.

#####Numbers

Common Lisp supports numbers such as integers, floating point numbers, complex numbers, and even ratios:

```

(+ 1 2)      		; => 3

(+ 1.5 2.6)			; => 4.1

(+ 1.5s0 2.6s0)  	; => 4.1s0 (single precision)

(+ 1.5d0 + 2.6d0) 	; => 4.1d0 (double precision)

\#C(1 1)     		; => 1 + i

(/ 2 3)      		; => 2/3

```

Ratio's and other functions can be forced into evaluating as decimals by contaminating the ratio with a floating point number rather than an integer.

```

(/ 2.0 3) 			; => 0.6666667

(+ 1.0 2)			; => 3.0

```

Common Lisp also supports numbers in several different bases, for example:

```
\#b1101 			; => 13 (in binary)

\#o015  			; => 13 (in octal)

\#xd    			; => 13 (in hexadecimal)

```

#####Strings and Characters

Common Lisp supports Strings and Characters. String are defined by surrounding double quotations.

```
(princ "Hello, World!")

```

And characters are escaped with the combination `#\`.

```
(princ #\a)

```


#####Symbols

Symbols are common to any Lisp Dialect, and Common Lisp is no exception. Symbols are string like structures that are guarenteed to be interned into memory. This means that creating the same symbol twice will only create it once in memory, or more simply, creating a symbol a second time does absolutely nothing because it already exists. This allows using symbols to be generally faster than using Strings as things like checking for equality simply becomes a form of pointer comparison. In fact symbols have their own equality function called `eq`. It is very efficient but only available to symbols because of this interned quality.

It should be noted that symbols are case incensitive both in comparison and use, this is because they are automatically forced to all capital letters by Lisp when stored or used. For example:

```

(eq 'csci 'CSCI)	; => T (true)

```

Symbols exist because Common Lisp and other Dialects embed their own syntax as a data type of the language. They are generally useful for macros, constants and enums, and any situation where one would want to ensure it is only declared once.

#####Booleans

Boolean values in Common Lisp are especially interesting. Although there exists a literal `t`, for true, this is a formality and not generally necessary. This is because everything in Common Lisp is true, the only thing that is false is an empty list.

There do exist some pseudonym's for the empty list evaluation, namely:

```

'()
'nil
()
nil

```
Despite the different appearances of these symbols, they are all resolved to an empty list by the underlying compiler or interpreter.

The fact that any non empty list value evaluates to true has at least one obvious benefit. It allows two pieces of information, or return values, to be gathered from the output of a function. One can use the output of a function to check for the existance of a value, as well as use the value itself. For example:

```
(let ((x 5))
	(if x
		x
		'nothing-here))
		
					; => 5

```

###Equality

There are several functions for comparing values in Common Lisp, not all of them can be used for every value type. There are so many that it may be easier to create a small table to denote their application.

|          | eq  | equal | eql  | equalp  | =  |  string-equal  | char-equal  |
|----------|-----|-------|------|---------|----|----------------|-------------|
| lists    |     |   √   |      |         |    |                |             |
|symbols   |  √  |   √   |   √  |   √     |    |                |             |
| Integers |     |    √  |  √   |   √     | √  |                |             |
|floats    |     |   √   |  √   |   √     | √  |                |             |
|strings   |     |  √    |      |   √     |    |   √            |             |
|characters|     |  √    |  √   |    √    |    |                |   √         |

Although I may have missed a few cases of each function, this is a fairly exhaustive outline on how each functions *should* be used. But a few special notes should be made.

`equal` and `equalp` are very similar in application, the difference between the two is that `equalp` will provde a much looser definition of equality. For example it allows the integer 1 and float 1.0 to be considered equal. It also provides a case-insenstive matching for strings and characters. Neither of these 2 cases would pass with the main `equal` function.

As mentioned in the prior section the `eq` function is made specifically for symbols and should be used as such. If you are certain the data that you are comparing will be symbols it is always best to use this function as it provides the best performance and most accurate results for symbols.

As a general rule of thumb when learning, beginners are told that it is acceptable to use the `eq` funtion for symbols and `equal` for everything else.


###Some Basic Functions and Special Operators

Aside from the equality functions, there exists a few other mention worthy functions to make any further code snippets easier to understand. If any needed functions are explained elsewhere in the document I will skip over them here.


* `+`     	- add 
* `-`		- subtract
* `*`		- multiply
* `/` 		- divide 2 numbers, or if they are integers, return a ratio
* `and` 	- logical AND
* `or`		- logical OR
* `expt` 	- exponential
* `print`  - prints a string to the console, with a new line after
* `prin1` - prints a string to the console without a new line after.
* `princ` - prints a string to the console in a human readable format, as well as returning the value of the string.
* `read` - read data from console input, evaluation possible
* `read-line` - read a line of text from console input
* `oddp`  	- returns true if a value is odd, nil otherwise.
* `evenp` 	- returns true if a value is even, nil otherwise.
* `defun` 	- defines a function
* `defparameter` - defines a global variable, that can be redefined with `defparameter`
* `defvar` 	- defines a global variables that cannot be redefined with `defvar`
* `let` 		- defines local variable bindings
* `flet` 		- defines local function bindings
* `labels` 	- defines local functions, and allows those functions to call themselves or other functions declared within the same `labels` statement.
* `progn` - evaluates multiple S-expressions in order.
* `append` - join several lists into a larger one.
* `apply`  - pass each variable in a list as a parameter to a function.
* `member` - predicate to check the existance of an item in a list
* `push`   - add an item to the front of a list



It should be noted that functions that act as predicates are usually ended with the letter p. A predicate of course meaning that it returns true or false.

###Special functions for accessing List elements

There are 2 main functions for accessing list elements.

`car` - returns the first element in a list
`cdr` - returns a list of all the elements except the first.

Common Lisp has provided prebuilt combinations of these functions to allow accessing different elements. Simply add multiple `d`'s and `a`'s between the c and r to specify the order in which you want to access elements, for example:

```
(defparameter x '(1 2 3 4 5))

(equal 
	(car (cdr x)) 
	(cadr))         ; => T

```

Both of these S-expressions return the value 2. They first perform `cdr` to access everything but the first element, and then `car` to retrieve the first element of that new list.

Common Lisp has defined these expressions up to 4 deep/nested calls. Anything passed that must be defined by the user. Meaning that `cadddr` exists, but `caddddr` does not.



###Conditionals and Branching

#####If

The `if` function as we have already seen accepts three paramaters; a value or predicate, the branch to be executed if the predicate is true, and the branch to be executed if it is false. And as we recall, this will only be false if a function/predicate returns false, or the empty list is present. For example:

```

(if () 'this-will-not-print 'this-will-print)       ; => THIS-WILL-PRINT

(if (< 2 3) 'this-will-print 'this-will-not-print)  ; => THIS-WILL-PRINT


```

It should be noted that a feature of Lisp is it's laziness, and this applies to `if` and other branching techniques. For the branches of an `if` statement, or other branch, that are not selected for in the condition, that branches code block will not be evaluated. Note that both of our `'this-will-not-print` symbols above will not be evaluated by the compiler, simply because there is no need for them to be.

#####When and Unless

The `when` and `unless` function can serve a similar purpose to `if`. The only difference is they only provide a single branch as a code block, the code which will only be executed if the function is satisfied. `when` will evaluate when a function or predicate is true, and `unless` will evaluate when a function or predicate is false. For example:

```

(when t 'this-will-print)						    ; => THIS-WILL-PRINT

(unless t 'this-will-not-print)                     ; => NIL

```

#####Case

`case` provides a method of branching similar to the classic switch statements found in Java and C++. A parameter is provider and checked against several possible values and a defualt case. Should one of the values match, it's corresponding code block will be executed, otherwise the default block will be executed. (The default code block is coincidentally named 'otherwise')

```
(defun check-prof (prof-name) 
	(case prof-name 
				  	((randy) 'csci2040) 
				  	((ken) 'csci3055)
				  	((jeremy) 'csci2010)
				  	(otherwise '(i dont have a class with them))))
				  	
(check-prof 'ken) 			; => CSCI3055
				  	
```

`case` provides a powerful, readable, and very easy to use syntax. However it has a flaw, it's underlying implementation uses the `eq` function to check for equality by default, and if you recall from the previous section, `eq` is only a viable equality checker for symbols. Thus if you have the need for using Strings or numbers with a case function, you must find another route. 

#####Cond

`cond` is the mother of all Common Lisp branching statements, although it does not have the most beautiful syntax, it can essentially be used to replace any of the other branching techniques that we've talked about, as well as provide a few more use cases that the others don't. `cond` has a similar flow to the if-elseif-else construct of OOP languages. `cond` uses a series of predicates or functions to check for the first true response, it then executes the corresponding code block of that section. Although `cond` does not have a built in default case, one cas simply label the last predicate as `t` for true to ensure that it is executed if none of the others are.

```

(defun check-value (num)
	(cond ((equal 7 num) '(your number is seven))
		  ((equal 3 num) '(your number is three))
		  ((equal 13 num) '(your number is thirteen))
		  (t '(i dont know what your number is))))

(check-num 7) ; => YOUR NUMBER IS SEVEN
(check-num 1) ; => I DONT KNOW WHAT YOUR NUMBER IS

```

If a new Common Lisp programmer is going to remember a single method of branching, it should be `cond`.

#####AND and OR

The logical operators provide additional methods of branching that may not be completely obvious. Consider the following code:

```

(or (oddp 5) (princ "This will not be printed"))  ; => T (true)

(or (evenp 5) (princ "This will be printed"))     ; => This will be printed

```

These code snippets work in a similar way to the `unless` function that we saw earlier. The second body of the `or` function will only be evaluated if the first body is false. This works because of Common Lisp's laziness. If the first argument of `or` is found to be true, then the second argument will never need to be evaluated, because we now know that the overall result of the `or` will be true.

`and` can provide us with a similar shortcuts for a nested `if` structure. Consider the following code:

```

(if (= 2 2) (if (= 3 3) (princ "This will print"))) ; => This will print

```

This code prints the String specified in the deepest nested `if`, but only because the first 2 predicates evaluated to true. Now consider the following alternative to the above code:

```

(and (= 2 2) (= 3 3) (princ "This will print"))    ; => This will print

```

The final statement is evaluated, and therefore prints, only because the first two predicates evaluated to true. This, again, works because of Common Lisp's laziness. If either of the first 2 predicates had evaluated to false, there would have been no need to evaluate the latter ones, because at that point the compiler knows that the output of the `and` function will be false. This statement therefore gives the same functionality as the previous nested `if` statements, without out the need for the second function. Some of course would argue that it is perhaps less readable.

###Two Types of Code

Common Lisp has two types of code as seen by the compiler. There is 'code' and there is 'data'. Consider the following code snippet:

```

(cons (+ 1 2) '(2 1))  		; => (3 2 1)

```

As we know in Common Lisp, and functional programming in general, S-expressions are generally written in the form `(function data1 data2... etc.)`. Then why in this case was the first nested s-expression `(+ 1 2)` evaluated as a function, but the second just passed as a list of data? The answer is the single quotation in front of the second s-expression. This tells the compiler that the inner data is to be treated as exactly that, data. We can see that if we add the same single quote in front of the first nested s-expression, we will recieve different results:

```

(cons '(+ 1 2) '(2 1))		; => ((+ 1 2) 2 1)

```

Due to the precense of the single quote, the compiler now sees the first nested s-expression as simple data, rather than code to be executed.

We can also force a data list to evaluate using the `eval` function:

```
(defparameter *not-code* '(+ 2 1))

(eval *not-code*) 			; => 3

```

Despite, our declaration of `'(+ 2 1)` as a list of data, the eval function forces it to evaluate in the traditional method, taking the first item as the function and the rest as data, resulting in '3'.

Using the backtick \` we can even nest code inside of data. If we want most parts of a list to be evaluated as data, but a few nested sections as code, we use the backtick to start the data expression, and preceded any nested code sections with a comma. Consider the example below.

```
`(1 2 (+ 1 2))				; => (1 2 (+ 1 2))

`(1 2 ,(+ 1 2))				; => (1 2 3)

```
We denote the list as data with a back tick in both examples, but in the second example the nested S-expression is preceeded with a comma, telling the compiler to evaluate that expression as code, and then add it to the data.

What if we need to tell the compiler that a piece of code is specifically that, code, but it is not nested in a data backtick (meaning we cant use the comma). This is often the case for higher order functions, which can take another function as a parameter. We can specify a block of code by preceding it with the `#'` character pair. Consider the following code:

```

(mapcar #'sqrt '(1 4 9 16 25))
 
 ```
 
 `mapcar` takes 2 parameters, a function, and a list of values to apply that function to. It uses the function on every value in the list and returns a new list with the result. The above code works because we explicitly tell the compiler that the first parameter is code, and the second parameter is data. If we did not specify `sqrt` as a function, we would have received a `variable SQRT has no value` error. This is because we need to explicitly tell the compiler when we are naming a function instead of data, if it is not in that first position of the S-expression.
 
All of this means that Common Lisp is a homoiconic language. Homoiconic means that the language uses the same data structures to store its code as its data.
 

###Looping in Common Lisp


###Higher Order Functions and Lambda




















