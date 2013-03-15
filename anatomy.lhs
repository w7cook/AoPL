% Anatomy of Programming Languages
% William R. Cook
% Copyright (C) 2013 %1

> --------------------BEGIN-HIDE-------------------------
> {-# OPTIONS -XRankNTypes #-}
> import Prelude hiding (LT, GT, EQ, id)
> import Data.Maybe
>
> main_list = [main'1, main'2, main'4, main'5, check1, main'6, main'10]
> main = do
>   sequence [ do
>     putStrLn ("---- " ++ show i ++ " ----")
>     cmd | (i, cmd) <- zip [1..] main_list]
>   return ()
> check msg a b = putStrLn (if a == b then "OK" else "*** CHECK " ++ msg ++ " Failed ***")
> --------------------END-HIDE-------------------------
> -- %OPTI1

 # Preliminaries

 ## Preface

 ### What?

This document is a series of notes about programming languages, originally written for
students of the undergraduate programming languages course at UT. %What2

 ### Why?

I'm writing these notes because I want to teach the theory of programming
languages with a practical focus, but I don't want to use Scheme (or ML) as the
host language. Thus many excellent books do not fit my needs, including
[*Programming Languages: Application and Interpretation*](http://cs.brown.edu/~sk/Publications/Books/ProgLangs)
by Shriram Krishnamurthi,
[*Essentials of Programming Languages*](http://www.cs.indiana.edu/eopl)
by Daniel P. Friedman, Mitchell Wand, and Christopher T. Haynes,
or [*Concepts in Programming Languages*](http://theory.stanford.edu/~jcm/books.html)
by John C. Mitchell.
Last year I asked Shriram if he would let me create an official
version of his textbook in Haskell, but he said "no."
Recently I realized that his book is published under Creative Commons license,
so I could create a derivative work from it. Shriram even offered
to let me have the source code, but in the end I decided to write a new book.
The title of this book is derived from another great book,
[*The Anatomy of Lisp*](http://www.amazon.com/Anatomy-Lisp-McGraw-Hill-computer-science/dp/007001115X)
by John Allen. %Why2

 ### Who?

These notes assume knowledge of programming, and in particular assume basic
knowledge of programming in Haskell. When I teach the course I give a few hours
of lectures to introduce Haskell. I teach the built-in data types including lists,
the basic syntax for conditionals, function definitions, function calls,
list comprehensions, and how to print out strings. I also spend a day on |data| definitions
(algebraic data types) and pattern matching.
Finally, I give a quick introduction to type classes so student will understand
how |Eq| and |Show| work. During the course I teach more advanced topics,
including first-class functions and monads. As background resources,
I point students to the many excellent tutorials on Haskell.
[Search Google for "Haskell Tutorial" to find one](https://www.google.com/search?q=Haskell+Tutorial).
I recommend
[Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
or the
[Gentle Introduction To Haskell](http://www.haskell.org/tutorial/). %Who2

 #### Acknowledgments

I thank the students in the spring 2013 semester of CS 345 *Programming Languages*
at the University of Texas at Austin, who helped out while I was writing the book.
Special thanks to Chris Roberts and Guy Hawkins for corrections and Aiden Song 
for careful proofreading. Tyler Allman Young
captured notes in class. Chris Cotter improved the makefile. %Ackn2

 ## Introduction

In order to understand programming languages, it is useful to spend some time thinking
about *languages* in general. Usually we treat language like the air we breath:
it is everywhere but it is invisible. I say that language is invisible because we are
usually more focused on the message, or the content, that is being conveyed than
on the structure and mechanisms of the language itself. Even when we focus on
our use of language, for example in writing a paper or a poem, we are
still mostly focused on the message we want to convey, while working with (or struggling
with) the rules and vocabulary of the language as a given set of constraints.
The goal is to work around and avoid problems.
A good language is invisible, allowing us to speak and write our intent clearly and
creatively. %Intr2

The same is true for programming. Usually we have some important goal in mind when
writing a program, and  the programming language is a vehicle to achieve the goal.
In some cases the language may fail us,
by acting as an impediment or obstacle rather than an enabler. The normal reaction in
such situations is to work around the problem and move on. %Intr5

The study of language, including the study of programming languages, requires a
different focus. We must examine the language itself, as an artifact. What are its rules?
What is the vocabulary? How do different parts of the language work together to
convey meaning? A user of a language has an implicit understanding of answers to
these questions. But to really study language we must create an explicit description
of the answers to these questions. %Intr4

The concepts of structure and meaning have technical names. The structure of
a language is called its *syntax*. The rules that defined the meaning of a language
are called *semantics*. Syntax is a particular way to structure information, while
semantics can be viewed as a mapping from syntax to its meaning, or interpretation.
The meaning of a program is usually some form of behavior, because programs
*do* things.
Fortunately, as programmers we are adept at describing the structure of information,
and at creating mappings between different kinds of information and behaviors.
This is what data structures and functions/procedures are for. %Intr6

Thus the primary technique in these notes is to use programming to study programming
languages. In other words, we will write programs to represent and manipulate programs.
One general term for this activity is *metaprogramming*. A metaprogram is
any program whose input or output is a program. Familiar examples of metaprograms
include compilers, interpreters, virtual machines. In this course we will read, write and
discuss many metaprograms. %Intr7

 # Expressions and Variables {#Chapter1}

 ## Simple Language of Arithmetic

A good place to start is analyzing the language of arithmetic, which is
familiar to every grade-school child: %Simp2

````Java
4
-5+6
3--2--7
3*(8+5)
3+(8*2)
3+8*2
````

These are examples of arithmetic *expressions*. The rules for understanding
such expressions are surprisingly complex. For example, in the third expression
the first and third minus signs ($-$) mean subtraction, while the second
and fourth mean that the following number is negative. The last two
examples mean the same thing, because of the rule that multiplication must be
performed before addition. The third expression is potentially confusing,
even given knowledge of the rules for operations. It means $(3 - (-2)) - (-7)$
not $3 - ((-2) - (-7))$ because subtraction associates to the left. %Simp4

Part of the problem here is that there is a big difference between our conceptual
view of what is going on in arithmetic and our particular conventions for expressing
arithmetic expressions in written form. In other words, there isn't any confusion about
what negative number are or what subtraction or exponentiation do, but there is
room for confusion about how to write them down. %Simp5

The conceptual structure of a given expression can be defined much more
clearly using pictures. For example, the following pictures make are a
clear description of the underlying arithmetic operations specified in the
expressions given above: %Simp6

![Graphical illustration of abstract structure](figures/abstract_syntax.eps) %Simp7

The last picture represents both the last expressions in the previous example.
This is because the pictures do not need parentheses, since the grouping
structure is explicit. %Simp8

The conceptual structure (illustrated by the pictures) is called the *abstract
syntax* of the language. The particular details and rules for writing expressions
as strings of characters is called the *concrete syntax*. The abstract
syntax for arithmetic expressions is very simple, while the concrete syntax
is quite complex. As a result, for now we will focus on the abstract syntax,
and deal with concrete syntax later. %Simp9

 ### Abstract Syntax in Haskell

Arithmetic expressions can be represented in Haskell with the following data type: %Abst2

> data Exp'1 = Number'1     Int
>          | Add'1        Exp'1 Exp'1
>          | Subtract'1   Exp'1 Exp'1
>          | Multiply'1   Exp'1 Exp'1
>          | Divide'1     Exp'1 Exp'1
> -- %Abst3

This data type defines four representational variants, one for numbers,
and three for the the binary operators of addition, subtraction, multiplication,
and division.
A number that appears in a program is called a *literal*. %Abst4

The five examples given above can be written as values of type |Exp| to
create five test cases: %Abst5

> -- 4
> t1 = Number'1 4
> -- -5 + 6
> t2 = Add'1 (Number'1 (-5)) (Number'1 6)
> -- 3 - (-2) - (-7)
> t3 = Subtract'1 (Subtract'1 (Number'1 3) (Number'1 (-2))) (Number'1 (-7))
> -- 3 * (8 + 5)
> t4 = Multiply'1 (Number'1 3) (Add'1 (Number'1 8) (Number'1 5))
> -- 3 + 8 * 2
> t5 = Add'1 (Number'1 3) (Multiply'1 (Number'1 8) (Number'1 2))
> -- %Abst6

NOTE: It is not legal to write |Add (-4) 6| because |-4| and |6|
are of type |Int| not |Exp|. Also, Haskell requires parentheses
around negative numbers, for some reason. %Abst7

Writing abstract syntax directly in Haskell is certainly very ugly.
There is approximately a 10-fold expansion in the number of characters
needed to represent a concept: a 5-character mathematical expression
|3 + 8 * 2| uses 47 characters to create the corresponding
Haskell data structure. This is not a defect of Haskell, it is
merely a side effect of the lack of a proper parser, which we haven't
developed yet.
Writing these data constructors explicitly is not something that we enjoy doing,
but for now it is useful to
be very explicit about the representation of our programs. %Abst8

For now expressions will be written using the
concise and familiar concrete syntax $3+7$, adding parentheses where
necessary. But keep in mind that this concise syntax is just
a short-hand for the real value |Add (Number 3) (Number 7)|.
As new features are added to the language, both the familiar
concrete syntax and the abstract syntax will be extended. %Abst9

TODO: talk about meta language: language of study versus language of implementation. Better words? %Abst10

 ### Evaluating Arithmetic Expressions

The normal meaning assigned to arithmetic expressions is the evaluation of the
arithmetic operators to compute a final answer. This evaluation process is
defined by cases in Haskell: %Eval2

> evaluate'1 :: Exp'1 -> Int
> evaluate'1 (Number'1 i)      = i
> evaluate'1 (Add'1 a b)       = evaluate'1 a + evaluate'1 b
> evaluate'1 (Subtract'1 a b)  = evaluate'1 a - evaluate'1 b
> evaluate'1 (Multiply'1 a b)  = evaluate'1 a * evaluate'1 b
> evaluate'1 (Divide'1 a b)    = evaluate'1 a `div` evaluate'1 b
> -- %Eval3

In Haskell, the two-argument function |div| can be used as an
infix operator by surrounding it in back-quotes.
Here is a main program that tests evaluation: %Eval4

> main'1 = do
>   putStrLn "Evaluating the following expression:"
>   putStr "  "
>   print t3
>   putStrLn "Produces the following result:"
>   putStr "  "
>   print (evaluate'1 t3)
> -- %Eval5

The output is %Eval6

    Evaluating the following expression:
      Subtract (Number 3) (Subtract (Number (-2)) (Number (-7)))
    Produces the following result:
      -2 %Eval7

This looks pretty good, except that the default |Show| format for
expressions is quite ugly. %Eval8

 ### Formatting Expressions

TODO: Question: is it useful to go into this much detail about formatting at this point?

Another way to interpret abstract |Exp| values is as
a string that corresponds to our normal way of writing arithmetic
expressions, with binary operators for |+|, |*|, |-| and |/|. %Form2

````
instance Show Exp where
  show (Number i)      = show i
  show (Add a b)       = showBinary a "+" b
  show (Subtract a b)  = showBinary a "-" b
  show (Multiply a b)  = showBinary a "*" b
  show (Divide a b)  = showBinary a "/" b
showBinary a op b = show a ++ op ++ show b
````

If you don't know about *instance* declarations in Haskell, please
go and read about *type classes*. (TODO: need citation here) %Form4

Note that the |show| function for expressions is fairly similar
to the |eval| function, but it performs string concatenation instead
of numeric operations. To test many different
kinds of functions, it is useful to define a generalized test function. %Form5

> test fun input = do
> --------------------BEGIN-HIDE-------------------------
>   putStr "    "
> --------------------END-HIDE-------------------------
>   putStr (show input)
>   putStr " ==> "
>   putStrLn (show (fun input))
> -- %Form6

The |test| function takes a function and an input as arguments. It
prints the input and then prints the result of applying the function to the input.
The following main program invokes |test| to evaluate each of the
five sample expressions defined above: %Form7

> main'2 = do
>   test evaluate'1 t1
>   test evaluate'1 t2
>   test evaluate'1 t3
>   test evaluate'1 t4
>   test evaluate'1 t5
> -- %Form8

Running this main program produces less than satisfactory results: %Form9

````Java
4 ==> 4
-4+6 ==> 2
3--2--7 ==> -2
1*8+5 ==> 13
1+8*2 ==> 17
````

We are back to the ambiguous expressions that we started with.
The ambiguity can be resolved by adding parentheses around
every expression: %Form11

````
showBinary a op b = paren (show a) ++ op ++ paren (show b)
````

> paren str = "(" ++ str ++ ")"
> -- %Form13

But the results are still not very satisfying: %Form14

````Java
4 ==> 4
(-4)+(6) ==> 2
(3)-((-2)-(-7)) ==> -2
````

There are either too many or too few parentheses. The right thing to do is
to check whether parentheses are needed, by comparing the *precedence* of
an operator with the *precedence* of the operators nested within it.
Multiplication |\*| has higher precedence than addition |+| because
|1+2\*3| means |1+(2\*3)| not |(1+2)\*3|. In what follows,
addition has precedence 1 and multiplication has precedence 2. %Form16

> instance Show Exp'1 where
>   show e = showExp 0 e
> -- %Form17

> showExp level (Number'1 i) = if i < 0 then paren (show i) else show i
> showExp level (Add'1 a b)       = showBinary level 1 a " + " b
> showExp level (Subtract'1 a b)  = showBinary level 1 a " - " b
> showExp level (Multiply'1 a b)  = showBinary level 2 a "*" b
> showExp level (Divide'1 a b)    = showBinary level 2 a "/" b
>
> showBinary outer inner a op b =
>   if inner < outer then paren result else result
>      where result = showExp inner a ++ op ++ showExp inner b
> -- %Form18

The add and subtract operators are also modified to include a little more space.
This definition produces an appealing result: %Form19

````Java
4 ==> 4
(-4) + 6 ==> 2
3 - (-2) - (-7) ==> -2
1*(8 + 5) ==> 13
1 + 8*2 ==> 17
````

The example of formatting expression is a concrete illustration of
the complexity of dealing with concrete syntax. The formatter
converts abstract syntax into readable text. A later chapter
presents a *parser* for expressions, which converts text into
abstract syntax. %Form21

 ### Errors

There are many things that can go wrong when evaluating an expression.
In our current, very simple language, the only error that can arise
is attempting to divide by zero. For example, consider this small expression: %Erro2

> testDBZ = evaluate'1 (Divide'1 (Number'1 8) (Number'1 0))
> -- %Erro3

In this case, the |div| operator in
Haskell throws a low-level error, which terminates execution of
the program and prints an error message: %Erro4

````
*** Exception: divide by zero
````

As our language becomes more complex, there will be many more kinds of
errors that can arise. For now, we will rely on Haskell to
terminate the program when these situations arise,
but in [Chapter 5](#Monads) we will investigate how to
manage errors within our evaluator. %Erro6

 ## Variables

Arithmetic expressions often contain variables in addition
to constants. In grade school the first introduction to variables
is usually to evaluate an expression where a variable has a specific value.
For example, young students learn to evaluate $x+2$ where $x=5$.
The rule is to substitute every occurrence of $x$ with the value $5$
and the perform the required arithmetic computations. %Vari2

To program this in Haskell, the first thing needed is to extend
the abstract syntax of expressions to include variables.
Since the name of a variable "x" can be represented
as a string of characters, it is easy to represent variables as an additional
kind of expression. The following data definition modifies |Exp| to include
a |Variable| case. %Vari3

> data Exp'2 = Number'2   Int
>          | Add'2      Exp'2 Exp'2
>          | Subtract'2 Exp'2 Exp'2
>          | Multiply'2 Exp'2 Exp'2
>          | Divide'2   Exp'2 Exp'2
>          | Variable'2 String        -- added
> --------------------BEGIN-HIDE-------------------------
>    deriving Eq
> instance Show Exp'2 where
>   show e = showExp'2 0 e
>
> showExp'2 level (Number'2 i) = if i < 0 then paren (show i) else show i
> showExp'2 level (Add'2 a b)       = showBinary'2 level 1 a " + " b
> showExp'2 level (Subtract'2 a b)  = showBinary'2 level 1 a " - " b
> showExp'2 level (Multiply'2 a b)  = showBinary'2 level 2 a "*" b
> showExp'2 level (Divide'2 a b)    = showBinary'2 level 2 a "/" b
> showExp'2 level (Variable'2 a)    = a
> showBinary'2 outer inner a op b =
>   if inner < outer then paren result else result
>      where result = showExp'2 inner a ++ op ++ showExp'2 inner b
> --------------------END-HIDE-------------------------
> -- %Vari4

An association of a variable $x$ with a value $v$ is called a *binding*,
which can be written $x \mapsto v$.
Bindings can be represented in Haskell as a pair. For example, the
binding of $x \mapsto 5$ can be represented as |(\"x\", 5)|. %Vari5

 ### Variable Discussion

We are used to calling $x$ and $y$ "variables" without really thinking
much about what it means to be a "variable". Intuitively a variable is something
that varies. But what does it mean for a name like $x$ to vary?
My view on this it that we call them variables because they can have
different values in different contexts. For example, the equation
$\pi r^2$ defines a relationship between several variables, but
in the context of a particular word problem, the radius $r$ has a
particular value.  In any particular context,
a variable does *not* vary or change. It has exactly one
value that is fixed and constant within that context. A variable can
be bound to different values in different contexts, but in a given
context the binding of a variable is fixed. In the examples above, the
context is indicated by the phrase "where $x=5$". The same expression,
$x+2$ can be evaluated in different contexts, for example, where $x=7$. %Vari2

This interplay between being constant and being variable can be quite confusing,
especially since variables in most programming languages *can change*
over time. The process of actually changing a variable's value over time, within a
single context, is called *mutation*.
This seems like a major difference between programming language
variables and mathematical variables. However, if you think about things
in a slightly different way then it is possible to unify these two
apparently conflicting means for "variable". As a preview, we will keep
the idea of variables having a fixed binding, but introduce the concept
of a *mutable container* that can change over time. The
variable will then be bound to the container. The variable's binding will
not change (it will remain bound to the same container), but the contents
of the container will change. %Vari3

Mutatable variables this will be discussed in full later.
For now, just remember that a variable has a fixed binding to a value
in a given context. %Vari4

Note that another common use for variables is to define *equations* or
*constraints*. In this case, it is normal to use algebraic operations to
simplify or *solve* the equation to find a value for the variable that
satisfies the equation. While equation solving and constraints are fascinating topics,
we will not discuss them directly in these notes. For our purposes, we will
assume that we already know the value of the variable, and that the problem
is to compute a result using that value. %Vari5

 ## Substitution

Substitution replaces a variable with a value in an expression.
Here are some examples of substitution: %Subs2

* substitute $x \mapsto 5$ in $x+2$   $\longrightarrow$ $5+2$ %Subs3
* substitute $x \mapsto 5$ in $2$     $\longrightarrow$ $2$ %Subs4
* substitute $x \mapsto 5$ in $x$     $\longrightarrow$ $5$ %Subs5
* substitute $x \mapsto 5$ in $x*x+x$ $\longrightarrow$ $5*5+5$ %Subs6
* substitute $x \mapsto 5$ in $x+y$   $\longrightarrow$ $5+y$ %Subs7

Note that if the variable names don't match, they are left alone.
Given these data types, the process of *substitution* can be defined by cases.
The following Haskell function implements this behavior: %Subs8

> substitute1:: (String, Int) -> Exp'2 -> Exp'2
> substitute1 (var, val) exp = subst exp
>  where
>   subst (Number'2 i)      = Number'2 i
>   subst (Add'2 a b)       = Add'2 (subst a) (subst b)
>   subst (Subtract'2 a b)  = Subtract'2 (subst a) (subst b)
>   subst (Multiply'2 a b)  = Multiply'2 (subst a) (subst b)
>   subst (Divide'2 a b)    = Divide'2 (subst a) (subst b)
>   subst (Variable'2 name) = if var == name
>                           then Number'2 val
>                           else Variable'2 name
> -- %Subs9

The first case says that substituting a variable for a value in
a literal expression leaves the literal unchanged. The next three cases define
substitution on binary operators as recursively substituting into the sub-expressions
of the operator. The final case is the only interesting one. It defines
substitution into a |Variable'2| expression as a choice: if the variable in the
expression (|name|) is the *same* as the variable being substituted (|var|)
then the value is %Subs10

> x = Variable'2 "x"
> y = Variable'2 "y"
> main'4 = do
>   test (substitute1 ("x", 5)) (Add'2 x (Number'2 2))
>   test (substitute1 ("x", 5)) (Number'2 2)
>   test (substitute1 ("x", 5)) x
>   test (substitute1 ("x", 5)) (Add'2 (Multiply'2 x x) x)
>   test (substitute1 ("x", 5)) (Add'2 x y)
> -- %Subs11

Running these tests produces the following results: %Subs12

````Java
x + 2 ==> 5 + 2
2 ==> 2
x ==> 5
x*x + x ==> 5*5 + 5
x + y ==> 5 + y
````

It is important to keep in mind that there are now two stages for
evaluating an expression containing a variable. The first stage
is to *substitute* the variable for its value, then the second
stage is to *evaluate* the resulting arithmetic expression. %Subs14

TODO: talk about *renaming* variables, or substituting one variable for another %Subs15

 ### Multiple Substitution using Environments

There can be multiple variables in a single expression. For example,
evaluating $2*x+y$ where $x=3$ and $y=-2$. A collection of bindings
is called an *environment*. %Mult2

Since a binding is represented as a pair, an environment can be
represented as a list of pairs. The environment mentioned above
would be %Mult3

> e1 = [ ("x", 3), ("y", -1) ]
> -- %Mult4

The corresponding type is %Mult5

> type Env = [(String, Int)]
> -- %Mult6

The substitution function is easily modified to work with
environments rather than single bindings: %Mult7

> substitute:: Env -> Exp'2 -> Exp'2
> substitute env exp = subst exp where
>   subst (Number'2 i)      = Number'2 i
>   subst (Add'2 a b)       = Add'2 (subst a) (subst b)
>   subst (Subtract'2 a b)  = Subtract'2 (subst a) (subst b)
>   subst (Multiply'2 a b)  = Multiply'2 (subst a) (subst b)
>   subst (Divide'2 a b)    = Divide'2 (subst a) (subst b)
>   subst (Variable'2 name) =
>     case lookup name env of
>       Just val -> Number'2 val
>       Nothing  -> Variable'2 name
> -- %Mult8

The last case is the only one that is different from the previous
definition of substitution for a single binding. It uses the
|lookup| function to search the list of bindings to find
the corresponding value (|Just val|) or |Nothing| if the variable
is not found. For the |Nothing| case, the substitute function
leaves the variable alone. %Mult9

> z = Variable'2 "z"
> main'5 = do
>   test (substitute e1) (Add'2 x y)
>   test (substitute e1) (Number'2 2)
>   test (substitute e1) x
>   test (substitute e1) (Add'2 (Multiply'2 x x) x)
>   test (substitute e1) (Add'2 x (Add'2 (Multiply'2 (Number'2 2) y) z))
> -- %Mult10

The test results show that multiple variables are substituted with
values, but that unknown variables are left intact: %Mult11

````Java
x + y ==> 3 + (-1)
2 ==> 2
x ==> 3
x*x + x ==> 3*3 + 3
x + 2*y + z ==> 3 + 2*(-1) + z
````

Note that it is also possible to substitute multiple variables one at a time: %Mult13

> substitute'2 env exp = foldr substitute1 exp env
> -- %Mult14

The |foldr fun init list| function applies a given function to each item
in a list, starting with a given initial value. %Mult15

> --------------------BEGIN-HIDE-------------------------
> exp1 = Add'2 x (Add'2 (Multiply'2 (Number'2 2) y) z)
> check1 = check "subst-fold" (substitute e1 exp1) (substitute'2 e1 exp1)
> --------------------END-HIDE-------------------------
> -- %Mult16

 ### Local Variables

So far all variables have been defined *outside*
the expression itself. It is also useful to allow variables to be
defined *within* an expression. Most programming languages support
this capability by allowing definition of *local variables*. %Loca2

In C or Java one can define local variables in a declaration: %Loca3

````C
int x = 3;
return 2*x + 5;
````

JavaScript is similar but does not specify the type of the variable: %Loca5

````C
var x = 3;
return 2*x + 5;
````

Haskell and ML define local variables with a |let| expression: %Loca7

> test1 = let x = 3 in 2*x + 5
> -- %Loca8

In these languages |let| is an expression, because it can be
used inside other expressions: %Loca9

> test2 = 2 * (let x = 3 in x + 5)
> -- %Loca10

TODO: note that |where| in Haskell is similar to |let|. %Loca11

It is also possible to define multiple local variables in Java or C: %Loca12

````Java
int x = 3;
int y = x*2;
return x + y;
````

and Haskell or ML %Loca14

> test3 = let x = 3 in let y = x*2 in x + y
> -- %Loca15

which is equivalent to %Loca16

> test4 = let x = 3 in (let y = x*2 in x + y)
> -- %Loca17

In general a |let| expression has the following concrete syntax: %Loca18

|let| *variable* |=| *bound-expression* |in| *body* %Loca19

The meaning of a |let| expression is to evaluate the bound expression,
then bind the local variable to the resulting value, and then
evaluate the body of the expression %Loca20

In Haskell, a |let| expression can be represented by adding
another case to the definition of expressions: %Loca21

````
data Exp'3 = ...
         | Let'3 String Exp'3 Exp'3
````

where the string is the variable name, the first Exp is the bound expression
and the second expression is the body. %Loca23

 ### Scope

The *scope* of a variable is the portion of the text of a program
in which a variable is defined. Normally the scope of a local
variable is all of the body of the let in which the variable is defined.
However, it is possible for a variable to be redefined, which creates
a hole in the scope of the outer variable: %Scop2

![Variable Scope](figures/scopes.eps) %Scop3

In this example there are two variables named |x|. Even though
two variables have the same name, they are not the same variable. %Scop4

TODO: talk about *free* versus *bound* variables %Scop5

TODO: talk about renaming %Scop6

 ### Substituting into |Let| Expressions {#BasicSubst}

When substituting a variable into an expression, care must
be taken to correctly deal with holes in the variable's scope.
In particular, when substituting for *x* in an expressions, if
there is an expression of the form |let| *x* |=| *e* |in| *body* then
*x* should be substituted within *e* but not in *body*.
Because *x* is redefined, the *body* is a hole in the scope of *x*. %Subs2

````
substitute1'3 (var, val) exp = subst exp
  ...
  subst (Let'3 x exp body)  = Let'3 x (subst exp) body'
    where body' = if x == var
                  then body
                  else subst body
````

In the |Let| case for |subst|, the variable is always substituted
into the bound expression |e|. But the substitution is only performed
on the body |b| if the variable |var| being substituted is *not* the
same as the variable |x| defined in the let expression. %Subs4

TODO: need some test cases here %Subs5

 ### Evaluating |Let| Expressions using Substitution

The evaluation of a let expression is based on substitution.
To evaluate |let| *x* |=| *e* |in| *b*,
first evaluate the bound expression *e*, then substitute its value
for variable *x* in the body *b*. Finally, the result of
substitution is evaluated. %Eval2

````
evaluate'3 :: Exp'3 -> Int
...
evaluate'3 (Let'3 x exp body) = evaluate'3 (substitute1'3 (x, evaluate'3 exp) body)
````

 There is no rule for evaluating a variable because all variables
 are substituted away before evaluation begins. %Eval4

TODO: need some test cases here %Eval5

 ### Undefined Variable Errors

With the introduction of variables into our language, a new kind of
error can arise: attempting to evaluate an expression
containing a variable that does not have a value. For example, these
expressions all contain undefined variables: %Unde2

````
x + 3
let x = 2 in x * y
(let x = 3 in x) * x
````

What will happen when these expressions are evaluated? The definition of
|evaluate| does not include a case for evaluating a variable. This is because all variables
should be substituted for values before evaluation takes place. If a variable
is not substituted then it is undefined. Since no case is defined for
|evaluate| of a |Variable|, Haskell terminates the program and prints this
error message: %Unde4

````
 *** Exception: anatomy.lhs: Non-exhaustive patterns in function evaluate'3
````

The fact that a variable is undefined is a *static* property of the
program: whether a variable is undefined depends only on the text of the program,
not upon the particular data that the program is manipulating. This is different
from the divide by zero error, which depends upon the particular data that the program is manipulating.
As a result, divide by zero is a *dynamic* error. Of course, it might be possible to identify, just from
examining the text of a program, that it will always divide by zero. Alternatively,
it may be the case that the code containing an undefined variable is never
executed at runtime. Thus the boundary between static and dynamic errors is not
absolute. The issue of static versus dynamic properties of programs
is discussed in more detail later (TODO: reference to chapter on Types). %Unde6

 ### Summary

Here is the full code evaluation using substitution of a language
with local variables. %Summ2

> data Exp'3 = Number'3     Int
>          | Add'3        Exp'3 Exp'3
>          | Subtract'3   Exp'3 Exp'3
>          | Multiply'3   Exp'3 Exp'3
>          | Divide'3     Exp'3 Exp'3
>          | Variable'3   String
>          | Let'3        String Exp'3 Exp'3
>
> substitute1'3 (var, val) exp = subst exp
>  where
>   subst (Number'3 i)      = Number'3 i
>   subst (Add'3 a b)       = Add'3 (subst a) (subst b)
>   subst (Subtract'3 a b)  = Subtract'3 (subst a) (subst b)
>   subst (Multiply'3 a b)  = Multiply'3 (subst a) (subst b)
>   subst (Divide'3 a b)    = Divide'3 (subst a) (subst b)
>   subst (Variable'3 name) = if var == name
>                           then Number'3 val
>                           else Variable'3 name
>   subst (Let'3 x exp body)  = Let'3 x (subst exp) body'
>     where body' = if x == var
>                   then body
>                   else subst body
> -- %Summ3

> evaluate'3 :: Exp'3 -> Int
> evaluate'3 (Number'3 i)       = i
> evaluate'3 (Add'3 a b)        = evaluate'3 a + evaluate'3 b
> evaluate'3 (Subtract'3 a b)   = evaluate'3 a - evaluate'3 b
> evaluate'3 (Multiply'3 a b)   = evaluate'3 a * evaluate'3 b
> evaluate'3 (Divide'3 a b)     = evaluate'3 a `div` evaluate'3 b
> evaluate'3 (Let'3 x exp body) = evaluate'3 (substitute1'3 (x, evaluate'3 exp) body)
> -- %Summ4

 ## Evaluation using Environments {#BasicEvalEnv}

For the basic evaluator substitution and evaluation were
completely separate, but the evaluation rule for |let|
expressions involves substitution. %Eval2

One consequence of this
rule is that the body of every let expression is copied,
because substitution creates a copy of the expression with
variables substituted. When let expressions are *nested*,
the body of the inner let expression is copied multiple times.
In the following example, the expression |x\*y\*z| is copied
three times: %Eval3

> test5 = let x = 2 in
>   let y = x+1 in
>     let z = y+2 in
>       x*y*z
> -- %Eval4

The steps are as follows: %Eval5

Step                                 Result
-------------------------            -----------------------------------
initial expression                   |let x = 2 in|
                                     \ \ \ \ |let y = x+1 in|
                                     \ \ \ \ \ \ \ \ |let z = y+2 in x\*y\*z|
evaluate bound expression            |2| $\Rightarrow$ |2|
substitute x $\mapsto$ 2 in body     |let y = 2+1 in (let z = y+2 in 2\*y\*z)|
evaluate bound expression            |2+1| $\Rightarrow$ |3|
substitute y $\mapsto$ 3 in body     |let z = 3+2 in 2\*3\*z|
evaluate bound expression            |3+2| $\Rightarrow$ |5|
substitute z $\mapsto$ 5 in body     |2\*3\*5|
evaluate body                        |2\*3\*5| $\Rightarrow$ |30| %Eval6

While this is a reasonable approach it is not necessary. We
have already seen that multiple variables can be substituted
at the same time. Rather than performing the substitution
fully for each |let| expression, instead the |let|
expression can add another binding to the list
of substitutions being performed. %Eval7

> -- Evaluate an expression in an environment
> evaluate'4 :: Exp'3 -> Env -> Int
> evaluate'4 exp env = eval exp
>  where
>   eval (Number'3 i)      = i
>   eval (Add'3 a b)       = eval a + eval b
>   eval (Subtract'3 a b)  = eval a - eval b
>   eval (Multiply'3 a b)  = eval a * eval b
>   eval (Divide'3 a b)    = eval a `div` eval b
> -- %Eval8

>   eval (Variable'3 x)    = fromJust (lookup x env)
>   eval (Let'3 x exp body)     = evaluate'4 body newEnv
>     where newEnv = (x, eval exp) : env
> -- %Eval9

The helper function |eval| is defined in the scope of the |env|
argument of the main |evaluate| function. Since the environment
|env| does not change in most cases, it is convenient to not
have to pass it around on every call to |eval|. Note that the
final case, for |Let|, *does* change the environment so it
calls |evaluate| rather than |eval|. %Eval10

The case for |Let| first evaluates the bound expression in the
current environment |ev e|, then it creates a new
environment |newEnv| with that binds |x| to the value of
the bound expressions. It then evaluates the body |b| in the
new environment |newEnv|. %Eval11

The steps in evaluation with environments do not copy the expression: %Eval12

Environment                                         Evaluation
-------------------------------------------         -----------------------------------
$\emptyset$                                         |let x = 2 in|
                                                    \ \ \ \ |let y = x+1 in|
                                                    \ \ \ \ \ \ \ \ |let z = y+2 in x\*y\*z|
                                                    { evaluate bound expression |2|}
$\emptyset$                                         |2| $\Rightarrow$ |2|
                                                    { add new binding for |x| and evaluate body of let }
|x| $\mapsto$ 2                                     |let y = x+1 in (let z = y+2 in x\*y\*z)|
                                                    { evaluate bound expression |x+1| }
|x| $\mapsto$ 2                                     |x+1| $\Rightarrow$ |3|
                                                    { add new binding for |y| and evaluate body of let }
|y| $\mapsto$ 3, |x| $\mapsto$ 2                    |let z = y+2 in x\*y\*z|
                                                    { evaluate bound expression |y+2| }
|y| $\mapsto$ 3, |x| $\mapsto$ 2                    |y+2| $\Rightarrow$ |5|
                                                    { add new binding for |z| and evaluate body of let }
|z| $\mapsto$ 5, |y| $\mapsto$ 3, |x| $\mapsto$ 2   |x\*y\*z| $\Rightarrow$ |70| %Eval13

In the |Let| case of |eval|, a new environment |newEnv| is created and used
as the environment for evaluation of the body |b|. %Eval14

The new environments
add the additional bindings to the *front* of the list of environments.
Since |lookup| searches an environment list from left to right, it will
find the most recent enclosing binding for a variable, and ignore any
additional bindings. For example, consider the evaluation of this
expression: %Eval15

> test6 = let x = 9 in (let x = x*x in x+x)
> -- %Eval16

Environment                                         Evaluation
-------------------------------------------         -----------------------------------
$\emptyset$                                         |let x = 9 in (let x = x*x in x+x)|
                                                    { evaluate bound expression |9| }
$\emptyset$                                         |9| $\Rightarrow$ |9|
                                                    { add new binding for |x| and evaluate body of let }
|x| $\mapsto$ 9                                     |let x = x*x in x+x|
                                                    { evaluate bound expression |x*x| }
|x| $\mapsto$ 9                                     |x*x| $\Rightarrow$ |81|
                                                    { add new binding for |x| and evaluate body of let }
|x| $\mapsto$ 81, |x| $\mapsto$ 9                   |x+x| $\Rightarrow$ |162| %Eval17

Note that the environment contains two bindings for |x|, but only the first
one is used. Having multiple bindings for the same name implements the concept of 'holes'
in the scope of a variable: when a new binding for the same variable is added to the
environment, the original binding is no longer accessible. %Eval18

The old environment
is not changed, so there is no need to reset or restore the previous
environment. For example, evaluating the following expression
creates to extensions of the base environment %Eval19

> test7 = let x = 3 in
>   (let y = 3*x in 2+y) + (let z = 7*x in 1+z)
> -- %Eval20

The first |let| expressions creates an environment |x| $\mapsto$ 3 with a
single binding. The next two let expressions create environments %Eval21

|y| $\mapsto$ 9, |x| $\mapsto$ 3 %Eval22

|z| $\mapsto$ 21, |x| $\mapsto$ 3 %Eval23

Internally Haskell allows these two environments to share the definition
of the original environment |x| $\mapsto$ 3. %Eval24

The Haskell function |fromJust| raises an exception if its
argument is |Nothing|, which occurs when the variable named
by |x| is not found in the environment |env|. This is where
undefined variable errors arise in this evaluator. %Eval25

TODO: define *exception*? %Eval26

 #### Exercise: Multi-variable |let| expressions
 
Modify the |let| expression to take a list of bindings, rather than a single one. 
Modify the |evaluate| function to handle evaluation of multi-variable |let| expressions.

 ## More Kinds of Data: Booleans and Conditionals

In addition to arithmetic computations, it is useful for expressions
to include conditions and also return different kinds of values.
Until now our expressions have always returned |Int| results, because
they have only performed arithmetic computations. The type |Value|
is defined to support multiple different kinds of values: %More2

> data Value = Int  Int
>            | Bool Bool
>  deriving Eq
> -- %More3

> --------------------BEGIN-HIDE-------------------------
> instance Show Value where
>   show (Int i) = if i < 0 then paren (show i) else show i
>   show (Bool b) = show b
> --------------------END-HIDE-------------------------
> -- %More4

Some example values are |Bool True| and
|Int 3|. We will define additional kinds of values, including functions and lists, later.
Keep in mind that the first uses of |Int| and |Bool| in this type definition are
the *labels* for data variants, while the second uses are *types* that define what kind of
data are associated with that data variant. %More5

The abstract syntax of expressions can now be expanded to include operations involving
booleans. Some examples are $4 < 10$ and $3*10 = 7$. Once booleans are included
in the language, it is possible to define a *conditional* expression, with the following
concrete syntax: %More6

|if| *test* |then| *true-part* |else| *false-part* %More7

A conditional expression allows selection of one of two different values
based on whether a boolean is true or false. Note that a conditional *expression* is
expected to produce a value. This is different from the conditional *statement*
found in many languages (most notably C and Java), which executes one of two blocks but
does not produce a value. In these languages, conditional expressions are written
*test* |?| *true-part* |:| *false-part*. Haskell, however, only has
conditional expressions of the kind discussed here. %More8

Given a full set of arithmetic operators, some comparison operators
(equality |EQ|, less than |LT|, greater than |GT|, less than or equal |LE|),
plus |and|, |or| and |not| for
booleans, it is useful to generalize the abstract syntax to support a general notation
for binary and unary operators.
When an expression includes a value it is called a *literal* value. Literals generalize
the case of |Number| used above to include constants in an arithmetic expression.
The conditional expression is sometimes called a *ternary* operator because it has three arguments.
But since there is only one ternary operator, and also because a conditional expression
is fairly special, it is included directly as |If| expression.
These changes are implemented in the
following definition for the abstract syntax |Exp|: %More9

> data BinaryOp = Add | Sub | Mul | Div | And | Or
>               | GT | LT | LE | GE | EQ
>   deriving Eq
>
> data UnaryOp = Neg | Not
>   deriving Eq
>
> data Exp'4 = Literal'4   Value
>          | Unary'4     UnaryOp Exp'4
>          | Binary'4    BinaryOp Exp'4 Exp'4
>          | If'4        Exp'4 Exp'4 Exp'4
>          | Variable'4  String
>          | Let'4       String Exp'4 Exp'4
>   deriving Eq
> --------------------BEGIN-HIDE-------------------------
> instance Show BinaryOp where
>   show op = fromJust (lookup op [(Add, " + "), (Sub, " - "), (Mul, "*"), (Div, "/"), (And, "&"), (Or, " | "),
>                                   (LT, " < "), (LE, " <= "), (GE, " >= "), (GT, " > "), (EQ, " = ")])
> instance Show UnaryOp where
>   show op = fromJust (lookup op [(Neg, "-"), (Not, "not ")])
> instance Show Exp'4 where
>   show e = showExp'4 0 e
>
> showExp'4 level (Literal'4 v)     = show v
> showExp'4 level (Variable'4 a)    = a
> showExp'4 level (Unary'4 op a)    = show op ++ showExp'4 99 a
> showExp'4 level (Binary'4 op a b) = showBinary'4 level (precedence op) a op b
> showExp'4 level (Let'4 x exp body) =
>   if 0 < level then paren result else result
>      where result = "let " ++ x ++ " = " ++ showExp'4 0 exp ++ " in " ++ showExp'4 0 body
> showExp'4 level (If'4 c a b) =
>   if 0 < level then paren result else result
>      where result = "if " ++ showExp'4 0 c ++ " then " ++ showExp'4 0 a ++ " else " ++ showExp'4 0 b
> showBinary'4 outer inner a op b =
>   if inner < outer then paren result else result
>      where result = showExp'4 inner a ++ show op ++ showExp'4 inner b
> precedence op = fromJust (lookup op [(Add, 4), (Sub, 4), (Mul, 5), (Div, 5), (And, 2), (Or, 1),
>                                      (LT, 3), (LE, 3), (GE, 3), (GT, 3), (EQ, 3)])
> --------------------END-HIDE-------------------------
> -- %More10

Evaluation is then defined by cases as before. Two helper functions, |binary| and |unary| (defined below),
perform the actual computations for binary and unary operations, respectively. %More11

> type Env'1 = [(String, Value)]
>
> -- Evaluate an expression in an environment
> evaluate'5 :: Exp'4 -> Env'1 -> Value
> evaluate'5 exp env = eval exp
>   where
>     eval (Literal'4 v)      = v
>     eval (Unary'4 op a)     = unary op (eval a)
>     eval (Binary'4 op a b)  = binary op (eval a) (eval b)
>     eval (Variable'4 x)     = fromJust (lookup x env)
>     eval (Let'4 x exp body) = evaluate'5 body newEnv
>       where newEnv = (x, eval exp) : env
> -- %More12

The conditional expression first evaluates the condition, forces it to be a boolean,
and then evaluates either the *then* or *else* expression. %More13

>     eval (If'4 a b c)      = if fromBool (eval a)
>                            then eval b
>                            else eval c
> fromBool (Bool b) = b
> -- %More14

The binary and unary helper functions perform case analysis on the operator
and the arguments to compute the result of basic operations. %More15

> unary Not (Bool b) = Bool (not b)
> unary Neg (Int i)  = Int (-i)
>
> binary Add (Int a)  (Int b)  = Int (a + b)
> binary Sub (Int a)  (Int b)  = Int (a - b)
> binary Mul (Int a)  (Int b)  = Int (a * b)
> binary Div (Int a)  (Int b)  = Int (a `div` b)
> binary And (Bool a) (Bool b) = Bool (a && b)
> binary Or  (Bool a) (Bool b) = Bool (a || b)
> binary LT  (Int a)  (Int b)  = Bool (a < b)
> binary LE  (Int a)  (Int b)  = Bool (a <= b)
> binary GE  (Int a)  (Int b)  = Bool (a >= b)
> binary GT  (Int a)  (Int b)  = Bool (a > b)
> binary EQ  a        b        = Bool (a == b)
> -- %More16

TODO: talk about strictness! %More17

Using the new format, here are the expressions for the test
cases given above: %More18

> -- 4
> t1'4 = Literal'4 (Int 4)
> -- -4 - 6
> t2'4 = Binary'4 Sub (Literal'4 (Int (-4))) (Literal'4 (Int 6))
> -- 3 - (-2) - (-7)
> t3'4 = Binary'4 Sub (Literal'4 (Int 3))
>                 (Binary'4 Sub (Literal'4 (Int (-2))) (Literal'4 (Int (-7))))
> -- 3*(8 + 5)
> t4'4 = Binary'4 Mul (Literal'4 (Int 3))
>                 (Binary'4 Add (Literal'4 (Int 8)) (Literal'4 (Int 5)))
> -- 3 + 8 * 2
> t5'4 = Binary'4 Add (Literal'4 (Int 3))
>                 (Binary'4 Mul (Literal'4 (Int 8)) (Literal'4 (Int 2)))
> -- %More19

In addition, new expressions can be defined to represent conditional expressions: %More20

> -- if 3 > 3*(8 + 5) then 1 else 0
> t6'4 = If'4 (Binary'4 GT (Literal'4 (Int 3)) t4'4)
>         (Literal'4 (Int 1))
>         (Literal'4 (Int 0))
> -- 2 + (if 3 <= 0 then 9 else -5)
> t7'4 = Binary'4 Add (Literal'4 (Int 2))
>                 (If'4 (Binary'4 LE (Literal'4 (Int 3))
>                                         (Literal'4 (Int 0)))
>                     (Literal'4 (Int 9))
>                     (Literal'4 (Int (-5))))
> --------------------BEGIN-HIDE-------------------------
> main'6 = do
>   test (\e-> evaluate'5 e []) t1'4
>   test (\e-> evaluate'5 e []) t2'4
>   test (\e-> evaluate'5 e []) t3'4
>   test (\e-> evaluate'5 e []) t4'4
>   test (\e-> evaluate'5 e []) t5'4
>   test (\e-> evaluate'5 e []) t6'4
>   test (\e-> evaluate'5 e []) t7'4
> --------------------END-HIDE-------------------------
> -- %More21

Running these test cases with the |test| function defined above yields these results: %More22

````Java
  4 ==> 4
  (-4) - 6 ==> (-10)
  3 - (-2) - (-7) ==> (-2)
  1*(8 + 5) ==> 13
  3 + 8*2 ==> 17
  if 3 > 1*(8 + 5) then 1 else 0 ==> 0
  2 + (if 3 <= 0 then 9 else (-5)) ==> (-3)
````

 ### Type Errors

Now that our language supports two kinds of values, it is possible for
an expression to get *type errors*. A type error occurs when evaluation of
an expression attempts to perform an operation but one or more of the
values involved are not the right type. For example, attempting to add an
integer and a boolean value, as in |3 + True|, leads to a type error. %Type2

In our Haskell program, type errors exhibit themselves in the
|binary| and |unary| functions, which match certain legal patterns of
operations, but leave illegal combinations of operations and arguments
undefined.  Attempting to evaluate |3 + True| results in a call to
|binary Add (Int 3) (Bool True)|, which is not one of the patterns
handled by the |binary| function. As a result, Haskell generates a
*Non-exhaustive pattern* error: %Type3

    Main> evaluate'5 [] (Binary'4 Add (Literal'4 (Int 3)) (Literal'4 (Bool True)))
    *** Exception: Non-exhaustive patterns in function binary %Type4

Here are some examples of expression that generate type errors: %Type5

> -- if 3 then 5 else 8
> err1 = If'4 (Literal'4 (Int 3)) (Literal'4 (Int 5)) (Literal'4 (Int 8))
> -- 3 + True
> err2 = Binary'4 Add (Literal'4 (Int 3)) (Literal'4 (Bool True))
> -- 3 || True
> err3 = Binary'4 Or (Literal'4 (Int 3)) (Literal'4 (Bool True))
> -- -True
> err4 = Unary'4 Neg (Literal'4 (Bool True))
> -- %Type6

We will discuss techniques for preventing type errors later, but for now
it is important to realize that programs may fail at runtime. %Type7

 # Functions

Functions are familiar to any student of mathematics. The first hint of
a function in grade school may be some of the standard operators that
are introduced early in the curriculum. Examples include absolute value
$\mid x \mid$ and square root $\sqrt{x}$. The concept of a function is also implicit in the
standard equation for a line $y = mx + b$. Trigonometry introduces
the standard functions *sin(a)* and *cos(a)* to support computation on angles.
While these operators use more traditional function syntax, they are
still considered predefined computations, much like absolute value
or square root.
However, the concept of a function as an explicit object of study is
not usually introduced until calculus. %Func2

Programming languages all support some form of function definition.
A function allows a computation to be written down once and reused many times. %Func3

TODO: explain why this is about "first-order" and "top-level" functions. %Func4

 ## Top-Level Function Definitions

Some programming languages, including C and ACL2, allow functions to be defined only
at the top level of the program. The "top level" means outside of
any expression. In this case, the program itself is a list of
function definitions followed by a main expression. The
main expression in a C program is an implicit call to a
function named |main|. Even if a programming language does support more flexible
definition of functions, top-level functions are quite common.
Here is an example of some top-level functions, written in JavaScript: %Top2

````Java
// compute n raised to the m-th power
function power(n, m) {
  if (m == 0)
    return 1;
  else
    return n * power(n, m - 1);
} %Top3

function main() {
  return power(3, 4);
}
````

------BEGIN-HIDE-------
edited "we" up to this point
------END-HIDE--------- %Top5

This code resembles C or Java, but without types.
Our expression language does not need
|return| statements, because every expression automatically returns
a value. A similar program can be written in Haskell, also
without return statements: %Top6

````
power(n, m) =
  if (m == 0) then
    1
  else
    n * power(n, m - 1) %Top7

main =         -- not really a valid Haskell main function
  power(3, 4)
````

These examples provides an outline for the basic concrete syntax of a function: %Top9

|function| *function-name* |(| *parameter-name*, ..., *parameter-name* |)| *body-expression* %Top10

The exact syntax varies from language to language. Some languages
begin with a keyword |function| or |def|. Other languages require brackets
|{| ... |}| around the body of the function.
These functions are less powerful than Haskell, because they take a
simple parameter list rather than a full pattern. But this simple form
of function defined above captures the essence of function definition
in many languages. %Top11

A call to a function is an expression that has the following concrete syntax: %Top12

*function-name* |(| *expression*, ..., *expression* |)| %Top13

Again, there are some variations on this theme. For example,
in Haskell the parentheses are optional. The program has a
series of named functions, each of which has a list of parameter
names and a body expression. The following data type definitions
provide a means to represent such programs: %Top14

> type FunEnv = [(String, Function'6)]
> data Function'6 = Function'6 [String] Exp'6
> -- %Top15

A list of function definitions is a *function environment*.
This list represents a list of bindings of function names
to function definitions. %Top16

A program is then a function environment together with a
main expression: %Top17

> data Program = Program FunEnv Exp'6
> -- %Top18

Any of the expressions can contain calls to the top-level
functions. A call has a function name and a list
of actual argument expressions: %Top19

````
data Exp'6 = ...
         | Call'6      String [Exp'6]
````

As an example, here is an encoding of the example program: %Top21

> f1 = Function'6 ["n", "m"]
>       (If'6 (Binary'6 EQ (Variable'6 "m") (Literal'6 (Int 0)))
>           (Literal'6 (Int 1))
>           (Binary'6 Mul
>             (Variable'6 "n")
>             (Call'6 "power" [Variable'6  "n",
>                            Binary'6  Sub (Variable'6  "m")
>                                          (Literal'6 (Int 1))])))
>
> p1 = Program [("power", f1)]
>              (Call'6 "power" [Literal'6 (Int 3),
>                             Literal'6 (Int 4)])
> -- %Top22

 ### Evaluating Top-Level Functions {#EvalTopLevel}

A new function, |execute|, runs a program. It does so
by evaluating the main expression in the context of the
programs' function environment and an empty variable environment: %Eval2

> execute :: Program -> Value
> execute (Program funEnv main) = evaluate'6 main [] funEnv
> -- %Eval3

The evaluator is extended to take a function environment |funEnv| as
a additional argument. %Eval4

````
-- Evaluate an expression in a variable environment with a given function environment
evaluate'6 :: Exp'6 -> Env'1 -> FunEnv -> Value
evaluate'6 exp env funEnv = eval exp
  where
    ...
     eval (Call'6 fun args)   = evaluate'6 body newEnv funEnv
       where Function'6 xs body = fromJust (lookup fun funEnv)
             newEnv = zip xs [eval a | a <- args]
````

Evaluation of a call expression performs the following steps: %Eval6

1. Look up the function definition by name |lookup fun funEnv|,
   to get the functions' parameter list |xs| and |body|.
2. Evaluate the actual arguments |[eval a BAR a <- args]| to get a list of values
3. Create a new environment |newEnv| by zipping together the
    parameter names with the actual argument values.
4. Evaluate the function |body| in the new environment |newEnv| %Eval7

TODO: work out an example to illustrate evaluation of functions? %Eval8

The only variables that can be used in a function body are the
parameters of the function. As a result, the only environment
needed to evaluate the function body is the new environment
created by zipping together the parameters and the actual arguments. %Eval9

The evaluator now takes two environments as input: one for
functions and one for normal variables. A given name is
always looked up in one or the other of these two environments,
and there is never any confusion about which place to look.
The certainty about where to look up a name comes from the
the fact that the names appear in completely different places
in the abstract syntax: %Eval10

````
data Exp = ...
     | Variable'6  String         -- variable name
     | Call'6      String [Exp'6]   -- function name
````

A variable name is tagged as a |Variable| and a function name
appears in a |Call| expression. %Eval12

Because the names of function and the names of variables are
completely distinct, they are said to be in different *namespaces*.
The separation of the variable and function namespace is clear
in the following (silly) example: %Eval13

````Java
    function pow(pow)
      if pow <= 0 then
        2
      else
        let pow = pow(pow - 1) in
          pow * pow(pow - 2)
````

This is the same as the following function, in which variables
are renamed to be less confusing: %Eval15

````Java
    function pow(a)
      if a <= 0 then
        2
      else
        let b = pow(a - 1) in
          b * pow(b - 2)
````

When renaming variables, the *functions* are *not* renamed.
This is because functions and variables are in separate namespaces. %Eval17

Another consequence of the separation between variable and
function namespaces is that functions can not be passed as arguments
to other functions, or returned as values from functions.
In the expression |pow(pow)| the two uses of |pow| are
completely distinct. This is analogous to the concept of a
[*homonym*](http://en.wikipedia.org/wiki/Homonym)
in natural languages like English. The exact same word has two
completely different meanings, which are distinguished only by
context. English has many homonyms, including 'stalk' and 'left'.
In our expression language, the first |pow| must mean the
function because it appears in front of a parenthesis where
a function name is expected, while
the second |pow| must be a variable because it appears where
an expression is expected. %Eval18

In this language functions are *not* values. When something
is treated specially in a programming language, so that it
cannot be used where a any value is allowed, it is called
*second class*. %Eval19

It is worth noting that many of the example functions presented above,
including |power| and |pow|, are *recursive*. Recursion is possible
because the function definitions can be used in any expression,
including in the body of the functions themselves. This means that
all functions have *global scope*. %Eval20

 ### Stack Diagrams

TODO: illustrate how stacks work in languages that don't have
first-class functions %Stac2

 ### Summary

Here is the full code for the evaluator supporting
top-level functions definitions. %Summ2

> data Exp'6 = Literal'6   Value
>          | Unary'6     UnaryOp Exp'6
>          | Binary'6    BinaryOp Exp'6 Exp'6
>          | If'6        Exp'6 Exp'6 Exp'6
>          | Variable'6  String
>          | Let'6       String Exp'6 Exp'6
>          | Call'6      String [Exp'6]
>
> evaluate'6 :: Exp'6 -> Env'1 -> FunEnv -> Value
> evaluate'6 exp env funEnv = eval exp
>   where
>     eval (Literal'6 v)      = v
>     eval (Unary'6 op a)     = unary op (eval a)
>     eval (Binary'6 op a b)  = binary op (eval a) (eval b)
>     eval (If'6 a b c)       = if fromBool (eval a)
>                             then eval b
>                             else eval c
>     eval (Variable'6 x)     = fromJust (lookup x env)
>     eval (Let'6 x exp body) = evaluate'6 body newEnv funEnv
>       where newEnv = (x, eval exp) : env
>     eval (Call'6 fun args)   = evaluate'6 body newEnv funEnv
>       where Function'6 xs body = fromJust (lookup fun funEnv)
>             newEnv = zip xs [eval a | a <- args]
> -- %Summ3

 #### Exercise: Stack-based evaluation
 
Modify the evaluator for top-level functions to use a stack with a list of
values, rather than an environment. Use a function to look up the position of
a variable in an argument list, then access the corresponding position from
the top of the stack.

 ## First-Class Functions

In the [Section on Top-Level Functions](#TopLevel), function definitions were defined using
special syntax and only at the top of a program.
The function names and the variable names are in
different namespaces. One consequence of this is that all
the expressive power we have built into our language, for
local variables, conditionals and even functions,
does not work for creating function themselves. If you believe
that functions are useful for writing reusable computations, as suggested
above, then it should be useful to use functions to create
and operate on functions. In this section we rework the
concept of functions presented above to integrate them into
the language, so that functions are *first-class* values. %Firs2

Consider the following function definition: %Firs3

````
f(x) = x * 2
````

The intent here is to define |f|, but it doesn't really
say what |f| is, it only says what |f| does when applied to
an argument. A true definition for |f| would have the form |f = ...|. %Firs5

Finding a value for |f| is related the idea of solving equations
in basic algebra. For example, consider this equation: %Firs6

$x^2 = 5$ %Firs7

This means that $x$ is value that when squared equals $5$.
We can solve this equation to compute the value of $x$: %Firs8

$x = \sqrt{5}$ %Firs9

But this involved creating a new concept, the *square root*
of a number.
We know we have a solution for a variable when the variable
appears by itself on the left side of an equation. %Firs10

The function definition |f(x) = x * 2| is similar.
It means that |f| is a function that when applied to an
argument |x| computes the value |x * 2|.
*But we don't have a solution for* |f|, because |f| does
not appear on the left side of an equation by itself.
To 'solve for |f|' we need some new notation, just the
way that the square root symbol $\sqrt{\ }$ was introduced
to represent a new operation. %Firs11

 ## Lambda Notation

The standard solution is to use a *lambda expression*, or
*function expression*,
which is a special notation for representing a function.
Here is a solution for |f| using a lambda: %Lamb2

|f =| $\lambda$|x|. |x * 2| %Lamb3

The symbol $\lambda$ is the greek letter *lambda*. Just like
the symbol $\sqrt{\ }$, $\lambda$ has no inherent meaning, but
is assigned a meaning for our purposes. The general form of a
function expression is: %Lamb4

$\lambda$*var*. *body* %Lamb5

This represents a function with parameter *var* that computes a
result defined by the *body* expression. The *var* may of course
be used within the *body*. In other words, *var* may be
free in *body*, but *var* is bound (not free) in $\lambda$*var*. *body*.
A function expression is sometimes called an *abstraction* or a
*function abstraction* (TODO: discuss this more later). %Lamb6

Thus |f =| $\lambda$|x|. |x * 2| means that |f| is defined to be a function of one
parameter |x| that computes the result |x * 2| when
applied to an argument. One benefit of function expressions
is that we don't need special syntax to name functions,
which was needed in dealing with [top-level functions](#TopLevel). Instead, we can use the
existing |let| expression to name functions, because
functions are just another kind of value. %Lamb7

Lambda notation was invented in 1930s by
[Alonzo Church](http://en.wikipedia.org/wiki/Alonzo_Church),
who was investigating the foundations of functions.
Lambda notation is just one part of the
[*lambda calculus*](http://en.wikipedia.org/wiki/Lambda_calculus),
which is an extremely elegant analysis of functions. Lambda
calculus has had huge influence on programming languages.
We will study the lambda calculus in more detail in a
later section, but the basic concepts are introduced here. %Lamb8

 ### Using Lambdas in Haskell {#LambdaDefinition}

Haskell is based directly on the lambda calculus. In
fact, the example illustrating how to "solve" for the
function |f| can be written in Haskell. The following
definitions are all equivalent in Haskell: %Usin2

> f'1(x) = x * 2
> f'2 x  = x * 2
> f'3 = \x -> x * 2
> -- %Usin3

The last example uses Haskell's notation for writing a lambda expression.
Because $\lambda$ is not a standard character on most
keyboards (and it is not part of ASCII), Haskell uses
an *ASCII art* rendition of $\lambda$ as a backslash |\\|.
The dot used in a traditional lambda expression is replaced
by ASCII art |->| for an arrow. The idea is that the function
maps from |x| to its result, so an arrow makes some sense. %Usin4

The concept illustrated above is an important general rule,
which we will call the *Rule of Function Arguments*: %Usin5

\ \ \ \ |name var = body| \ \ \ \  $\equiv$  \ \ \ \   |name = \var -> body| %Usin6

A parameter can always be moved from the left of an
equality sign to the right. Haskell programmers prefer
to write them on the left of the equals if possible, thus
avoiding explicit use (and somewhat ugly ASCII encoding) of
lambdas. Technically in Haskell the |var| can be any
pattern, but for now we will focus on the case where
the pattern is just a single variable. (TODO: see later chapter?)
Since every function definition in Haskell is
implicitly a lambda expression, you have already been
using lambdas without realizing it. As the old
dishwashing soap commercial said "You are soaking in it." %Usin7

 ### Function Calls

A function call in Haskell is represented by placing
one expression next to another expression. Placing two
expressions next to each other is sometimes called
*juxtaposition*. It is useful to think of juxtaposition
as an operator much like |+| and |*|. The only difference
is that juxtaposition is the *invisible* operator.
In other words, just as |n+m| means addition,
|f n| means function call. This is not to say that the
space character is an operator, because the space is only
needed to separate the two characters, which otherwise would
be a single symbol |fn|. It is legal to add parenthesis,
yielding the more traditional function call syntax, |f(n)|,
just as it is legal (but useless) to add parentheses to
|n+(m)|. A function call in Haskell can also be written
as |(f)n| or |(f)(n)|. There are no spaces in these examples,
but they do exhibit juxtaposition of two expressions.[^1] %Func2

[^1]: Church's original presentation of the lambda calculus
followed the mathematical convention that all variables were
single characters. Thus |xy| means a function call, |x y|,
just as |xy| is taken to mean |x*y| in arithmetic expressions.
Normally in computer science we like to allow variables to
have long names, so |xy| would be the name of a single variable.
We don't like it when |foo| means |f(o(o))|. %Func3

Haskell has the property that definitions really are
equations, so that it is legal to substitute |f| for
|\x -> x * 2| anywhere that |f| occurs. For example,
we normally perform a function call |f(3)| by
looking up the definition of |f| and then evaluating
the body of the function in the normal way.
However, it is also legal to substitute |f| for its
definition. %Func4

````
-- version A
f(3)
````

In this form, the function |f| is *applied* to the argument |3|.
The expression |f(3)| is called a function *application*.
In this book I use "function call" and "function application"
interchangeably. %Func6

> -- version B
> test9 = (\x -> x*2)(3)
> -- %Func7

The A and B versions of this expression are equivalent. The latter is a
juxtaposition of a function expression |\x->x*2| with its argument, |3|.
When a function expression is used on its own, without giving it a name, it is
called an *anonymous function*. %Func8

The *Rule of Function Invocation* says that applying a function
expression to an argument is evaluated by substituting the
argument in place of the function's bound variable everywhere it occurs
in the body of the function expression. %Func9

**Rule of Function Invocation** (informal): %Func10

($\lambda$*var*. *body*)arg  \ \ \ **evaluates to** \ \ \  *body* with *arg* substituted for *var* %Func11

For now this is an informal definition.
We will make it more precise when we
write an evaluator that handles function expressions correctly. %Func12


 ## Examples of First-Class Functions {#FirstClassExamples}

Before we begin a full analysis of the semantics of first-class
functions, and subsequently implementing them in Haskell, it is useful
to explore some examples of first-class functions. Even if you have
used first-class functions before, you might find these examples
interesting. %Exam2

 ### Function Composition {#Compose}

One of the simplest examples of a using functions as values is
defining a general operator for *function composition*. The
composition $f \circ g$ of two functions $f$ and $g$ is a new function that
first performs $g$ on an input, then performs $f$ on the result.
Composition can be defined in Haskell as: %Func2

> compose f g = \x -> f(g x)
> -- %Func3

The two arguments are both functions, and the result of composition is
also a function. The type of |compose| is %Func4

> compose :: (b -> c) -> (a -> b) -> (a -> c)
> -- %Func5

As an example of function composition, consider two functions that
operate on numbers: %Func6

> square n = n * n
> mulPi m = pi * m
> -- %Func7

Now using composition we can define a function for computing the area of a circle,
given the radius: %Func8

> areaR = compose mulPi square
> -- %Func9

To compute the area given the diameter, we can compose this function with a function that
divides by two: %Func10

> areaD = compose areaR (\x -> x / 2)
> -- %Func11


 ### Mapping {#Map}

One of the earliest and widely cited examples of first class functions
is in the definition of a |map| function, which applies a function to
every element of a list, creating a new list with the results. %Mapp2

For example, given the standard Haskell function |negate|
that inverts the sign of a number, it is easy to quickly negate a list of numbers: %Mapp3

> testM1 = map negate [1, 3, -7, 0, 12]   -- returns [-1, -3, 7, 0, -12]
> -- %Mapp4

The |map| function takes a function as an argument. You can see that
|map| takes a function argument by looking at its type: %Mapp5

````
map :: (a -> b) -> [a] -> [b]
````

The first argument |a -> b| is a function from |a| to |b| where
|a| and |b| are arbitrary types. %Mapp7

Personally, I tend to
use list comprehensions rather than |map|, because list comprehensions give
a nice name to the items of the list. Here is an equivalent example using comprehensions: %Mapp8

> testm2 = [ negate n | n <- [1, 3, -7, 0, 12] ]   -- returns [-1, -3, 7, 0, -12]
> -- %Mapp9

A function that takes another function as an input is called a *higher-order function*.
Higher-order functions are quite useful, but what I find even more interesting
are functions that *return* functions as results. %Mapp10

The comprehensions used earlier in this document could be replace by invocations of
|map|: %Mapp11

|[eval a BAR a <- args]|   \ \ \ \  $\equiv$  \ \ \ \  |map eval args| %Mapp12

TODO: make a comment about point-free style? %Mapp13

TODO: is a function that returns a function also called higher order? %Mapp14

 ### Representing Environments as Functions {#EnvAsFun}

In [Chapter 1](#Chapter1), an environment was defined as a list of bindings.
However, it is often useful to consider the *behavior* of a concept
rather than its concrete *representation*. The purpose of a
environment is to map variable names to values. A map is just
another name for a function. Thus it is
very reasonable to think of an environment as a *function* from
names to values. Consider the environment %Repr2

> type EnvL = [(String, Value)]
> envL1 = [("x", Int 3), ("y", Int 4), ("size", Int 10)]
> -- %Repr3

Since environments always have a finite number of
bindings, it is more precise to say that an environment is a
*partial function* from names to values. A partial function is one
that produces a result for only some of its inputs. One common way to
implement partial functions in Haskell is by using the |Maybe| type,
which allows a function to return a value
(tagged by |Just|) or |Nothing|. Here is an implementation of the
same environment as a function: %Repr4

> type EnvF = String -> Maybe Value
> envF1 "x"    = Just (Int 3)
> envF1 "y"    = Just (Int 4)
> envF1 "size" = Just (Int 10)
> envF1 _      = Nothing
> -- %Repr5

Looking up the value of a variable in either of these environments
is quite different: %Repr6

> x1 = lookup "x" envL1
> x2 = envF1 "x"
> -- %Repr7

The |lookup| function searches a list environment |envL1| for an appropriate binding.
An functional environment |envF1| is applied to the name to get the result.
One benefit of the function environment is that we don't need to know how the bindings are
represented. All we need to do is call it to get the desired answer.[^2] There is
no need to use a |lookup| function, because the functional environment *is* the
lookup function. %Repr8

[^2]: This kind of behavioral representation will come again when we discuss object-oriented programming. %Repr9

The only other thing that is done with an environment is to extend it with
additional bindings. Let's define bind functions that add a binding to
an environment, represented as lists or functions. For lists, the |bindL| function
creates a binding |(val, val)| and then prepends it to the front of the list: %Repr10

> bindL :: String -> Value -> EnvL -> EnvL
> bindL var val env = (var, val) : env
> -- %Repr11

Since |lookup| searches lists from the front, this new binding can shadow existing bindings. %Repr12

> envL2 = bindL "z" (Int 5) envL1
>    -- [("z", Int 5), ("x", Int 3), ("y", Int 4), ("size", Int 10)]
> envL3 = bindL "x" (Int 9) envL1
>    -- [("x", Int 9), ("x", Int 3), ("y", Int 4), ("size", Int 10)]
> -- %Repr13

To extend an environment expressed as a partial function, we need to
write a *higher-order* function. A higher-order function is one that
takes a function as input or returns a function as an result. The
function |bindF| takes an |EnvF| as an input and returns a new |EnvF|. %Repr14

````
bindF :: String -> Value -> EnvF -> EnvF
````

Expanding the definition of |EnvF| makes the higher-order nature of |bindF| clear: %Repr16

````
bindF :: String -> Value -> (String -> Maybe Int) -> (String -> Maybe Int)
````

The definition of |bindF| is quite different from |bindL|: %Repr18

> bindF var val env = \testVar -> if testVar == var
>                                 then Just val
>                                 else env testVar
> -- %Repr19

Understanding how this function works takes a little time. The first
thing to keep in mind is that |env| is a function. It is a function
representing an environment, thus it has
type |EnvF = String -> Maybe Int|. The other arguments, |var|
and |val| are the same as for |bindL|: a string and an integer. %Repr20

The second thing to notice is that the return value (the expression
on the right side of the | = | sign) is a function expression |\testVar -> ...|.
That means the return value is a function. The argument of this
function is named |testVar| and the body of the function is a
conditional expression. The conditional expression checks if
|testVar| is equal to |var|. It returns |val| if they are equal,
and otherwise it calls the function |env| with |testVar| as an
argument. %Repr21

The key to understanding how this works is to keep in mind that
there are two very different *times* or *contexts* involved
in |bindF|. The first time is when the environment is being
extended with a new binding. At this time the arguments
|var|, |val|, and |env| are determined. The second important
time is when the newly extended environment is searched for
a particular variable. This is when |testVar| is bound. Since
the environment can be searched many times, |testVar| will
be bound many times. Consider a specific example: %Repr22

> -- version A
> envF2 = bindF "z" (Int 5) envF1
> -- %Repr23

Let's execute this program manually. The call to |bindF| has three
arguments, creating these bindings:
|var| $\mapsto$ |\"z\"|, |val| $\mapsto$ |5|, |env| $\mapsto$ |envF1|.
Substituting these bindings into the definition of |bindF| gives %Repr24

> -- version B
> envF2'1 = \testVar -> if testVar == "z"
>                     then Just (Int 5)
>                     else envF1 testVar
> -- %Repr25

This makes more sense! It says that |envF2| is a function that
takes a variable name as an argument. It first tests if the
variable is named |z| and if so it returns 5. Otherwise it returns
what |envF1| returns for that variable. Another way to write
this function is %Repr26

> -- version C
> envF2'2 "z" = Just (Int 5)
> envF2'2 testVar = envF1 testVar
> -- %Repr27

These two versions are the same because of the way Haskell deals
with functions defined by cases: it tries the first case (argument == |\"z\"|),
else it tries the second case. Since |bindF| tests for the most
recently bound variable first, before calling the base environment,
variables are properly shadowed when redefined. %Repr28

It is also useful to consider the *empty* environment for both
list and function environments. %Repr29

> emptyEnvL :: EnvL
> emptyEnvL = []
> -- %Repr30

> emptyEnvF :: EnvF
> emptyEnvF = \var -> Nothing
> -- %Repr31

The empty function environment |emptyEnvF| is interesting: it
maps every variable name to |Nothing|. %Repr32

In conclusion, functions can be used to represent environments.
This example illustrates passing a function as an argument as well
as returning a function as a value. The environment-based
evaluators for [expressions](#BasicEvalEnv) and [top-level functions](#EvalTopLevel)
could be easily modified to
use functional environments rather than lists of bindings. For
example, the environment-based evaluation function becomes: %Repr33

> -- Evaluate an expression in a (functional) environment
> evaluate'5a :: Exp'4 -> EnvF -> Value
> evaluate'5a exp env = eval exp
>   where
>     eval (Literal'4 v)      = v
>     eval (Unary'4 op a)     = unary op (eval a)
>     eval (Binary'4 op a b)  = binary op (eval a) (eval b)
>     eval (Variable'4 x)     = fromJust (env x)            -- changed
>     eval (Let'4 x exp body) = evaluate'5a body newEnv
>       where newEnv = bindF x (eval exp) env               -- changed
> -- %Repr34

The result looks better than the previous version, because
it does not have spurious references to list functions |lookup|
and |:|, which are a distraction from the
fundamental nature of environments as maps from names to values.
It is still OK to think of environments as 'data', because
functions are data and this function is being used to represent
an environment. In this case it is a functional representation of data.
In the end, the line between data and behavior and data is quite 
blurry. %Repr35

TODO: define "shadow" and use it in the right places. %Repr36

 ### Multiple Arguments and Currying {#Curry}

Functions in the lambda calculus always have exactly *one* argument.
If Haskell is based on Lambda calculus, how should we
understand all the functions we've defined with multiple arguments?
The answer is surprisingly subtle. Let's consider a very
simple Haskell function that appears to have two arguments: %Mult2

> add a b = b + a
> -- %Mult3

The [Rule of Function Arguments](#LambdaDefinition) for Haskell says that arguments
on the left of a definition are short-hand for lambdas.
The |b| argument can be moved to the right hand side to
get an equivalent definition: %Mult4

> add'1 a = \b -> b + a
> -- %Mult5

Now the |a| argument can also be moved. We have now
"solved" for |add|: %Mult6

> add'2 = \a -> \b -> b + a
> -- %Mult7

It's useful to add parentheses to make the grouping explicit: %Mult8

> add'3 = \a -> (\b -> b + a)
> -- %Mult9

What this means is that |add| is a function of one argument |a|
whose return value is the function |\b -> b + a|. The function
that is returned also takes one argument, named |b|, and
finally returns the value of |b + a|. In other words, a
function of two arguments is actually a function that takes
the first argument and returns a new function that takes the
second argument. Even for this simplest case Haskell uses
a function returning a function! %Mult10

One consequence of this arrangement is that it is possible
to apply the |add| function to the arguments one at a time.
For example applying |add| to just one argument returns a new
function: %Mult11

> inc = add 1      -- \b. b + 1
> dec = add (-1)   -- \b. b + (-1)
> -- %Mult12

These two functions each take a single argument.
The first adds one to its argument. The second subtracts one.
Here are two examples that use the resulting functions: %Mult13

> eleven = inc 10
> nine   = dec 10
> -- %Mult14

To see how the definition of |inc| works, we can analyze the function call
|add 1| in more detail. Replacing |add| by its definition yields: %Mult15

> inc'1 = (\a -> (\b -> b + a)) 1
> -- %Mult16

The Rule of Function Invocation says that in this situation, |a| is
substituted for |1| in the body |\b -> b + a| to yield: %Mult17

> inc'2 = \b -> b + 1
> -- %Mult18

Which is the same (by the [Rule of Function Arguments](#LambdaDefinition)) as: %Mult19

> inc'3 b = b + 1
> -- %Mult20

One way to look at what is going on here is that the two arguments
are split into stages. Normally both arguments are supplied at the same
time, so the two stages happen simultaneously. However, it is legal to
perform the stages at different times. After completing the first stage
to create an increment/decrement function, the new increment/decrement function
can be used many times. %Mult21

> testinc = inc 5 + inc 10 + dec 20 + dec 100
> -- %Mult22

(remember that this means |(inc 5) + (inc 10) + (dec 20) + (dec 100)|) %Mult23

Separation of arguments into different stages is exactly the same
technique used in the [section on representing environments
as functions](#EnvAsFun). The |bindF| function takes three arguments in the first stage,
and then returns a function of one argument that is invoked in a second
stage. To make it look nice, the first three arguments were listed to the
left of the |=| sign, while the last argument was placed to the right as an
explicit lambda. However, this choice of staging is just the intended use
of the function. The function could also have been defined as follows: %Mult24

> bindF'1 var val env testVar = if testVar == var
>                             then Just val
>                             else env testVar
> -- %Mult25

The ability to selectively stage functions suggests a design principle
for Haskell that is not found in most other languages: *place arguments
that change most frequently at the end of the argument list*. Conversely,
arguments that change rarely should be placed early in the argument list. %Mult26

TODO: talk about pairs and define curry/uncurry %Mult27


 ### Church Encodings

Other kinds of data besides environments can be represented as functions.
These examples are known as Church encodings. %Chur2

 #### Booleans

Booleans represent a choice between two alternatives. Viewing the
boolean itself as a behavior leads to a view of a boolean as a
function that chooses between two options. One way to represent a
choice is by a function with two arguments that returns one or the
other of the inputs: %Bool2

> true  x y = x
> false x y = y
> -- %Bool3

The |true| function returns its first argument. The |false| function
returns its second argument. For example |true 0 1| returns |0| while
|false \"yes\" \"no\"| returns |\"no\"|.  One way to write the type
for booleans is a generic type: %Bool4

> type BooleanF = forall a. a -> a -> a
> true :: BooleanF
> false :: BooleanF
> -- %Bool5

Things get more interesting when performing operations on booleans.
Negation of a boolean |b| returns the result of applying |b| to |false|
and |true|. If |b| is true then it will return the first argument, |false|.
If |b| is false then it will return the second argument, |true|. %Bool6

> notF :: BooleanF -> BooleanF
> notF b = b false true
> -- %Bool7

The unary function |not| is a higher-order function: it takes a
functional boolean as an input and returns a functional boolean as
a result. We can also define binary operations on booleans: %Bool8

> orF :: BooleanF -> BooleanF -> BooleanF
> orF a b  = a true b
> -- %Bool9

The behavior of "or" is to return true if |a| is true, and return |b|
if |a| is false. It works by calling |a| as a function, passing
true and |b| as arguments. %Bool10

> andF :: BooleanF -> BooleanF -> BooleanF
> andF a b = a b false
> -- %Bool11

You get the idea. Calling |a| with |b| and false as arguments will
return |b| if |a| is true and false otherwise. %Bool12

To use a Church boolean, the normal syntax for if expressions is
completely unnecessary. For example, %Bool13

> testb1 = if not True then 1 else 2
> -- %Bool14

is replaced by %Bool15

> testb2 = (notF true) 1 2
> -- %Bool16

This code is not necessarily more readable, but it is concise.
In effect a Church boolean *is* an if expression: it is a
function that chooses one of two alternatives. %Bool17

--------------------------------------------------------------------
(everything above this line is relatively stable, but the text below is in flux)
-------------------------------------------------------------------- %Bool18

 #### Natural Numbers

Natural numbers can also be represented functionally. %Natu2

TODO: write this section %Natu3

 ### Relationship between Let and Functions

TODO: prove that |let x =| $e$ |in| $b$ is equivalent to
   ($\lambda$|x.|$b$)$e$ %Rela2

 #### Others

There are many other uses of first-class functions, including
callbacks, event handlers, thunks, continuations, etc. %Othe2

 ## Evaluating First-Class Functions using Environments

Its now time to define the syntax and semantics of a
language with first-class functions. Based on the examples
in the [previous section](#FirstClassExamples), some features are no longer needed.
For example, `let` expressions are not needed because they
can be expressed using functions. Functions only need one
argument, because multi-argument functions can be expressed
by returning functions from functions. %Eval2

Evaluation of first-class functions (lambdas) is
complicated by the need to properly enforce *lexical scoping*
rules. Lexical scope means that a variable refers to the
closest enclosing definition of that variable.
TODO: move this discussion earlier! %Eval3

\StartIncorrect %Eval4

 ### A Non-Solution: Function Expressions as Values

The first idea for achieving "functions are values" is
to make function expressions be values. It turns out that
this "solution" does not really work.
The reason I spend so much time
discussing an incorrect solution is that understanding
why the obvious and simple solution is wrong helps to
motivate and explain the correct solution.
This section is
colored red to remind you that the solution it presents
is *incorrect*. The correct solution is given in
the [next section, on closures](#Closures). %A2

To try this approach, function expressions
are included in the |Value| data type, which
allows functions appears a literal values in a program: %A3

````
data Value'8 = ...
           | Function'8 String Exp'8  -- new
  deriving Eq
````

The two components of a function expression |Function| are
the *bound variable* |String| and the *body expression* |Exp|.
This new kind of value for functions looks a little strange.
Its not like the others. %A5

We normally think of values as things that a simple data,
like integers, strings, booleans, and dates. Up until now, this is
what values have been. Up until now, values have not
contained *expressions* in them. On the other hand, we
are committed to making functions be values, and the body of
a function is necessarily an expression, so one way or the
other values are going to contain expressions. %A6

TODO: the call expression discussion is really not part of this *incorrect*
solution, so it could be moved out? The only problem is that the
code assumes that functions are literals, which is not the code
in the correct version. Sigh. %A7

The call expression changes slightly from the version with
top-level functions. Instead of the *name* of the function
to be called, the |Call| expression now contains an expression |Exp|
for both the function and the argument: %A8

> data Value'8 = Scalar'8 Value
>            | Function'8 String Exp'8  -- new
>   deriving Eq

To create the new value type, the simpler type of basic |Value|
is included with the tag |Scalar|. A *scalar* value is a simple
basic primitive value.

To clarify the effect of this change, consider these two versions
of a simple program, written using top-level functions or
first-class functions: %A10

Top-Level Functions (A)  First-Class Functions (B)
-----------------------  ----------------------
|function f(x) x*x|      |let f = |$\lambda$|x. x*x in|
|f(10)|                  \ \ \ \ |f(10)| %A11

The explicit abstract syntax for example (A) is: %A12

> testP1 = Program
>   [("f", Function'6 ["x"]
>            (Binary'6 Mul (Variable'6 "x")
>                        (Variable'6 "x")))]
>   (Call'6 "f" [Literal'6 (Int 10)])
> -- %A13

The explicit abstract syntax for example (B) is: %A14

> testP2 =
>  Let'8 "f" (Literal'8 (Function'8 "x"
>                       (Binary'8 Mul (Variable'8 "x")
>                                   (Variable'8 "x"))))
>    (Call'8 (Variable'8 "f") (Literal'8 (Scalar'8 (Int 10))))
> -- %A15

Note that the function in the |Call| is string |\"f\"|
in the first version, but is an expression |Variable'8 "f"|
in the second version. %A16

In many cases the
first expression (the function) will be *variable* that
names the function to be called. Since there is no longer any
special function environment, the names of functions are looked
up in the normal variable environment. (TODO: should this come
earlier?)
TODO: example where function to be called is not a variable. %A17

The first few cases for evaluation are exactly the same
as before. In particular, evaluating a literal value is
the same, although now the literal value might be a function. %A18

````
evaluate'8 :: Exp'8 -> Env'8 -> Value'8
evaluate'8 exp env = eval exp
  where
    eval (Literal'8 v)      = v
    ...
````

Calling a function works almost the same as the case for
function calls in the [language with top-level functions](#TopLevel).
Here is the code: %A20

````
    eval (Call'8 fun arg)   = evaluate'8 body newEnv
      where Function'8 x body = eval fun
            newEnv = bindF x (eval arg) env
````

To evaluate a function call |Call'8 fun arg|, %A22

 1. First evaluate |eval fun| the function |fun| of the call.
 2. Use pattern matching to ensure that the result of step 1 is a |Function| value,
     binding |x| and |body| to the argument name and body of the function.
 3. Evaluate the actual argument (|eval arg|) and then extend the environment |env|
    with a binding between the function parameter |x| and the argument value: %A23

    |bindF x (eval arg) env|
 4. Evaluate the |body| of the function in the extended environment |newEnv|: %A24

    |evaluate'8 newEnv body| %A25

Note that this explanation jumps around in the source code. The explanation follows
the sequence of data dependencies in the code: what logically needs to be evaluated first,
rather than the order in which expressions are written. Since Haskell is a lazy language,
it will actually evaluate the expressions in a completely different order! %A26

The main difference from the case of [top-level functions](#TopLevel) is that the
function is computed by calling |eval fun| rather than
|lookup fun funEnv|. The other difference is that functions
now only have one argument, while we allowed multiple arguments
in the previous case. %A27

> --------------------BEGIN-HIDE-------------------------
> data Exp'8 = Literal'8   Value'8
>          | Unary'8     UnaryOp Exp'8
>          | Binary'8    BinaryOp Exp'8 Exp'8
>          | If'8        Exp'8 Exp'8 Exp'8
>          | Variable'8  String
>          | Let'8       String Exp'8 Exp'8
>          | Call'8      Exp'8 Exp'8         -- changed
>   deriving Eq
>
> type Env'8 = String -> Maybe Value'8
>
> evaluate'8 :: Exp'8 -> Env'8 -> Value'8
> evaluate'8 exp env = eval exp
>   where  -- TODO: not needed to show this code here?
>     eval (Literal'8 v)      = v
>     eval (Unary'8 op a)     = unary'8 op (eval a)
>     eval (Binary'8 op a b)  = binary'8 op (eval a) (eval b)
>     eval (If'8 a b c)       = if fromBool'8 (eval a)
>                             then eval b
>                             else eval c
>     eval (Let'8 x exp body) = evaluate'8 body newEnv
>       where newEnv = bindF x (eval exp) env
>     eval (Variable'8 x)     = fromJust (env x)
>     eval (Call'8 fun arg)   = evaluate'8 body newEnv
>       where Function'8 x body = eval fun
>             newEnv = bindF x (eval arg) env
>
> fromBool'8 (Scalar'8 (Bool b)) = b
>
> unary'8 op (Scalar'8 a) = Scalar'8 (unary op a)
> binary'8 op (Scalar'8 a) (Scalar'8 b) = Scalar'8 (binary op a b)
> -- %A29
> --------------------END-HIDE-------------------------

The key question is: **why doesn't the code given above work?**
There are two problems. One has to do with returning functions
as values, and the other with passing functions as arguments.
They both involve the handling of free variables in the
function expression. %A30

 #### Problems with Returning Functions as Values

Let's look at the problem of returning functions as values first.
The section on [Multiple Arguments](#Curry) showed how a two-argument
function could be implemented by writing a function that takes
one argument, but then returns a function that takes the second
argument. Here is a small program that illustrates this technique: %Prob2

> teste1 = let add = \a -> (\b -> b + a) in add 3 2
> -- %Prob3

This program is encoded in our language as follows: %Prob4

> testE2 =
>  Let'8 "add" (Literal'8 (Function'8 "a"
>              (Literal'8 (Function'8 "b"
>                 (Binary'8 Add (Variable'8 "b")
>                             (Variable'8 "a"))))))
>              (Call'8 (Call'8 (Variable'8 "add")
>                              (Literal'8 (Scalar'8 (Int 3))))
>                    (Literal'8 (Scalar'8 (Int 2))))
> -- %Prob5

Rather than work with the ugly constructor syntax in
Haskell, we will continue to use the convention of writing
|b + a| to mean |(Binary'8 Add (Variable'8 "b") (Variable'8 "a"))|. %Prob6

Here is how evaluation of this sample program proceeds: %Prob7

 1. Evaluate |let add = \a -> (\b -> b + a) in add 3 2|
 2. Bind |add| $\mapsto$ |\a -> (\b -> b + a)|
 3. Call |(add 3) 2|
     a. Call |add 3|
     b. Evaluate the variable |add|, which looks it up in the
        environment to get |\a -> (\b -> b + a)|
     c. Bind |a| $\mapsto$ |3|
     d. Return |\b -> b + a| as result of |add 3|
 4. Call |\b -> b + a| on argument |2|
     a. Bind |b| $\mapsto$ |2|
     b. Evaluate |b + a|
     c. Look up |b| to get |2|
     d. Look up |a| to get... **unbound variable!** %Prob8

To put this more concisely, the problem arises because the call to |add 3|
returns |\b -> b + a|. But this function expression is not well defined because
it has a free variable |a|. What happened to the binding for |a|? It had
a value in Steps 12 through 14 of the explanation above. But this
binding is lost when returning the literal |\b -> b + a|. The problem doesn't
exhibit itself until the function is called. %Prob9

The problems with returning literal function expressions as values is that
bindings for free variables that occur in the function are lost, leading
to later unbound variable errors. Again, this problem arises because
we are trying to treat function expressions as *literals*, as if they
were number or booleans. But function expressions are different because
they contain variables, so care must be taken to avoid losing the
bindings for the variables. %Prob10

 #### Problems with Rebinding Variables

A second problem can arise when passing functions as values. This
problem can occur, for example, when [composing two functions](#Compose),
[mapping a function over a list](#Map), or many other situations.
Here is a program that illustrates the problem. %Prob2

> testP = let k = 2 in
>   let double = \n -> k * n in
>     let k = 9 in
>       double k
> -- %Prob3

The correct answer, which is produced if you run this program in Haskell,
is 18. The key point is that |k| is equal to |2| in the body of |double|,
because that occurrence of |k| is within the scope of the first |let|.
Evaluating this function with the evaluator given above produces |81|,
which is not correct.
In summary, the evaluation of this expression proceeds as follows: %Prob4

 1. Bind |k| $\mapsto$ |2|
 2. Bind |double| $\mapsto$ |\n -> k * n|
 3. Bind |k| $\mapsto$ |9|
 4. Call |double k|
    a. Bind |n| $\mapsto$ |9|
    b. Evaluate body |k * n|
    c. Result is |81| given |k=9| and |n=9| %Prob5

The problem is that when |k| is looked up in step 4b, the
most recent binding for |k| is |9|. This binding is based on the
*control flow* of the program, not on the *lexical* structure.
Looking up variables based on control flow is called *dynamic binding*. %Prob6

\EndIncorrect %Prob7

 ### A Correct Solution: Closures {#Closures}

As we saw in the previous section, the problem with using a
function expression as a value is that the bindings of the free variables
in the function expression are either lost or may be overwritten.
The solution is to *preserve the bindings that existed at the
point when the function was defined*. The mechanism for doing
this is called a *closure*. A closure is a combination of a
function expression and an environment. Rather than think of
a function expression as a function value, instead think of it
as a part of the program that *creates* a function. The actual
function value is represented by a closure, which captures the
current environment at the point when the function expression is
executed. %A2

To implement this idea, we revise the definition of |Exp|
and |Value|. First we add function expressions as a new kind
of expression: %A3

````
data Exp'7 = ....
         | Function'7 String Exp'7      -- new
````

As before, the two components of a function expression are
the *bound variable* |String| and the *body expression* |Exp|.
Function expressions resemble |let| expressions, so they fit
in well with the other kinds of expressions. %A5

The next step is to introduce *closures* as a new kind of value.
Closures have all the same information as a function expressions
(which we previously tried to add as values), but they have
one important difference: closures also contain an environment. %A6

> data Value'7 = Scalar'7 Value
>            | Closure'7 String Exp'7 Env'7  -- new
>   deriving (Eq, Show)
>

The three parts of a closure are the *bound variable* |String|,
the *function body* |Exp|, and *the closure environment* |Env|.
The bound variable and function body are the same as the
components of a function expression. %A8

With these data types, we can now define a correct evaluator for
first-class functions using environments. The first step is to
*create a closure* when evaluating a function expression. %A9

````
-- Evaluate an expression in an environment
evaluate'7 :: Exp'7 -> Env'7 -> Value'7
evaluate'7 exp env = eval exp
  where
    ...
    eval (Function'7 x body) = Closure'7 x body env
````

The resulting closure is the value that represents a
function. The function expression |Function'7 x body|
is not actually a function itself, it is an
expression that *creates* a function when executed.
Once a closure value has been created, it can be bound
to a variable just like any other value, or passed to
a function or returned as the result of a function.
Closures are values. %A11

Since closures represent functions,
the only thing you can *do* with a closure is *call* it.
The case for evaluating a function call starts by
analyzing the function call expression, |eval (Call'7 fun arg)|.
This pattern says that call expression has two components:
a function |fun| and
an argument |arg|. Here is the code for this case: %A12

````
    eval (Call'7 fun arg)   = evaluate'7 body newEnv
      where Closure'7 x body closeEnv = eval fun
            newEnv = (x, eval arg) : closeEnv
````

The code starts by evaluating both the function part |fun| to
produce a value. The |where| clause
|Closure'7 x body newEnv = eval fun| says that the result of
evaluating |fun| must be a closure, and the variables |x|,
|body|, and |newEnv| are bound to the parts of the closure.
If the result is not a closure, Haskell throws a runtime error. %A14

Next the environment from the closure |newEnv| is extended to
include a new binding |(x, eval arg)| of the function parameter
to the value of the argument expression. The new environment is
called |newEnv|. At a high level, the environment is the same
environment that existed when the function was created, together
with a binding for the function parameter. %A15

Finally, the |body| of the function is evaluated in this new
environment, |evaluate'7 body newEnv|. %A16

TODO: give an example of how this runs? %A17

 #### Exercise: Multiple Arguments
 
Modify the definition of |Function| and |Call| to allow multiple
arguments. Modify the |evaluate| function to correctly handle the
extra arguments.

 ## Environment/Closure Diagrams

The behavior of this evaluator is quite complex, but its
operation on specific programs can be illustrated by showing
all the environments and closures created during its execution,
together with the relationships between these structures. %Envi2

An Environment/Closure Diagram is a picture that shows
the closures and environments created during execution of an
expression. %Envi3

* Start State %Envi4

    Set current environment to empty environment $\emptyset$ %Envi5

* Case |Let x e body|
    1.  Draw binding box for |x| with unknown value %Envi6

        Set parent of new binding to be the current environment
    2.  Create the diagram for bound expression |e| %Envi7

        Put the value of |e| into the binding as the value of |x|
    3.  Set current environment to be the new binding
    4.  Draw diagram for |body| and remember value
    5.  Set current environment back to what it was before %Envi8

* Case |Call fun arg|
    1.  Draw diagram for |fun| %Envi9

        Result must be a closure with variable |x|, |body| and |env|
    2.  Make binding for argument using name |x| from closure %Envi10

        Set parent of new binding to be the environment of the closure |env|
    3.  Draw diagram for |arg| %Envi11

        Put the value into the new binding as the value of |x|
    4.  Set current environment to be the new binding
    5.  Draw diagram for |body| and remember value
    6.  Set current environment back to what it was before %Envi12

* Case |Var x|
    1.  Look up the variable in the current environment %Envi13

* Case |Function x e|
    1.  Make a closure with variable |x|, body |e| %Envi14

        Set the environment of the closure to be the current environment %Envi15

 ### Example 1

````
let k = 2 in
  let double = \n -> k * n in
    let k = 9 in
      double k
````

![Environment Diagram 1](figures/env1.eps) %Exam3

 ### Example 2

````
let add = \a -> (\b -> b + a) in (add 3) 2
````

![Environment Diagram 2](figures/env2.eps) %Exam3

 ### Example 3
 
````
let m = 2 in 
  let proc = \n -> m + n
      part = \(g,n) -> \m -> n * g(m) 
  in let inc = part(proc, 3) in 
      inc 7
````             

![Environment Diagram 3](figures/env3.png) %Exam4

 ## Summary of First-Class Functions

Here is the full code for first-class functions with non-recursive definitions: %Summ2

> data Exp'7 = Literal'7   Value'7
>          | Unary'7     UnaryOp Exp'7
>          | Binary'7    BinaryOp Exp'7 Exp'7
>          | If'7        Exp'7 Exp'7 Exp'7
>          | Variable'7  String
>          | Let'7       String Exp'7 Exp'7
>          | Function'7  String Exp'7      -- new
>          | Call'7      Exp'7 Exp'7         -- changed
>   deriving (Eq, Show)
>
> type Env'7 = [(String, Value'7)]
>
> evaluate'7 :: Exp'7 -> Env'7 -> Value'7
> evaluate'7 exp env = eval exp
>   where
>     eval (Literal'7 v)      = v
>     eval (Unary'7 op a)     = unary'7 op (eval a)
>     eval (Binary'7 op a b)  = binary'7 op (eval a) (eval b)
>     eval (If'7 a b c)       = if fromBool'7 (eval a)
>                             then eval b
>                             else eval c
>     eval (Variable'7 x)     = fromJust (lookup x env)
>     eval (Let'7 x exp body) = evaluate'7 body newEnv
>       where newEnv = (x, eval exp) : env
>     eval (Function'7 x body) = Closure'7 x body env     -- new
>     eval (Call'7 fun arg)   = evaluate'7 body newEnv    -- changed
>       where Closure'7 x body closeEnv = eval fun
>             newEnv = (x, eval arg) : closeEnv
>
> fromBool'7 (Scalar'7 (Bool b)) = b
>
> unary'7 op (Scalar'7 a) = Scalar'7 (unary op a)
> binary'7 op (Scalar'7 a) (Scalar'7 b) = Scalar'7 (binary op a b)
> -- %Summ3

 # Recursive Definitions

One consequence of using a simple |let| expression to define functions
is that it is no longer possible to define *recursive functions*, which
were supported in the [Section on Top-Level Functions](#TopLevel). A recursive
function is a function that calls itself within its own definition.
For example, consider this definition of the factorial function: %Recu2

> testLet =
>   let fact = \n -> if n == 0 then 1 else n * fact(n-1)
>   in fact(10)
> -- %Recu3

The |fact| function is recursive because it calls |fact| within its definition. %Recu4

The problem with our existing language implementation is that
the scope of the variable |fact| is the body of the
|let| expression, which is |fact(10)|, so while the use of |fact| in
|fact(10)| is in scope, the other use in |fact(n-1)| is *not* in scope.
(TODO: wordy) %Recu5

To solve this problem, we need to change how we understand the |let|
expression: the scope of the bound variable must be both the body
of the let, and the bound expression that provides a definition for
the variable. This means that the variable can be defined in terms of
itself. This is exactly what we want for recursive functions, but it
can cause problems. For example, %Recu6

> testLet3 = let x = x + 1 in x
> -- %Recu7

This is now syntactically correct, as the bound variable |x| is in scope
for the expression |x + 1|. However, such a program is either meaningless, or
it can be understood to mean "infinite loop". There are similar cases that
are meaningful. For example, this program is meaningful: %Recu8

> testLet2 =
>   let x = y + 1
>       y = 99
>   in x * y
> -- %Recu9

This example includes two bindings at the same time (which we do not
currently support. TODO: see homework?).
In this case the result is |9900| because |x = 100| and
|y = 99|. It works because the binding expression for |x|, namely |y + 1|,
is in the scope of |y|. %Recu10

--------------------BEGIN-HIDE-------------------------
If we convert this program to our abstract syntax, it comes out as: %Recu11

> testLet1 = Let'7
>  "fact" (Function'7 "n"
>     (If'7 (Binary'7 EQ (Variable'7 "n") (Literal'7 (Scalar'7 (Int 0))))
>         (Literal'7 (Scalar'7 (Int 1)))
>         (Binary'7 Mul (Variable'7 "n")
>                     (Call'7 (Variable'7 "fact")
>                           (Binary'7 Sub (Variable'7 "n") (Literal'7 (Scalar'7 (Int 1))))))))
>  (Call'7 (Variable'7 "fact") (Literal'7 (Scalar'7 (Int 10))))
> --------------------END-HIDE-------------------------
> -- %Recu12

 ## Semantics of Recursion

A more fundamental question is *what does a recursive definition* **mean**?
In grade school we get used to dealing with equations that have the same
variable on both sides of an equal sign. For example, consider this simple
equation: %Sema2

$a = 1 + 3a$ %Sema3

Our instinct, honed over many years of practice, is to "solve for *a*". %Sema4

* $a = 1 + 3a$ %Sema5
* *{ subtract $3a$ from both sides }* %Sema6
* $-2a = 1$ %Sema7
* *{ divide both sides by $-2$ }* %Sema8
* $a = -1/2$ %Sema9

I feel a little silly going through this in detail (although I have spent a lot
of time recently practicing algebra with my son, so I know how hard it is to master).
The point is that the definition of |fact| has exactly the same form: %Sema10

> fact = \n -> if n == 0 then 1 else n * fact(n-1)
> -- %Sema11

This is an equation where |fact| appears on both sides, just as $a$ appears
on both sides in $a = 1 + 3a$. The question is: *how do we solve for |fact|*?
It's not so easy, because we don't have algebraic rules to divide by lambda
and subtract conditionals, to get both occurrences of |fact| onto the same
side of the equation. We are going to have to take another approach. %Sema12

The first thing to notice is that |fact| is a function, and like most functions
it is an *infinite* structure. This
makes sense in several ways. It is infinite in the sense that it defines
the factorial for every natural number, and there is an infinity of natural numbers.
If you consider the grade-school definition of
a function as a set of pairs, then the set of pairs in the factorial function is
infinite. %Sema13

Finally, and most importantly for us, if you consider |fact| as a
computational method or rule, then the computational rule has an unbounded
number of steps that it can perform. We can count the steps: first it performs
an equality comparison |n == 0|, then it either stops or it performs a subtraction
|n-1| and then *performs the steps recursively*, then when it is done with that
it performs a multiplication |n * ...|. In other words, given a natural number $n$
the computation will perform $3n + 1$ steps. Since it will handle any natural number,
there is no bound on the number of steps it performs. If you tried to write out
the steps that might be performed, then the list of steps would be infinite. %Sema14

 ### Three Analyses of Recursion

In what follows we will explore three ways to understand recursion. The first
explanation just allows us to define recursive |let| expression by using
the capabilities for recursion that are built into Haskell. This explanation is
elegant and concise, but not very satisfying (like pure sugar!). The problem is
that we have just relied on recursion in Haskell, so we don't really have an
explanation of recursion. The second explanation is a practical introduction to the
concept of fixed points. This solution can also be implemented elegantly in Haskell,
and has the benefit of providing a mathematically sound explanation of recursive
definitions. While fixed points can be implemented directly, they are not the
most efficient approach, especially in conventional languages. As a result, we will
consider a third implementation, based on self application. This explanation is
messy but practical. In fact, it is the basis for real-world implementations of
C++ and Java. A forth explanation
A fourth explanation,
languages in traditional imperative languages. %Thre2

 ## Understanding Recursion using Haskell Recursion {#Cyclic}

Haskell makes it easy to create infinite structures and functions. Understanding
how this works can help us in implementing our language. We've already seen many
examples of recursive functions in Haskell: for example, every version of
|evaluate| has been recursive. However, Haskell also allows creation of recursive
data structures. For example, this line creates an infinite list of 2's: %Unde2

> twos = 2 : twos
> -- %Unde3

Remember that the |:| operator adds an item to the front of a list.
This means that |twos| is a list with |2| concatenated onto the front of
the list |twos|. In other words, |twos| is an infinite list of 2's: %Unde4

````
twos = [2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, ... ]
````

It's also possible to make infinite lists that change: %Unde6

> numbers = 0 : [ n + 1 | n <- numbers ]
> -- %Unde7

This creates an infinite list of the natural numbers: %Unde8

````
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, ...]
````

All these definitions work in Haskell because of *laziness*.
Haskell creates an internal representation of a potentially infinite
value, but it only creates as much of the value as the program actually
needs. If you try to use all of |two| or |numbers| then the result will
be an infinite loop that never stops. However, if the program only
needs the first 10 items of |twos| or |numbers| then only the first
10 elements of the infinite value will be created. %Unde10

Interestingly, Haskell also accepts the algebraic expression
discussed earlier: %Unde11

> a = 1 + 3 * a
> -- %Unde12

Haskell considers this a valid program, but it does *not* solve for |a|.
Instead it treats the definition as a computational rule: to evaluate
|a|, add one to three times the value of |a|, which requires evaluating
|a|, and so on, again, and again, and again. The result is an infinite loop.
The quickest way to write an infinite loop is: %Unde13

> inf = inf
> -- %Unde14

TODO: make pictures to illustrate the cyclic values in this section. %Unde15

Attempting to use this value leads to an immediate infinite loop[^3]. If the
value is not used, then it has no effect on the program results. %Unde16

[^3]: Oddly enough, this kind of |inf| value is not useless! It
has some legitimate uses in debugging Haskell programs (more on this later). %Unde17

It is not always easy to determine if a value will loop infinitely or not.
One rule of thumb is that if the recursive variable is used *within* a
data constructor (e.g. |:|) or inside a function (in the body of a lambda),
then it will probably not loop infinitely. This is because both
data constructors and functions are lazy in Haskell. %Unde18

 ### Using Results of Functions as Arguments

TODO: consider discussing this example. Here is an outline: %Usin2

A type for trees: %Usin3

> data Tree = Leaf Int | Branch Tree Tree
>   deriving Show
> -- %Usin4

An example tree: %Usin5

> testTr = Branch (Branch (Leaf 5) (Leaf 3))
>             (Leaf (-99))
> -- %Usin6

Computing the minimum and maximum of a tree: %Usin7

> minTree (Leaf n) = n
> minTree (Branch a b) = min (minTree a) (minTree b)
> -- %Usin8

> maxTree (Leaf n) = n
> maxTree (Branch a b) = max (maxTree a) (maxTree b)
> -- %Usin9

Point out that computing both requires two traversals. %Usin10

Computing minimum and maximum at the same time. %Usin11

> minMax (Leaf n) = (n, n)
> minMax (Branch a b) = (min min1 min2, max max1 max2)
>   where (min1, max1) = minMax a
>         (min2, max2) = minMax b
> -- %Usin12

|minMax| is an example of *fusing* two functions together. %Usin13

Another operation: copying a tree and replacing all the
leaves with a specific integer value: %Usin14

> repTree x (Leaf n) = Leaf x
> repTree x (Branch a b) = Branch (repTree x a) (repTree x b)
> -- %Usin15

Now for our key puzzle: replacing every leaf in a tree with
the minimum value of the tree: %Usin16

> repMinA tree = repTree (minTree tree) tree
> -- %Usin17

This requires two traversals. It seems to truly *require* two
traversals the minimum must be identified before the process
of replacement can begin. %Usin18

But lets fuze them anyway:
TODO: need to develop this in a few more steps! Here is a
helper function: %Usin19

> repMin' (Leaf n, r) = (n, Leaf r)
> repMin' (Branch a b, r) = (min min1 min2, Branch newTree1 newTree2)
>   where (min1, newTree1) = repMin' (a, r)
>         (min2, newTree2) = repMin' (b, r)
> -- %Usin20

Finally to do the replacement with the minimum: %Usin21

> repMin tree = newTree
>    where (min, newTree) = repMin'(tree, min)
> -- %Usin22

Note how one of the results of the function call, the |min| value,
is passed as an argument to the function call itself! %Usin23

TODO: Explain how this works, and give a picture. %Usin24

> --------------------BEGIN-HIDE-------------------------
> main'10 = do
>   print testTr
>   print (minTree testTr)
>   print (maxTree testTr)
>   print (minMax testTr)
>   print (repMin' (testTr, 9999))
>   print (repMin testTr)
> --------------------END-HIDE-------------------------
> -- %Usin25


 ### Implementing Recursive |Let| with Haskell

The powerful techniques for recursive definition illustrated
in the previous section are sufficient to implement recursive |let|
expressions. In the Section on [Evaluation using Environments](#BasicEvalEnv),
|let| was defined as follows: %Impl2

````
eval (Let'4 x exp body) = evaluate'5 body newEnv
   where newEnv = (x, eval exp) : env
````

The problem here is that the bound expression |exp| is evaluated
in the parent environment |env|. To allow the bound variable |x| to
be used within the expression |exp|, the expression must be evaluated
in the new environment. Fortunately this is easy to implement
in Haskell: %Impl4

````
eval (Let'7 x exp body) = evaluate'7 body newEnv
  where newEnv = (x, evaluate'7 exp newEnv) : env
````

The new environment being created is passed as an argument to the
evaluation function that is used during the creation of the new environment!
It may see odd to use the result of a function as one of its arguments.
However, as we have seen, Haskell allows such definitions. %Impl6

The explanation of recursion in Haskell is almost too simple.
In fact, it is too simple: it involved changing 6 characters in the
code for the non-recursive program. The problem is that we haven't really
explained recursion in a detailed way, because we have simply
used Haskell's recursion mechanism to implement recursive
|let| expressions in our language. The question remains: how
does recursion work? %Impl7

TODO: come up with a *name* for the little language we are defining
and exploring. PLAI uses names like ArithC and ExprC. %Impl8

 #### Recursive Definitions in Environment/Closure Diagrams

For the case of recursive bindings, the only difference is that the
order of step 2 and 3 is swapped: %Recu2

* Case **Recursive** |Let x e body|
    1.  Draw binding box for |x| with unknown value
        
        Set parent of new binding to be the current environment
    2.  Set current environment to be the new binding
    3.  Create the diagram for bound expression |e|
        
        Put the value of |e| into the binding as the value of |x|
    4.  Draw diagram for |body| and remember value
    5.  Set current environment back to what it was before %Recu3

Note that in this case the binding in Step 3 becomes the current
environment *before* it is fully defined. %Recu4

TODO: examples here %Recu5

 ## Understanding Recursion with Fixed Points

Another way to explain recursion is by using the mathematical
concept of a fixed point. A *fixed point* of a function $f$
if a value $x$ where $x = f(x)$. If you think of a function as
a transformation on values, then fixed points are values that
are unchanged by the function. For example, if the function
represents a rotation (imagine simple rotation of a book on a table)
then the fixed point is the center of the rotation... that is
the point on the book that is unchanged by rotating it.
If you really did rotate a book, you'd probably push your finger
down in the middle, then rotate the book around your finger.
The spot under your finger is the fixed point of the rotation
function. %Unde2

There is a large body of theory about fixed points, including
applications in mathematics and fundamental theorems (see
the Knaster Tarski theorem), but I'm going to avoid the math
and give a practical discussion of fixed-points with examples.
TODO: give citations to appropriate books. %Unde3

TODO: nice picture of the book and the fixed point? Use a fun
book, like "Theory of Lambda Conversion". %Unde4

 ### Fixed Points of Numeric Functions

Fixed-points can also be identified for simple mathematical
functions: %Fixe2

*function*\ \ \ \ \ \ \ \ \ \ \ \ \ \ \  *fixed point(s)*
--------------------                     --------------------
$i_{10}(x) = 10 - x$                     $5$
$square(x) = x^2$                        $0, 1$
$g_\phi(x) = 1 + \cfrac{1}{x}$           $1.6180339887...$
$k_4(x) = 4$                             $4$
$id(x) = x$                              all values are fixed points
$inc(x) = x + 1$                         no fixed points %Fixe3

As you can see, some functions have one fixed point.
Some functions have multiple fixed points. Others have
an infinite number of fixed points, while some don't have
any at all.
The fixed point of $g_\phi$ is the *golden ratio*,
also known as $\phi$. %Fixe4

Fixed points are useful because they can provide a
general approach to solving equations where a variable
appears on both sides of an equation. Consider this simple
equation: %Fixe5

$x = 10 - x$ %Fixe6

Rather than performing the normal algebraic manipulation
to solve it, consider expressing the right side of the
equation using a new helper function, $g$: %Fixe7

$g(x) = 10 - x$ %Fixe8

Functions created in this way are called *generators* for
recursive equations.
Given the generator $g$, the original equation can be rewritten as: %Fixe9

$x = g(x)$ %Fixe10

Any value $x$ that satisfies $x = g(x)$ is a fixed point of $g$.
Conversely, any fixed point of $g$ is a solution to the original
equation. This means that finding a solution to the original equation
is equivalent to finding a fixed point for $g$. Imagine that
there was a magic function |fix| that could automatically find
a fixed point for any function[^4]. Then one way to find a fixed point
of $g$ would be to use |fix|, by calling |fix|$(g)$.
Then the solution to the equation above could be rewritten using |fix|: %Fixe11

$x =$ |fix|$(g)$ %Fixe12

[^4]: The function |fix| is often called $Y$. For further reading,
see @ScottDataTypes, @GunterPL, @WhyY and @thomas2006end. %Fixe13

This result looks like a *solution* for $x$, in the sense that
it is an equation where
$x$ appears only by itself on the left of the equation. Any equation
where a variable appears by itself on the left and anywhere
on the right side of the
equation, can be rewritten as a fixed point equation. %Fixe14

Note that |fix| is a higher-order function: it takes a
function as an input, and returns a value as a result. %Fixe15

The problem is that the solution relies on |fix|, a function that
hasn't been defined yet, and maybe cannot be defined. Is it
possible to automatically find a fixed point of any function?
Does the function |fix| exist? Can it be defined? %Fixe16

 ### Fixed Points by Iterative Application

It turns out that there is no way to find fixed points for
*any* arbitrary function $f$, but for a certain class of
well behaved functions, it *is* possible to compute fixed points
automatically. In this case, "well behaved" means that
the function converges on the solution when applied repeatedly.
For example, consider function $g_\phi$ defined above: %Fixe2

$g_\phi(x) = 1 + \cfrac{1}{x}$ %Fixe3

Consider multiple invocations of $g_\phi$ starting
with $g_\phi(1)$. The following table summarizes this
process. The first column represents the iteration number,
which starts at one and increases with each iteration.
The second column is a representation of the computation
as an explicit *power* of a function. The power of a function
$f^n(x)$ means to apply $f$ repeatedly until it has been
performed $n$ times, passing the result of one call as the
input of the next call. For example, $f^3(x)$ means $f(f(f(x)))$.
The next column shows just the application of $g_\phi$ to the
previous result. The final column gives the result for that
iteration. %Fixe4

\#  power            previous                     result
--  ---------------- -------------------------- - ------------
 1  $g_\phi^{1}(1) $ $g_\phi(1)               $ = 2
 2  $g_\phi^{2}(1) $ $g_\phi(2)               $ = 1.5
 3  $g_\phi^{3}(1) $ $g_\phi(1.5)             $ = 1.6666666667
 4  $g_\phi^{4}(1) $ $g_\phi(1.66666666666667)$ = 1.6
 5  $g_\phi^{5}(1) $ $g_\phi(1.6)             $ = 1.625
 6  $g_\phi^{6}(1) $ $g_\phi(1.625)           $ = 1.6153846154
 7  $g_\phi^{7}(1) $ $g_\phi(1.61538461538462)$ = 1.619047619
 8  $g_\phi^{8}(1) $ $g_\phi(1.61904761904762)$ = 1.6176470588
 9  $g_\phi^{9}(1) $ $g_\phi(1.61764705882353)$ = 1.6181818182
10  $g_\phi^{10}(1)$ $g_\phi(1.61818181818182)$ = 1.6179775281
11  $g_\phi^{11}(1)$ $g_\phi(1.61797752808989)$ = 1.6180555556
12  $g_\phi^{12}(1)$ $g_\phi(1.61805555555556)$ = 1.6180257511
13  $g_\phi^{13}(1)$ $g_\phi(1.61802575107296)$ = 1.6180371353
14  $g_\phi^{14}(1)$ $g_\phi(1.61803713527851)$ = 1.6180327869
15  $g_\phi^{15}(1)$ $g_\phi(1.61803278688525)$ = 1.6180344478
16  $g_\phi^{16}(1)$ $g_\phi(1.61803444782168)$ = 1.6180338134
17  $g_\phi^{17}(1)$ $g_\phi(1.61803381340013)$ = 1.6180340557 %Fixe5

TODO: create a little plot of this function convergence. %Fixe6

The result converges on $1.6180339887...$
which is the value of $\phi$. It turns out that iterating
$g_\phi$ converges on $\phi$ for any starting number.
The fixed point is the *limit* of applying the
transformation function $g_\phi$ infinitely many times.
One way to express the fixed point is %Fixe7

|fix|$(f) = f^\infty(start)$ %Fixe8

This means the application of $f$ an infinite number of
times to some starting value. Finding the right starting
value can be difficult. In some cases any starting value will
work, but in other cases it's important to use a particular
value. In the theory of fixed points, (TODO: discuss the theory
somewhere), the initial value is the bottom of an appropriate
lattice. %Fixe9

The fixed point of some, but not all, functions
can be computed by repeated function application. Here are
the results for this technique, when applied to the examples
given above: %Fixe10

*function*\ \ \ \ \ \ \ \ \ \ \ \ \ \ \  result for repeated invocation
--------------------                     --------------------
$inv_{10}(x) = 10 - x$                   infinite loop
$square(x) = x^2$                        infinite loop
$g_\phi(x) = 1 + \cfrac{1}{x}$           $1.6180339887...$
$const_4(x) = 4$                         $4$
$id(x) = x$                              infinite loop
$inc(x) = x + 1$                         infinite loop %Fixe11

Only two of the six examples worked. Fixed points are not a general
method for solving numeric equations. %Fixe12

 ### Fixed Points for Recursive Structures

The infinite recursive structures discussed in
[Section on Haskell Recursion](#Cyclic) can also be defined using fixed points: %Fixe2

> g_twos l = 2 : l
> -- %Fixe3

The function |g_twos| is a non-recursive function that adds a 2 to the front
of a list. Here are some test cases for applying |g_twos| to various lists: %Fixe4

input                   output            input = output
------------------      ------------      ----------------
|[]|                    |[2]|             no
|[1]|                   |[2,1]|           no
|[3,4,5]|               |[2,3,4,5]|       no
|[2,2,2,2,2]|           |[2,2,2,2,2,2]|   no
|[2,2,2,...]|           |[2,2,2,...]|     *yes* %Fixe5

The function |g_twos| can be applied to any list. If it is applied to
any finite list, then the input and output lists cannot be the same
because the output is one element longer then the input. This is not a
problem for infinite lists, because adding an item to the front of
an infinite list is still an infinite list. Adding a 2 onto the front
of an infinite list of 2s will return an infinite list of 2s. Thus
an infinite list of 2s is a fixed point of |g_twos|. %Fixe6

````
fix(g_twos) = [2,2,2,...]
````

Functions used in this way are called generators because they
generate recursive structures. One way to think about them is that
the function performs *one step* in the creation of a infinite
structure, and then the |fix| function repeats that step over
and over until the full infinite structure is created. Consider
what happens when the output of the function is applied to the
input of the previous iteration. The results are
|[]|, |[2]|, |[2,2]|, |[2,2,2]|, |[2,2,2,2]|, ...
At each step the result is a better approximation of the final
solution. %Fixe8

The second example, a recursive definition
that creates a list containing the natural numbers, is more interesting: %Fixe9

> g_numbers ns = 0 : [ n + 1 | n <- ns ]
> -- %Fixe10

This function takes a list as an input, it adds one to each item in the
list and then puts a |0| on the front of the list. %Fixe11

Here are the result when applied to the same test cases listed above: %Fixe12

input                   output            input = output
------------------      ------------      ----------------
|[]|                    |[0]|             no
|[1]|                   |[0,2]|           no
|[3,4,5]|               |[0,4,5,6]|       no
|[2,2,2,2,2]|           |[0,3,3,3,3,3]|   no
|[2,2,2,...]|           |[0,3,3,3,...]|   no %Fixe13

A more interesting set of test cases involves starting with the empty
list, then using each function result as the next test case: %Fixe14

input                   output                 input = output
------------------      ------------           ----------------
|[]|                    |[0]|                  no
|[0]|                   |[0,1]|                no
|[0,1]|                 |[0,1,2]|              no
|[0,1,2]|               |[0,1,2,3]|            no
|[0,1,2,3]|             |[0,1,2,3,4]|          no
|[0,1,2,3,4]|           |[0,1,2,3,4,5]|        no
|[0,1,2,3,4,5,...]|     |[0,1,2,3,4,5,6,...]|  *yes* %Fixe15

The only list that is unchanged after applying |g_numbers|
is the list of natural numbers: %Fixe16

````
fix(g_numbers) = [0,1,2,3,4,5,...]
````

By staring with the empty list and then applying |g_numbers| repeatedly,
the result eventually converges on the fixed point. Each step is a
better approximation of the final answer. %Fixe18

 ### Fixed Points of Higher-Order Functions

TODO: text explaining how to implement |fact| using fix. %Fixe2

> g_fact = \f -> \n -> if n == 0 then 1 else n * f(n-1)
> -- %Fixe3

> fact'6 = fix g_fact
> -- %Fixe4

more... %Fixe5

 ### A Recursive Definition of |fix|

Haskell allows an elegant definition of |fix| using recursion, which
avoids the issue of selecting a starting value for the iteration. %A2

> fix g = g (fix g)
> -- %A3

This definition is beautiful because it is a direct translation of the
original mathematic definition of a fixed point: |fix|$(f)$ is a value $x$ such
that $x = f(x)$. Substituting |fix|$(f)$ for $x$ gives the definition
above. %A4

From an algorithmic viewpoint, the definition of only works because of
lazy evaluation in Haskell. To compute |fix g| Haskell evaluates
|g (fix g)| but does not immediately evaluate the argument |fix g|.
Remember that arguments in Haskell are only evaluated if they are *needed*.
Instead it begins evaluating the body of |g|, which may or may not
use its argument. %A5

 ### A Non-Recursive Definition of |fix|

It is also possible to define |fix| non-recursively, by using *self application*.
Self application is when a function is applied to itself.
This works because functions are values, so a function can be
passed as an argument to itself. For example, consider the identity
function, which simply returns its argument: %A2

> id x = x
> -- %A3

The identity function can be applied to *any* value, because it doesn't
do anything with the argument other than return it. Since it can be
applied to any value, it can be applied to itself: %A4

> testID = id(id)   -- returns id
> -- %A5

Self application is not a very common technique, but it is certainly
interesting. Here is a higher-order function that takes a function
as an argument and immediately applies the function to itself: %A6

````
stamp f = f(f)
````

Unfortunately, the |stamp| function cannot be coded in Haskell, because it is
rejected by Haskell's type system. When a function of type $a \rightarrow b$
is applied to itself, the argument type $a$ must be equivalent to $a \rightarrow b$.
There are no types in the Haskell type system that can express a solution
to type equation $a = a \rightarrow b$. Attempting to define |stamp| results
in a Haskell compile-time error: %A8

    Occurs check: cannot construct the infinite type: t1 = t1 -> t0 %A9

Many other languages allow |stamp| to be defined, either using more complex or
weaker type systems. Dynamic languages do not have any problem defining |stamp|.
For example, here is a definition of |stamp| in JavaScript: %A10

````Java
stamp = function (f) { return f(f); }
````

The interesting question is what happens when
|stamp| is applied to itself: |stamp(stamp)|. This call binds |f| to |stamp|
and then executes |f(f)| which is |stamp(stamp)|. The effect is an
immediate infinite loop, where stamp is applied to itself over and over
again. What is interesting is that |stamp| is not recursive, and it does
not have a while loop. But it manages to generate an infinite loop anyway. %A12

Given the ability to loop infinitely, it is also possible to execute
a function infinitely many times. %A13

````
fix g = stamp (g . stamp)
````

TODO: explain composition (|.|) operator %A15

Here are the steps in executing |fix| for a function |g|: %A16

* |fix g| %A17

    { definition of |fix|} %A18
* = |stamp (g . stamp)| %A19

    { definition of |stamp|} %A20
* = |(g . stamp)(g . stamp)| %A21

    { definition of |.|} %A22
* = |g(stamp(g . stamp))| %A23

    { definition of |fix|} %A24
* = |g(fix g)| %A25

This version of |fix| uses self-application to create
a self-replicating program, which is then harnessed as
an engine to invoke a function infinitely many times.
This version of |fix| is traditionally written as
$\lambda g. (\lambda x. g(x x)) (\lambda x. g(x x))$,
but this is the same as the version given above with
the definition of |stamp| expanded. %A26

A second problem with this definition of |fix| is that it
*diverges*, or creates an infinite loop, when executed
in non-lazy languages. Thus it cannot be used in Haskell
because of self-application, and it cannot be used in
most other languages because of strict evaluation. A
non-strict version can be defined: %A27

Y = |stamp|($\lambda$f.($\lambda$x.f($\lambda$v.(|stamp| x v)))) %A28

Finally, explicit fixed points involve creation of
many closures. %A29

 ## Understanding Recursion with Self-Application

Another way to implement recursion is by writing
self-application directly into a function. For
example, here is a non-recursive version of fact
based on integrated self-application, defined in JavaScript. %Unde2

````
fact_s = function (f, n) {
  if (n == 0)
    return 1;
  else
    return n * f(f, n - 1);
}
````

To call this function, it is necessary to pass itself
as an argument to itself: %Unde4

````
fact_s(fact_s, 10);
````

This definition builds the self-application into
the |fact_s| function, rather than separating it
into a generator and a fixed point function.
One way to derive |fact_s| is from the self-applicative
|fix| function. Remember that %Unde6

````
fact = stamp (g_fact . stamp)
````

|fact_s| is created by *merging* |g_fact| with |stamp|.
The other use of |stamp| indicates that |fact_s| must be
applied to itself to compute a factorial. %Unde8

One interesting thing about this final implementation
strategy is that it is *exactly* the strategy used in
the actual implementation of languages like C++ and Java. %Unde9

 # Computational Strategies {#Monads}

In previous sections the Exp language was extended with
specific kinds of expressions and values, for example
the |let| and |functions|. In addition to augmenting the
language with new expression types, it is also possible to
consider extensions that have a general impact on every part of the
language. Some examples are error handling, tracing of code,
and mutable state.

 ## Handling Errors

Error handling
is a pervasiave feature of a language, beause it affects 
the way that every expression is evaluated. For example,
the expression |a+b| may not cause any errors,
but if evaluating |a| or |b| can cause an error,
then the evaluation of |a+b| will have to deal with the 
possiblity that |a| or |b| is an error.

Error handling is a notorious problem in programming languages.
When coding in C, everyone agrees that the return codes of
all system calls should be checked to make sure that an error
did not occur. However, most C programs don't check the return
codes, leading to serious problems when things start to go wrong.

Errors are pervasiave because any expression
can either return a value or it can signal an error.
One way to represent this possibility is by defining
a new data type that has two possibilities: either a
*good* value or an error. 

> data Checked a = Good a | Error String
>  deriving Show

The declaration defines a generic |Checked| type that has a parameter |a|
representing the type of the good value. The |Checked| type has two constructors,
|Good| and |Error|. The |Good| constructor takes a value of type |a|
and labels it as good. The |Error| constructor has an error message.

To keep things simple and focused on error handling, 
this section will only consider expressions
with literals, varibles, binary operators. This smaller language is 
similar to the one that was introduced at the beginning of the book.
More features will be added later.
Although the syntax of expressions does not have to change, but the
type of the |evaluate| function must be changed to return an |Error|
value:

> evaluate'10 :: Exp'7 -> Env'7 -> Checked Value'7
> evaluate'10 exp env = eval exp
>   where
>     eval (Literal'7 v)      = Good v

Evaluation of a literal can never cause an error. The value is marked
as a |Good| value and returned. 

A variable can be undefined, 
so it evaluating a variable may return an error:

>     eval (Variable'7 x)     = 
>        case lookup x env of
>          Nothing -> Error ("Variable " ++ x ++ " undefined")
>          Just v  -> Good v

The case for binary operations is more interesting. 
Here is the original rule for evaluating binary expressions:

````
     eval (Binary'7 op a b)  = binary'7 op (eval a) (eval b)
````

The problem is that either |eval a| or |eval b| could return an |Error| value.
The actual binary operation is only performed if they both return |Good| values.
Finally, the binary operation itself might cause a new error. Thus there are
three places where errors can arise: in |eval a|, in |eval b|, or in |binary|.
This definition for |eval| of a binary operator handles the first two situations:

>     eval (Binary'7 op a b)  = 
>         case eval a of
>           Error msg -> Error msg
>           Good av -> 
>             case eval b of
>               Error msg -> Error msg
>               Good bv -> 
>                 checked_binary op av bv

Now it should be clear why error return codes are not always checked. What was
originally a one-line program is now 8 lines and uses additional temporary
variables. The |binary| helper
function must be updated to signal divide by zero:

````
checked_binary :: BinaryOp -> Value'7 -> Value'7 -> Checked Value'7
checked_binary Div (Scalar'7 (Int a))  (Scalar'7 (Int b))  = 
  if b == 0 
  then Error "Divide by zero" 
  else Good (Scalar'7 (Int (a `div` b)))
checked_binary op a b = Good (binary'7 op a b)
````

All the other cases are the same as before, so |checked_binary|
calls |binary| and then tags the resulting value as |Good|.

--------------------BEGIN-HIDE-------------------------

An |if| expression must deal with the possibility that
the condition is either an error or is not a boolean value:

>     eval (If'7 a b c) =
>       case eval a of
>         Error msg -> Error msg
>         Good v ->
>           case v of 
>             Scalar'7 (Bool t) -> eval (if t then b else c)
>             o    -> Error ("Condition value " ++ show o ++ " not a boolean")

Recursive let expressions are the most complex. The problem is that an 
error during evaluation can cause the new environement to be bad. Having a 
bad environment is a problem because it cannot be used for evaluation.

>     eval (Let'7 x exp body) = 
>         case newEnv of
>           Error msg -> Error msg
>           Good goodEnv -> evaluate'10 body goodEnv
>       where newEnv = case evaluate'10 exp (fix_env newEnv) of
>                         Error msg -> Error msg
>                         Good val -> Good ((x, val) : env)

To solve this problem a dummy environment is used in case the
creation of the new environment fails.

> fix_env (Error _ ) = []
> fix_env (Good env) = env
> -- %Summ3 

> checked_binary :: BinaryOp -> Value'7 -> Value'7 -> Checked Value'7
> checked_binary Div (Scalar'7 (Int a)) (Scalar'7 (Int b)) = 
>   if b == 0 
>   then Error "Divide by zero" 
>   else Good (Scalar'7 (Int (a `div` b)))
> checked_binary op a b = Good (binary'7 op a b)
> --------------------END-HIDE-------------------------

Evaluating an expression may now return an error for unbound variables:

> testUBV = evaluate'10 (Variable'7 "x") []

The result of evaluation is:

    Error "Variable x undefined"

Or for divide by zero:    

> testDBZ2 = evaluate'10 (Binary'7 Div (Literal'7 (Scalar'7 (Int 3))) (Literal'7 (Scalar'7 (Int 0))) ) []

The result of evaluation is:

    Error "Divide by zero"

Your take-away from this section should be that checking error everywhere
is messy and tedious. The code for binary operators has to deal with 
errors, even though most binary operators don't have anything to do with
error handling. 

 ### Exercise: Complete Error Handling
 
Extend the evaluator with error handling to implement the 
remaining expression cases, including
|if|, |let|, and function definition/calls.
As a bonus, implement error checking for recursive |let|
expressions.

 ## Mutable State

A second common pervasic computational strategy, besides
error handling, is the use of *mutable state*. Mutable state
means means that the state of a program changes or mutates:
that a variable can be assigned a new value or a part of a
data structure can be modified. Mutable state is a pervasive
feature becuase it is something that happens in addition to
the normal computation of a value or result from a function.

Here is one typical example of a program that uses mutable varables.
The code is valid in C, Java or JavaScript:

````Java
x = 1;
for (i = 2; i <= 5; i = i + 1) {
  x = x * i;
}
````

It declares a local variable named |x| with inital value |1| 
and then performs an iteration where the variable |i| changes
from 1 to 10. On each iteration of the loop the variable |x|
is multiplied by |i|. The result of |x| a the end is the 
factorial of 5, namely 120.

Another typical example of mutable state is modification of
data structures. The following code, written in JavaScript, 
creates a circular data structure

```Java
record = { first: 2, next: null };
record.next = record;
```

Roughly equivalent code could be implemented in C or Java (or
any other imperative language), although
the resulting code is usually somewhat longer.

It would be easy to recode the factorial example above as a pure functional
program. With more work it may be possible to encoding the circular data
structure as well. But the point of this book is not to teach you how to do functional
programming. The point is to explain programming languages, and to code the
explanation explicitly as an evaluator. Since many 
programming languages allow mutable values, it is important to be able to 
explain mutation. But we cannot *use* mutation to provide the explanation,
because we have chosen to write the evaluator in Haskell, a pure funtional language.
The hope is that detailed and explicit analysis of how mutation
works in programming languages will 
lead to insights about the costs and benefits of using mutation. 

 ### Addresses 
 
Imperative languages typically allow everything to be mutable
by default: all variables and mutable and all data structures
are mutable. While this is often convenient, it has the disadvantage
that there is no way to turn off mutation. Many variables and 
data structures, even in imperative languages, are logically immutable.
Even when the programmer *intends* for the variables or data structure
to be constant and unchanging, there is no way in most imperative languages for
the programmer to make this intention explicit.

To rectify this situation, at the cost of being somewhat unconventional,
this book takes a different approach to mutable state, where mutability must
be explicitly declared. Variables are not mutable by default. Instead
a new kind of value, an *address*, is introduced to support mutation.
An address identifies a mutable container that stores a single value, 
but whose contents can change over time. The storage identified by an 
address is sometimes called a *cell*. You can think of it as a *box* 
that contains a value.  Addresses are sometimes called *locations*.
(Note that the concept of an adress of a mutable container is also used in
ML and BLISS for mutable values, where they are known as |ref| values. This is
also closely related to the concept of an address of a memory cell, as it appears
in assembly language or C).

There are three fundamental operations involving addresses: creating a new cell with an
initial value and a new address, accessing
the current value at a address, and changing the value stored at an address. 
The following table gives the concrete syntax of these operations.

Operation        Meaning
---------------- -----------------------
|Mutable(e)|     Creates a mutable cell with initial value given by |e|
|!a|             Accesses the contents stored at address |a|
|a := e|         Updates the contents at address |a| to be value of expression |e|

Using these operations, the factorial program given above can be expressed
as follows, using mutable cells:

````Java
x = Mutable(1);
for (i = Mutable(2); !i <= 5; i := !i + 1) {
  x := !x * !i;
}
````

In this model a variable always denotes the address to which it is bound.
If the variable |x| appears on the right side of an assignment, it must be *dereferenced*
as |!x|. If the variable appears on the left side of an assignment, it
denotes an address that is updated. 

It should be clear that the *variables* don't actually change in this model.
The variables are bound to an address, and this binding does not change. What
changes is the value stored at an address. 
This interpretation resembles the computational model underlying C, 
where address identify memory cells. (TODO: make more careful 
comparison to C, with attention to *l-values* and *r-values*) 

An address is a new kind of value. Although addresses can be
represented by any unique set of labels, one convenient representation for
addresses is as integers. Using integers as addresses is also similar to the
use of integers for addresses in a computer memory.

> data Value'9 = Scalar'9 Value
>            | Closure'9 String Exp'9 Env'9
>            | Address Int        -- new
>   deriving (Eq, Show)

When writing programs and values, it is usefull to distinguish addresses 
from ordinary integer values. As a convention, addresses will be tagged with 
an "at sign", so that |Address 3| will be written @3.

Another advantage of explicit cells for mutability is that the treatment of local variables given
in previous chapters is still valid. Variables are still immutably bound
to values. By introducing a new kind of value, namely addresses, it is possible
to bind a variable to an address. It is the content stored at an address 
that changes, not the variable.
(reminds me of the line of The Matrix: "it is not the spoon that bends...")
Introducing cells and addresses does not fundamentally change the nature or capabilities of
imperative languages, it just modifies how the imperative features
are expressed.
 
 #### Memory
 
The current value of all mutable cells used in a program can be represented 
in many different ways. Logically, a memory is a map or association of
addresses to values. The same techniques used for environments could
be used for memories, as a list of pairs or a function. 
Memory can also be represented as a function mapping integers
to values, similar to the [representation of environments as functions](#EnvAsFun).

But since addresses are integers, one natural
representation is as a list or array of values, where the address is
the position or index of the value. 
Such an array is directly analogous
to the *memory* of a computer system, which can be thought of as
an array of 8 bit values. In this chapter memory will be implemented
as a list of values, although many other representations are certainly
possible.

> type Memory = [Value'9]

One complication is that the memory must be able to *grow* by adding
new addresses. The initial empty memory is the empty list |[]|.
The first address added is zero [@0]. The next address is one to 
create a memory [@1, @0]. In general a memory with $n$ cells will
have addresses [$n-1$, ..., 1, 0]. 
Here is an example memory, with two addresses:

````
[Value'7 (Scalar'7 (Int 6)), Value'7 (Scalar'7 (Int 120))] 
````

This memory has value 6 at address 1 and value 120 at address 0.
More concisely, this memory can be written as

[11, 120]

This memory could be the result of executing the factorial program given above,
under the assumption that |i| is bound to address 1 and |x| is bound to address 0.
An appropriate environment is:

[|i| $\mapsto$ @1, |x| $\mapsto$ @0]

During the execution of the program that computes the factorial of 5, there
are 10 different memory configurations that are created:

Step                    Memory                  
-------------------     ------------------------------------------
*start*                 $[]$
|x = Mutable(1);|       $[1]$
|i = Mutable(2);|       $[2, 1]$
|x = !x * !i;|          $[2, 2]$
|i = !i + 1;|           $[3, 2]$
|x = !x * !i;|          $[3, 6]$
|i = !i + 1;|           $[4, 6]$
|x = !x * !i;|          $[4, 24]$
|i = !i + 1;|           $[5, 24]$
|x = !x * !i;|          $[5, 120]$
|i = !i + 1;|           $[6, 120]$

 ### Pure Functional Operations on Memory

The two fundamental operations on memory are memory *access*,
which looks up the contents of a memory cell, and *update*, which
modifies the contents of a memory cell. 

 #### Access
 
The memory |access| function takes a memory address $i$
and a memory (list) and returns the item of the list at position
$i$ counting from the right of the list.
The Haskell function |!!| returns the $n$th item of a list,
so it almost serves as an implemenetation for the memory |access|
function. However, the |!!| function counts from the *left* of the
list, not the right. To compute an index from the right of a list,
the index must be subtracted from the length of the list:

> access i mem = mem !! (length mem - i - 1)

 #### Update 
 
It is not possible to actually *change* memory in pure functional
languages, including Haskell, because there is no way to modify a
data structure after is has been constructed.
But it is possible to compute a new data struture
that is based on an existing one. This is the notion of 
*functional update* or *functional change*: a function can
act as a transformation of a value into a new value.
A functional update to memory is a function of type |Memory -> Memory|
Such functions take a memory as input and create a *new* memory
as an output. The new memory is typically nearly identical to the
input memory, but with a small change.

For example, the |update| operator on memory replaces the
contents of a single address with a new value.

> update :: Int -> Value'9 -> Memory -> Memory
> update addr val mem = 
>   let (before, _:after) = splitAt (length mem - addr - 1) mem in
>     before ++ [val] ++ after

The |update| function works by splitting the memory into the part
before the address and the part starting with the address |addr|. The pattern
$_:after$ binds $after$ to be the memory after the address. The |update|
function then recreates a new memory containing the before part,
the updated memory cell, and the after part. The function
is innefficient because it has to copy all the memory cells it has scanned up to that
point! We are not worried about efficiency, however, so just relax. It is 
fast enough.

Using |access| and |update| it is possible to define interesting *transformations*
on memory.
For example, the function |mul10| multiplies the contents of a memory address by 10:

> mul10 addr mem = 
>   let n = fromInt (access addr mem) in
>     update addr (toValue (10 * n)) mem
>
> fromInt (Scalar'9 (Int n)) = n
> toValue n = (Scalar'9 (Int n))

Here is an example calling |mul10| on a memory with 4 cells:

> testMul10 = mul10 1 [toValue 3, toValue 4, toValue 5, toValue 6]

The result is 

    [Scalar'9 (Int 3), Scalar'9 (Int 4), Scalar'9 (Int 50), Scalar'9 (Int 6)]

The fact that |mul10| is a transformation on memory is evident from its type:

> mul10 :: Int -> Memory -> Memory

This means that |mul10| takes an memory address as an input and returns
a function that transforms an input memory into an output memory. 

 ### Semantics of a Language with Mutation

The first step in creating a function with mutable cells is to add
abstract syntax for the three operations on mutable cells. The following
table defines the abstract syntax:

Operation        Abstract Syntax    Meaning
---------------- ------------------ -----------------------
|Mutable(e)|     |Mutable e|        Allocate memory
|!a|             |Access a|         Accesses memory
|a := e|         |Assign a e|       Updates memory

The abstract syntax is added to the data type representing expressions in
our language:

````
data Exp'9 = ...
         | Mutable'9   Exp'9         -- new 
         | Access'9    Exp'9         -- new
         | Assign'9    Exp'9 Exp'9   -- new 
````

TODO: Explain basic strategy, of returning new value and memory.

````
    eval (Mutable'9 e) mem = 
      let (ev, mem') = eval e mem
          i = newAddress mem'
      in
          (Address i, update i ev mem')
````

> newAddress mem = length mem

TODO: explain

````
    eval (Access'9 a) mem = 
      let (Address i, mem') = eval a mem in
          (access i mem', mem')
````

TODO: explain

````
    eval (Assign'9 a e) mem = 
      let (Address i, mem') = eval a mem in
        let (ev, mem'') = eval e mem' in
          (ev, update i ev mem'')
````

The interesting thing is that even parts of the evaluator
that have nothing to do with mutable cells have to be 
copmletely rewritten:

````
    eval (Binary'9 op a b) mem =
      let (av, mem') = eval a mem in
        let (bv, mem'') = eval b mem' in
          (binary'9 op av bv, mem'')
````

 ### Summary of Mutable State

Again, the take-away should be that mutation is messy when
programmed in this way. Mutation affects every part of the
evaluation process, even for parts that are not involved
with creating or manipulating mutable cells.

Here is the complete code for mutable cells.

> data Exp'9 = Literal'9   Value'9
>          | Unary'9     UnaryOp Exp'9
>          | Binary'9    BinaryOp Exp'9 Exp'9
>          | If'9        Exp'9 Exp'9 Exp'9
>          | Variable'9  String
>          | Let'9       String Exp'9 Exp'9
>          | Function'9  String Exp'9
>          | Call'9      Exp'9 Exp'9
>          | Mutable'9   Exp'9         -- new 
>          | Access'9    Exp'9         -- new
>          | Assign'9    Exp'9 Exp'9   -- new 
>   deriving (Eq, Show)
>
> type Env'9 = [(String, Value'9)]
>

All the existing parts of the evaluator are modified:

> evaluate'9 :: Exp'9 -> Env'9 -> Memory -> (Value'9, Memory)
> evaluate'9 exp env mem = eval exp mem
>   where
>     eval (Literal'9 v) mem    = (v, mem)
>     eval (Unary'9 op a) mem   = 
>       let (av, mem') = eval a mem in
>         (unary'9 op av, mem')
>     eval (Binary'9 op a b) mem =
>       let (av, mem') = eval a mem in
>         let (bv, mem'') = eval b mem' in
>           (binary'9 op av bv, mem'')
>     eval (If'9 a b c) mem =
>       let (av, mem') = eval a mem in
>         eval (if fromBool'9 av then b else c) mem'
>     eval (Variable'9 x) mem = (fromJust (lookup x env), mem)
>     eval (Let'9 x e body) mem =
>       let (ev, mem') = eval e mem
>           newEnv = (x, ev) : env
>       in
>         evaluate'9 body newEnv mem'
>     eval (Function'9 x body) mem = (Closure'9 x body env, mem)
>     eval (Call'9 f a) mem  = 
>       let (Closure'9 x body closeEnv, mem') = eval a mem
>           (av, mem'') = eval a mem'
>           newEnv = (x, av) : closeEnv
>       in
>           evaluate'9 body newEnv mem''

Here are the mutation-specific parts of the evaluator:

>     eval (Mutable'9 e) mem = 
>       let (ev, mem') = eval e mem
>           i = newAddress mem'
>       in
>           (Address i, update i ev mem')
>     eval (Access'9 a) mem = 
>       let (Address i, mem') = eval a mem in
>           (access i mem', mem')
>     eval (Assign'9 a e) mem = 
>       let (Address i, mem') = eval a mem in
>         let (ev, mem'') = eval e mem' in
>           (ev, update i ev mem'')
>
> fromBool'9 (Scalar'9 (Bool b)) = b
>
> unary'9 op (Scalar'9 a) = Scalar'9 (unary op a)
> binary'9 op (Scalar'9 a) (Scalar'9 b) = Scalar'9 (binary op a b)

 ## Abstracting Computational Strategies
 
At first glance it does not seem there is anything that can be
done about the messy coding involved in implementing errors
and mutable state. These features are *aspects* of the evaluation
process, because they effect all the code of the evaluator, not
just the part that directly involves the new feature. 

What is worse is that combining the code for errors and mutable
state is not possible without writing yet another completely 
different implementation. The *feature* of our evaluator are not
implemented in a modular way.

One solution to this problem is to use a *monad*. That is the
subject of this section!




 # More Chapters on the way...
 ## Abstract Interpretation and Types
 ## Data Abstraction: Objects and Abstract Data Types
 ## Algebra and Coalgebra
 ## Partial Evaluation
 ## Memory Management
 
 

 
> --------------------BEGIN-HIDE-------------------------
> -- %I2

 # Pervasive Computational Strategies {#Monads}
 ## Strategies
 ### Failure
 ### Mutable State
 ### Identifying a Common Pattern
 ## Defining Computational Strategies as Monads
 ### The Monad Pattern
 ### Maybe Monad
 ### State Monad
 ### List Monad
 ### Special Kinds of States: Readers and Writers
 ## Order of Evaluation
 ### Strict versus non-strict
 ### Lazy

 # Domain-Specific Languages
 ## Parsing
 ## Attribute grammars
 ## State machines (???)

 # Abstract Interpretation and Types
 ## Abstract Interpretation
 ## Type Checking
 ## Soundness Proofs
 ### Progress
 ### Preservation

 # Data Abstraction (content from essay)
 ## Abstract Data Types
 ## Objects (content from essay and partial evaluation paper)
 ### Inheritance & Delegation

 # Alternative Approaches to Semantics
 ## Rule-based Specifications
 ## Small-step semantics

 # Additional Topics
 ## Algebra and Coalgebra
 ## Partial Evaluation
 ## Memory Management (??)
 ## Continuations

 > --------------------END-HIDE------------------------- %Cont2

 # References