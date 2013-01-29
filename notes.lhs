 # Preface #

 ## What? ##

This document is a series of notes about programming languages, intended for
students of the undergraduate programming languages course at UT.

 ## Why? ##

I'm writing these notes because I want to teach the theory of programming 
languages with a practical focus, but I don't want to use Scheme (or ML) as the 
host language. Thus many excellent books do not fit my needs, including 
*Programming Languages: Application and Interpretation* by Shriram Krishnamurthi, and
*Essentials of Programming Languages* by 
Daniel P. Friedman, Mitchell Wand, and Christopher T. Haynes.
I asked Shriram if he would let me create a Haskell version of his textbook, but
he said no. Hence I seem to be left to write some notes myself.

 ## Who? ##

These notes assume knowledge of programming, and in particular assume some
knowledge of programming in Haskell. When I teach the course I give a few hours
of lectures to introduced Haskell, and point students to the many excellent 
tutorials on Haskell. Search Google for "Haskell Tutorial" to find one. I recommend

 # Introduction #

In order to understand programming languages, it is useful to spend some time thinking
about *languages* in general. Usually we treat language like the air we breath:
it is everywhere but it is invisible. I say that language is invisible because we are
usually more focused on the message, or the content, that is being conveyed than
on the structure and mechanisms of the language itself. Even when we focus on 
our use of language, for example in writing a paper or a poem, we are 
still mostly focused on the message we want to convey, while working with (or struggling 
with) the rules and vocabulary of the language as a given set of constraints. 
A good language is invisible, allowing us to speak and write our intent clearly and
creatively.

The same is true for programming. Usually we have some important goal in mind when
writing a program, and  the programming language is a vehicle to achieve the goal. 
In some cases the language may fail us,
by acting as an impediment or obstacle rather than an enabler. The normal reaction in
such situations is to work around the problem and move on. 

The study of language, including the study of programming languages, requires a 
different focus. We must examine the language itself, as an artifact. What are its rules?
What is the vocabulary? How do different parts of the language work together to 
convey meaning? A user of a language has an implicit understanding of answers to
these questions. But to really study language we must create an explicit description
of the answers to these questions.

The concepts of structure and meaning have technical names. The structure of
a language is called its *syntax*. The rules that defined the meaning of a language
are called *semantics*. Syntax is a particular way to structure information, while
semantics can be viewed as a mapping from syntax to its meaning, or interpretation.
The meaning of a program is usually some form of behavior, because programs
*do* things. 
Fortunately, as programmers we are adept at describing the structure of information,
and at creating mappings between different kinds of information and behaviors.
This is what data structures and functions/procedures are for.

Thus the primary technique in these notes is to use programming to study programming
languages. In other words, we will write programs to represent and manipulate programs.
One general term for this activity is *metaprogramming*. A metaprogram is
any program whose input or output is a program. Familiar examples of metaprograms 
include compilers, interpreters, virtual machines. In this course we will read, write and 
discuss many metaprograms.

 # Simple Language of Arithmetic*

A simple place to start is analyzing the language of simple arithmetic, which is 
familiar to every grade-school child:

    4
    -5+6
    3--2--7
    1*(8+5)
    1+8*2
    1+(8*2)

These are examples of arithmetic *expressions*. The rules for understanding
such expressions are surprisingly complex. For example, in the third expression
the first and third minus signs ($-$) mean subtraction, while the second
and fourth mean that the following number is negative. The last two
examples mean the same thing, because of the rule that multiplication must be
performed before addition. For more complex expressions, 
even advanced students can be confused about whether
$-3^2$ means $(-3)^2$ or $-(3^2)$.

Part of the problem here is that there is a big difference between our conceptual
view of what is going on in arithmetic and our particular conventions for expressing
arithmetic expressions in written form. In other words, there isn't any confusion about
what negative number are or what subtraction or exponentiation do, but there is
room for confusion about how to write them down.

The conceptual structure of a given expression can be defined much more 
clearly using pictures. For example, the following pictures make are a
clear description of the underlying arithmetic operations specified in the
expressions given above:

![Graphical illustration of abstract structure](abstract_syntax.eps)

The last picture represents both the last expressions in the previous example.
This is because the pictures do not need parentheses, since the grouping
structure is explicit.

The conceptual structure (illustrated by the pictures) is called the *abstract
syntax* of the language. The particular details and rules for writing expressions
as strings of characters is called the *concrete syntax*. The abstract
syntax for arithmetic expressions is very simple, while the concrete syntax
is quite complex. As a result, for now we will focus on the abstract syntax,
and deal with concrete syntax later.

 ## Abstract Syntax in Haskell ##

Arithmetic expressions are easily represented in Haskell as a data type.

> data Exp = Number   Int
>          | Add      Exp Exp
>          | Subtract Exp Exp
>          | Multiply Exp Exp

This data type defines four representational variants, one for numbers,
one for unary negation, and two for the binary operators of addition and multiplication.

The examples above are easily written as data values using the constuctors of the
`Exp` data type:

> t1 = Number 4
> t2 = Add (Number (-4)) (Number 6)
> t3 = Subtract (Number 3) (Subtract (Number (-2)) (Number (-7)))
> t4 = Multiply (Number 1) (Add (Number 8) (Number 5))
> t5 = Add (Number 1) (Multiply (Number 8) (Number 2))

NOTE: It is not legal to write `Add (-4) 6` because `-4` and `6`
are of type `Int` not `Exp`. Also, Haskell requires parentheses
around negative numbers, for some reason.

 ## Evaluation ##

The normal meaning assigned to arithmetic expressions is the evaluation of the
arithmetic operators to compute a final answer. This evaluation process is
defined by cases in Haskell:

> eval (Number n)      = n
> eval (Add a b)       = eval a + eval b
> eval (Subtract a b)  = eval a - eval b
> eval (Multiply a b)  = eval a * eval b

To test this program we can execute

> main1 = do
>   putStr "Evaluating the following expression:\n"
>   print t3
>   putStr "Produces the following result:\n"
>   print (eval t3)

The output is 

	Evaluating the following expression:
	Subtract (Number 3) (Subtract (Number (-2)) (Number (-7)))
	Produces the following result:
	-2

This looks pretty good, except that the default `Show` format for
expressions is quite ugly.

 ## Formatting ##
 
Another way to interpret abstract `Exp` values is as
a string that corresponds to our normal way of writing arithmetic
expressions, with binary operators for `+` and `*`.

```haskell
instance Show Exp where
  show (Number n)      = show n
  show (Add a b)       = showBinary a "+" b
  show (Subtract a b)  = showBinary a "-" b
  show (Multiply a b)  = showBinary a "*" b
showBinary a op b = show a ++ op ++ show b
```

If you don't know about *instance* declarations in Haskell, please
go and read about *type classes* [need citation here!].

Note that the `show` function for expressions is fairly similar
to the `eval` function, but it performs string concatenation instead
of numeric operations. We can write a useful test function, 

> test fun input = putStr (show input ++ " ==> " ++ show (fun input) ++ "\n")

and then write tests for the show function and eval:

> main2 = do
>   test eval t1
>   test eval t2
>   test eval t3
>   test eval t4
>   test eval t5

Testing `show` produces less than satisfactory results:

	4 ==> 4
	-4+6 ==> 2
	3--2--7 ==> -2

We are back to the ambiguous expressions that we started with.
It is easy to add parentheses to remove the ambiguity:

```haskell
showBinary a op b = "(" ++ show a ++ ")" ++ op ++ "(" ++ show b ++ ")" 
```

But the results are still not very satisfying:

	4 ==> 4
	(-4)+(6) ==> 2
	(3)-((-2)-(-7)) ==> -2

We either have too many or too few parentheses. The right thing to do is
to check whether parentheses are needed, by comparing the *precedence* of
an operator with the *precedence* of the operators nested within it.
Multiplication `*` has higher precedence than addition `+` because 
we interpret `1+2*3` as `1+(2*3)` not `(1+2)*3`. In what follows, 
addition has precedence 1 and multiplication has precedence 2.

> instance Show Exp where
>   show e = showExp 0 e
  
> showExp level (Number n) = if n < 0 then paren (show n) else show n
> showExp level (Add a b)       = showBinary level 1 a "+" b
> showExp level (Subtract a b)  = showBinary level 1 a "-" b
> showExp level (Multiply a b)  = showBinary level 2 a "*" b
> showBinary outer inner a op b = 
>   if inner < outer then paren result else result
>      where result = showExp inner a ++ op ++ showExp inner b
> paren str = "(" ++ str ++ ")"

This definition produces an appealing result:

	4 ==> 4
	(-4)+6 ==> 2
	3-(-2)-(-7) ==> -2
	1*(8+5) ==> 13
	1+8*2 ==> 17

The example of formatting expression is a concrete illustration of 
the complexity of dealing with concrete syntax. The formatter 
converts abstract syntax into readable text. In a later chapter
we will develop a *parser* for expressions, which converts text into
abstract syntax.

> main = do
>   main1
>   main2

 ## Variables and Environments
 
[more to come...]

