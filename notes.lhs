% Anatomy of Programming Languages
% William R. Cook
% January 2013

> --------------------BEGIN-HIDE-------------------------
> import Prelude hiding (LT, GT, EQ)
> import Data.Maybe
>
> main_list = [main'1, main'2, main'4, main'5, check1, main'6]
> main = do
>   sequence [ do
>     putStr ("---- " ++ show i ++ " ----\n")
>     cmd | (i, cmd) <- zip [1..] main_list]
>   return ()
> check msg a b = putStr (if a == b then "OK\n" else "*** CHECK " ++ msg ++ " Failed ***\n")
> --------------------END-HIDE-------------------------

 # Preliminaries

 ## Preface

 ### What?

This document is a series of notes about programming languages, intended for
students of the undergraduate programming languages course at UT.

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
I asked Shriram if he would let me create a Haskell version of his textbook, but
he said no (although I recently learned that his book is published under
Creative Commons, so I could create a derived work, I decided in the end to 
branch out on my own). Hence I seem to be left to write some notes myself.

 ### Who?

These notes assume knowledge of programming, and in particular assume some
knowledge of programming in Haskell. When I teach the course I give a few hours
of lectures to introduced Haskell, and point students to the many excellent 
tutorials on Haskell. 
[Search Google for "Haskell Tutorial" to find one](https://www.google.com/search?q=Haskell+Tutorial). 
I recommend
[Learn You a Haskell for Great Good!](http://learnyouahaskell.com/).

 ## Introduction #

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

 # Expressions and Variables
 
 ## Simple Language of Arithmetic

A good place to start is analyzing the language of arithmetic, which is 
familiar to every grade-school child:

    4
    -5+6
    3--2--7
    3*(8+5)
    3+(8*2)
    3+8*2

These are examples of arithmetic *expressions*. The rules for understanding
such expressions are surprisingly complex. For example, in the third expression
the first and third minus signs ($-$) mean subtraction, while the second
and fourth mean that the following number is negative. The last two
examples mean the same thing, because of the rule that multiplication must be
performed before addition. The first expression is potentially confusing, 
even given knowledge of the rules for operations. It means $(3 - (-2)) - (-7)$
not $3 - ((-2) - (-7))$ because subtraction associates to the left.

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

 ### Abstract Syntax in Haskell

Arithmetic expressions can be represented in Haskell with the following data type:

> data Exp'1 = Number'1     Int
>          | Add'1        Exp'1 Exp'1
>          | Subtract'1   Exp'1 Exp'1
>          | Multiply'1   Exp'1 Exp'1

This data type defines four representational variants, one for numbers,
one for unary negation, and two for the binary operators of addition and multiplication.
A number that appears in a program is called a *literal*.

The examples above can be written as values of type `Exp` using the constructors
defined above:

> -- 4
> t1 = Number'1 4        
> -- -4 - 6
> t2 = Add'1 (Number'1 (-4)) (Number'1 6)        
> -- 3 - (-2) - (-7)  
> t3 = Subtract'1 (Number'1 3) (Subtract'1 (Number'1 (-2)) (Number'1 (-7)))
> -- 3 * (8 + 5)
> t4 = Multiply'1 (Number'1 3) (Add'1 (Number'1 8) (Number'1 5))        
> -- 3 + 8 * 2
> t5 = Add'1 (Number'1 3) (Multiply'1 (Number'1 8) (Number'1 2))

NOTE: It is not legal to write `Add (-4) 6` because `-4` and `6`
are of type `Int` not `Exp`. Also, Haskell requires parentheses
around negative numbers, for some reason.

Writing abstract syntax directly in Haskell is certainly very ugly.
There is approximately a 10-fold expansion in the number of characters
needed to represent a concept: a 5-character mathematical expression
`3 + 8 * 2` uses 47 characters to create the corresponding
Haskell data structure. This is not a defect of Haskell, it is 
merely a side effect of the lack of a proper parser, which we haven't
developed yet. 
Writing these data constructors explicitly is not something that we enjoy doing, 
but for now it is useful to
be very explicit about the representation of our programs.

For now we will continue to write expressions using the 
concise and familiar concrete syntax $3+7$, adding parentheses where
necessary. But keep in mind that this concise syntax is just
a short-hand for the real value `Add (Number 3) (Number 7)`.
As new features are added to the language, both the familar
concrete syntax and the abstract syntax will be extended.

 ### Evaluation

The normal meaning assigned to arithmetic expressions is the evaluation of the
arithmetic operators to compute a final answer. This evaluation process is
defined by cases in Haskell:

> evaluate'1 :: Exp'1 -> Int
> evaluate'1 (Number'1 i)      = i
> evaluate'1 (Add'1 a b)       = evaluate'1 a + evaluate'1 b
> evaluate'1 (Subtract'1 a b)  = evaluate'1 a - evaluate'1 b
> evaluate'1 (Multiply'1 a b)  = evaluate'1 a * evaluate'1 b

To test this program we can execute the following main program:

> main'1 = do
>   putStr "Evaluating the following expression:\n  "
>   print t3
>   putStr "Produces the following result:\n  "
>   print (evaluate'1 t3)

The output is 

    Evaluating the following expression:
      Subtract (Number 3) (Subtract (Number (-2)) (Number (-7)))
    Produces the following result:
      -2

This looks pretty good, except that the default `Show` format for
expressions is quite ugly.

 ### Formatting
 
Another way to interpret abstract `Exp` values is as
a string that corresponds to our normal way of writing arithmetic
expressions, with binary operators for `+` and `*`.

```haskell
instance Show Exp where
  show (Number i)      = show i
  show (Add a b)       = showBinary a "+" b
  show (Subtract a b)  = showBinary a "-" b
  show (Multiply a b)  = showBinary a "*" b
showBinary a op b = show a ++ op ++ show b
```

If you don't know about *instance* declarations in Haskell, please
go and read about *type classes* [need citation here!].

Note that the `show` function for expressions is fairly similar
to the `eval` function, but it performs string concatenation instead
of numeric operations. Since we will be testing many different
kinds of functions, it is useful to write a generalized test function.

> test fun input = do 
> --------------------BEGIN-HIDE-------------------------
>   putStr "    "
> --------------------END-HIDE-------------------------
>   putStr (show input)
>   putStr " ==> "
>   putStr (show (fun input))
>   putStr "\n"

The `test` function takes a function and an input as arguments. It
prints the input and then prints the result of applying the function to the input.
The following main program invokes `test` to evaluate each of the 
five sample expressions defined above:

> main'2 = do
>   test evaluate'1 t1
>   test evaluate'1 t2
>   test evaluate'1 t3
>   test evaluate'1 t4
>   test evaluate'1 t5

Running this main program produces less than satisfactory results:

    4 ==> 4
    -4+6 ==> 2
    3--2--7 ==> -2
    1*8+5 ==> 13
    1+8*2 ==> 17
    
We are back to the ambiguous expressions that we started with.
The ambiguity can be resolved by adding parentheses around
every expression:

```haskell
showBinary a op b = paren (show a) ++ op ++ paren (show b)
```

> paren str = "(" ++ str ++ ")"

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

> instance Show Exp'1 where
>   show e = showExp 0 e
  
> showExp level (Number'1 i) = if i < 0 then paren (show i) else show i
> showExp level (Add'1 a b)       = showBinary level 1 a " + " b
> showExp level (Subtract'1 a b)  = showBinary level 1 a " - " b
> showExp level (Multiply'1 a b)  = showBinary level 2 a "*" b
>
> showBinary outer inner a op b = 
>   if inner < outer then paren result else result
>      where result = showExp inner a ++ op ++ showExp inner b

The add and subtract operators are also modified to include a little more space.
This definition produces an appealing result:

    4 ==> 4
    (-4) + 6 ==> 2
    3 - (-2) - (-7) ==> -2
    1*(8 + 5) ==> 13
    1 + 8*2 ==> 17

The example of formatting expression is a concrete illustration of 
the complexity of dealing with concrete syntax. The formatter 
converts abstract syntax into readable text. In a later chapter
we will develop a *parser* for expressions, which converts text into
abstract syntax.

 ## Variables

Arithmetic expressions often contain variables in addition
to constants. In grade school the first introduction to variables
is usually to *evaluate an expression relative to a set of variables*
For example, young students learn to evaluate $x+2$ where $x=5$.
The rule is to substitute every occurrence of $x$ with the value $5$
and the perform the required arithmetic computations.

To program this in Haskell, we the first thing we need is a representation for
expressions with variables. Since the name of a variable "x" can be represented
as a string of characters, it is easy to represent variables as an additional
kind of expression. The following data definition modifies `Exp` to include
a `Variable` case.

> data Exp'2 = Number'2   Int
>          | Add'2      Exp'2 Exp'2
>          | Subtract'2 Exp'2 Exp'2
>          | Multiply'2 Exp'2 Exp'2
>          | Variable'2 String
> --------------------BEGIN-HIDE-------------------------
>    deriving Eq
> instance Show Exp'2 where
>   show e = showExp'2 0 e
>
> showExp'2 level (Number'2 i) = if i < 0 then paren (show i) else show i
> showExp'2 level (Add'2 a b)       = showBinary'2 level 1 a " + " b
> showExp'2 level (Subtract'2 a b)  = showBinary'2 level 1 a " - " b
> showExp'2 level (Multiply'2 a b)  = showBinary'2 level 2 a "*" b
> showExp'2 level (Variable'2 a)    = a
> showBinary'2 outer inner a op b = 
>   if inner < outer then paren result else result
>      where result = showExp'2 inner a ++ op ++ showExp'2 inner b
> --------------------END-HIDE-------------------------

An association of a variable $x$ with a value $v$ is called a *binding*,
which can be written $x \mapsto v$.
Bindings can be represented in Haskell as a pair. For example, the
binding of $x \mapsto 5$ can be represented as `("x", 5)`.

Given these data types, the process of *substitution* can be defined by cases.
What we want is a function that has the following behavior:

* substitute $x$ for 5 in $x+2$   $\longrightarrow$ $5+2$
* substitute $x$ for 5 in $2$     $\longrightarrow$ $2$
* substitute $x$ for 5 in $x$     $\longrightarrow$ $5$
* substitute $x$ for 5 in $x*x+x$ $\longrightarrow$ $5*5+5$
* substitute $x$ for 5 in $x+y$   $\longrightarrow$ $5+y$

Note that if the variable names don't match, they are left alone.
The following Haskell function implements this behavior: 

> substitute1:: (String, Int) -> Exp'2 -> Exp'2
> substitute1 (var, val) exp = subst exp where
>   subst (Number'2 i)      = Number'2 i
>   subst (Add'2 a b)       = Add'2 (subst a) (subst b) 
>   subst (Subtract'2 a b)  = Subtract'2 (subst a) (subst b)
>   subst (Multiply'2 a b)  = Multiply'2 (subst a) (subst b)
>   subst (Variable'2 name) = if var == name 
>                           then Number'2 val 
>                           else Variable'2 name

The first case says that substituting a variable for a value in
a literal expression leaves the literal unchanged. The next three cases define 
substitution on binary operators as recursively substituting into the sub-expressions
of the operator. The final case is the only interesting one. It defines 
substitution into a `Variable'2` expression as a choice: if the variable in the
expression (`name`) is the *same* as the variable being substituted (`var`)
then the value is 

> x = Variable'2 "x"
> y = Variable'2 "y"
> main'4 = do 
>   test (substitute1 ("x", 5)) (Add'2 x (Number'2 2))
>   test (substitute1 ("x", 5)) (Number'2 2)
>   test (substitute1 ("x", 5)) x
>   test (substitute1 ("x", 5)) (Add'2 (Multiply'2 x x) x)
>   test (substitute1 ("x", 5)) (Add'2 x y)

Running these tests produces the following results:

    x + 2 ==> 5 + 2
    2 ==> 2
    x ==> 5
    x*x + x ==> 5*5 + 5
    x + y ==> 5 + y
    
It is important to keep in mind that we now have two stages for
evaluating an expression containing a variable. The first stage
is to *substitute* the variable for its value, then the second
stage is to *evaluate* the resulting arithmetic expression.

TODO: talk about *renaming* variables, or substituting one variable for another

 ### Variable Discussion
 
We are used to calling $x$ and $y$ "variables" without really thinking
much about what it means to be a "variable". A variable is something
that varies. But what does it mean for a name like $x$ to vary? 
My view on this it that we call them variables because they can have
different values in different contexts. For example, the equation
$\pi r^2$ defines a relationship between several variables, but
in the context of a particular word problem, the radius $r$ has a 
particular value.  In any particular context, 
a variable does *not* vary or change. It has exactly one
value that is fixed and constant within that context. A variable can
be bound to different values in different contexts, but in a given 
context the binding of a viable is fixed. In the examples above, the
context is indicated by the phrase "where $x=5$". The same expression,
$x+2$ can be evaluated in different contexts, for example, where $x=7$.

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
of the container will change. 

Mutatable variables this will be discussed in full later.
For now, just remember that a variable has a fixed binding to a value
in a given context.

Note that another common use for variables is to define *equations* or
*constraints*. In this case, it is normal to use algebraic operations to
simplify or *solve* the equation to find a value for the variable that
satisfies the equation. While equation solving and constraints are fascinating topics,
we will not discuss them directly in these notes. For our purposes, we will
assume that we already know the value of the variable, and that the problem 
is to compute a result using that value. 

 ### Environments

There can be multiple variables in a single expression. For example,
evaluating $2*x+y$ where $x=3$ and $y=-2$. A collection of bindings
is called an *environment*.

Since a binding is represented as a pair, an environment can be 
represented as a list of pairs. The envronment mentioned above
would be 

> e1 = [ ("x", 3), ("y", -1) ]

The corresponding type is

> type Env = [(String, Int)]

The substitution function is easily modified to work with
environments rather than single bindings:

> substitute:: Env -> Exp'2 -> Exp'2
> substitute env exp = subst exp where
>   subst (Number'2 i)      = Number'2 i
>   subst (Add'2 a b)       = Add'2 (subst a) (subst b) 
>   subst (Subtract'2 a b)  = Subtract'2 (subst a) (subst b)
>   subst (Multiply'2 a b)  = Multiply'2 (subst a) (subst b)
>   subst (Variable'2 name) = 
>     case lookup name env of
>       Just val -> Number'2 val 
>       Nothing  -> Variable'2 name

The last case is the only one that is different from the previous
definition of substitution for a single binding. It uses the
`lookup` function to search the list of bindings to find
the corresponding value (`Just val`) or `Nothing` if the variable
is not found. For the `Nothing` case, the substitute function
leaves the variable alone.

> z = Variable'2 "z"
> main'5 = do 
>   test (substitute e1) (Add'2 x y)
>   test (substitute e1) (Number'2 2)
>   test (substitute e1) x
>   test (substitute e1) (Add'2 (Multiply'2 x x) x)
>   test (substitute e1) (Add'2 x (Add'2 (Multiply'2 (Number'2 2) y) z))

The test results show that multiple variables are subsituted with
values, but that unknown variables are left intact:

    x + y ==> 3 + (-1)
    2 ==> 2
    x ==> 3
    x*x + x ==> 3*3 + 3
    x + 2*y + z ==> 3 + 2*(-1) + z

Note that it is alsopossible to substitute multiple variables one at a time:

> substitute'2 env exp = foldr substitute1 exp env

The `foldr fun init list` function applies a given function to each item
in a list, starting with a given initial value.

> --------------------BEGIN-HIDE-------------------------
> exp1 = Add'2 x (Add'2 (Multiply'2 (Number'2 2) y) z)
> check1 = check "subst-fold" (substitute e1 exp1) (substitute'2 e1 exp1)
> --------------------END-HIDE-------------------------

 ### Local Variables

So far we have only considered variables that are defined *outside*
the expression itself. It is also useful to allow variables to be 
defined *within* an expression. Most programming languages support
this cabability by allowing definition of *local variables*.

In C or Java one can define local variables in a declaration:
 
    int x = 3;
    return 2*x + 5;
    
JavaScript is similar but does not specify the type of the variable:

    var x = 3;
    return 2*x + 5;

Haskell and ML define local variables with a `let` expression:

```haskell
    let x = 3 in 2*x + 5
````
    
In the languages the `let` really is an expression, becuase it can be
used inside other expressions:

```haskell
    2 * (let x = 3 in x + 5)
````
    
It is also possible to define multiple local variables in Java or C:

    int x = 3;
    int y = x*2;
    return x + y;

and Haskell or ML

```haskell
    let x = 3 in let y = x*2 in x + y
````

which is equivalent to

```haskell
    let x = 3 in (let y = x*2 in x + y)
````

In general a `let` expression has the following concrete syntax:

`let` *variable* `=` *bound-expression* `in` *body*

The meaning of a `let` expression is to evaluate the bound expression,
then bind the local variable to the resulting value, and then 
evaluate the body of the expression

In Haskell, a `let` expression can be represented by adding
another case to the definition of expressions:

```haskell
data Exp'3 = ...
         | Let'3 String Exp'3 Exp'3  
````

where the string is the variable name, the first Exp is the bound expression
and the second expression is the body.

 ### Scope
 
The *scope* of a variable is the portion of the text of a program
in which a variable is defined. Normally the scope of a local
variable is the body of the let in which the variable is defined.
However, it is possible for a variable to be redefined, which creates
a hole in the scope of the outer variable:

![Varible Scope](scopes.eps)

In this example there are two variables named `x`. Even though
two variables have the same name, they are not the same variable.

TODO: talk about *free* versus *bound* variables

TODO: talk about renaming

 ### Substituting into `Let` Expressions

When substutiting a variable into an expression, care must
be taken to correctly deal with holes in the variable's scope.
In particular, when subsituting for *x* in an expressions, if
there is an expression of the form `let` *x* `=` *e* `in` *body* then
*x* should be substituted within *e* but not in *body*. 
Because *x* is redefined, the *body* is a hole in the scope of *x*.
 
```haskell
substitute1'3 (var, val) exp = subst exp
  ...
  subst (Let'3 x exp body)  = Let'3 x (subst exp) body'
    where body' = if x == var 
                  then body 
                  else subst body
````

In the `Let` case for `subst`, the variable is always substituted
into the bound expression `e`. But the substitution is only performed
on the body `b` if the variable `var` being substituted is *not* the
same as the variable `x` defined in the let expression.

TODO: need some test cases here

 ### Evaluating `Let` Expressions

The evaluation of a let expression is based on substitution.
To evaluate `let` *x* `=` *e* `in` *b*,
first evaluate the bound expression *e*, then substitute its value
for variable *x* in the body *b*. Finally, the result of 
substitution is evaluated. 

```haskell
evaluate'3 :: Exp'3 -> Int
...
evaluate'3 (Let'3 x exp body) = evaluate'3 (substitute1'3 (x, evaluate'3 exp) body)
````
 
 There is no rule for evaluating a variable because all variables
 are substituted away before evaluation begins.
 
TODO: need some test cases here
 
 ### Summary

Here is the full code evaluation using substitution of a language
with local variables.
 
> data Exp'3 = Number'3     Int
>          | Add'3        Exp'3 Exp'3
>          | Subtract'3   Exp'3 Exp'3
>          | Multiply'3   Exp'3 Exp'3
>          | Variable'3   String
>          | Let'3        String Exp'3 Exp'3    
>
> substitute1'3 (var, val) exp = subst exp 
>  where
>   subst (Number'3 i)      = Number'3 i
>   subst (Add'3 a b)       = Add'3 (subst a) (subst b) 
>   subst (Subtract'3 a b)  = Subtract'3 (subst a) (subst b)
>   subst (Multiply'3 a b)  = Multiply'3 (subst a) (subst b)
>   subst (Variable'3 name) = if var == name 
>                           then Number'3 val 
>                           else Variable'3 name
>   subst (Let'3 x exp body)  = Let'3 x (subst exp) body'
>     where body' = if x == var 
>                   then body
>                   else subst body

> evaluate'3 :: Exp'3 -> Int
> evaluate'3 (Number'3 i)       = i
> evaluate'3 (Add'3 a b)        = evaluate'3 a + evaluate'3 b
> evaluate'3 (Subtract'3 a b)   = evaluate'3 a - evaluate'3 b
> evaluate'3 (Multiply'3 a b)   = evaluate'3 a * evaluate'3 b
> evaluate'3 (Let'3 x exp body) = evaluate'3 (substitute1'3 (x, evaluate'3 exp) body)

 ## Evaluation with Environments

For the basic evaluator substitution and evaluation were 
completely separate, but the evaluation rule for `let` 
expressions involves substitution. 

One consequence of this
rule is that the body of every let expression is copied, 
because substitution creates a copy of the expession with
variables substituted. When let expressions are *nested*,
the body of the inner let expression is copied multiple times.
In the following example, the expression `x*y*z` is copied
three times:

```haskell
    let x = 2 in 
      let y = x+1 in 
        let z = y+2 in 
          x*y*z
````

The steps are as follows:

Step                                 Result
-------------------------            -----------------------------------
initial expression                   `let x = 2 in`
                                     \ \ \ \ `let y = x+1 in`
                                     \ \ \ \ \ \ \ \ `let z = y+2 in x*y*z`
evaluate bound expression            `2` $\Rightarrow$ `2`
substitute x $\mapsto$ 2 in body     `let y = 2+1 in (let z = y+2 in 2*y*z)`
evaluate bound expression            `2+1` $\Rightarrow$ `3`
substitute y $\mapsto$ 3 in body     `let z = 3+2 in 2*3*z`
evaluate bound expression            `3+2` $\Rightarrow$ `4`
substitute z $\mapsto$ 5 in body     `2*3*5`
evaluate body                        `2*3*5` $\Rightarrow$ `30`

While this is a reasonable approach it is not necessary. We
have already seen that multiple varialbes can be substituted
at the same time. Rather than performing the substitution
fully for each `let` expression, instead the `let` 
expression can add another binding to the list 
of substitutions being performed.

> evaluate'4 :: Env -> Exp'3 -> Int
> evaluate'4 env exp = eval exp 
>  where
>   eval (Number'3 i)      = i
>   eval (Add'3 a b)       = eval a + eval b
>   eval (Subtract'3 a b)  = eval a - eval b
>   eval (Multiply'3 a b)  = eval a * eval b

>   eval (Variable'3 x)    = fromJust (lookup x env)
>   eval (Let'3 x exp body)     = evaluate'4 env' body
>     where env' = (x, eval exp) : env

The helper function `eval` is defined in the scope of the `env`
argument of the main `evaluate` function. Since the environment
`env` does not change in most cases, it is convenient to not
have to pass it around on every call to `eval`. Note that the
final case, for `Let`, *does* change the environment so it
calls `evaluate` rather than `eval`.

The case for `Let` first evaluates the bound expression in the
current environment `ev e`, then it creates a new 
environment `env'` with that binds `x` to the value of
the bound expressions. It then evaluates the body `b` in the
new environment `env'`.

The steps in evaluation with environments do not copy the expression:

Environment                                         Evaluation
-------------------------------------------         -----------------------------------
$\emptyset$                                         `let x = 2 in`
                                                    \ \ \ \ `let y = x+1 in`
                                                    \ \ \ \ \ \ \ \ `let z = y+2 in x*y*z`
                                                    { evaluate bound expression `2`}
$\emptyset$                                         `2` $\Rightarrow$ `2`
                                                    { evaluate body of let }
`x` $\mapsto$ 2                                     `let y = x+1 in (let z = y+2 in x*y*z)`
                                                    { evaluate bound expression `x+1` }
`x` $\mapsto$ 2                                     `x+1` $\Rightarrow$ `3`
                                                    { evaluate body of let }
`y` $\mapsto$ 3, `x` $\mapsto$ 2                    `let z = y+2 in x*y*z`
                                                    { evaluate bound expression `y+2` }
`y` $\mapsto$ 3, `x` $\mapsto$ 2                    `y+2` $\Rightarrow$ `5`
                                                    { evaluate body of let }
`z` $\mapsto$ 5, `y` $\mapsto$ 3, `x` $\mapsto$ 2   `x*y*z` $\Rightarrow$ `70`

In the `Let` case of `eval`, a new environment `env'` is created and used
as the environment for evaluation of the body `b`. 

The new environments
add the additional bindings to the *front* of the list of environments.
Since `lookup` searches an environment list from left to right, it will
find the most recent enclosind binding for a variable, and igore any
additional bindings. For example, consider the evaluation of this
expression:

```haskell
    let x = 9 in (let x = x*x in x+x)
````

Environment                                         Evaluation
-------------------------------------------         -----------------------------------
$\emptyset$                                         `let x = 9 in (let x = x*x in x+x)`
                                                    { evaluate bound expression `9` }
$\emptyset$                                         `9` $\Rightarrow$ `9`
                                                    { evaluate body of let }
`x` $\mapsto$ 9                                     `let x = x*x in x+x`
                                                    { evaluate bound expression `x*x` }
`x` $\mapsto$ 2                                     `x*x` $\Rightarrow$ `81`
                                                    { evaluate body of let }
`x` $\mapsto$ 81, `x` $\mapsto$ 9                   `x+x` $\Rightarrow$ `162`

Note that the environment contains two bindings for `x`, but only the first
one is used. Having multiple bindings for the same name implements the concept of 'holes'
in the scope of a variable: when a new binding for the same variable is added to the
environment, the original binding is no longer accessible.

The old environment
is not changed, so there is no need to reset or restore the previous
environment. For example, evaluating the following expression 
creates to extensions of the base environment

```haskell
    let x = 3 in
      (let y = 3*x in 2+y) + (let z = 7*x in 1+z)
````

The first `let` expressions creates an environment `x` $\mapsto$ 3 with a
single binding. The next two let expressions create environments 
 
`y` $\mapsto$ 9, `x` $\mapsto$ 3

`z` $\mapsto$ 21, `x` $\mapsto$ 3

Internally Haskell allows these two environments to share the definition
of the original environment `x` $\mapsto$ 3.

 ## Conditionals and Booleans

In addition to arithmetic computations, it is useful to allow expressions
to evaluation conditions and also return different kinds of values.
Until now our expressions have always returned `Int` results, because
they have only performed arithmetic computations. The type `Value` 
is defined to support multiple different kinds of values:

> data Value = Int  Int
>            | Bool Bool
>  deriving Eq

> --------------------BEGIN-HIDE-------------------------
> instance Show Value where
>   show (Int i) = if i < 0 then paren (show i) else show i
>   show (Bool b) = show b
> --------------------END-HIDE-------------------------

Some example values are `Bool True` and
`Int 3`. We will define additional kinds of values, including functions and lists, later.
Keep in mind that the first uses of `Int` and `Bool` in this type definition are
the *labels* for data variants, while the second uses are *types* that define what kind of
data are associated with that data variant.

The abstract syntax of expressions can now be expanded to include operations involving
booleans. Some examples are $4 < 10$ and $3*10 = 7$. Once booleans are included
in the language, it is possible to define a *conditional* expression, with the following
concrete syntax:

`if` *test* `then` *true-part* `else` *false-part*

A conditional expression allows selection of one of two diffent values
based on whether a boolean is true or false. Note that a conditional *expression* is
expected to produce a value. This is different from the conditional *statement* 
found in many languages (most notably C and Java), which executes one of two blocks but
does not produce a value. In these languages, conditional expressions are written 
*test* `?` *true-part* `:` *false-part*. Haskell, however, only has 
conditional expressions of the kind discussed here.

Given a full set of arithmetic operators, some comparison operators 
(equaltiy `EQ`, less than `LT`, greater than `GT`, less than or equal `LE`),
plus *and*, *or* and *not* for
booleans, it is useful to generalize the abstract syntax to support a general notation
for binary and unary operators.
When an expression includes a value it is called a *literal* value. Literals generalize
the case of `Number` used above to include constants in an arithmetic expression.
The conditional expression is somtimes called a *ternary* operator because it has three arguments.
But since there is only one ternary operator, and also because a conditional expression
is fairly special, it is included directly as `If` expression.
These changes are implemented in the
following definition for the abstact syntax `Exp`:

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

Evaluation is then defined by cases as before. Two helper functions, `binary` and `unary` (defined below),
perform the actual computations for binary and unary operations, respectively.

> type Env'1 = [(String, Value)]
>
> evaluate'5 :: Env'1 -> Exp'4 -> Value
> evaluate'5 env exp = eval exp 
>   where
>     eval (Literal'4 v)      = v
>     eval (Unary'4 op a)     = unary op (eval a)
>     eval (Binary'4 op a b)  = binary op (eval a) (eval b)
>     eval (Variable'4 x)     = fromJust (lookup x env)
>     eval (Let'4 x exp body) = evaluate'5 env' body
>       where env' = (x, eval exp) : env

The conditional expression first evaluates the condition, forces it to be a boolean, 
and then evaluates either the *then* or *else* expression.

>     eval (If'4 a b c)      = if fromBool (eval a) 
>                            then (eval b) 
>                            else (eval c)
> fromBool (Bool b) = b

The binary and unary helper functions perform case analysis on the operator
and the arguments to compute the result of basic operations. 

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

Using the new format, here are the expressions for the test
cases given above:

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

In addition, new expressions can be defined to represent conditional expressions:

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
>   test (evaluate'5 []) t1'4
>   test (evaluate'5 []) t2'4
>   test (evaluate'5 []) t3'4
>   test (evaluate'5 []) t4'4
>   test (evaluate'5 []) t5'4
>   test (evaluate'5 []) t6'4
>   test (evaluate'5 []) t7'4
> --------------------END-HIDE-------------------------

Running these test cases with the `test` function defined above yields these results:

    4 ==> 4
    (-4) - 6 ==> (-10)
    3 - (-2) - (-7) ==> (-2)
    1*(8 + 5) ==> 13
    3 + 8*2 ==> 17
    if 3 > 1*(8 + 5) then 1 else 0 ==> 0
    2 + (if 3 <= 0 then 9 else (-5)) ==> (-3)

 ### Type Errors

Now that our language supports two kinds of values, it is possible for
an expression to get *type errors*. A type error occurs when evaluation of
an expression attempts to perform an operation but one or more of the 
values involaved are not the right type. For example, attemping to add an
integer and a boolean value, as in `3 + True`, leads to a type error.

In our Haskell program, type errors exhibit themselves in the 
`binary` and `unary` functions, which match certain legal patterns of
operations, but leave illegal combinations of operations and arguments
undefined.  Attemping to evaluate `3 + True` results in a call to
`binary Add (Int 3) (Bool True)`, which is not one of the patterns
handled by the `binary` funtion. As a result, Haskell generates a 
*Non-exhaustive pattern* error:

    Main> evaluate'5 [] (Binary'4 Add (Literal'4 (Int 3)) (Literal'4 (Bool True)))
    *** Exception: Non-exhaustive patterns in function binary

Here are some examples of epxression that generate type errors:

> -- if 3 then 5 else 8
> err1 = If'4 (Literal'4 (Int 3)) (Literal'4 (Int 5)) (Literal'4 (Int 8))
> -- 3 + True
> err2 = Binary'4 Add (Literal'4 (Int 3)) (Literal'4 (Bool True))
> -- 3 || True
> err3 = Binary'4 Or (Literal'4 (Int 3)) (Literal'4 (Bool True))
> -- -True
> err4 = Unary'4 Neg (Literal'4 (Bool True))

We will discuss techniques for preventing type errors later, but for now
it is important to realize that programs may fail at runtime.

 # Functions (in progress)
 
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
not usually introduced until calculus.
 
Programming languages all support some form of function definition. 
A function allows a computation to be written down once and reused many times.

 ## Top-Level Function Definitions

Many programming languages allow functions to be defined only 
at the top level of the program. The "top level" means outside of
any expression. In this case, the program itself is a list of
function definitions followed by a main expression. The 
main expression in a C program is an implicit call to a
function named `main`. Here is an example of some top-level 
functions, written in JavaScript:
    
    // compute n raised to the m-th power
    function power(n, m) {
      if (m == 0)
        return 1;
      else 
        return n * power(n, m - 1);
    }
    
    function main() {
      return power(3, 4);
    }

This code resembles C or Java, but without types.
The expression language we are developing does not need
return statements, because every expression automatically returns
a value. A similar program can be written in Haskell, also
without return statements:

    power(n, m) = 
      if (m == 0) then
        1
      else 
        n * power(n, m - 1)
    
    main() = 
      power(3, 4)    

These examples provides an outline for the basic concrete syntax of a function:

*function-name* `(` *parameter-name*, ..., *parameter-name* `)` *body-expression*

The exact syntax varies from language to language. Some languages 
begin with a keyword `function` or `def`. Other languages require brackets
`{` ... `}` around the body of the function. 
These functions are less powerful than Haskell, because they take a
simple parameter list rather than a full patterns. But this simple form
of function defined above captures the essense of function definition
in many languages.

A call to a function is an expression that has the following concrete syntax:

`function` *function-name* `(` *expression*, ..., *expression* `)`

Again, there are some variations on this theme. For example, 
in Haskell the parentheses are optional. The program has a 
series of named functions, each of which has a list of parameter
names and a body expression. The following data type definitions
provide a means to represent such programs:

> type FunEnv = [(String, FunctionDefinition)]
> data FunctionDefinition = FunctionDefinition [String] Exp'6

A list of function defintions is a *function environment*.
This list represents a list of bindings of function names
to function definitions. 

A program is then a function environment together with a
main expression:

> data Program = Program FunEnv Exp'6

Any of the expressions can contain calls to the top-level 
functions. A call has a function name and a list
of actual argument expressions:

```haskell
data Exp'6 = ...
         | Call'6      String [Exp'6]
````

As an exmple, here is an encoding of the example program:

> f1 = FunctionDefinition ["n", "m"]
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

A new function, `execute`, runs a program. It does so
by evaluating the main expression in the context of the
programs' function environment and an empty variable environment:

> execute :: Program -> Value
> execute (Program funEnv main) = evaluate'6 funEnv [] main

The evaluator is extended to take a function environment `funEnv` as
a additional argument. 

```haskell
evaluate'6 :: FunEnv -> Env'1 -> Exp'6 -> Value
evaluate'6 funEnv env exp = eval exp 
  where
    ...
     eval (Call'6 fun args)   = evaluate'6 funEnv env' body
       where FunDef xs body = fromJust (lookup fun funEnv)
             env' = zip xs (map eval args)
````

Evaluation of a call expression performs the following steps:

1. Look up the function definition by name `lookup fun funEnv`,
   to get the functions' parameter list `xs` and `body`.
2. Evaluate the actual arguments `map eval args` to get a list of values
3. Create a new envionment `env'` by zipping together the 
    parameter names with the actual argument values.
4. Evaluate the function `body` in the new environment `env'`

TODO: give an example to illustrate evaluation of functions?

The only variables that can be used in a function body are the
parameters of the function. As a result, the only environment
needed to evaluate the function body is the new environment
created by zipping together the parameters and the actual arguments.

The evaluator now takes two environments as input: one for
functions and one for normal variables. A given name is
always looked up in one or the other of these two environments,
and there is never any confusion about which place to look.
The certainty about where to look up a name comes from the
the fact that the names appear in completely different places
in the abstract syntax:

    data Exp = ...
         | Variable'6  String         -- variable name
         | Call'6      String [Exp'6] -- function name

A variable name is tagged as a `Variable` and a function name
appears in a `Call` expression.         

Because the names of function and the names of variables are
completely distinct, they are said to be in different *namespaces*.
The sepration of the variable and function namespace is clear
in the following (silly) example:

    function pow(pow)
      if pow <= 0 then 2
                  else let pow = pow(pow - 1) in
                          pow * pow(pow - 2)

This is the same as the following function, in which variables
are renamed to be less confusing:

    function pow(a)
      if a <= 0 then 2
                else let b = pow(a - 1) in
                        b * pow(b - 2)

When renaming varibles, the *functions* are *not* renamed. 
This is because functions and variables are in separate namespaces.

Another consequence of the separation between variable and
function namespaces is that functions can not be passed as arguments
to other functions, or returned as values from functions.
In the expression `pow(pow)` the two uses of `pow` are 
completely distinct. This is analogous to the concept of a 
[*homonym*](http://en.wikipedia.org/wiki/Homonym)
in natural languages like English. The exact same word has two
completely different meanings, which are distinguished only by 
context. English has many homonyms, including 'stalk' and 'left'.
In our expression language, the first `pow` must mean the
function because it appears in front of a parenthesis where
a function mame is expected, while
the second `pow` must be a variable because it appears where
an expression is expected.

In this language functions are *not* values. When something
is treated specially in a programming language, so that it
cannot be used where a any value is allowed, it is called
*second class*.

It is worth noting that many of the example functions presented above,
including `power` and `pow`, are *recursive*. Recursion is possible
because the function definitions can be used in any expression,
including in the body of the functions themselves. This means that
all functions have *global scope*. 

TODO: what about one argument versus multiple?
 
 ### Summary
 
Here is the full code for the evaluator supporting
global functions definitions.

> data Exp'6 = Literal'6   Value
>          | Unary'6     UnaryOp Exp'6
>          | Binary'6    BinaryOp Exp'6 Exp'6
>          | If'6        Exp'6 Exp'6 Exp'6
>          | Variable'6  String
>          | Let'6       String Exp'6 Exp'6
>          | Call'6      String [Exp'6]
>
> evaluate'6 :: FunEnv -> Env'1 -> Exp'6 -> Value
> evaluate'6 funEnv env exp = eval exp 
>   where
>     eval (Literal'6 v)      = v
>     eval (Unary'6 op a)     = unary op (eval a)
>     eval (Binary'6 op a b)  = binary op (eval a) (eval b)
>     eval (If'6 a b c)       = if fromBool (eval a) 
>                             then (eval b) 
>                             else (eval c)
>     eval (Variable'6 x)     = fromJust (lookup x env)
>     eval (Let'6 x exp body) = evaluate'6 funEnv env' body
>       where env' = (x, eval exp) : env
>     eval (Call'6 fun args)   = evaluate'6 funEnv env' body
>       where FunctionDefinition xs body = fromJust (lookup fun funEnv)
>             env' = zip xs (map eval args)

 ## First-Class Functions and Closures

In the last section, function definitions are a special 
syntax. The function names and the variable names are in
different namespaces. One consequence of this is that all
the expressive power we have built into our language, for 
local variables, conditionals and even functions, 
does not work for creating function themselves. If you believe
that functions are useful for reusing computations, as suggested
above, then it might be useful to use functions to create
and operate on functions. In this section we rework the 
concept of functions presented above to integrate them into
the language, so that functions are *first-class* values.

Consider the following function definition:

    f(x) = x * 2

The intent here is to define `f`, but it doesn't really
say what `f` is, it only says what `f` does when applied to
an argument. A true definition for `f` would have the form `f = ...`.

Finding a value for `f` is related the idea of solving equations 
in basic algebra. For example, consider this equation:

$x^2 = 5$

This means that $x$ is value that when squared equals $5$. 
We can solve this equation to compute the value of $x$:

$x = \sqrt{5}$

But this involved creating a new concept, the *square root*
of a number.
We know we have a solution for a variable when the variable
appears by itself on the left side of an equation. 

The function definition `f(x) = x * 2` is similar.
It means that `f` is a function that when applied to an 
argument `x` computes the value `x * 2`. 
*But we don't have a solution for* `f`, because `f` does
not appear on the left side of an equation by itself.
To 'solve for `f`' we need some new notation, just the
way that the square root symbol $\sqrt{\ }$ was introduced
to represent a new operation. 

The standard solution is to use a *lambda expression*, 
which is a special notation for representing a function.
Here is a solution for `f` using a lambda:

`f` = $\lambda$`x`. x * 2

This means that `f` is defined to be a function of one
arguments, named `x`, that computes the result `x * 2` when
applied to an argument.

Lambda notation was invented in 1930s by 
[Alonzo Church](http://en.wikipedia.org/wiki/Alonzo_Church),
who was investigating the mathematical concept of a function.
Lambda notation is just one part of the 
[*lambda calculus*](http://en.wikipedia.org/wiki/Lambda_calculus),
which is an extremely elegant analysis of functions. Lambda
calculus has had huge influence on programming languages.
We will study the lambda calculus in more detail in a 
later section, but the basic concepts are introduced here.

There are really only two basic ideas in lambda calculus: 
function creation and function application. 

TODO: more here

Closure definition: 

````haskell
data Value'7 = ...
           | Closure'7 String Exp'7 Env'7  -- new
````

````haskell
data Exp'7 = ....
         | Function'7  String Exp'7      -- new
         | Call'7      Exp'7 Exp'7         -- changed
````

````haskell
evaluate'7 :: Env'7 -> Exp'7 -> Value'7
evaluate'7 env exp = eval exp 
  where
    ...
    eval (Function'7 x body) = Closure'7 x body env
    eval (Call'7 fun arg)   = evaluate'7 env'' body
      where Closure'7 x body env' = eval fun
            env'' = (x, eval arg) : env'
````
 
 ### Relationship between Let and Functions
 
 TODO: prove that `let x =` $e$ `in` $b$ is equivalent to
   ($\lambda$`x.`$b$)$e$ 

 ## Recursive Functions
 
 ### Recursive Functions and Fixed-Points
 
 ### Self Application

 ## Summary of First-Class Functions
  
Here is the full code:

> data Value'7 = Int'7  Int
>            | Bool'7 Bool
>            | Closure'7 String Exp'7 Env'7  -- new
>   deriving Eq
>
> type Env'7 = [(String, Value'7)]
>
> data Exp'7 = Literal'7   Value'7
>          | Unary'7     UnaryOp Exp'7
>          | Binary'7    BinaryOp Exp'7 Exp'7
>          | If'7        Exp'7 Exp'7 Exp'7
>          | Variable'7  String
>          | Let'7       String Exp'7 Exp'7
>          | Function'7  String Exp'7      -- new
>          | Call'7      Exp'7 Exp'7         -- changed
>   deriving Eq
>
> evaluate'7 :: Env'7 -> Exp'7 -> Value'7
> evaluate'7 env exp = eval exp 
>   where
>     eval (Literal'7 v)      = v
>     eval (Unary'7 op a)     = unary'7 op (eval a)
>     eval (Binary'7 op a b)  = binary'7 op (eval a) (eval b)
>     eval (If'7 a b c)       = if fromBool'7 (eval a) 
>                             then (eval b) 
>                             else (eval c)
>     eval (Variable'7 x)     = fromJust (lookup x env)
>     eval (Let'7 x exp body) = evaluate'7 env' body
>       where env' = (x, eval exp) : env
>     eval (Function'7 x body) = Closure'7 x body env   -- new
>     eval (Call'7 fun arg)   = evaluate'7 env'' body   -- changed
>       where Closure'7 x body env' = eval fun
>             env'' = (x, eval arg) : env'
> --------------------BEGIN-HIDE-------------------------
> fromBool'7 (Bool'7 b) = b
>
> unary'7 Not (Bool'7 b) = Bool'7 (not b)
> unary'7 Neg (Int'7 i)  = Int'7 (-i)
>
> binary'7 Add (Int'7 a)  (Int'7 b)  = Int'7 (a + b)
> binary'7 Sub (Int'7 a)  (Int'7 b)  = Int'7 (a - b)
> binary'7 Mul (Int'7 a)  (Int'7 b)  = Int'7 (a * b)
> binary'7 Div (Int'7 a)  (Int'7 b)  = Int'7 (a `div` b)
> binary'7 And (Bool'7 a) (Bool'7 b) = Bool'7 (a && b)
> binary'7 Or  (Bool'7 a) (Bool'7 b) = Bool'7 (a || b)
> binary'7 LT  (Int'7 a)  (Int'7 b)  = Bool'7 (a < b)
> binary'7 LE  (Int'7 a)  (Int'7 b)  = Bool'7 (a <= b)
> binary'7 GE  (Int'7 a)  (Int'7 b)  = Bool'7 (a >= b)
> binary'7 GT  (Int'7 a)  (Int'7 b)  = Bool'7 (a > b)
> binary'7 EQ  a        b        = Bool'7 (a == b)
> --------------------END-HIDE-------------------------
 
> --------------------BEGIN-HIDE-------------------------

 ## Lambda-Calculus (planned)

 ### Representing Data as Functions
 
 # Mutation, Effects, and Monads 
 
 ## Mutable State
 
 ### Transformation Functions
 
 ### Monads
 
 ### Order of Evaluation
   Strict versus non-strict
   Lazy
 
 ## Parsing (planned)
 
 # Abstract Interpretation and Types
 
 ## Abstract Interpretation (planned)
 
 ### Type Checking
 
 # Data Abstraction (planned)
 
 
 ## Abstract Data Types
 
 ## Objects
 
 ## Partial Evaluation
 
 > --------------------END-HIDE-------------------------
 






 
 
 