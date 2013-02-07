% Notes on Programming Languages using Haskell
% William R. Cook
% Jan 2013

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

 # Simple Language of Arithmetic

A simple place to start is analyzing the language of simple arithmetic, which is 
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

 ## Abstract Syntax in Haskell ##

Arithmetic expressions are easily represented in Haskell as a data type.

> data Exp'1 = Number'1     Int
>          | Add'1        Exp'1 Exp'1
>          | Subtract'1   Exp'1 Exp'1
>          | Multiply'1   Exp'1 Exp'1

This data type defines four representational variants, one for numbers,
one for unary negation, and two for the binary operators of addition and multiplication.
A number that appears in a program is called a *literal*.

The examples above are easily written as data values using the constuctors of the
`Exp` data type:

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

 ## Evaluation ##

The normal meaning assigned to arithmetic expressions is the evaluation of the
arithmetic operators to compute a final answer. This evaluation process is
defined by cases in Haskell:

> eval'1 :: Exp'1 -> Int
> eval'1 (Number'1 i)      = i
> eval'1 (Add'1 a b)       = eval'1 a + eval'1 b
> eval'1 (Subtract'1 a b)  = eval'1 a - eval'1 b
> eval'1 (Multiply'1 a b)  = eval'1 a * eval'1 b

To test this program we can execute the following main program:

> main'1 = do
>   putStr "Evaluating the following expression:\n  "
>   print t3
>   putStr "Produces the following result:\n  "
>   print (eval'1 t3)

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
>   test eval'1 t1
>   test eval'1 t2
>   test eval'1 t3
>   test eval'1 t4
>   test eval'1 t5

Running this main program produces less than satisfactory results:

    4 ==> 4
    -4+6 ==> 2
    3--2--7 ==> -2
    1*8+5 ==> 13
    1+8*2 ==> 17
    
We are back to the ambiguous expressions that we started with.
It is easy to add parentheses to remove the ambiguity:

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

 # Variables

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

 ## Variable Discussion
 
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

 ## Environments

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

 ## Local Variables

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

    let x = 3 in 2*x + 5
    
In the languages the `let` really is an expression, becuase it can be
used inside other expressions:

    2 * (let x = 3 in x + 5)
    
It is also possible to define multiple local variables in Java or C:

    int x = 3;
    int y = x*2;
    return x + y;

and Haskell or ML

    let x = 3 in let y = x*2 in x + y

which is equivalent to

    let x = 3 in (let y = x*2 in x + y)

In general a `let` expression has the following syntax:

`let` *variable* `=` *bound-expression* `in` *body*

The meaning of a `let` expression is to evaluate the bound expression,
then bind the local variable to the resulting value, and then 
evaluate the body of the expression

In Haskell, a `let` expression can be represented by adding
another case to the definition of expressions:

    data Exp'3 = ...
             | Let'3 String Exp'3 Exp'3  

where the string is the variable name, the first Exp is the bound expression
and the second expression is the body.

 ## Scope
 
The *scope* of a variable is the portion of the text of a program
in which a variable is defined. Normally the scope of a local
variable is the body of the let in which the variable is defined.
However, it is possible for a variable to be redefined, which creates
a hole in the scope of the outer variable:

![Varible Scope](scopes.eps)

In this example there are two variables named `x`. Even though
two variables have the same name, they are not the same variable.

 ## Substituting into `Let` Expressions

When substutiting a variable into an expression, care must
be taken to correctly deal with holes in the variable's scope.
In particular, when subsituting for *x* in an expressions, if
there is an expression of the form `let` *x* `=` *e* `in` *body* then
*x* should be substituted within *e* but not in *body*. 
Because *x* is redefined, the *body* is a hole in the scope of *x*.
 
    substitute1'3 (var, val) exp = subst exp
      ...
      subst (Let'3 x e b)  = Let'3 x (subst e) b'
        where b' = if x == var 
                   then b 
                   else subst b

In the `Let` case for `subst`, the variable is always substituted
into the bound expression `e`. But the substitution is only performed
on the body `b` if the variable `var` being substituted is *not* the
same as the variable `x` defined in the let expression.

TODO: need some test cases here

 ## Evaluating `Let` Expressions

The evaluation of a let expression is based on substitution.
To evaluate `let` *x* `=` *e* `in` *b*,
first evaluate the bound expression *e*, then substitute its value
for variable *x* in the body *b*. Finally, the result of 
substitution is evaluated. 

    eval'3 :: Exp'3 -> Int
    ...
    eval'3 (Let'3 x e b)     = eval'3 (substitute1'3 (x, eval'3 e) b)
 
 There is no rule for evaluating a variable because all variables
 are substituted away before evaluation begins.
 
TODO: need some test cases here
 
 ## Substitution and Evaluation of `Let` Expressions

Here is the full code for substitution and evaluation of a language
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
>   subst (Let'3 x e b)  = Let'3 x (subst e) b'
>     where b' = if x == var 
>                then b 
>                else subst b

> eval'3 :: Exp'3 -> Int
> eval'3 (Number'3 i)      = i
> eval'3 (Add'3 a b)       = eval'3 a + eval'3 b
> eval'3 (Subtract'3 a b)  = eval'3 a - eval'3 b
> eval'3 (Multiply'3 a b)  = eval'3 a * eval'3 b
> eval'3 (Let'3 x e b)     = eval'3 (substitute1'3 (x, eval'3 e) b)

 # Integrating Substitution and Evaluation

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

    let x = 2 in 
      let y = x+1 in 
        let z = y+2 in 
          x*y*z

The steps are as follows:

Step                        Result
-------------------------   -----------------------------------
evaluate                     `2` $\Rightarrow$ `2`
substitute x $\mapsto$ 2     `let y = 2+1 in (let z = y+2 in 2*y*z)`
evaluate                     `2+1` $\Rightarrow$ `3`
substitute y $\mapsto$ 3     `let z = 3+2 in 2*3*z`
evaluate                     `3+2` $\Rightarrow$ `4`
substitute z $\mapsto$ 5     `2*3*5`
evaluate                     `2*3*5` $\Rightarrow$ `30`

While this is a reasonable approach it is not necessary. We
have already seen that multiple varialbes can be substituted
at the same time. Rather than performing the substitution
fully for each `let` expression, instead the `let` 
expression can simply add another binding to the list 
of substitutions being performed.

> eval'4 :: Env -> Exp'3 -> Int
> eval'4 env exp = eval' exp where
>   eval' (Number'3 i)      = i
>   eval' (Add'3 a b)       = eval' a + eval' b
>   eval' (Subtract'3 a b)  = eval' a - eval' b
>   eval' (Multiply'3 a b)  = eval' a * eval' b

>   eval' (Variable'3 x)    = fromJust (lookup x env)
>   eval' (Let'3 x e b)     = eval'4 env' b
>     where env' = (x, eval' e) : env

The steps in evaluation with environments do not copy the expression:

Environment                                         Evaluation
-------------------------------------------         -----------------------------------
$\emptyset$                                         `let x = 2 in`
                                                    \ \ \ \ `let y = x+1 in`
                                                    \ \ \ \ \ \ \ \ `let z = y+2 in x*y*z`
$\emptyset$                                         `2` $\Rightarrow$ `2`
`x` $\mapsto$ 2                                     `let y = x+1 in (let z = y+2 in x*y*z)`
`x` $\mapsto$ 2                                     `x+1` $\Rightarrow$ `3`
`x` $\mapsto$ 2, `y` $\mapsto$ 3                    `let z = y+2 in x*y*z`
`x` $\mapsto$ 2, `y` $\mapsto$ 3                    `y+2` $\Rightarrow$ `5`
`x` $\mapsto$ 2, `y` $\mapsto$ 3, `z` $\mapsto$ 5   `x*y*z` $\Rightarrow$ `70`

In the `Let` case of `eval`, a new environment `env'` is created and used
as the environment for evaluation of the body `b`. The old environment
is not changed, so there is no need to reset or restore the previous
environment. For example, evaluating the following expression 
creates to extensions of the base environment

    let x = 3 in
      (let y = 3*x in 2+y) + (let z = 7*x in 1+z)

The first `let` expressions creates an environment `x` $\mapsto$ 3 with a
single binding. The next two let expressions create environments 
 
`y` $\mapsto$ 9, `x` $\mapsto$ 3

`z` $\mapsto$ 21, `x` $\mapsto$ 3

Internally Haskell allows these two environments to share the definition
of the original environment `x` $\mapsto$ 3.

 # Extending the Language with more Values


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
booleans. Some simple examples are $4 < 10$ and $3*10 = 7$. Once booleans are included
in the language, it is possible to define a *conditional* expression, of the form
`if` *test* `then` *true-part* `else` *false-part*.
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
The conditional expression is called a *ternary* operator because it has three arguments.
But since there is only one ternary operator, it is simply included as a `Conditional`
expression.
These changes are implemented in the
following definition for the abstact syntax `Exp`:

> data BinaryOp = Add | Sub | Mul | Div | And | Or 
>               | GT | LT | LE | GE | EQ
>   deriving Eq
> 
> data UnaryOp = Neg | Not 
>   deriving Eq
> 
> data Exp'4 = Literal'4     Value
>          | Unary'4       UnaryOp Exp'4
>          | Binary'4      BinaryOp Exp'4 Exp'4
>          | Conditional'4 Exp'4 Exp'4 Exp'4
>          | Variable'4    String
>          | Let'4         String Exp'4 Exp'4
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
> showExp'4 level (Let'4 x e b) = 
>   if 0 < level then paren result else result
>      where result = "let " ++ x ++ " = " ++ showExp'4 0 e ++ " in " ++ showExp'4 0 b
> showExp'4 level (Conditional'4 c a b) = 
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
> eval'5 :: Env'1 -> Exp'4 -> Value
> eval'5 env (Literal'4 v)     = v
> eval'5 env (Unary'4 op a)    = unary op (eval'5 env a)
> eval'5 env (Binary'4 op a b) = binary op (eval'5 env a) (eval'5 env b)
> eval'5 env (Variable'4 x)    = fromJust (lookup x env)
> eval'5 env (Let'4 x e b)     = eval'5 env' b
>   where env' = (x, eval'5 env e) : env

The conditional expression first evaluates the condition, forces it to be a boolean, 
and then evaluates either the *then* or *else* expression.

> eval'5 env (Conditional'4 a b c)  = if fromBool (eval'5 env a) 
>                                then (eval'5 env b) 
>                                else (eval'5 env c)
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
> t6'4 = Conditional'4 (Binary'4 GT (Literal'4 (Int 3)) t4'4) 
>                  (Literal'4 (Int 1)) 
>                  (Literal'4 (Int 0))
> -- 2 + (if 3 <= 0 then 9 else -5)
> t7'4 = Binary'4 Add (Literal'4 (Int 2))
>                 (Conditional'4 (Binary'4 LE (Literal'4 (Int 3)) 
>                                         (Literal'4 (Int 0))) 
>                              (Literal'4 (Int 9)) 
>                              (Literal'4 (Int (-5))))
> --------------------BEGIN-HIDE-------------------------
> main'6 = do
>   test (eval'5 []) t1'4
>   test (eval'5 []) t2'4
>   test (eval'5 []) t3'4
>   test (eval'5 []) t4'4
>   test (eval'5 []) t5'4
>   test (eval'5 []) t6'4
>   test (eval'5 []) t7'4
> --------------------END-HIDE-------------------------

Running these test cases with the `test` function defined above yields these results:

    4 ==> 4
    (-4) - 6 ==> (-10)
    3 - (-2) - (-7) ==> (-2)
    1*(8 + 5) ==> 13
    3 + 8*2 ==> 17
    if 3 > 1*(8 + 5) then 1 else 0 ==> 0
    2 + (if 3 <= 0 then 9 else (-5)) ==> (-3)

 ## Type Errors

Now that our language supports two kinds of values, it is possible for
an expression to get *type errors*. A type error occurs when evaluation of
an expression attempts to perform an operation but one or more of the 
values involaved are not the right type. For example, attemping to add an
integer and a boolean value, as in $3 + true$, leads to a type error.

In our Haskell program, type errors exhibit themselves in the 
`binary` and `unary` functions, which match certain legal patterns of
operations, but leave illegal combinations of operations and arguments
undefined.  Attemping to evaluate $3 + true$ leads to a runtime error:

    Main> eval'5 (Binary'4 Add (Literal'4 (Int 3)) (Literal'4 (Bool True)))
    *** Exception: Non-exhaustive patterns in function binary

We will discuss techniques for preventing type errors later, but for now
it is important to realize that programs may fail at runtime.

