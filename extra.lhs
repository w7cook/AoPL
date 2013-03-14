

 ## Evaluating First-Class Functions by Substitution

[This section might be useful if I decide to cover the
normal implementation of lambda-calculus using beta reduction]
 
 Now its time for write a semantics for a language with
first-class functions. Since we have seen that booleans,
integers and other data types can be represented by functions,
there is no reason to include them in the language. What
remains are just variables, functions (lambda expressions),
and function application.

> data Exp'9 = Variable'9 String     -- variables
>          | Function'9 String Exp'9   -- function creation
>          | Call'9 Exp'9 Exp'9       -- function call

This is exactly the abstract syntax of
the *lambda calculus*, as defined by Church in the 1930s.
As we will see in (TODO: reference Church Encodings), we don't need to
define a type of *values* because we are going to use 
lambda expressions as the values. 

Church defined the semantics of lambda calculus using
substitution. 
We introduced substitution for arithmetic
expressions in [Chapter 1](#BasicSubst) so that you would familiar with
the concept by the time we got here.

Substitution for
lambda expressions is a little more complex, however.
In the previous approach we substituted variables for
values in expressions, but now we will substitute variables
for expressions in expressions. 

> substitute'9 :: String -> Exp'9 -> Exp'9 -> Exp'9
> substitute'9 var exp target = subst target
>  where
>    -- uses subst function defined in the next few paragraphs

Since expressions can 
contain variables, we will have to be more careful.
The first case, for substitution into a variable 
expression, is the same as previous cases:

>    subst (Variable'9 name) = 
>      if var == name then exp else Variable'9 name

The second case is substitution into a function call. It just
substitutes into the function and the argument:

>    subst (Call'9 fun arg) = 
>      Call'9 (subst fun) (subst arg)

The last case is substitution *into* a lambda expression.
This case is similar to the case of substitution into
a |let| expression (TODO: reference chapter), because
both a |let| and a lambda introduce a new variable name.
Here is a first attempt at substituting *into* a lambda
expression. 

````
-- first version is not quite correct
    subst (Function'9 name body) = 
      if var == name 
      then Function'9 name body
      else Function'9 name (subst body)
````

This version properly handles *shadowing* of the outer
|var| by the |name| bound in the lambda: if the names are
the same, then substitution does not occur in the body.

Consider some cases that this handles correctly:

* Substitute $x \mapsto 5$ in $\lambda f.f(x)$ \ \ \ $\Rightarrow$ \ \ \ $\lambda f.f(5)$
* Substitute $x \mapsto 5$ in $\lambda x.f(x)$ \ \ \ $\Rightarrow$ \ \ \ $\lambda x.f(x)$
* Substitute $x \mapsto y$ in $\lambda f.f(x)$ \ \ \ $\Rightarrow$ \ \ \ $\lambda f.f(y)$
* Substitute $f \mapsto \lambda z.z+3$ in $\lambda x.f(x)$ \ \ \ $\Rightarrow$ \ \ \ $\lambda x.(\lambda z.z+3)(x)$
* Substitute $f \mapsto \lambda z.z+3$ in $\lambda z.f(z)$ \ \ \ $\Rightarrow$ \ \ \ $\lambda z.(\lambda z.z+3)(z)$

However, this version does *not* properly handle the case
where the |exp| being substituted has free variables!
Here is a case that does not work correctly:

* Substitute $y \mapsto x*2$ in $\lambda x.y+x$ \ \ \ $\Rightarrow$ \ \ \ $\lambda x.(x*2)+x$

The problem here is that the $x+1$ in the expression being substituted
is a free variable, but it becomes bound in the result because it is
substituted into the body of a lambda that uses the same variable name.
They aren't really the same variable, even though they have the same name!
One way to see that they are not the same variable is to rename
the bound occurrence of $x$ to be some other variable name, $m$.

* Substitute $y \mapsto x*2$ in $\lambda m.y+m$ \ \ \ $\Rightarrow$ \ \ \ $\lambda m.(x*2)+m$

In this case the substitution is correct. 

When a variable that was free ends up becoming bound, it is called *variable capture*.
*variable capture* has never been a problem in any previous versions of
substitution that we have discussed, because all previous
cases of substitution have involved substitution of a
variable for a *value*, and values do not have variables in them. 

The definition of substitution is it must replace all
*free* occurrences of a variable with a new value, and that
all variables that are *free* in either the target or expression
being substituted must remain free. In other words, substitution must
avoid variable capture.

The solution to variable capture identified above was to rename
the bound variable if variable capture is about to happen.

>    subst (Function'9 name body) = 
>      if var == name
>      then Function'9 name body
>      else Function'9 name' (subst body')
>        where Function'9 name' body' = avoid_capture var exp (Function'9 name body)

> avoid_capture var exp (Function'9 name body) = 
>   if elem name (free_vars exp) 
>   then let name' = new_variable (Call'9 exp body) in
>        Function'9 name' (substitute'9 name (Variable'9 name') body)
>   else Function'9 name body

TODO: need to define these property!

> new_variable exp = "foo"
> free_vars exp = []
 
 
 ### Other Junk
 
 
Here is a typical example of a program that uses mutable state,
written in JavaScript:

````Java
var count = 0;
function makeVar() {
  count = count + 1;
  return "v" + count;
}
````

It declares a global variable named |count| with inital value |0| and defines a
funtion |makeVar| that returns a new variable name each time
it is called. The function |count| works by incrementing the
|count| and then returning a string formed by concatenating the
letter |"v"| with the string representation of |count|. Running
|makeVar| is easy, as it produces a new variable name each time it is
executed:

```Java
print( makeVar() );  // print "v1"
print( makeVar() );  // print "v3"
print( makeVar() );  // print "v4"
```

The use of global variables is poor programming style.
A fix for this problem appears later, in section on Objects (TODO:REF).

Mutable state is not possible in pure functional
languages, including Haskell, because there is no way to modify a
data structure or variable value after is has been constructed
or bound. But it is possible to compute a new data struture
that is based on an existing one. This is the notion of 
*functional update* or *functional change*: a function can
act as a transformation of a value into a new value.

This idea can be used to implement a pure functional version of
|makeVar|. The pure functional version of |makeVar| will 
take *as an input* the current value of the counter, and it
will produce as outputs the new variable name *and* the 
updated counter. 

> makeVar count = 
>   let count' = count + 1 in 
>     ("v" ++ show count', count')

The |count| variable is now an explicit input and output of the
function, rather than a global variable. The fact that the
counter is now explicit is evident in the type of |makeVar|:

> makeVar :: Int -> (String, Int)

Calling the pure version of |makeVar| is not so easy, because it
requires and extra input and produces an extra output:

> main'99 = do
>   let c0 = 0
>   let (var1, c1) = makeVar c0
>   print var1
>   let (var2, c2) = makeVar c1
>   print var2
>   let (var3, c3) = makeVar c1
>   print var3

To simulate the behavior of |makeVar|, for example, 


 