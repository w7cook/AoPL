# Anatomy of Programming Languages

[William R. Cook][wcook]

Copyright &copy; 2013

## What?

This document is a series of notes about programming languages, originally
written for students of the [undergraduate programming languages][cs345] course
at the [University of Texas at Austin][ut].

## Install

Install "cabal"
Install lhs2TeX by using "cabal install lhs2tex"
Install Pandoc by using "cabal install pandoc"
Install pandoc-citeproc by using "cabal install pandoc-citeproc"
Use "make" to build. Here are some things you can build:
  pretty - builds anatomy.pdf and opens it
  verb - builds anatomyVerbatim.pdf and opens it
  anatomy.pdf - builds the pretty version
  anatomyVerbatim.pdf - builds the text-based version
  anatomy.htm - builds the HTML version (with source code) and comments
  clean - clean the builds
  update - upload a new copy of the book to the public server
    (requires server access)
  execute - run all the code and build the tests
  fixup - modify the core anatomy.lhs file to have updated paragraph marks and code

## Why?

I’m writing these notes because I want to teach the theory of programming
languages with a practical focus, but I don’t want to use Scheme (or ML) as the
host language. Thus many excellent books do not fit my needs, including
Programming Languages: Application and Interpretation (Krishnamurthi 2012),
Essentials of Programming Languages (Friedman and Wand 2008) or Concepts in
Programming Languages (Mitchell and Apt 2001).

This book uses Haskell, a pure functional language. Phil Wadler (Wadler 1987)
gives some good reasons why to prefer Haskell over Scheme in his review of
Structure and Interpretation of Computer Programs (Abelson and Sussman 1996). I
agree with most but not all of his points. For example, I do not care much for
the fact that Haskell is lazy. None of the examples in this book rely upon this
feature.

I believe Haskell is particularly well suited to writing interpreters. But one
must be careful to read Haskell code as one would read poetry, not the way one
would read a romance novel. Ponder each line and extract its deep meaning. Don’t
skim unless you are pretty sure what you are doing. comment

The title of this book is derived from one of my favorite books, The Anatomy of
Lisp (Allen 1978).

## Who?

These notes assume knowledge of programming, and in particular assume basic
knowledge of programming in Haskell. When I teach the course I give a few hours
of lectures to introduce Haskell. I teach the built-in data types including
lists, the basic syntax for conditionals, function definitions, function calls,
list comprehensions, and how to print out strings. I also spend a day on data
definitions (algebraic data types) and pattern matching. Finally, I give a quick
introduction to type classes so student will understand how `Eq` and `Show`
work.  During the course I teach more advanced topics, including first-class
functions and monads. As background resources, I point students to the many
excellent tutorials on Haskell. Search Google for “Haskell Tutorial” to find
one. I recommend [Learn You a Haskell for Great Good!][lyah] or the [Gentle
Introduction To Haskell][gith].

[wcook]: https://www.cs.utexas.edu/~wcook/
[cs345]: https://www.cs.utexas.edu/~wcook/Courses/345/
[ut]: https://www.utexas.edu/
[lyah]: http://learnyouahaskell.com/
[gith]: http://www.haskell.org/tutorial/
