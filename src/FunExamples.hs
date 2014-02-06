{-# OPTIONS -XRankNTypes #-}
import Value
import IntBool
import FunctionalEnvironment

--BEGIN:Func17
compose f g = \x -> f(g x)
--END:Func17

--BEGIN:Func5
compose :: (b -> c) -> (a -> b) -> (a -> c)
--END:Func5

--BEGIN:Func20
square n = n * n
mulPi m = pi * m
--END:Func20

--BEGIN:Func22
areaR = compose mulPi square
--END:Func22

--BEGIN:Func24
areaD = compose areaR (\x -> x / 2)
--END:Func24

--BEGIN:Mapp4
testM1 = map negate [1, 3, -7, 0, 12]   
-- returns [-1, -3, 7, 0, -12]
--END:Mapp4

--BEGIN:Mapp9
testM2 = [ negate n | n <- [1, 3, -7, 0, 12] ]   
-- returns [-1, -3, 7, 0, -12]
--END:Mapp9


--BEGIN:Repr3
type EnvL = [(String, Value)]
envL1 = [("x", IntV 3), ("y", IntV 4), ("size", IntV 10)]
--END:Repr3

--BEGIN:Repr5
envF1 "x"    = Just (IntV 3)
envF1 "y"    = Just (IntV 4)
envF1 "size" = Just (IntV 10)
envF1 _      = Nothing
--END:Repr5

--BEGIN:Repr7
x1 = lookup "x" envL1
x2 = envF1 "x"
--END:Repr7




--BEGIN:Repr11
bindL :: String -> Value -> EnvL -> EnvL
bindL var val env = (var, val) : env
--END:Repr11


--BEGIN:Repr13
envL2 = bindL "z" (IntV 5) envL1
   -- [("z", IntV 5), ("x", IntV 3), ("y", IntV 4), ("size", IntV 10)]
envL3 = bindL "x" (IntV 9) envL1
   -- [("x", IntV 9), ("x", IntV 3), ("y", IntV 4), ("size", IntV 10)]
--END:Repr13

--BEGIN:Repr23
-- version A
envF2 = bindF "z" (IntV 5) envF1
--END:Repr23

--BEGIN:Repr25
-- version B
envF2'1 = \testVar -> if testVar == "z"
                    then Just (IntV 5)
                    else envF1 testVar
--END:Repr25

--BEGIN:Repr27
-- version C
envF2'2 "z" = Just (IntV 5)
envF2'2 testVar = envF1 testVar
--END:Repr27

--BEGIN:Repr30
emptyEnvL :: EnvL
emptyEnvL = []
--END:Repr30

--BEGIN:Mult29
add a b = b + a
--END:Mult29

--BEGIN:Mult31
add'1 a = \b -> b + a
--END:Mult31

--BEGIN:Mult33
add'2 = \a -> \b -> b + a
--END:Mult33

--BEGIN:Mult35
add'3 = \a -> (\b -> b + a)
--END:Mult35


--BEGIN:Mult12
inc = add 1      -- \b. b + 1
dec = add (-1)   -- \b. b + (-1)
--END:Mult12


--BEGIN:Mult39
eleven = inc 10
nine   = dec 10
--END:Mult39

--BEGIN:Mult41
inc'1 = (\a -> (\b -> b + a)) 1
--END:Mult41

--BEGIN:Mult18
inc'2 = \b -> b + 1
--END:Mult18


--BEGIN:Mult20
inc'3 b = b + 1
--END:Mult20

--BEGIN:Mult22
testinc = inc 5 + inc 10 + dec 20 + dec 100
--END:Mult22

--BEGIN:Mult25
bindF'1 var val env testVar = if testVar == var
                            then Just val
                            else env testVar
--END:Mult25

--BEGIN:Bool3
true  x y = x
false x y = y
--END:Bool3

--BEGIN:Bool5
type BooleanF = forall a. a -> a -> a
true :: BooleanF
false :: BooleanF
--END:Bool5

--BEGIN:Bool7
notF :: BooleanF -> BooleanF
notF b = b false true
--END:Bool7

--BEGIN:Bool9
orF :: BooleanF -> BooleanF -> BooleanF
orF a b  = a true b
--END:Bool9

--BEGIN:Bool11
andF :: BooleanF -> BooleanF -> BooleanF
andF a b = a b false
--END:Bool11

--BEGIN:Bool14
testb1 = if not True then 1 else 2
--END:Bool14

--BEGIN:Bool16
testb2 = (notF true) 1 2
--END:Bool16

--BEGIN:Natu11
zero = \f -> \x -> x
one = \f -> \x -> f x
two = \f -> \x -> f (f x)
three = \f -> \x -> f (f (f x))
--END:Natu11

--BEGIN:Natu17
type ChurchN = forall a. (a -> a) -> a -> a
--END:Natu17

--BEGIN:Natu19
church :: Integer -> ChurchN
church 0 = \f -> \x -> x
church n = \f -> \x -> f (church (n-1) f x)
--END:Natu19

--BEGIN:Natu21
unchurch :: ChurchN -> Integer
unchurch n = n (+1) 0
-- 5 == (unchurch (church 5)) -- this evaluates to True
--END:Natu21


--BEGIN:Natu23
plus :: ChurchN -> ChurchN -> ChurchN
plus n m = \f -> \x -> n f (m f x)
mul :: ChurchN -> ChurchN -> ChurchN
mul n m = \f -> n (m f)
--END:Natu23

main = do
  print testM1
  print testM2
  