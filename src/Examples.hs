

--BEGIN:Loca8
test1 = let x = 3 in 2*x + 5
--END:Loca8

--BEGIN:Loca10
test2 = 2 * (let x = 3 in x + 5)
--END:Loca10

--BEGIN:Loca15
test3 = let x = 3 in let y = x*2 in x + y
--END:Loca15

--BEGIN:Loca17
test4 = let x = 3 in (let y = x*2 in x + y)
--END:Loca17

--BEGIN:Eval33
test5 = let x = 2 in
  let y = x+1 in
    let z = y+2 in
      x*y*z
--END:Eval33

--BEGIN:Eval16
test6 = let x = 9 in (let x = x*x in x+x)
--END:Eval16

--BEGIN:Eval20
test7 = let x = 3 in
  (let y = 3*x in 2+y) + (let z = 7*x in 1+z)
--END:Eval20


--BEGIN:Usin3
f'1(x) = x * 2
f'2 x  = x * 2
f'3 = \x -> x * 2
--END:Usin3


--BEGIN:Recu3
testLet =
  let fact = \n -> if n == 0 then 1 else n * fact(n-1)
  in fact(10)
--END:Recu3


--BEGIN:A63
testID = id(id)   
-- returns id
--END:A63
