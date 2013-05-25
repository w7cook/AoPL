
--BEGIN:Sema11
fact = \n -> if n == 0 then 1 else n * fact(n-1)
--END:Sema11

--BEGIN:Unde3
twos = 2 : twos
--END:Unde3

--BEGIN:Unde7
numbers = 0 : [ n + 1 | n <- numbers ]
--END:Unde7

--BEGIN:Unde12
a = 1 + 3 * a
--END:Unde12

--BEGIN:Unde14
inf = inf
--END:Unde14

--BEGIN:A58
fix g = g (fix g)
--END:A58

--BEGIN:Fixe30
g_twos l = 2 : l
--END:Fixe30

--BEGIN:Fixe37
g_numbers ns = 0 : [ n + 1 | n <- ns ]
--END:Fixe37

--BEGIN:Fixe45
g_fact = \f -> \n -> if n == 0 then 1 else n * f(n-1)
--END:Fixe45

--BEGIN:Fixe46
fact'1 = fix g_fact
--END:Fixe46

--BEGIN:A61
id x = x
--END:A61

main = return ()
