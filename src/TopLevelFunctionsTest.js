function power(n, m) { 
 if (m == 0) 
   1 
 else 
   n * power(n, m - 1) 
} 

power(3, 4)
# IntV 81