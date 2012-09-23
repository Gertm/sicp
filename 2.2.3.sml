fun square x = x * x

fun accumulate2 f initial nil = initial
  | accumulate2 f initial (h::t) =
    f h (accumulate2 f initial t);


                
