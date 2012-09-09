fun helper _ b 0 = b
                 | helper
                 

fun fib-iter n =
    local
        helper (a b 0) = 
        if count = 0 then
            b
        else
            helper (a + b) a (count - 1)
    in
    helper 1 0 n
    end
        
