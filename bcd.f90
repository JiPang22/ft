!recursive function
program my_factorial
        implicit none
        integer :: factorial
        print*, factorial(5)
        end program
recursive function factorial(n) result (fact)
        if (n == 0 .or. n==1) then
                fact =1
        else
                fact = n * factorial(n-1)
        end if

        end function

