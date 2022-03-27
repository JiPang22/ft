!sub
program change
        implicit none
        integer :: a,b,diff,total
        a=5
        b=6

        print*, a, b

        call interchange(a,b,diff,total)

        print*, a, b, diff, total
        contains
        subroutine interchange(x,y,z,a)
                        implicit none
                                integer :: x, y, temp,a,z

                                 temp = x
                                 x = y
                                 y = temp
                                 z=x-y
                                 a=x+y
                                                                                return
                                                                                end subroutine
        end

