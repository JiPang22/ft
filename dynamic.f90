program v 
implicit none
integer*8 i
real*8 t,dt,x2,x1,dx2,dx1
parameter(dt=1.e-2)

open(1,file='xt')
open(2,file='vt')
open(3,file='vx')

!initial condition
x2=0.;x1=0.

do i=1,2000
t=i*dt
!D.E.
dx1=-4*x1-2*x2
dx2=3*x1-11*x2
write(1,*) t, x1
write(2,*) t, x2
write(3,*) x1, x2
call eular(x1,x2,dx1,dx2,dt)
enddo

end

subroutine eular(x1,x2,dx1,dx2,dt)
real*8 x1,x2,dx1,dx2,dt
x1=x1+dx1*dt
x2=x2+dx2*dt
return
end subroutine

