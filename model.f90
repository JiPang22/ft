program hook_FT
integer*8 i, k, n
real*8 dt, x, dx, v, dv, dw, w0, re, im
parameter(dt = 1.e-2, dw = 1.e-2, w0 = 5.)
real x_t(1000)

open(1,file='xt')
open(2,file='vt')
open(3,file='vx')
open(4,file='xw')

x = 0.; v = 1.

do i = 1, 1000
write(1,*) i * dt, x
write(2,*) i * dt, v
write(3,*) x, v
x_t(i) = x; dx = v; dv = -w0**2 * x
x = x + dx * dt; v = v + dv * dt
enddo

do n = 1, 1000! n is omega index 
sum_re = 0.; sum_im = 0.
do k = 1, 1000 ! k is time index in sumation loop 
re = re + x_t(k) * cos(n * dw * k * dt) * dt
im = im -x_t(k) * sin(n * dw * k * dt) * dt
enddo
write(4,*) n * dw, sqrt(sum_im**2 + sum_re**2)
print*, "step... ", n
enddo

stop
end
