program ft
integer*8 i, k, n, imax, kmax, nmax
real*8 dt, x, dx, v, dv, dw, w0, re, im, tmax, wmax
parameter(dt = 1.e-2, dw = 1.e-1, w0 = 10., tmax = 1.e+2, wmax = 5.e+1)
parameter(imax = int(tmax/dt), nmax = int(wmax/dw))
real*8 x_t(imax)

open(1,file='xt')
open(2,file='vt')
open(3,file='vx')
open(4,file='xw')

x = 0.; v = 1.

do i = 1, imax
write(1,*) i * dt, x
write(2,*) i * dt, v
write(3,*) x, v
x_t(i) = x; dx = v; dv = -w0**2 * x
x = x + dx * dt; v = v + dv * dt
print*, "step", 100 * i/imax, "%"
enddo

do n = 1, nmax! n is omega index 
re = 0.; im = 0.

do i = 1, imax 
re = re + x_t(k) * cos(n * dw * i * dt) * dt
im = im -x_t(k) * sin(n * dw * i * dt) * dt
enddo

write(4,*) n * dw, sqrt(re ** 2 + re ** 2)
print*, "step", 100 * n/nmax, "%"
enddo

stop
end
