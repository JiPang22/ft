program ft
integer*8 i, n, imax, nmax
real*8 dt, dx, dv, dw, t, x, v, w0, gam, re, im, tmax, wmax
parameter(dt = 1.e-2, dw = 1.e-2, w0 = 2., tmax = 1.e+2, wmax = 1.e+2, gam = 2.
parameter(imax = int(tmax/dt), nmax = int(wmax/dw))
real xt(imax)

open(1,file='xt')
open(2,file='vt')
open(3,file='vx')
open(4,file='xw')

t = 0.; x = 0.; v = 1.

do i = 1, imax
write(1,*) t, x
write(2,*) t, v
write(3,*) x, v
xt(i) = x
dx = v; dv = -w0**2 * x - 2 * gam * v
t = i * dt; x = x + dx * dt; v = v + dv * dt
print*, "step", 100 * i/imax, "%"
enddo

do n = 1, nmax! n is omega index 
t = 0.; re = 0.; im = 0.
write(4,*) n * dw, sqrt(re ** 2 + re ** 2)
do i = 1, imax 
re = re + x_t(i) * cos(n * dw * i * dt) * dt
im = im -x_t(i) * sin(n * dw * i * dt) * dt
enddo
print*, "step", 100 * n/nmax, "%"
enddo

stop
end
