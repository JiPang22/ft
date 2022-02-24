program underdamping
integer*8 i, n, imax, nmax
real*8 dt, dx, dv, dw, x, v, re, im, tmax, wmax, a, w0, gam, v1, v0
parameter(dt = 1.e-3, dw = 1.e-1, tmax = 1.e+1, wmax = 1.e+2, a = 10.)
parameter(imax = int(tmax/dt), nmax = int(wmax/dw), w0 = 3., gam = 5.)
real xt(imax)

open(1,file='xt')
open(2,file='vt')
open(3,file='vx')
open(4,file='xw')
open(5,file='xen')
open(6,file='predict')

x = a; v = 0.

do i = 1, imax
write(1,*) i * dt, x
write(2,*) i * dt, v
write(3,*) x, v
xt(i) = x
dx = v; dv = -w0**2 * x - 2 * gam * v
t = i * dt; x = x + dx * dt; v = v + dv * dt
enddo

do n = 1, nmax! n is omega index 
re = 0.; im = 0.
write(4,*) n * dw, sqrt(re**2 + im**2)
do i = 1, imax 
re = re + xt(i) * cos(n * dw * i * dt) * dt
im = im - xt(i) * sin(n * dw * i * dt) * dt
enddo
enddo

x = a; v = 0.
do i = 1, imax
write(5,*) i * dt, xen
xen = a * exp(-gam * i * dt)
enddo

x=a;v0=0.
do i=1, imax
dx = v0; dv = -w0**2 * x - 2 * gam * v0
v1 = v0 + dv * dt

if (v0*v1 .LT. 0) then
write(6, *) i * dt, x
endif

x = x + dx * dt
enddo

stop
end
