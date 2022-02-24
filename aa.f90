program aa
real*8 x, v, dx, dv, om0, gam, re, im, dw, dt, tmax, wmax
integer*8 i, imax, n, nmax
parameter(tmax=100.,wmax=100.)
parameter(om0=50.,gam=1.,dt=1.e-2,dw=1.e-2,imax=int(tmax/dt), nmax=int(wmax/dw))
real xt(imax)
open(1, file="xt")
open(2, file="xw")
x=10.;v=0.
do i=1, imax
dx=v;dv=-om0**2-2*gam*v
write(1,*) i * dt, x
xt(i)=x
x=x+dx*dt;v=v+dv*dt
print*,x,v
enddo
do n=1, nmax
re=0.;im=0.
do i=1, imax
re=re+xt(i)*cos(n*dw*i*dt)
im=im-xt(i)*sin(n*dw*i*dt)
enddo
write(2,*) n*dw, sqrt(re**2+im**2)
enddo
end
