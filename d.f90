program c 
real*8 x,v,dt,dx,dv,dw,gam,re,im,tmax,wmax,om0,om1,a0
parameter(dt=1.e-4,dw=1.e-2,tmax=20.,wmax=20.,gam=0.5)
integer*8 i,n,imax,nmax
parameter(imax=int(tmax/dt),nmax=int(wmax/dw),om0=0.5,om1=0.5)
real xt(imax)

open(1,file='xt')
open(2,file='xw')
<<<<<<< HEAD
open(3,file='xv')

x=1.;v=0. ! initial condition

do i=1,imax ! "i" is time index
write(1,*) i*dt, x
write(3,*) x, v
xt(i)=x

dx=v;dv=-om0**2*x-2*gam*v+a0*cos(om1*i*dt)
=======
x=1.;v=0.
do i=1,imax
xt(i)=x
dx=v;dv=-om0**2*x-2*gam*v+cos(om1 * t)
>>>>>>> cab02d59779c8261e13660300d7cb169135904b0
write(1,*) i*dt, x
x=x+dx*dt;v=v+dv*dt
enddo

do n=1,nmax ! "n" is omega index
re=0.;im=0.
do i=1,imax
re=re+xt(i)*cos(n*dw*i*dt)
im=im-xt(i)*sin(n*dw*i*dt)
enddo
write(2,*) n*dw, sqrt(re**2+im**2)
enddo
end
