program b
real*8 x,v,dt,dx,dv,dw,om0,gam,re,im,tmax,wmax,om1,a
parameter(dt=1.e-3,dw=1.e-2,tmax=40.,wmax=20.,gam=0.1,om0=1.,a=1.)
integer*8 i,n,imax,nmax
parameter(imax=int(tmax/dt),nmax=int(wmax/dw),om1=sqrt(om0**2-2*gam**2))
real xt(imax)
open(1,file='xt')
open(2,file='xw')
x=1.;v=0.
do i=1,imax
xt(i)=x
dx=v;dv=-om0**2*x-2*gam*v+cos(om1 * t)
write(1,*) i*dt, x
x=x+dx*dt;v=v+dv*dt
enddo
do n=1,nmax
re=0.;im=0.
do i=1,imax
re=re+xt(i)*cos(n*dw*i*dt)
im=im-xt(i)*sin(n*dw*i*dt)
enddo
write(2,*) n*dw, sqrt(re**2+im**2)
enddo
end
