program d 

integer*8 i,n,imax,nmax
real*8 x,v,dt,dx,dv,dw,tmax,wmax,om0,om1,a,gam
parameter(dt=1.e-2,dw=1.e-2,tmax=50.,wmax=10.,gam=0.5,a=1.)
parameter(imax=int(tmax/dt),nmax=int(wmax/dw),om0=0.5,om1=0.5)
open(1,file='xt')
x=1.;v=0.

do i=1,imax
dx=v;dv=-(om0**2)*x-2*gam*v+a*cos(om1*i*dt)
write(1,*) i*dt, x
x=x+dx*dt;v=v+dv*dt
enddo

end
