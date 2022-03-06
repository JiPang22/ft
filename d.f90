program d 

integer*8 i,n,imax,nmax
real*8 x,v,dt,dx,dv,dw,tmax,wmax,om0,om1,a,gam
parameter(dt=1.e-2,dw=1.e-2,tmax=50.,wmax=10.,gam=1.,a=1.)
parameter(imax=int(tmax/dt),nmax=int(wmax/dw),om0=1.,om1=1.)
open(1,file='xt')
x=0.;v=0.5

do i=1,imax
dx=v;dv=-(om0**2)*x-2*gam*v+a*cos(om1*i*dt)
write(1,*) i*dt, x
x=x+dx*dt;v=v+dv*dt
enddo

end
