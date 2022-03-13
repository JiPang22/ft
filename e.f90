program e 

integer*8 i,j,imax,jmax
real*8 x,v,dt,dx,dv,dw,tmax,wmax,om0,om1,a,gam
parameter(dt=1.e-2,dw=1.e-2,tmax=20.,wmax=20.)
parameter(imax=int(tmax/dt),jmax=int(wmax/dw))
real xt(imax)

!system parameter
parameter(gam=1.,a=1.,om0=10.,om1=0.) !standard gam=1 a=1 om0=10 om1=5

!initial condition
x=0.1;v=0.

open(1,file='xt')
open(2,file='xw')
do i=1,imax
dx=v; dv=-(om0**2)*x-2*gam*v+a*cos(om1*i*dt)
write(1,*) i*dt, x
xt(i)=x
x=x+dx*dt;v=v+dv*dt
enddo

do j=1,jmax
re=0.;im=0.
do i=1,imax
re = re + xt(i)*cos(j*dw * i*dt)
im = im - xt(i)*sin(j*dw * i*dt)
enddo
write(2,*) j*dw, sqrt(re**2 + im**2)
enddo

end
