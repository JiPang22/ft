program e 

integer*8 i,j,imax,jmax
real*8 x,v,dt,dx,dv,dw,tmax,wmax,om0,om1,a,gam
parameter(dt=1.e-2,dw=1.e-2,tmax=50.,wmax=10.,gam=1.5,a=1.)
parameter(imax=int(tmax/dt),jmax=int(wmax/dw),om0=1.,om1=1.)
real xt(imax)
open(1,file='xt')
open(2,file='xw')
x=0.;v=0.5

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
