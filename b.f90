program b
real*8 x,v,a,dt,dx,dv,dw,om0,gam,re,im, tmax, wmax
parameter(dt=1.e-2,dw=1.e-2,a=1.,om0=10.,gam=1.,tmax=20.,wmax=20.)
integer*8 i,n,imax,nmax
parameter(imax=int(tmax/dt),nmax=int(wmax/dw))
real xt(imax)
open(1,file='xt')
open(2,file='xw')
x=a;v=0.
do i=1,imax
xt(i)=x
dx=v;dv=-om0**2*x-2*gam*v
write(1,*) i*dt, x
print*,xt(i)
x=x+dx*dt;v=v+dv*dt
enddo
do n=1,nmax
re=0.;im=0.
do i=1,imax
re=re+xt(i)*cos(n*dw*i*dt)
im=im-xt(i)*sin(n*dw*i*dt)
enddo
write(2,*) n*dw, sqrt(re**2+im**2)
print*,sqrt(re**2+im**2)
enddo
end
