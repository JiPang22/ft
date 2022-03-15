implicit none
integer*8 i,j,imax,jmax, iflag
real*8 time, x,v,dt,dx,dv,dw,tmax,wmax,om0,om1,a,gam
parameter(dt=1.e-2,dw=1.e-2,tmax=100.,wmax=20.)
parameter(imax=int(tmax/dt),jmax=int(wmax/dw))
real*8 xt(imax), xw(jmax)

!system parameter
!parameter(gam=1.,a=1.,om0=10.,om1=0.) !standard gam=1 a=1 om0=10 om1=5
!parameter(a=1.,om0=10.,om1=0.) !standard gam=1 a=1 om0=10 om1=5


xt=0.
iflag=0
do i=1,imax
time=i*dt

if(i/200*200.eq.i)then
iflag=iflag+1
endif

if(mod(iflag,2).eq.0) xt(i)=10

!write(6,*) time, xt(i)

enddo



call fft_0(xt, imax, xw, jmax, dt, dw)

do i = 1, jmax
write(6,*) i*dw, xw(i)
enddo


stop

end

subroutine fft_0(xt, itmax, xw, iomax, dt, dom)
implicit none
integer*8 i, j, itmax, iomax
real*8 re, im, dt, dom
real*8 xt(itmax), xw(iomax)

do j=1,iomax
re=0.;im=0.
do i=1,itmax
re = re + xt(i)*cos(j*dom * i*dt)
im = im - xt(i)*sin(j*dom * i*dt)

enddo
!write(2,*) j*dw, sqrt(re**2 + im**2)
xw(j)=sqrt(re**2+im**2)
enddo
return
end subroutine fft_0




