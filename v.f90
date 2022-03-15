program phv
integer*8 k,ialpha
real*8 v, ph, dv, dph, i, alpha, dt,alphaa(10)
parameter(dt=1.e-2,i=1.,alpha=1.)

open(1,file='vt')
open(2,file='pht')
open(3,file='valpha')

v=0.;ph=0.
do ialpha=1,10
alpha=alphaa(ialpha)

do k=1,10
write(1,*) k*dt, v
write(2,*) k*dt, ph
dph=v;dv=i-sin(ph)-alpha*v
v=dv*dt;ph=dph*dt+ph
enddo

write(3,*) ialpha
stop
end
