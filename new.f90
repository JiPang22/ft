program test
real*8 dt, w0, x, v, dx, dv
x = 0., v = 10.
end




















subroutine rk4(t, y)
real*8 k1, k2, k3, k4, h, t, y
integer*8 i, imax

do i = 1, imax

k1 = f(t, y)
k2 = f(t + h/2, y + k2 * h/2)
k3 = f(t + h/2, y + k2 * h)
k4 = f(t + h, y + k3 * h)

y = y + (k1 + 2 * k2 + 2 * k3 + k4) * h/6
print*, i * h, y
enddo
return
end

real*8 function f(t, y)
f = - w0 ** 2 * x
return
end
