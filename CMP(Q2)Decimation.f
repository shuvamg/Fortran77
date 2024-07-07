2.f Page 1
 implicit none
 complex(8)::E,eps,t,g
 integer::i,n,l
 real(8)::pi=4.*atan(1.),E0,eta(2)
 n=1000
 E0=2.5
 eta=[0.01,0.001]
 open(1,file='data2_i.txt')
 open(2,file='data2_ii.txt')
 do l=1,2
 do i=1,n+1
 E=complex((−E0+2.*E0/n*(i−1)),eta(l))
 eps=(0.0,0.0)
 t=(1.0,0.0)
 do while (abs(t) .gt. 1e−10)
 t=t**2/(E−eps)
 eps=eps+2.*t
 end do
 g=1./(E−eps)
 write(l,*)real(E),−imag(g)/pi
 end do
 end do
 end
