3.f
 implicit none
 integer::i,j,n,k
 real(8)::E0(2),eps,t
 real(8),allocatable,dimension(:)::psi
 n=1000
 allocate(psi(n))
 open(1,file='data3_i.txt')
 open(2,file='data3_ii.txt')
 E0=[0.015,0.05]
 eps=0.
 t=1.0
 do k=1,2
 psi(1)=0.
 psi(2)=1.
 print*,k
 do j=2,n−1
 psi(j+1)=(E0(k)−eps)/t*psi(j)−psi(j−1)
 end do
 do i=1,n
 write(k,*)i,abs(psi(i))**2
 end do
 end do
 end
