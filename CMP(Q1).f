!Consider a periodic lattice of identical atoms. Take, for example, 1000 atoms in the lattice, with
interatomic spacing a (will not be used in computation though). Assign an “on-site” potential equal
to zero for each atomic site. Take the nearest neighor hopping integral equal to `1’. Diagonalize the
Hamiltonian written in the Wannier basis (real space basis), and obtain the Average Density of
States (Use the defining equation of the Green’s function). Take the imaginary part equal to 0.01,
0.001 and observe if there is any notable change in your plot.
! code
1.f Page 1
 implicit none
 integer::i,j,k,n,ierr
 real(8)::t,eps,pi=4.*atan(1.),img
 complex(8)::En,sum,g
 real(8)::E0,rho,pythag
 real(8),allocatable,dimension(:,:)::H,Id
 real(8),allocatable,dimension(:)::diag, e
 open(1,file='den_i.txt')
 open(2,file='den_ii.txt')
 n=5000
 allocate(H(n,n),Id(n,n),diag(n),e(n))
 E0=2.5
 t=1.
 Id=0.
 do i=1,n
 e(i)=t
 diag(i)=0
 Id(i,i)=1.
 do j=1,n
 if (abs(i−j)==1) then
 H(i,j)=t
 else
 H(i,j)=0.
 end if
 end do
 end do
 call tqli(n,n,diag,e,Id,ierr)
 img=0.01
 do k=1,2
 do i=1,n+1
 sum=0.
 En=complex(−E0+(i−1)*2.*E0/n,img)
 do j=1,n
 sum=sum+1./(En−(diag(j)))
 end do
 G=1./n*sum
 rho=−imag(g/pi)
 write(k,*)real(En),rho
 end do
 img=img/10
 end do
 end
c Finding the eigenvalues and eigenvectors of a tridiagonal matrix
 subroutine tqli(nm,n,d,e,z,ierr)
 real(8):: d(n),e(n),z(nm,n),f,tst1,h,tst2,g,p,r,pythag
 real(8)::c,c2,c3,el1
 ierr = 0
 if (n .eq. 1) go to 1001
 do 100 i = 2, n
 e(i−1) = e(i)
100 continue
 f = 0.0e0
 tst1 = 0.0e0
 e(n) = 0.0e0
 do 240 l = 1, n
 j = 0
 h = abs(d(l)) + abs(e(l))
 if (tst1 .lt. h) tst1 = h
 do 110 m = l, n
 tst2 = tst1 + abs(e(m))
 if (tst2 .eq. tst1) go to 120
 110 continue
1.f Page 2
 120 if (m .eq. l) go to 220
 130 if (j .eq. 30) go to 1000
 j = j + 1
 l1 = l + 1
 l2 = l1 + 1
 g = d(l)
 p = (d(l1) − g) / (2.0e0 * e(l))
 r = pythag(p,dble(1.0))
 d(l) = e(l) / (p + sign(r,p))
 d(l1) = e(l) * (p + sign(r,p))
 dl1 = d(l1)
 h = g − d(l)
 if (l2 .gt. n) go to 145
 do 140 i = l2, n
 d(i) = d(i) − h
140 continue
 145 f = f + h
 p = d(m)
 c = 1.0e0
 c2 = c
 el1 = e(l1)
 s = 0.0e0
 mml = m − l
 do 200 ii = 1, mml
 c3 = c2
 c2 = c
 s2 = s
 i = m − ii
 g = c * e(i)
 h = c * p
 r = pythag(p,e(i))
 e(i+1) = s * r
 s = e(i) / r
 c = p / r
 p = c * d(i) − s * g
 d(i+1) = h + s * (c * g + s * d(i))
 do 180 k = 1, n
 h = z(k,i+1)
 z(k,i+1) = s * z(k,i) + c * h
 z(k,i) = c * z(k,i) − s * h
 180 continue
 200 continue
 p = −s * s2 * c3 * el1 * e(l) / dl1
 e(l) = s * p
 d(l) = c * p
 tst2 = tst1 + abs(e(l))
 if (tst2 .gt. tst1) go to 130
 220 d(l) = d(l) + f
 240 continue
 do 300 ii = 2, n
 i = ii − 1
 k = i
 p = d(i)
 do 260 j = ii, n
 if (d(j) .ge. p) go to 260
 k = j
 p = d(j)
 260 continue
 if (k .eq. i) go to 300
 d(k) = d(i)
 d(i) = p
 do 280 j = 1, n
 p = z(j,i)
 z(j,i) = z(j,k)
 z(j,k) = p
 280 continue
 300 continue
 go to 1001
 1000 ierr = l
 1001 return
 end
c finds sqrt(a**2+b**2)
1.f Page 3
 real(8) function pythag(a,b)
 real(8)::p,a,b,r,t,s,u
 p = amax1(abs(a),abs(b))
 if (p .eq. 0.0e0) go to 20
 r = (amin1(abs(a),abs(b))/p)**2
 10 continue
 t = 4.0e0 + r
 if (t .eq. 4.0e0) go to 20
 s = r/t
 u = 1.0e0 + 2.0e0*s
 p = u*p
 r = (s/u)**2 * r
 go to 10
 20 pythag = p
 return
 end
