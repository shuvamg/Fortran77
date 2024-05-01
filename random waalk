author- shuvam ghosh 23409110037

      program random_walk
      implicit none
      integer::Ns,w,e,i,j,l
      real(8)::rp,d,avg,rms,p,q
      integer,allocatable , dimension(:)::s,N
      Ns = 5000      !Sample
      p=0.7
      q=0.3
      allocate(s(Ns),N(3))
      N = (/1000, 5000, 10000/)  !Number of steps
      open(1,file="x1.txt")
      open(2,file="x2.txt")
      open(3,file="x3.txt")
      open(10,file="x4.txt")
      do i=1,3
      do w=1,Ns
      d = 0
      do e = 1,N(i)
      call random_number(rp)
      if(rp.lt.q) then
      d = d-1.0
      else if(rp.gt.q) then
      d = d+1.0
      endif
      end do
      s(w) = d
      end do
      do j=-N(i),N(i),2
      write(i,*) j,count(s.eq.j)
      end do
      avg=sum(s)/Ns
      write(10,*) N(i)*(p-q),avg
      end do
      close(1)
      close(2)
      close(3)
      close(10)
      end





