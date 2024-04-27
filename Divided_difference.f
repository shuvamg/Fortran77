
      !author Shuvamghosh_23409110037
      program divided_differencetable
      real , dimension (:), allocatable :: x, y
      real , dimension (:,:), allocatable :: dd
      n= 6
      allocate (x(n), y(n), dd(n, n+1))
      x= (/1.0, 1.1, 1.3, 1.6, 1.7, 2.0 /)
      y= (/ 2.718, 3.004,3.669, 4.953, 5.474, 7.389/)
      dd = 0.0
      do 17 i= 1,n
      dd(i,1) = x(i)
      dd(i,2) = y(i)
 17   continue
      do 7 j = 3, n+1
      do 53  i = 1, n-j+2
      dd(i,j) = abs(dd(i+1, j-1)- dd(i, j-1))/ abs(x(i+j-2)-x(i))
 53   continue
 7    continue
      do 18 i= 1,n
      print*, dd(i,:)
 18   continue
      end


