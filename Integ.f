      program integration
      implicit none
      real (8) :: v, value
      print*, "          n" , "        exact",
     & "                       trap", "                     simp",
     & "                error_trap", "                   error_simp"
      value= v(50)
      value = v(100)
      value = v(500)
      value= v(1000)
      end
      real (8) function v(n)
      implicit none
      real (8):: a,b,h, p,sum1,sum2,sum3,exact, trap,simp,f
      real  (8) :: error_trap, error_simp
      integer :: i,j,n
      a= 0.0
      b= 3.0
      h= (b-a)/n
      p= f(a)+f(b)
      sum1= 0.0
      sum2= 0.0
      sum3 = 0.0
      do 17 i = 1,n-1,1
      sum1= sum1+ f(a+i*h)
      if (mod(i,2)==0.0) then
      sum2= sum2+f(a+i*h)
      else
      sum3= sum3+f(a+i*h)
      end if
 17   continue
      trap=  (h/2.0)*(p+2*sum1)
      simp=   (h/3.0)*(p+2*sum2+ 4* sum3)
      exact= log(cosh(b))- log(cosh(a))
      error_trap  = abs(exact- trap)
      error_simp= abs(exact- simp)
      print*, n, exact , trap, simp,error_trap, error_simp
      end
      real (8) function f(x)
      implicit none
      real (8) :: x
      f= tanh(x)
      return
      end
