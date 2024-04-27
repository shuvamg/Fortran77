
author shuvam ghosh 23409110037

      program gauss_quad
      implicit double precision (a-h, o-z)
      real(8) :: a,b,w1,w2,w3,u1,u2,u3,x1,x2,x3,sum
      a= 0.d0
      b= dacos(-1.d0)
      w1= 5.d0/ 9.d0
      w2= 8.d0/ 9.d0
      w3= 5.d0/ 9.d0

      u1= -sqrt(3.d0/5.d0)
      u2= 0.d0
      u3= sqrt(3.d0/5.d0)


      x1= ((b-a)/2.d0)*u1 + (b+a)/2.d0
      x2= ((b-a)/2.d0)*u2 + (b+a)/2.d0
      x3= ((b-a)/2.d0)*u3 + (b+a)/2.d0

      exact= -(cos(b)- cos(a))
      sum= ((b-a)/2.d0)*(w1*f(x1) + w2*f(x2)  + w3 * f(x3) )
      print*, "THE INTEGRATED VALUE = " , sum
      print*, "THE EXACT SOLUTION = " , exact
      print*, "ERROR= ", abs(exact- sum)

      end

      real (8) function f(x)
      implicit none
      real (8) :: x
      f = dsin(x)
      end
