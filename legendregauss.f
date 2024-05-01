#show that the p2, p3 (legendre polynomial) are orthogonal using gauss-quadrature 3 point method#
          !regn -235409110037
         !crn= 37
     
      
      
      program gauss_quadrature
      implicit double precision (a-h, o-z)
      
      real(kind= 8) :: w1,w2,w3,u1,u2,u3,x1,x2,x3,sum,a,b,f
! weighted points

      w1= 5.d0/ 9.d0
      w2= 8.d0/ 9.d0
      w3= 5.d0/9.d0
      
      a= -1.d0    
      b= 1.d0   
      
      u1= -sqrt(3.d0/5.d0)
      u2= 0.d0
      u3= sqrt(3.d0/5.d0)
      
      x1 =   ((b-a)/2.d0)* u1 + (b+a)/2.d0
      x2 =   ((b-a)/2.d0)* u2 + (b+a)/2.d0
      x3 =   ((b-a)/2.d0)* u3 + (b+a)/2.d0

      result= abs (b-a)/2.d0 * (w1*f(x1) +  w2* f( x2) + w3 * f(x3) )
      
      print*, "THE INTEGRATED VALUE= ", result
      end
      
      real(kind=8) function f(x)
      implicit double precision (a-h, o-z)
      real(kind= 8) ::x

 !p2(x) = 0.5* (3*x**2-1) 
 !p3(x) = 0.5*(5*x**3 - 3*x)
 !f= p2(x) * p3(x)
 
      f= (1.d0/4.d0)*(15*x**5 - 14 *x**3 + 3*x)
      return
      end
          
 
      

      


