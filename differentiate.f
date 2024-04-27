! author shuvam ghosh 23409110037

      program  derivative
      implicit none
      real :: value, v
      print*, "          h   " , "       exact" , "          fd" ,
     & "                    cd" , "            error_fd" ,
     & "        error_cd"
      value = v(0.3,0.1)
      value= v(0.3,0.05)
      value= v(0.3, 0.025)
      end

      function  v(x,h)
      real :: h,x f,fd,cd, v, value, exact
      fd= (f(x+h)- f(x))/h
      cd= (f(x+h)- f(x-h)) / (2.0*h)
      exact= (1-(x/2))*exp(-(x/2))
      error_fd= abs(exact- fd)
      error_cd= abs(exact- cd)
      print*, h, exact, fd, cd, error_fd, error_cd
      end



      function f(x)
      !implicit none
      real :: x
      f= x*exp(-(x/2))
      return
      stop
      end
