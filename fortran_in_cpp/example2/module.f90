module mod

real(8),parameter:: a=2.d0;

contains 

! The testing function
real(8) function func(x)
real(8)::x

func = x**a

end function


end module mod
