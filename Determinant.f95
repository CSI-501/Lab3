program MainProgram
   ! Nicholas Maynard
   ! CSI 501
   ! Lab 3
   ! 02/16/2023
   ! This Program shows off functions in fortran by utilizing a determinant function.

   implicit none
   
   integer :: n
   real, allocatable :: Matrix1(:,:)
   real :: Determinant
   
   ! NOTE how the function name 'Determinant' has to be declared with a type.
   ! This is so the program knows what kind of variable it returns.
      
   ! Compute the determinant of a 2x2 matrix.
   n = 2
   allocate(Matrix1(n,n))
   Matrix1(1,1:2) = (/  3.0, 4.0 /)
   Matrix1(2,1:2) = (/ -1.0, 3.0 /)
   print*,'2x2 Determinant is: ', Determinant(Matrix1,n)
   deallocate(Matrix1)
   
   ! Compute the determinant of a 3x3 matrix.
   n = 3
   allocate(Matrix1(n,n))
   Matrix1(1,1:3) = (/  1.0,  4.0,  3.0 /)
   Matrix1(2,1:3) = (/ -1.0,  2.0,  1.0 /)
   Matrix1(3,1:3) = (/  2.0,  2.0,  3.0 /)
   print*,'3x3 Determinant is: ', Determinant(Matrix1,n)
   deallocate(Matrix1)
      
end program MainProgram


function Determinant(M, n) result(Det)
   ! Clear the memory for the variables
   implicit none

   ! Define the variable types
   real :: Det
   integer :: n
   real :: M(n,n)

   ! Perform determinant for 2x2 matrix.
   if ( n == 2 ) then
      Det = (M(1,1) * M(2,2)) - (M(1,2) * M(2,1))
   
   ! Perform determinant for 3x3 matrix.
   else if ( n == 3 ) then
      Det = (M(1,1) * M(2,2) * M(3,3)) + (M(1,2) * M(2,3) * M(3,1)) + (M(1,3) * M(2,1) * M(3,2)) &
      - (M(1,3) * M(2,2) * M(3,1)) - (M(1,2) * M(2,1) * M(3,3)) - (M(1,1) * M(2,3) * M(3,2))
   end if

      
end function Determinant
