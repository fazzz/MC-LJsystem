! This module contains the subroutine
! to generate the initial coordinate for Monte Carlo simulation.
module initialize
  implicit none

  contains

    subroutine initialize_conformation(r, N, box_size)
      implicit none

      double precision,allocatable,dimension(:,:),intent(inout) :: r
      double precision,intent(in) :: box_size
      integer,intent(in) :: N

      integer :: ON=1
      integer :: OFF=0
      
      integer i, j, k, l, num
      integer flagi, flagj
      double precision len

      num=int(N**(1.0/3.0))

      if ( N > num**3.0 ) then
         num = num + 1
      end if

      len = box_size/num

      flagi = OFF
      flagj = OFF

      l = 1
      do i=1,num,1
         if ( flagi == ON ) then
            exit
         end if
         do j=1,num,1
            if ( flagj == ON ) then
               flagi = ON
               exit
            end if
            do k=1,num,1
!               print *,l, N, num
               r(l,1) = i * len
               r(l,2) = j * len
               r(l,3) = k * len
!               print *,l ,N
!               print *,l, r(l,1), r(l,2), r(l,3)
               l = l + 1
!               print *,l, N, num
               if ( l > N ) then
!                  print *,l
                  flagj = ON
                  exit
               end if
!               print *,l, N, num
            end do
         end do
      end do
      
    end subroutine initialize_conformation
end module initialize

