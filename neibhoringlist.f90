module neibhoringlist
  implicit none

  contains

  integer function make_neibhoring_list(P_list, L_list, deltaS, r, N)
    implicit none

    integer, allocatable,dimension(:),intent(inout) :: P_list, L_list
    double precision, intent(in) :: deltaS
    double precision, allocatable, dimension(:), intent(in) :: r
    integer, intent(in) :: N

    integer i, j

    do i=1,N,1
       do j=1,N,1

       end do
    end do


  end function make_neibhoring_list

end module neibhoringlist
