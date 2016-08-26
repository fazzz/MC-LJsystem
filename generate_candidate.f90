module generate_candidate
  implicit none

contains

  subroutine g_candidate(r, r_candidate, Num, delta, seed)
    use mtmod
    implicit none

    double precision, allocatable,dimension(:,:),intent(in) :: r
    double precision, allocatable,dimension(:,:),intent(inout) :: r_candidate
    integer ,intent(in)  :: Num,seed
    double precision ,intent(in) :: delta

    integer i
    double precision u,u1,u2,u3

    r_candidate = r

    u=grnd()

    i=int(Num*u)+1

!    print *,i

    u1=grnd()-0.5d0
    u2=grnd()-0.5d0
    u3=grnd()-0.5d0

!    print *,u1,u2,u3

    r_candidate(i,1) = r(i,1) + delta*u1
    r_candidate(i,2) = r(i,2) + delta*u2
    r_candidate(i,3) = r(i,3) + delta*u3

  end subroutine g_candidate
end module generate_candidate
