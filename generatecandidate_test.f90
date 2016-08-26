program main
  use read_inputs
  use write_outputs
  use generate_candidate
  use mtmod
  use pbs
  implicit none

  double precision, allocatable, dimension(:,:) :: r, r_candidate

  integer Num, maxsteps, outputinterval, seed
  data Num,maxsteps,outputinterval,seed/108,100000,100,4358/
  double precision delta, L_box_size
  data delta,L_box_size/0.1d0,34.9d0/

  character pdbfilename*50,trjfilename*50
  double precision e_total

  integer i

  pdbfilename="initial.pdb"
  trjfilename="trajectry.pdb"

  allocate(r(Num,3))
  allocate(r_candidate(Num,3))

  open(21,file=pdbfilename,status='old')
  call read_coordinate(21, r, Num)
  close(21)

  open(19,file=trjfilename,position='append')

  call sgrnd(seed)

  do i=1,maxsteps,1
     call g_candidate(r, r_candidate, Num, delta, seed)
     call pbs_coordinate(r_candidate, Num, L_box_size)

     r = r_candidate

     if (mod(i,outputinterval) .eq. 0) then
        call write_coordinate(r_candidate, 19, Num)
     end if
  end do

  close(19)

  deallocate(r) 
  deallocate(r_candidate)

end program main
