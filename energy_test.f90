program main
  use read_inputs
  use write_outputs
  use energy
  implicit none

  integer i,j
  double precision, allocatable, dimension(:,:) :: r
  double precision ene
  double precision box_size
  character filename*50, filename2*50

  integer Num
  data Num/108/
  double precision epsilon, sigma, L_box_size, cut_off
  data epsilon,sigma,L_box_size,cut_off/119.8d0,3.822d0,30.0d0,10.0d0/

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! print *,"Inputfile name is:"  !
  ! read *,filename               !
  ! print *,"Outputfile name is:" !
  ! read *,filename2              !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  filename="initial.pdb"
  filename2="energy_test.txt"

  allocate(r(Num,3))

  open(21,file=filename,status='old')
  call read_coordinate(21, r, Num)
  close(21)

  ene = total_energy_LJ(r, Num, L_box_size, epsilon, sigma, cut_off)

  open(20,file=filename2,position='append')

  call write_energy(ene,20,1)

  close(20)

  deallocate(r)
end program main
