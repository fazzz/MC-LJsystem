program main
  use pdb
  use initialize
  use write_outputs

  implicit none

  integer i
  type (PDB_atom),allocatable,dimension(:) :: PA
  double precision,allocatable,dimension(:,:) :: r
  character :: filename*50

  integer Num
  data Num/108/
  double precision L_box_size
  data L_box_size/34.9d0/

  allocate(PA(Num))
  allocate(r(Num,3))

  call initialize_conformation(r, Num, L_box_size)

  filename="initial.pdb"

  open(19,file=filename,position='append')

  call write_coordinate(r,19,Num)

  close(19)

  deallocate(r)
  deallocate(PA)

end program main
