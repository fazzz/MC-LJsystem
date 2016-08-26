! This program performas Monte Carlo(MC) simulation
! for Lenord-Jones system with periodic boundary conditions.
program MonteCarlo
  use ini_conf
  use generate_candidate
  use pbs
  use read_inputs
  use write_outputs
  use energy
  use mtmod

  implicit none

  integer i,j,k

  integer Num, maxsteps, outputinterval, listinterval, seed
  ! data Num,maxsteps,outputinterval,listinterval,seed/108,10000,10,10,4358/
  ! Num: The number of atoms in the system.
  ! maxsteps: The number of steps for MC calculatin.
  ! outputinterbal: The frequency of steps to save the coordinate in the output file.
  ! listinterval: The frequenct of steps to re-calculate the neibhoring list.
  ! seed: The seed for random number generators.
  double precision T, epsilon, sigma, L_box_size, L_cut_off, deltaX, deltaS
  ! data T,epsilon,sigma,L_box_size,L_cut_off,deltaX,deltaS/328.0e0,119.8e0,3.822e0,30.0e0,10.0e0,0.1e0,1.0e0/
  ! T: The reduced temperature of the system.
  ! epsilon: The force constant of the Lenord-Jones potential.
  ! sigma: The collision radius of the atom in the system.
  ! L_box_size: The length of the box.
  ! L_cut_off: The length of the cutt off.
  ! deltaX: The maximum moving length of the each step.
  ! deltaS: The maximum difference of neibhoring list.

  integer :: ON=1
  integer :: OFF=0

  double precision, allocatable,dimension(:,:) :: r, r_candidate
  double precision e_total, e_total_candidate, r_d, u, beta

  integer M
  integer, allocatable :: P_list, L_list

  double precision acceptance_ratio
  data acceptance_ratio/0.0e0/

  integer initializeMODE
  integer time_start, time_finish, time

  character coordinate_filename*50
  character outputfilename_coordinate*50
  character outputfilename_energy*50

  character(10) :: argv, programname
  integer :: argc, iargc

  NAMELIST /mccontrols/ Num, maxsteps, outputinterval, seed, T, epsilon, sigma, L_box_size, L_cut_off, deltaX
  open(17,file='parameters_mc')                                                                                      
  read(17,mccontrols)                                                                                            
  close(17)                                                                                                          

  initializeMODE=ON

  argc=iargc()
  call getarg(0,programname)
  if (argc < 2) then
     call usage(programname)
  else if ( argc == 2 ) then
     call getarg(1,outputfilename_coordinate)
     call getarg(2,outputfilename_energy)
  else if ( argc == 3 ) then
     call getarg(1,coordinate_filename)
     call getarg(2,outputfilename_coordinate)
     call getarg(3,outputfilename_energy)
     initializeMODE = OFF
  else
     call usage(programname)
  end if

  allocate(r(Num,3))
  allocate(r_candidate(Num,3))

  call system_clock(count=time_start)

  if ( initializeMODE .eq. ON ) then
     call initialize_conformation(r, Num, L_box_size)
  else
     open(21,file=coordinate_filename,status='old')
     call read_coordinate(21, r, Num)
     close(21)
     call pbs_coordinate(r, Num, L_box_size)
  end if

  e_total = total_energy_LJ(r, Num, L_box_size, epsilon, sigma, L_cut_off)

  call sgrnd(seed)

  beta = 1.0d0 / T

  open(19,file=outputfilename_coordinate,position='append')
  open(20,file=outputfilename_energy,position='append')

!  M = make_neibhoring_list(P_list, L_list, deltaS)

  do i=1,maxsteps,1
     call g_candidate(r, r_candidate, Num, deltaX, seed)
     call pbs_coordinate(r_candidate, Num, L_box_size)

     e_total_candidate = total_energy_LJ(r_candidate, Num, L_box_size, epsilon, sigma, L_cut_off) !, \\
!                                         M, P_list, L_list)

     r_d = beta*(e_total-e_total_candidate)
     r_d = exp(r_d)

     u=grnd()

     if (u<r_d) then
        r = r_candidate
        e_total = e_total_candidate
        acceptance_ratio=acceptance_ratio + 1.0e0
     end if

!     print *,e_total

     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     ! if (mod(i,listterval) == 0) then                    !
     !    M = make_neibhoring_list(P_list, L_list, deltaS) !
     ! end if                                              !
     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     if (mod(i,outputinterval) == 0) then
        print *,i,e_total
        call write_coordinate(r,19,Num)
        call write_energy(e_total,20,i)
     end if
  end do

  close(19)
  close(20)

  deallocate(r)
  deallocate(r_candidate)

  acceptance_ratio = acceptance_ratio/maxsteps*100.0e0

  call system_clock(count=time_finish)

  time = time_finish - time_start
  time = time / 3600.0d0

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! write( *,programname,"is completed.",                     !
  ! print *,"The consuming time is", F8.3, "hours", time      !
  ! print *,"The acceptance ratio is", F8.3, acceptance_ratio !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

  subroutine usage(programname)
    implicit none
    character(10) :: programname

    print *,"Usage: ",programname," [ inputfilename (PDB file) ] outputfilename1(coordinate) outputfilename2(energy)"
    stop
  end subroutine usage

end program MonteCarlo
