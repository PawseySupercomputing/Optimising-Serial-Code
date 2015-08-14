module timeit

  implicit none
  integer, parameter :: dp = kind(1.d0)
contains

subroutine time_it(n,iters)
  integer :: i
  integer,dimension(8) :: ut1,ut2
  real :: t1, t2, etime
  real(dp), allocatable :: A(:, :), B(:, :)
  real(dp), allocatable :: C(:, :)
  integer, intent(in) :: n,iters
  character(len=32), dimension(3) :: methods
  real(dp), dimension(3) :: ctimes
  real(dp), dimension(3) :: etimes

  write(*,*)
  write(*,'(a16,i10)') "Matrix size:",n
  write(*,'(a16,i10)') "Iterations:",iters

  allocate(A(n, n), B(n, n), C(n, n))
  call random_number(A)
  call random_number(B)

  call cpu_time(t1)
  call date_and_time(values=ut1)
  do i = 1, iters
      call matmul1(A,B,C)
  end do
  call cpu_time(t2)
  call date_and_time(values=ut2)
  methods(1)="External dgemm"
  ctimes(1)=t2-t1
  etimes(1)=elapsedtime(ut1,ut2)

  call cpu_time(t1)
  call date_and_time(values=ut1)
  do i = 1, iters
      call matmul2(A,B,C)
  end do
  call cpu_time(t2)
  call date_and_time(values=ut2)
  methods(2)="Intrinsic matmul"
  ctimes(2)=t2-t1
  etimes(2)=elapsedtime(ut1,ut2)

  call cpu_time(t1)
  call date_and_time(values=ut1)
  do i = 1, iters
      call matmul3(A,B,C)
  end do
  call cpu_time(t2)
  call date_and_time(values=ut2)
  methods(3)="Manual loops"
  ctimes(3)=t2-t1
  etimes(3)=elapsedtime(ut1,ut2)

  call printtimes(methods,ctimes,etimes)

  deallocate(A)
  deallocate(B)
  deallocate(C)
end subroutine

real(dp) function elapsedtime(starttime,stoptime)
  integer, dimension(8), intent(in) :: starttime,stoptime
  elapsedtime=(stoptime(5)*3600._dp+stoptime(6)*60._dp+real(stoptime(7),kind=dp)+stoptime(8)/1000._dp)- &
       (starttime(5)*3600._dp+starttime(6)*60._dp+real(starttime(7),kind=dp)+starttime(8)/1000._dp)
end function

subroutine printtimes(methods,ctimes,etimes)
  character(len=*), dimension(3), intent(in) :: methods
  real(dp),dimension(3), intent(in) :: ctimes
  real(dp),dimension(3), intent(in) :: etimes
  write(*,'(a16,3a32)')    "Method:", trim(methods(1)),trim(methods(2)),trim(methods(3))
  write(*,'(a16,f30.3,a2,f30.3,a2,f30.3,a2)') "CPU time:", ctimes(1), " s", ctimes(2), " s", ctimes(3), " s"
  write(*,'(a16,f30.3,a2,f30.3,a2,f30.3,a2)') "Elapsed time:", etimes(1), " s", etimes(2), " s", etimes(3), " s"
end subroutine

subroutine matmul1(A,B,C)
  integer :: n
  real(dp), intent(out) :: C(:, :)
  real(dp), intent(in) :: A(:, :), B(:, :)
  external :: dgemm
  n = size(A,1)
  call dgemm('N', 'N', n, n, n, 1.0_dp, A, n, B, n, 0.0_dp, C, n);
end subroutine

subroutine matmul2(A,B,C)
  real(dp), intent(out) :: C(:, :)
  real(dp), intent(in) :: A(:, :), B(:, :)
  external :: dgemm
  C = matmul(A, B)
end subroutine

subroutine matmul3(A,B,C)
  real(dp), intent(out) :: C(:, :)
  real(dp), intent(in) :: A(:, :), B(:, :)
  integer :: i,j,k,l,n
  real(dp) :: tmp
  n = size(A,1)

  do j=1,n
    do l=1,n
      C(l,j)=0.0_dp
    end do

    do k=1,n
      tmp=B(k,j)
      do i=1,n
        C(i,j)=C(i,j)+tmp*A(i,k)
      end do
    end do
  end do
end subroutine

end module

program matrix

  use timeit
  implicit none

!  integer, parameter :: dp = kind(1.d0)

call time_it(1000,1)
!call time_it(20,1000000)
!call time_it(3,10000000)


!####################################################################
!End of executable statements.
!####################################################################
end program matrix
