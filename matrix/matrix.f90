module timeit

  implicit none
  integer, parameter :: dp = kind(1.d0)
contains

subroutine time_all(n,iters)
  integer :: i
  integer,dimension(8) :: ut1,ut2
  real :: t1, t2, etime
  real(dp), allocatable :: A(:, :), B(:, :)
  real(dp), allocatable :: C(:, :)
  integer, intent(in) :: n,iters
  character(len=32), dimension(5) :: methods
  real(dp), dimension(5) :: ctimes
  real(dp), dimension(5) :: etimes

  write(*,*)
  write(*,'(a16,i10)') "Matrix size:",n
  write(*,'(a16,i10)') "Iterations:",iters

  allocate(A(n, n), B(n, n), C(n, n))
  call random_number(A)
  call random_number(B)

  !Testing matmul1 (dgemm)
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

  !Testing matmul2 (intrinsic matmul)
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

  !Testing matmul3 (manual looping)
  call cpu_time(t1)
  call date_and_time(values=ut1)
  do i = 1, iters
      call matmul3(A,B,C)
  end do
  call cpu_time(t2)
  call date_and_time(values=ut2)
  methods(3)="Manual loops v1"
  ctimes(3)=t2-t1
  etimes(3)=elapsedtime(ut1,ut2)

  !Testing matmul4 (manual looping)
  call cpu_time(t1)
  call date_and_time(values=ut1)
  do i = 1, iters
      call matmul4(A,B,C)
  end do
  call cpu_time(t2)
  call date_and_time(values=ut2)
  methods(4)="Manual loops V2"
  ctimes(4)=t2-t1
  etimes(4)=elapsedtime(ut1,ut2)

  !Testing matmul5 (manual looping)
  call cpu_time(t1)
  call date_and_time(values=ut1)
  do i = 1, iters
      call matmul5(A,B,C)
  end do
  call cpu_time(t2)
  call date_and_time(values=ut2)
  methods(5)="Manual loops V3"
  ctimes(5)=t2-t1
  etimes(5)=elapsedtime(ut1,ut2)

  call printtimes(methods,ctimes,etimes,5)

  deallocate(A)
  deallocate(B)
  deallocate(C)
end subroutine

subroutine test_matmul1(n,iters)
  integer :: i
  integer,dimension(8) :: ut1,ut2
  real :: t1, t2, etime
  real(dp), allocatable :: A(:, :), B(:, :)
  real(dp), allocatable :: C(:, :)
  integer, intent(in) :: n,iters
  character(len=32), dimension(5) :: methods
  real(dp), dimension(5) :: ctimes
  real(dp), dimension(5) :: etimes

  write(*,*)
  write(*,'(a16,i10)') "Matrix size:",n
  write(*,'(a16,i10)') "Iterations:",iters

  allocate(A(n, n), B(n, n), C(n, n))
  call random_number(A)
  call random_number(B)

  !Testing matmul1 (dgemm)
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
  call printtimes(methods,ctimes,etimes,1)

  deallocate(A)
  deallocate(B)
  deallocate(C)
end subroutine

subroutine test_matmul2(n,iters)
  integer :: i
  integer,dimension(8) :: ut1,ut2
  real :: t1, t2, etime
  real(dp), allocatable :: A(:, :), B(:, :)
  real(dp), allocatable :: C(:, :)
  integer, intent(in) :: n,iters
  character(len=32), dimension(5) :: methods
  real(dp), dimension(5) :: ctimes
  real(dp), dimension(5) :: etimes

  write(*,*)
  write(*,'(a16,i10)') "Matrix size:",n
  write(*,'(a16,i10)') "Iterations:",iters

  allocate(A(n, n), B(n, n), C(n, n))
  call random_number(A)
  call random_number(B)

  !Testing matmul2 (intrinsic matmul)
  call cpu_time(t1)
  call date_and_time(values=ut1)
  do i = 1, iters
      call matmul2(A,B,C)
  end do
  call cpu_time(t2)
  call date_and_time(values=ut2)
  methods(1)="Intrinsic matmul"
  ctimes(1)=t2-t1
  etimes(1)=elapsedtime(ut1,ut2)
  call printtimes(methods,ctimes,etimes,1)

  deallocate(A)
  deallocate(B)
  deallocate(C)
end subroutine

subroutine test_matmul3(n,iters)
  integer :: i
  integer,dimension(8) :: ut1,ut2
  real :: t1, t2, etime
  real(dp), allocatable :: A(:, :), B(:, :)
  real(dp), allocatable :: C(:, :)
  integer, intent(in) :: n,iters
  character(len=32), dimension(5) :: methods
  real(dp), dimension(5) :: ctimes
  real(dp), dimension(5) :: etimes

  write(*,*)
  write(*,'(a16,i10)') "Matrix size:",n
  write(*,'(a16,i10)') "Iterations:",iters

  allocate(A(n, n), B(n, n), C(n, n))
  call random_number(A)
  call random_number(B)

  !Testing matmul3 (manual looping)
  call cpu_time(t1)
  call date_and_time(values=ut1)
  do i = 1, iters
      call matmul3(A,B,C)
  end do
  call cpu_time(t2)
  call date_and_time(values=ut2)
  methods(1)="Manual loops V1"
  ctimes(1)=t2-t1
  etimes(1)=elapsedtime(ut1,ut2)
  call printtimes(methods,ctimes,etimes,1)

  deallocate(A)
  deallocate(B)
  deallocate(C)
end subroutine

subroutine test_matmul4(n,iters)
  integer :: i
  integer,dimension(8) :: ut1,ut2
  real :: t1, t2, etime
  real(dp), allocatable :: A(:, :), B(:, :)
  real(dp), allocatable :: C(:, :)
  integer, intent(in) :: n,iters
  character(len=32), dimension(5) :: methods
  real(dp), dimension(5) :: ctimes
  real(dp), dimension(5) :: etimes

  write(*,*)
  write(*,'(a16,i10)') "Matrix size:",n
  write(*,'(a16,i10)') "Iterations:",iters

  allocate(A(n, n), B(n, n), C(n, n))
  call random_number(A)
  call random_number(B)

  !Testing matmul4 (manual looping)
  call cpu_time(t1)
  call date_and_time(values=ut1)
  do i = 1, iters
      call matmul4(A,B,C)
  end do
  call cpu_time(t2)
  call date_and_time(values=ut2)
  methods(1)="Manual loops V2"
  ctimes(1)=t2-t1
  etimes(1)=elapsedtime(ut1,ut2)
  call printtimes(methods,ctimes,etimes,1)

  deallocate(A)
  deallocate(B)
  deallocate(C)
end subroutine

subroutine test_matmul5(n,iters)
  integer :: i
  integer,dimension(8) :: ut1,ut2
  real :: t1, t2, etime
  real(dp), allocatable :: A(:, :), B(:, :)
  real(dp), allocatable :: C(:, :)
  integer, intent(in) :: n,iters
  character(len=32), dimension(5) :: methods
  real(dp), dimension(5) :: ctimes
  real(dp), dimension(5) :: etimes

  write(*,*)
  write(*,'(a16,i10)') "Matrix size:",n
  write(*,'(a16,i10)') "Iterations:",iters

  allocate(A(n, n), B(n, n), C(n, n))
  call random_number(A)
  call random_number(B)

  !Testing matmul5 (manual looping)
  call cpu_time(t1)
  call date_and_time(values=ut1)
  do i = 1, iters
      call matmul5(A,B,C)
  end do
  call cpu_time(t2)
  call date_and_time(values=ut2)
  methods(1)="Manual loops V3"
  ctimes(1)=t2-t1
  etimes(1)=elapsedtime(ut1,ut2)
  call printtimes(methods,ctimes,etimes,1)

  deallocate(A)
  deallocate(B)
  deallocate(C)
end subroutine

real(dp) function elapsedtime(starttime,stoptime)
  integer, dimension(8), intent(in) :: starttime,stoptime
  elapsedtime=(stoptime(5)*3600._dp+stoptime(6)*60._dp+real(stoptime(7),kind=dp)+stoptime(8)/1000._dp)- &
       (starttime(5)*3600._dp+starttime(6)*60._dp+real(starttime(7),kind=dp)+starttime(8)/1000._dp)
end function

subroutine printtimes(methods,ctimes,etimes,n_methods)
  integer, intent(in) :: n_methods
  character(len=*), dimension(n_methods), intent(in) :: methods
  real(dp),dimension(n_methods), intent(in) :: ctimes
  real(dp),dimension(n_methods), intent(in) :: etimes
  integer :: i
  write(*,'(a16)',advance='no')    "Method:"
  do i=1,n_methods
     write(*,'(a22)',advance='no')    trim(methods(i))
  end do
  write(*,*) ''
  write(*,'(a16)',advance='no') "CPU time:"
  do i=1,n_methods
     write(*,'(f20.3,a2)',advance='no') ctimes(i), " s"
  end do
  write(*,*) ''
  write(*,'(a16)',advance='no') "Elapsed time:"
  do i=1,n_methods
     write(*,'(f20.3,a2)',advance='no') etimes(i), " s"
  end do
  write(*,*) ''
end subroutine

subroutine matmul1(A,B,C) !Using dgemm
  integer :: n
  real(dp), intent(out) :: C(:, :)
  real(dp), intent(in) :: A(:, :), B(:, :)
  external :: dgemm
  n = size(A,1)
  call dgemm('N', 'N', n, n, n, 1.0_dp, A, n, B, n, 0.0_dp, C, n);
end subroutine

subroutine matmul2(A,B,C) !Using matmul
  real(dp), intent(out) :: C(:, :)
  real(dp), intent(in) :: A(:, :), B(:, :)
  C = matmul(A, B)
end subroutine

subroutine matmul3(A,B,C) !Manual loops V1 (Similar to dgemm logic)
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

subroutine matmul4(A,B,C) !Manual loops V2 (Not using tmp outside the i loop)
  real(dp), intent(out) :: C(:, :)
  real(dp), intent(in) :: A(:, :), B(:, :)
  integer :: i,j,k,l,n
  n = size(A,1)

  do j=1,n
    do l=1,n
      C(l,j)=0.0_dp
    end do

    do k=1,n
      do i=1,n
        C(i,j)=C(i,j)+B(k,j)*A(i,k)
      end do
    end do
  end do
end subroutine

subroutine matmul5(A,B,C) !Manual loops V3 (Not using initialization loop for C)
  real(dp), intent(out) :: C(:, :)
  real(dp), intent(in) :: A(:, :), B(:, :)
  integer :: i,j,k,n
  n = size(A,1)

  do j=1,n
    do i=1,n
      C(i,j)=0.0_dp
      do k=1,n
         C(i,j)=C(i,j)+B(k,j)*A(i,k)
      end do
    end do
  end do
end subroutine

end module

program matrix

  use timeit
  implicit none
  integer :: matrix_size,n_iter

!  integer, parameter :: dp = kind(1.d0)

matrix_size=1000
n_iter=1
call time_all(matrix_size,n_iter)
matrix_size=20
n_iter=1000000
call time_all(matrix_size,n_iter)
matrix_size=3
n_iter=10000000
call time_all(matrix_size,n_iter)

!matrix_size=1000
!n_iter=1
!call test_matmul1(matrix_size,n_iter)
!call test_matmul2(matrix_size,n_iter)
!call test_matmul3(matrix_size,n_iter)
!call test_matmul4(matrix_size,n_iter)
!call test_matmul5(matrix_size,n_iter)

!####################################################################
!End of executable statements.
!####################################################################
end program matrix
