program iobench_hdf5

  use HDF5

  implicit none

  integer, parameter :: n = 10000
  integer, parameter :: rank = 2
  integer, parameter :: n1 = 50
  integer, parameter :: dp = kind(1.d0)
  integer :: i, j, irecl
  integer,dimension(8) :: ut1,ut2
  real(kind=dp), dimension(:, :), allocatable :: a,a1
  character(30), dimension(:), allocatable :: buf
  real :: t1, t2, etime

  INTEGER(HID_T) :: file_id
  INTEGER(HID_T) :: dataset_id
  INTEGER(HID_T) :: dataspace_id

  INTEGER(HSIZE_T), DIMENSION(2) :: dims = (/n,n/) ! Dataset dimensions

  INTEGER     ::   error ! Error flag

  allocate (a(n,n))
  allocate (a1(n1,n1))
  allocate (buf(n))

  do j = 1, n
     do i = 1, n
        a(i, j) = sin(real(i,kind=dp)+real(j,kind=dp))
     end do
  end do
  do j = 1, n1
     do i = 1, n1
        a1(i, j) = sin(real(i,kind=dp)+real(j,kind=dp))
     end do
  end do

  !############################################################
  call cpu_time(t1)
  call date_and_time(values=ut1)
  do j = 1, n1
     do i = 1, n1
        open(unit=30, file="iobench.txt", form="formatted", position="append")
        write(30,*) a1(i, j)
        close(30)
     end do
  end do
  call cpu_time(t2)
  call date_and_time(values=ut2)
  etime=(ut2(5)*3600._dp+ut2(6)*60._dp+real(ut2(7),kind=dp)+ut2(8)/1000._dp)-(ut1(5)*3600._dp+ut1(6)*60._dp+real(ut1(7),kind=dp)+ut1(8)/1000._dp)

  write(*,*)
  write(*,'(a)')        "Text output, reopening file"
  write(*,'(a,f12.3,a)') "Speed        ", size(a1)*8/1048576._dp/etime, " MB/s"
  write(*,'(a,f12.3,a)') "CPU time     ", t2-t1, " s"
  write(*,'(a,f12.3,a)') "Elapsed time ", etime, " s"

  !############################################################
  call cpu_time(t1)
  call date_and_time(values=ut1)
  open(unit=30, file="iobench.txt", form="formatted")
  do j = 1, n
     do i = 1, n
        write(30,*) a(i, j)
     end do
  end do
  close(30)
  call cpu_time(t2)
  call date_and_time(values=ut2)
  etime=(ut2(5)*3600._dp+ut2(6)*60._dp+real(ut2(7),kind=dp)+ut2(8)/1000._dp)-(ut1(5)*3600._dp+ut1(6)*60._dp+real(ut1(7),kind=dp)+ut1(8)/1000._dp)

  write(*,*)
  write(*,'(a)')        "Text output"
  write(*,'(a,f12.3,a)') "Speed        ", size(a)*8/1048576._dp/etime, " MB/s"
  write(*,'(a,f12.3,a)') "CPU time     ", t2-t1, " s"
  write(*,'(a,f12.3,a)') "Elapsed time ", etime, " s"

  !############################################################
  call cpu_time(t1)
  call date_and_time(values=ut1)
  open(unit=30, file="iobench.txt", form="formatted")
  do j = 1, n
     do i = 1, n
        write(buf(i), *) a(i, j)
     end do

     write(30,*) buf
  end do
  close(30)
  call cpu_time(t2)
  call date_and_time(values=ut2)
  etime=(ut2(5)*3600._dp+ut2(6)*60._dp+real(ut2(7),kind=dp)+ut2(8)/1000._dp)-(ut1(5)*3600._dp+ut1(6)*60._dp+real(ut1(7),kind=dp)+ut1(8)/1000._dp)

  write(*,*)
  write(*,'(a)')        "Text output with a manual character buffer"
  write(*,'(a,f12.3,a)') "Speed        ", size(a)*8/1048576._dp/etime, " MB/s"
  write(*,'(a,f12.3,a)') "CPU time     ", t2-t1, " s"
  write(*,'(a,f12.3,a)') "Elapsed time ", etime, " s"

  !############################################################
  call cpu_time(t1)
  call date_and_time(values=ut1)
  open(unit=30, file="iobench.txt", form="formatted")
  write(30,*) a
  close(30)
  call cpu_time(t2)
  call date_and_time(values=ut2)
  etime=(ut2(5)*3600._dp+ut2(6)*60._dp+real(ut2(7),kind=dp)+ut2(8)/1000._dp)-(ut1(5)*3600._dp+ut1(6)*60._dp+real(ut1(7),kind=dp)+ut1(8)/1000._dp)

  write(*,*)
  write(*,'(a)')        "Text output full dump"
  write(*,'(a,f12.3,a)') "Speed        ", size(a)*8/1048576._dp/etime, " MB/s"
  write(*,'(a,f12.3,a)') "CPU time     ", t2-t1, " s"
  write(*,'(a,f12.3,a)') "Elapsed time ", etime, " s"

  !############################################################
  call cpu_time(t1)
  call date_and_time(values=ut1)
  inquire(IOLENGTH=irecl) a
  open(unit=30, file="io.bin", form="unformatted", access="direct", recl=irecl)
  write(30,rec=1) a
  close(30)

  call cpu_time(t2)
  call date_and_time(values=ut2)
  etime=(ut2(5)*3600._dp+ut2(6)*60._dp+real(ut2(7),kind=dp)+ut2(8)/1000._dp)-(ut1(5)*3600._dp+ut1(6)*60._dp+real(ut1(7),kind=dp)+ut1(8)/1000._dp)

  write(*,*)
  write(*,'(a)')        "Binary output full dump"
  write(*,'(a,f12.3,a)') "Speed        ", size(a)*8/1048576._dp/etime, " MB/s"
  write(*,'(a,f12.3,a)') "CPU time     ", t2-t1, " s"
  write(*,'(a,f12.3,a)') "Elapsed time ", etime, " s"

  !############################################################
  ! HDF5 write

  call cpu_time(t1)
  call date_and_time(values=ut1)

  CALL h5open_f(error)

  ! Create a new file using default properties.
  CALL h5fcreate_f("iobench.hdf5", H5F_ACC_TRUNC_F, file_id, error)

  ! Create the dataspace.
  CALL h5screate_simple_f(rank, dims, dataspace_id, error)

  ! Create the dataset with default properties.
  CALL h5dcreate_f(file_id, "matrixa", H5T_NATIVE_DOUBLE, dataspace_id, dataset_id, error)

  ! Write the dataset.
  CALL h5dwrite_f(dataset_id, H5T_NATIVE_DOUBLE, a, dims, error)

  ! End access to the dataset and release resources used by it.
  CALL h5dclose_f(dataset_id, error)

  ! Terminate access to the data space.
  CALL h5sclose_f(dataspace_id, error)

  ! Close the file.
  CALL h5fclose_f(file_id, error)

  ! Close FORTRAN interface.
  CALL h5close_f(error)

  call cpu_time(t2)
  call date_and_time(values=ut2)
  etime=(ut2(5)*3600._dp+ut2(6)*60._dp+real(ut2(7),kind=dp)+ut2(8)/1000._dp)-(ut1(5)*3600._dp+ut1(6)*60._dp+real(ut1(7),kind=dp)+ut1(8)/1000._dp)

  write(*,*)
  write(*,'(a)')        "HDF5"
  write(*,'(a,f12.3,a)') "Speed        ", size(a)*8/1048576._dp/etime, " MB/s"
  write(*,'(a,f12.3,a)') "CPU time     ", t2-t1, " s"
  write(*,'(a,f12.3,a)') "Elapsed time ", etime, " s"

  !############################################################
  deallocate(buf)
  deallocate(a1)
  deallocate(a)
end program iobench_hdf5
