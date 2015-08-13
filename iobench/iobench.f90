program iobench

  integer, parameter :: n = 10000
  integer, parameter :: n1 = 50
  integer, parameter :: dp = kind(1.d0)
  integer :: i, j, irecl
  integer,dimension(8) :: ut1,ut2
  real(kind=dp), dimension(:, :), allocatable :: a,a1
  character(30), dimension(:), allocatable :: buf
  real :: t1, t2, etime

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
  write(*,'(a)')        "Text output ful dump"
  write(*,'(a,f12.3,a)') "Speed        ", size(a)*8/1048576._dp/etime, " MB/s"
  write(*,'(a,f12.3,a)') "CPU time     ", t2-t1, " s"
  write(*,'(a,f12.3,a)') "Elapsed time ", etime, " s"

  !############################################################
  call cpu_time(t1)
  call date_and_time(values=ut1)
  inquire(IOLENGTH=irecl) a
  open(unit=30, file="iobench.bin", form="unformatted", access="direct", recl=irecl)
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
  deallocate(buf)
  deallocate(a1)
  deallocate(a)

end program iobench
