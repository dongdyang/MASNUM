!###############################################################################
!-------------------------------------------------------------------------------

  program masnum_wam_mpi

!-------------------------------------------------------------------------------
  use wamvar_mod
  use wammpi_mod
!Modified by Zhenya.Song, for timer, 2016/04/03
  use ympi_mod
! End Modify
  
  implicit none
! Modified by Zhenya.Song, for timer, 2016/04/03
  integer wav_count0, wav_count1, wav_count_rate, wav_count_max
  real :: elapse_time
! End Modify

!-------------------------------------------------------------------------------
  call wammpi_init
! Modified by Zhenya.Song, for timer, 2016/04/03
  if (myid == 0) then
    call system_clock(wav_count0, wav_count_rate, wav_count_max)
    if (wav_count_rate ==0) then
     write(6, '(a33)') '--- No system clock available ---'
     stop
    endif
  endif
! End Modify

  call readwi_mpi
! Modified by Zhenya.Song, for timer, 2016/04/03
  if (myid == 0) then
    call system_clock(wav_count1, wav_count_rate, wav_count_max)
  endif
  if (myid == 0) then
    if (wav_count1 > wav_count0) then
     elapse_time = (wav_count1 - wav_count0 ) * 1.0 / wav_count_rate
    else
     elapse_time = (wav_count_max + wav_count1 - wav_count0) * 1.0 / wav_count_rate
    endif
    open(unit=11, file="elapse_time.txt", status = "replace")
    write(11, *) "Elapsed Time is ", elapse_time, " seconds"
    close(11)
  endif
! End Modify
  call ympi_final
  
!-------------------------------------------------------------------------------

  end program masnum_wam_mpi

!-------------------------------------------------------------------------------
!###############################################################################
