!###############################################################################
!-------------------------------------------------------------------------------

  program masnum_wam

!-------------------------------------------------------------------------------

  use wamcor_mod

  implicit none
! Modified by Zhenya.Song, for timer, 2016/04/03
  integer wav_count0, wav_count1, wav_count_rate, wav_count_max
  real :: elapse_time
! End Modify

!-------------------------------------------------------------------------------
! Modified by Zhenya.Song, for timer, 2016/04/03
  call system_clock(wav_count0, wav_count_rate, wav_count_max)
  if (wav_count_rate ==0) then
   write(6, '(a33)') '--- No system clock available ---' 
   stop
  endif
! End Modify
  call precom
  call readwi

!-------------------------------------------------------------------------------
! Modified by Zhenya.Song, for timer, 2016/04/03
  call system_clock(wav_count1, wav_count_rate, wav_count_max)
  if (wav_count1 > wav_count0) then
   elapse_time = (wav_count1 - wav_count0 ) * 1.0 / wav_count_rate 
  else
   elapse_time = (wav_count_max + wav_count1 - wav_count0) * 1.0 / wav_count_rate
  endif
  open(unit=11, file="elapse_time.txt", status = "replace")
  write(11, *) elapse_time
  close(11)
! End Modify

  stop

!-------------------------------------------------------------------------------

  end program masnum_wam

!-------------------------------------------------------------------------------
!###############################################################################
