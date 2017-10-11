!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
!*DeckYinxq: readwi

  subroutine readwi

!-------------------------------------------------------------------------------

  implicit none

  real :: n, a
  real, allocatable :: em(:, :, :, :)

!-------------------------------------------------------------------------------

  allocate(em(kl,jl,ixl,iyl))

! --- Input restart file or set for cool restart.

  call inprst(rstfile, key)

!-------------------------------------------------------------------------------

  dtime0 = datenum(istime)
  dtimeend = datenum(ietime)
  number = (1-key) * cools_days * 1440. / delttm
  itend = number + (datenum(ietime) - dtime0 + 1) * 1440. / delttm

!-------------------------------------------------------------------------------

  key = 0
  dtime = dtime0

  do it = 0, itend

!-------------------------------------------------------------------------------

! --- Set time & key for cool start.

    dtime = dtime + key * delttm / 1440.
    if(it >= number)key = 1
!    dtime = dtime0 + key * (it - number) * delttm / 1440.
    itime = datevec(dtime)
    ctime = datestr(itime)

    if(dtime > dtimeend)exit

    write(6, *)'READWI: ', ctime(1:8), ' ', ctime(9:12), it - number

!-------------------------------------------------------------------------------

! --- Read in wind data: wx, wy, w is the wind used in model.

    call get_wind

    if(key == 0 .and. it == 0)then
      write(6, *)'READWI: ', 'Cool start'
      call setspec(1)
    endif

!-------------------------------------------------------------------------------

    call set_uv
    call set_ice

!-------------------------------------------------------------------------------

! --- For wave propagation.

    do ic=iys,iyl
      do ia=ixs,ixl
        do k=1,kl
          do j=1,jl
            if(ee(k,j,ia,ic).eq.0.0)ee(k,j,ia,ic) = small
          enddo
        enddo
      enddo
    enddo

    call propagat

!-------------------------------------------------------------------------------

! --- For source functions.

    do ic=iys,iyl
      do ia=ixs,ixl
        do k=1,kl
          do j=1,jl
            if(e(k,j,ia,ic) <= 0.0)e(k,j,ia,ic) = small
          enddo
        enddo
      enddo
    enddo

    call implsch

!-------------------------------------------------------------------------------

! --- For reginal model: setspec or nesting.

    if(glbflag == 1)then
      call setspec(2)
    elseif(glbflag == 2)then
      ! --- nest from ...
      !call wamnst_io
    endif

!-------------------------------------------------------------------------------

    a = 24
!   do ic=iy1,iy2
!   do ia=ix1,ix2
    do ic=iys,iyl
    do ia=ixs,ixl

      if(nsp(ia,ic) == 0)then
        em(:, :, ia, ic) = 0.0
      endif
      if(nsp(ia,ic) == 0)cycle

      if(ia /= 1 .and. ia /= ixl .and. ic /= 1 .and. ic /= iyl)then

        n=a
        if(nsp(ia-1,ic).gt.0)n=n+1
        if(nsp(ia+1,ic).gt.0)n=n+1
        if(nsp(ia,ic-1).gt.0)n=n+1
        if(nsp(ia,ic+1).gt.0)n=n+1
        do k = 1, kl
        do j = 1, jl
          em(k,j,ia,ic)=(a*ee(k,j,ia,ic)+ ee(k,j,ia-1,ic) + ee(k,j,ia+1,ic) &
                       + ee(k,j,ia,ic-1) + ee(k,j,ia,ic+1) ) / n
        enddo
        enddo

      elseif(glbflag == 0 .and. ia == 1 .and. ic /= 1 .and. ic /= iyl)then

        n=a
        if(nsp(ix2,ic).gt.0)n=n+1
        if(nsp(ia+1,ic).gt.0)n=n+1
        if(nsp(ia,ic-1).gt.0)n=n+1
        if(nsp(ia,ic+1).gt.0)n=n+1
        do k = 1, kl
        do j = 1, jl
          em(k,j,ia,ic)=(a*ee(k,j,ia,ic)+ ee(k,j,ix2,ic) + ee(k,j,ia+1,ic) &
                       + ee(k,j,ia,ic-1) + ee(k,j,ia,ic+1) )/n
        enddo
        enddo

      elseif(glbflag == 0 .and. ia == ixl .and. ic /= 1 .and. ic /= iyl)then

!       em(k,j,ia,ic) = em(k,j,1,ic)
        n=a
        if(nsp(ia-1,ic).gt.0)n=n+1
        if(nsp(ixs,ic).gt.0)n=n+1
        if(nsp(ia,ic-1).gt.0)n=n+1
        if(nsp(ia,ic+1).gt.0)n=n+1
        do k = 1, kl
        do j = 1, jl
          em(k,j,ia,ic)=(a*ee(k,j,ia,ic)+ ee(k,j,ia-1,ic) + ee(k,j,ixs,ic) &
                       + ee(k,j,ia,ic-1) + ee(k,j,ia,ic+1) )/n
        enddo
        enddo

      else

        em(:, :, ia, ic) = ee(:, :, ia, ic)

      endif

    end do
    end do

    ee = em

!-------------------------------------------------------------------------------

! --- Compute for wave parameters.

    do ic=iys,iyl
      do ia=ixs,ixl
        do k=1,kl
          do j=1,jl
            if(ee(k,j,ia,ic).eq.0.0)ee(k,j,ia,ic) = small
          enddo
        enddo
      enddo
    enddo

!-------------------------------------------------------------------------------

    call mean1

!-------------------------------------------------------------------------------

! --- Output & save restart.

    if(key == 0)cycle

!    if(itime(2) == 1 .and. itime(3) == 1)then
!    if(itime(3) == 1)then
      call outrst('restart_'//ctime(1:12)//'.nc')
!    else
!      call outrst(rstfile)
!    endif
    
    call output

!-------------------------------------------------------------------------------

  enddo

  deallocate(em)

!-------------------------------------------------------------------------------

  return

!-------------------------------------------------------------------------------

  end subroutine readwi

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
