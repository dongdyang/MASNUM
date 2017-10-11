!###############################################################################
!-------------------------------------------------------------------------------

  module wammpi_mod

!-------------------------------------------------------------------------------

  use time_mod
  use netcdf_mod
  
  use ympi_mod
  use wamvar_mod
  
  use wamfio_mod, only : wamfio_mod_init, get_wind
  use wamcor_mod, only : setspec, propagat, implsch, mean1
  use wamcpl_mod, only : set_uv, set_ice
  use wamcpl_mod, only : zyyz, bv, bvl, bh1, bh2
  use wamcpl_mod, only : taubb11, taubb12, taubb22, taubb33
  use wamcpl_mod, only : mixture, mixture_limit, intact
  use wamcpl_mod, only : mixture, mixture_limit, intact

  use wamcpl_mod, only : bv_wtv, bv_wtd, mixture_wit  !shenhj 2012-09-23
    
  implicit none

!-------------------------------------------------------------------------------

  public wammpi_init, readwi_mpi, ympi_final

  private
    
  include 'mpif.h'
!-------------------------------------------------------------------------------

  real, allocatable :: v2(:, :), v3(:, :, :)
  integer, allocatable :: iv2(:, :)
  integer :: recd
! Modified by Zhenya.Song, for point check, 2016/04/06
  integer, parameter :: ipoint = 225
  integer, parameter :: jpoint = 50
! End modify
  integer :: ierr
!-------------------------------------------------------------------------------

  contains
  
!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
!*DeckYinxq: readwi_mpi

  subroutine readwi_mpi

!-------------------------------------------------------------------------------

  implicit none

  real :: n, a
  real, allocatable :: em(:, :, :, :)

!-------------------------------------------------------------------------------
 open(11,file="point_check.txt",status="replace")
 close(11)

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

    if(myid==0)write(6, *)'READWI: ', ctime(1:14), delttm, it - number
!  print*, __FILE__, __LINE__
!-------------------------------------------------------------------------------

! --- Read in wind data: wx, wy, w is the wind used in model.

    call get_wind

    if(key == 0 .and. it == 0)then
      if(myid==0)write(6, *)'READWI: ', 'Cool start'
      call setspec(1)
    endif

!-------------------------------------------------------------------------------

    call set_uv
    call set_ice
    call mpi_set_timesteps  ! mpi, yinxq, 2011/10/13 11:45:51

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

!!-------------------------------------------------------------------------------
!
!! --- For source functions.
!
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

    ! mpi, yinxq, 2011/10/13 11:45:51
    if(halosize == 1)then
	!call mpi_barrier(mpi_comm_ympi,ierr)	
	call updatev(ee, ixl, iyl, kl, jl, halosize)
    endif

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

    ! mpi, yinxq, 2011/10/13 11:45:51
    ! call mpi_barrier(mpi_comm_ympi,ierr)
    call updatev(ee, ixl, iyl, kl, jl, halosize)

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
! Modified by Zhenya.Song, for point check, 2016/04/06
    if (mypebox%i1 <= ipoint .and. mypebox%i2 >= ipoint .and. mypebox%j1 <= jpoint .and. mypebox%j2 >= jpoint) then
      open(11,file="point_check.txt",access="append")
      write(11, *) "Time = ", ctime(1:12),", lon = ", x(ipoint-mypebox%ei1+1), ", lat = ", y(jpoint-mypebox%ej1+1), ", hs = ", h1_3(ipoint-mypebox%ei1+1, jpoint-mypebox%ej1+1)
      close(11)
    endif
! End modify

!-------------------------------------------------------------------------------

! --- Output & save restart.

    if(key == 0)cycle

!    if(itime(2) == 1 .and. itime(3) == 1)then
!    if(itime(3) == 1)then
      call outrst('restart_'//ctime(1:12)//'.nc')
!    else
!      call outrst('restart_'//ctime(1:8)//'.nc')
!    endif
    
!    if(myid==0)write(6, *)'output'
    call output
!    if(myid==0)write(6, *)'output over'

!-------------------------------------------------------------------------------

  enddo

  deallocate(em)

  call outrst('restart_'//ctime(1:12)//'.nc')

!-------------------------------------------------------------------------------

  return

!-------------------------------------------------------------------------------

  end subroutine readwi_mpi

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
!*DeckYinxq: wammpi_init

  subroutine wammpi_init

  implicit none
    	
  integer :: ncid, i, i1, i2
  character(len=80) :: filename

  real, allocatable :: lon1(:), lat1(:), mask1(:, :), dep(:), top1(:, :)
  real, allocatable :: lon2(:), lat2(:)
  
  real :: addlon
  
!-------------------------------------------------------------------------------

  call ympi_init

!-------------------------------------------------------------------------------

! --- Set parameters & pebox.

  halosize = 1
  
  open(11, file='ctlparams')
  read(11, nml=ctlparams)
  close(11)
  
  filename = trim(data_path)//'wamyyz.nc'

  call init_pebox(filename, 'lon', 'lat', 'nspyyz', npe, myid)
                    
!-------------------------------------------------------------------------------

! --- Input topo, x, y, depyyz, ...

  ixl = mypebox%isize
  iyl = mypebox%jsize

  !write(6, *)'read topo ...'

  call open_nc(ncid, trim(data_path)//'wamyyz.nc', 'r')
  kb = get_dimension_len(ncid, 'zyyz')
  allocate(lon2(im), lat2(jm))
  allocate(lon1(ixl), lat1(iyl), mask1(ixl, iyl), dep(kb), top1(ixl, iyl))
  call readnc(ncid, 'lon', lon2)
  call readnc(ncid, 'lat', lat2)
  call readnc(ncid, 'zyyz', dep)
  call readnc(ncid, 'nspyyz', mask1(is:ie, js:je), locs=loc2)
  call readnc(ncid, 'depyyz', top1(is:ie, js:je), locs=loc2)
  call close_nc(ncid)

  !write(6, *)'read topo is okay.'

  lat1(:) = lat2(mypebox%ej1:mypebox%ej2)
  
  if(flagxcycle == 1)then
  	
    lon1(:) = lon2(mypebox%ei1:mypebox%ei2)
    
  else
  	
	  do i = mypebox%ei1, mypebox%ei2
	  	
	    i1 = i - mypebox%ei1 + 1
	    
	    if(i < 1)then
	    	i2 = i + (im - ixoverlay)
	    	addlon = -360.
	    	!write(6, *)lon2(i2), i2, i, im, ixoverlay
	    elseif(i > im)then
		    i2 = i - (im - ixoverlay)
	    	addlon = 360.
	    else
		    i2 = i	    
	    	addlon = 0.
	    endif
	    
		  lon1(i1) = lon2(i2) + addlon

	  enddo
	  
  endif

  !call mpi_barrier(mpi_comm_ympi,ierr)
  call updatev(mask1, ixl, iyl, halosize)
  !call mpi_barrier(mpi_comm_ympi,ierr)
  call updatev(top1, ixl, iyl, halosize)
  
!-------------------------------------------------------------------------------

! --- wamfio_mod_init

  call wamfio_mod_init(lon1, lat1, mask1, dep, top1)

! --- Set timestep

  call mpi_set_timesteps

  deallocate(lon1, lat1, mask1, dep, top1, lon2, lat2)

  !print*, __FILE__, __LINE__
  !write(*,*) "--------------------------INIT DONE"
!-------------------------------------------------------------------------------

  end subroutine wammpi_init
  
!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
!*DeckYinxq: mpi_set_timesteps

  subroutine mpi_set_timesteps
  	
  implicit none
  	
! --- Set timestep

  call get_mpimin(delttm)
  !write(*,*)"delttm:",delttm
  deltt    = delttm  * 60.  ! yinxq
  deltt5   = delttm  * 30.  ! yinxq
  iwiofreq = wiofreq * 60. / delttm
  iciofreq = ciofreq * 60. / delttm
  irstfreq = rstfreq * 60. / delttm
  number   = (1-key) * cools_days * 1440. / delttm

  !if(myid==0)write(6, nml=ctlparams1)
  !write(6, *)'mpi_set_timesteps:', delttm
  deltt_p180p=p180p * deltt


  
  end subroutine mpi_set_timesteps

  include '../wave_cor/output.inc'
  
!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
!*DeckYinxq: outmix
subroutine outmix(filename)

  implicit none

  character(len=*), intent(in) :: filename

!-------------------------------------------------------------------------------

  integer :: ncid

!-------------------------------------------------------------------------------


  if(myid == 0)then
	
	  call open_nc_self(ncid, filename, 'c')
	
	  call dimension_define(ncid, 'lon', im, 'lon', nf_real)
	  call dimension_define(ncid, 'lat', jm, 'lat', nf_real)
	  call dimension_define(ncid, 'dep', kb, 'dep', nf_real)
	  call set_attribute(ncid, 'units', 'degrees_north', 'lat')
	  call set_attribute(ncid, 'units', 'degrees_east', 'lon')
	  call set_attribute(ncid, 'modulo', '', 'lon')
	  call set_attribute(ncid, 'ctime', ctime)
	
	  call variable_define(ncid, 'tau11', nf_real, ['lon', 'lat', 'dep'])
	  call variable_define(ncid, 'tau12', nf_real, ['lon', 'lat', 'dep'])
	  call variable_define(ncid, 'tau22', nf_real, ['lon', 'lat', 'dep'])
	  call variable_define(ncid, 'tau33', nf_real, ['lon', 'lat', 'dep'])
	
	  call set_attribute(ncid, 'missing_value', nf_fill_real, 'tau11')
	  call set_attribute(ncid, 'missing_value', nf_fill_real, 'tau12')
	  call set_attribute(ncid, 'missing_value', nf_fill_real, 'tau22')
	  call set_attribute(ncid, 'missing_value', nf_fill_real, 'tau33')
	
	  if(mixflag == 1 .or. mixflag == 3)then
	
	    call variable_define(ncid, 'bv'   , nf_real, ['lon', 'lat', 'dep'])
	    call set_attribute(ncid, 'missing_value', nf_fill_real, 'bv')
	
	  endif
	
	  if(mixflag == 2 .or. mixflag == 3)then
	
	    call variable_define(ncid, 'bvl'   , nf_real, ['lon', 'lat', 'dep'])
	    call set_attribute(ncid, 'missing_value', nf_fill_real, 'bvl')
	
	    call variable_define(ncid, 'bh1'   , nf_real, ['lon', 'lat', 'dep'])
	    call set_attribute(ncid, 'missing_value', nf_fill_real, 'bh1')
	
	    call variable_define(ncid, 'bh2'   , nf_real, ['lon', 'lat', 'dep'])
	    call set_attribute(ncid, 'missing_value', nf_fill_real, 'bh2')
	    
	  endif
	
	  call end_define(ncid)

	  call writenc(ncid, 'lon', lon)
	  call writenc(ncid, 'lat', lat)
	  call writenc(ncid, 'dep', zyyz)
	
	  call close_nc(ncid)

  endif

!-------------------------------------------------------------------------------
  call mpi_barrier(mpi_comm_ympi,ierr)
  call open_nc(ncid, filename, 'w')

  call setland_v3(taubb11)
  call writenc(ncid, 'tau11'   , v3(is:ie, js:je, :), locs=loc3)
  if(ixoverlay /= 0 )then
	if(loc2(1) == 1)then
	  call writenc(ncid, 'tau11', v3(is:is+ixoverlay, js:je, :), &
	               locs = [im-ixoverlay, loc3(2), loc3(3)])
	else
	  call writenc(ncid, 'tau11', v3(is:0, js:0, :0), locs=[1,1,1])
	endif
  endif

  call setland_v3(taubb12)
  call writenc(ncid, 'tau12'   , v3(is:ie, js:je, :), locs=loc3)
  if(ixoverlay /= 0 )then
	if(loc2(1) == 1)then
	  call writenc(ncid, 'tau12', v3(is:is+ixoverlay, js:je, :), &
	               locs = [im-ixoverlay, loc3(2), loc3(3)])
	else
	  call writenc(ncid, 'tau12'   , v3(is:0, js:0, :0), locs=[1,1,1])
	endif
  endif

  call setland_v3(taubb22)
  call writenc(ncid, 'tau22'   , v3(is:ie, js:je, :), locs=loc3)
  if(ixoverlay /= 0 )then
	if(loc2(1) == 1)then
	  call writenc(ncid, 'tau22', v3(is:is+ixoverlay, js:je, :), &
	               locs = [im-ixoverlay, loc3(2), loc3(3)])
	else
	  call writenc(ncid, 'tau22'   , v3(is:0, js:0, :0), locs=[1,1,1])
	endif
  endif

  call setland_v3(taubb33)
  call writenc(ncid, 'tau33'   , v3(is:ie, js:je, :), locs=loc3)
  if(ixoverlay /= 0 )then
	if(loc2(1) == 1)then
	  call writenc(ncid, 'tau33', v3(is:is+ixoverlay, js:je, :), &
	               locs = [im-ixoverlay, loc3(2), loc3(3)])
	else
	  call writenc(ncid, 'tau33'   , v3(is:0, js:0, :0), locs=[1,1,1])
	endif
  endif

  if(mixflag == 1 .or. mixflag == 3)then

	  call setland_v3(bv)
	  call writenc(ncid, 'bv'   , v3(is:ie, js:je, :), locs=loc3)
	  if(ixoverlay /= 0 )then
		if(loc2(1) == 1)then
		  call writenc(ncid, 'bv', v3(is:is+ixoverlay, js:je, :), &
		               locs = [im-ixoverlay, loc3(2), loc3(3)])
		else
		  call writenc(ncid, 'bv'   , v3(is:0, js:0, :0), locs=[1,1,1])
		endif
	  endif

  endif

  if(mixflag == 2 .or. mixflag == 3)then

	  call setland_v3(bvl)
	  call writenc(ncid, 'bvl'   , v3(is:ie, js:je, :), locs=loc3)
	  if(ixoverlay /= 0 )then
		if(loc2(1) == 1)then
		  call writenc(ncid, 'bvl', v3(is:is+ixoverlay, js:je, :), &
		               locs = [im-ixoverlay, loc3(2), loc3(3)])
		else
		  call writenc(ncid, 'bvl'   , v3(is:0, js:0, :0), locs=[1,1,1])
		endif
	  endif
	
	  call setland_v3(bh1)
	  call writenc(ncid, 'bh1'   , v3(is:ie, js:je, :), locs=loc3)
	  if(ixoverlay /= 0 )then
		if(loc2(1) == 1)then
		  call writenc(ncid, 'bh1', v3(is:is+ixoverlay, js:je, :), &
		               locs = [im-ixoverlay, loc3(2), loc3(3)])
		else
		  call writenc(ncid, 'bh1'   , v3(is:0, js:0, :0), locs=[1,1,1])
		endif
	  endif
	
	  call setland_v3(bh2)
	  call writenc(ncid, 'bh2'   , v3(is:ie, js:je, :), locs=loc3)
	  if(ixoverlay /= 0 )then
		if(loc2(1) == 1)then
		  call writenc(ncid, 'bh2', v3(is:is+ixoverlay, js:je, :), &
		               locs = [im-ixoverlay, loc3(2), loc3(3)])
		else
		  call writenc(ncid, 'bh2'   , v3(is:0, js:0, :0), locs=[1,1,1])
		endif
	  endif
	  
	  
  endif

  call close_nc(ncid)

!-------------------------------------------------------------------------------

  return

!-------------------------------------------------------------------------------

  end subroutine outmix

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
!*DeckYinxq: outmix_wit

  subroutine outmix_wit(filename)

  implicit none

  character(len=*), intent(in) :: filename

!-------------------------------------------------------------------------------

  integer :: ncid

!-------------------------------------------------------------------------------

 
  if(myid == 0)then
	
	  call open_nc_self(ncid, filename, 'c')
	
	  call dimension_define(ncid, 'lon', im, 'lon', nf_real)
	  call dimension_define(ncid, 'lat', jm, 'lat', nf_real)
	  call dimension_define(ncid, 'dep', kb, 'dep', nf_real)
	  call set_attribute(ncid, 'units', 'degrees_north', 'lat')
	  call set_attribute(ncid, 'units', 'degrees_east', 'lon')
	  call set_attribute(ncid, 'modulo', '', 'lon')
	  call set_attribute(ncid, 'ctime', ctime)

    	  call variable_define(ncid, 'bv_wtv'   , nf_real, ['lon', 'lat', 'dep'])   
    	  call set_attribute(ncid, 'missing_value', nf_fill_real, 'bv_wtv')         
                                                                              
    	  call variable_define(ncid, 'bv_wtd'   , nf_real, ['lon', 'lat', 'dep'])   
    	  call set_attribute(ncid, 'missing_value', nf_fill_real, 'bv_wtd')	        
	
	  call end_define(ncid)

	  call writenc(ncid, 'lon', lon)
	  call writenc(ncid, 'lat', lat)
	  call writenc(ncid, 'dep', zyyz)
	
	  call close_nc(ncid)

  endif

!-------------------------------------------------------------------------------
  call mpi_barrier(mpi_comm_ympi,ierr)
  call open_nc(ncid, filename, 'w')
  
  call setland_v3(bv_wtv)                                              
  call writenc(ncid, 'bv_wtv'   ,v3(is:ie, js:je, :), locs=loc3)       
  if(ixoverlay /= 0 )then                    
	if(loc2(1) == 1)then        
	  call writenc(ncid, 'bv_wtv', v3(is:is+ixoverlay, js:je, :), &      
	               locs = [im-ixoverlay, loc3(2), loc3(3)])       
	else
	  call writenc(ncid, 'bv_wtv'   ,v3(is:0, js:0, :0), locs=[1,1,1]) 
	endif
  endif                                                                
                                                                       
  call setland_v3(bv_wtd)                                              
  call writenc(ncid, 'bv_wtd'   , v3(is:ie, js:je, :), locs=loc3)      
  if(ixoverlay /= 0 )then                 
	if(loc2(1) == 1)then           
	  call writenc(ncid, 'bv_wtd', v3(is:is+ixoverlay, js:je, :), &      
	               locs = [im-ixoverlay, loc3(2), loc3(3)])          
	else
	  call writenc(ncid, 'bv_wtd'   , v3(is:0, js:0, :0), locs=[1,1,1])
	endif 
  endif                                                                
	  
  call close_nc(ncid)



!-------------------------------------------------------------------------------

  return

!-------------------------------------------------------------------------------

  end subroutine outmix_wit

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
!*DeckYinxq: outmix_tau

  subroutine outmix_tau(filename)

  implicit none

  character(len=*), intent(in) :: filename

!-------------------------------------------------------------------------------

  integer :: ncid

!-------------------------------------------------------------------------------

 
  if(myid == 0)then
  	
	  call open_nc_self(ncid, filename, 'c')
	
	  call dimension_define(ncid, 'lon', im, 'lon', nf_real)
	  call dimension_define(ncid, 'lat', jm, 'lat', nf_real)
	  call dimension_define(ncid, 'dep', kb, 'dep', nf_real)
	  call set_attribute(ncid, 'units', 'degrees_north', 'lat')
	  call set_attribute(ncid, 'units', 'degrees_east', 'lon')
	  call set_attribute(ncid, 'modulo', '', 'lon')
	  call set_attribute(ncid, 'ctime', ctime)
	
	  call variable_define(ncid, 'tau11', nf_real, ['lon', 'lat', 'dep'])
	  call variable_define(ncid, 'tau12', nf_real, ['lon', 'lat', 'dep'])
	  call variable_define(ncid, 'tau22', nf_real, ['lon', 'lat', 'dep'])
	  call variable_define(ncid, 'tau33', nf_real, ['lon', 'lat', 'dep'])
	
	  call set_attribute(ncid, 'missing_value', nf_fill_real, 'tau11')
	  call set_attribute(ncid, 'missing_value', nf_fill_real, 'tau12')
	  call set_attribute(ncid, 'missing_value', nf_fill_real, 'tau22')
	  call set_attribute(ncid, 'missing_value', nf_fill_real, 'tau33')
	
	  call end_define(ncid)
	
	  call writenc(ncid, 'lon', lon)
	  call writenc(ncid, 'lat', lat)
	  call writenc(ncid, 'dep', zyyz)
	
	  call close_nc(ncid)

  endif
  call mpi_barrier(mpi_comm_ympi,ierr)
!-------------------------------------------------------------------------------

  call open_nc(ncid, filename, 'w')

  call setland_v3(taubb11)
  call writenc(ncid, 'tau11'   , v3(is:ie, js:je, :), locs=loc3)
  if(ixoverlay /= 0 )then
	if(loc2(1) == 1)then
	  call writenc(ncid, 'tau11', v3(is:is+ixoverlay, js:je, :), &
	               locs = [im-ixoverlay, loc3(2), loc3(3)])
	else
	  call writenc(ncid, 'tau11'   , v3(is:0, js:0, :0), locs=[1,1,1])
	endif
  endif

  call setland_v3(taubb12)
  call writenc(ncid, 'tau12'   , v3(is:ie, js:je, :), locs=loc3)
  if(ixoverlay /= 0 )then
	if(loc2(1) == 1)then
	  call writenc(ncid, 'tau12', v3(is:is+ixoverlay, js:je, :), &
	               locs = [im-ixoverlay, loc3(2), loc3(3)])
	else
	  call writenc(ncid, 'tau12'   , v3(is:0, js:0, :0), locs=[1,1,1])
	endif
  endif

  call setland_v3(taubb22)
  call writenc(ncid, 'tau22'   , v3(is:ie, js:je, :), locs=loc3)
  if(ixoverlay /= 0 )then
	if(loc2(1) == 1)then
	  call writenc(ncid, 'tau22', v3(is:is+ixoverlay, js:je, :), &
	               locs = [im-ixoverlay, loc3(2), loc3(3)])
	else
	  call writenc(ncid, 'tau22'   , v3(is:0, js:0, :0), locs=[1,1,1])
	endif
  endif

  call setland_v3(taubb33)
  call writenc(ncid, 'tau33'   , v3(is:ie, js:je, :), locs=[1,1,1])
  if(ixoverlay /= 0 )then
	if(loc2(1) == 1)then
	  call writenc(ncid, 'tau33', v3(is:is+ixoverlay, js:je, :), &
	               locs = [im-ixoverlay, loc3(2), loc3(3)])
	else
	  call writenc(ncid, 'tau33'   , v3(is:0, js:0, :0), locs=[1,1,1])
	endif
  endif

  call close_nc(ncid)



!-------------------------------------------------------------------------------

  return

!-------------------------------------------------------------------------------

  end subroutine outmix_tau

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
!*DeckYinxq: outmix_bvl

  subroutine outmix_bvl(filename)

  implicit none

  character(len=*), intent(in) :: filename

!-------------------------------------------------------------------------------

  integer :: ncid

!-------------------------------------------------------------------------------

  if(myid == 0)then
  	
	  call open_nc_self(ncid, filename, 'c')
	
	  call dimension_define(ncid, 'lon', im, 'lon', nf_real)
	  call dimension_define(ncid, 'lat', jm, 'lat', nf_real)
	  call dimension_define(ncid, 'dep', kb, 'dep', nf_real)
	  call set_attribute(ncid, 'units', 'degrees_north', 'lat')
	  call set_attribute(ncid, 'units', 'degrees_east', 'lon')
	  call set_attribute(ncid, 'modulo', '', 'lon')
	  call set_attribute(ncid, 'ctime', ctime)
	
	  call variable_define(ncid, 'bvl'   , nf_real, ['lon', 'lat', 'dep'])
	  call set_attribute(ncid, 'missing_value', nf_fill_real, 'bvl')
	
	  call variable_define(ncid, 'bh1'   , nf_real, ['lon', 'lat', 'dep'])
	  call set_attribute(ncid, 'missing_value', nf_fill_real, 'bh1')
	
	  call variable_define(ncid, 'bh2'   , nf_real, ['lon', 'lat', 'dep'])
	  call set_attribute(ncid, 'missing_value', nf_fill_real, 'bh2')
	
	  call end_define(ncid)
	
	  call writenc(ncid, 'lon', lon)
	  call writenc(ncid, 'lat', lat)
	  call writenc(ncid, 'dep', zyyz)
	
	  call close_nc(ncid)

  endif
	
!-------------------------------------------------------------------------------
  call mpi_barrier(mpi_comm_ympi,ierr)
  call open_nc(ncid, filename, 'w')

  call setland_v3(bvl)
  call writenc(ncid, 'bvl'   , v3(is:ie, js:je, :), locs=loc3)
  if(ixoverlay /= 0 )then
	if(loc2(1) == 1)then
	  call writenc(ncid, 'bvl', v3(is:is+ixoverlay, js:je, :), &
	               locs = [im-ixoverlay, loc3(2), loc3(3)])
	else
	  call writenc(ncid, 'bvl'   , v3(is:0, js:0, :0), locs=[1,1,1])
	endif
  endif

  call setland_v3(bh1)
  call writenc(ncid, 'bh1'   , v3(is:ie, js:je, :), locs=loc3)
  if(ixoverlay /= 0 )then
	if(loc2(1) == 1)then
	  call writenc(ncid, 'bh1', v3(is:is+ixoverlay, js:je, :), &
	               locs = [im-ixoverlay, loc3(2), loc3(3)])
	else
	  call writenc(ncid, 'bh1'   , v3(is:0, js:0, :0), locs=[1,1,1])
	endif
  endif

  call setland_v3(bh2)
  call writenc(ncid, 'bh2'   , v3(is:ie, js:je, :), locs=loc3)
  if(ixoverlay /= 0 )then
	if(loc2(1) == 1)then
	  call writenc(ncid, 'bh2', v3(is:is+ixoverlay, js:je, :), &
	               locs = [im-ixoverlay, loc3(2), loc3(3)])
	else
	  call writenc(ncid, 'bh2'   , v3(is:0, js:0, :0), locs=[1,1,1])
	endif
  endif

  call close_nc(ncid)

!-------------------------------------------------------------------------------

  return

!-------------------------------------------------------------------------------

  end subroutine outmix_bvl

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
!*DeckYinxq: outmix_bv

  subroutine outmix_bv(filename)

  implicit none

  character(len=*), intent(in) :: filename

!-------------------------------------------------------------------------------

  integer :: ncid

!-------------------------------------------------------------------------------

  if(myid == 0)then
  	
	  call open_nc_self(ncid, filename, 'c')
	
	  call dimension_define(ncid, 'lon', im, 'lon', nf_real)
	  call dimension_define(ncid, 'lat', jm, 'lat', nf_real)
	  call dimension_define(ncid, 'dep', kb, 'dep', nf_real)
	  call set_attribute(ncid, 'units', 'degrees_north', 'lat')
	  call set_attribute(ncid, 'units', 'degrees_east', 'lon')
	  call set_attribute(ncid, 'modulo', '', 'lon')
	  call set_attribute(ncid, 'ctime', ctime)
	
	  call variable_define(ncid, 'bv'   , nf_real, ['lon', 'lat', 'dep'])
	  call set_attribute(ncid, 'missing_value', nf_fill_real, 'bv')
	
	  call end_define(ncid)
	
	  call writenc(ncid, 'lon', lon)
	  call writenc(ncid, 'lat', lat)
	  call writenc(ncid, 'dep', zyyz)
	
	  call close_nc(ncid)

  endif
	
!-------------------------------------------------------------------------------
  call mpi_barrier(mpi_comm_ympi,ierr)
  call open_nc(ncid, filename, 'w')

  call setland_v3(bv)
  call writenc(ncid, 'bv'   , v3(is:ie, js:je, :), locs=loc3)
  if(ixoverlay /= 0 )then
	if(loc2(1) == 1)then
	  call writenc(ncid, 'bv', v3(is:is+ixoverlay, js:je, :), &
	               locs = [im-ixoverlay, loc3(2), loc3(3)])
	else
	  call writenc(ncid, 'bv'   , v3(is:0, js:0, :0), locs=[1,1,1])
	endif
  endif

  call close_nc(ncid)

!-------------------------------------------------------------------------------

  return

!-------------------------------------------------------------------------------

  end subroutine outmix_bv

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
!*DeckYinxq: outeng_t

  subroutine outeng_t(filename, rec)

  implicit none

  character(len=*), intent(in) :: filename
  integer, intent(in) :: rec

!-------------------------------------------------------------------------------

  integer :: ncid, jd
  integer*2, parameter :: ivland = nf_fill_int2

  logical :: ext
  integer :: outrecord, timerec, ittt
  double precision, allocatable :: timealready(:)
  
!-------------------------------------------------------------------------------

  jd = datenum(1950, 1, 1, 0, 0, 0)

!-------------------------------------------------------------------------------

 
  if(myid == 0)then

	  inquire(file=filename, exist=ext)
	
	  if(.not.ext)then
		
	    outrecord = 1
	
	    call open_nc_self(ncid, filename, 'c')
	
	    call dimension_define(ncid, 'lon', im, 'lon', nf_real)
	    call dimension_define(ncid, 'lat', jm, 'lat', nf_real)
	    call dimension_define(ncid, 'time', 0, 'time', nf_real)
	    call set_attribute(ncid, 'units', 'degrees_north', 'lat')
	    call set_attribute(ncid, 'units', 'degrees_east', 'lon')
	    call set_attribute(ncid, 'modulo', '', 'lon')
	    call set_attribute(ncid, 'units', 'Days since 1950-01-01 00:00:0.0.', 'time')
	    call set_attribute(ncid, 'Start_time', ctime)
	
	    call variable_define(ncid, 'pein',    nf_real, ['lon', 'lat', 'time'])
	    call variable_define(ncid, 'pebo',    nf_real, ['lon', 'lat', 'time'])
	    call variable_define(ncid, 'peds',    nf_real, ['lon', 'lat', 'time'])
	
	    call set_attribute(ncid, 'missing_value', nf_fill_real, 'pein')
	    call set_attribute(ncid, 'missing_value', nf_fill_real, 'pebo')
	    call set_attribute(ncid, 'missing_value', nf_fill_real, 'peds')
	
	    call end_define(ncid)
	  
	    call writenc(ncid, 'lon', lon)
	    call writenc(ncid, 'lat', lat)
	    
	    call close_nc(ncid)
	
	  else
	
	    call open_nc_self(ncid, filename, 'w')
	    
	    timerec = get_dimension_len(ncid, 'time')
	    allocate(timealready(timerec))
	    call readnc(ncid, 'time', timealready)
	    
	    outrecord = 1
	    do ittt = 1, timerec !-1
	    	if(dtime-jd > timealready(ittt))then
	    	  outrecord = ittt + 1
	    	endif
	    enddo
	    
	    deallocate(timealready)
	    
	    call close_nc(ncid)
	
	  endif
	  
	endif ! if(myid == 0)then

  call bcast_to_all(outrecord)

!-------------------------------------------------------------------------------

  !write(6, *)'eng '
  call open_nc(ncid, filename, 'w')

  call writenc(ncid, 'time', dtime-jd, outrecord)

  call setland_v2(pein)
  call writenc(ncid, 'pein', v2(is:ie, js:je), locs=loc2, recnum=outrecord)
  if(ixoverlay /= 0 )then
	if(loc2(1) == 1)then
	  call writenc(ncid, 'pein', v2(is:is+ixoverlay, js:je), &
	               locs = [im-ixoverlay, loc2(2)], recnum=outrecord)
	else
	  call writenc(ncid, 'pein', v2(is:0, js:0), locs=[1,1])
	endif
  endif

  call setland_v2(pebo)
  call writenc(ncid, 'pebo', v2(is:ie, js:je), locs=loc2, recnum=outrecord)
  if(ixoverlay /= 0 )then
	if(loc2(1) == 1)then
	  call writenc(ncid, 'pebo', v2(is:is+ixoverlay, js:je), &
	               locs = [im-ixoverlay, loc2(2)], recnum=outrecord)
	else
	  call writenc(ncid, 'pebo', v2(is:0, js:0), locs=[1,1])
	endif
  endif

  call setland_v2(peds)
  call writenc(ncid, 'peds', v2(is:ie, js:je), locs=loc2, recnum=outrecord)
  if(ixoverlay /= 0 )then
	if(loc2(1) == 1)then
	  call writenc(ncid, 'peds', v2(is:is+ixoverlay, js:je), &
	               locs = [im-ixoverlay, loc2(2)], recnum=outrecord)
	else
	  call writenc(ncid, 'peds', v2(is:0, js:0), locs=[1,1])
	endif
  endif

  call close_nc(ncid)



  write(6, *)'eng over'

!-------------------------------------------------------------------------------

  return

!-------------------------------------------------------------------------------

  end subroutine outeng_t

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
!*DeckYinxq: outwav_t

  subroutine outwav_t(filename, rec)

  implicit none

  character(len=*), intent(in) :: filename
  integer, intent(in) :: rec

!-------------------------------------------------------------------------------

  integer :: ncid, jd
  integer*2, parameter :: ivland = nf_fill_int2

  logical :: ext
  integer :: outrecord, timerec, ittt
  double precision, allocatable :: timealready(:)
  
!-------------------------------------------------------------------------------

  jd = datenum(1950, 1, 1, 0, 0, 0)

!-------------------------------------------------------------------------------


  if(myid == 0)then
	  inquire(file=filename, exist=ext)
	!  if(rec == 1)then
	  if(.not.ext)then
	    outrecord = 1
	    call open_nc_self(ncid, filename, 'c')
	    call dimension_define(ncid, 'lon', im, 'lon', nf_real)
	    call dimension_define(ncid, 'lat', jm, 'lat', nf_real)
	    call dimension_define(ncid, 'time', 0, 'time', nf_double)
	    call set_attribute(ncid, 'units', 'degrees_north', 'lat')
	    call set_attribute(ncid, 'units', 'degrees_east', 'lon')
	    call set_attribute(ncid, 'modulo', '', 'lon')
	    call set_attribute(ncid, 'units', 'Days since 1950-01-01 00:00:0.0.', 'time')
	    call set_attribute(ncid, 'Start_time', ctime)
	    call variable_define(ncid, 'windx', nf_int2, ['lon', 'lat', 'time'])
	    call variable_define(ncid, 'windy', nf_int2, ['lon', 'lat', 'time'])
	    call variable_define(ncid, 'hs',    nf_int2, ['lon', 'lat', 'time'])
	    call variable_define(ncid, 'tp',    nf_int2, ['lon', 'lat', 'time'])
	    call variable_define(ncid, 'tz',    nf_int2, ['lon', 'lat', 'time'])
	    call variable_define(ncid, 'th',    nf_int2, ['lon', 'lat', 'time'])
	    call set_attribute(ncid, 'missing_value', ivland, 'windx')
	    call set_attribute(ncid, 'missing_value', ivland, 'windy')
	    call set_attribute(ncid, 'missing_value', ivland, 'hs')
	    call set_attribute(ncid, 'missing_value', ivland, 'tp')
	    call set_attribute(ncid, 'missing_value', ivland, 'tz')
	    call set_attribute(ncid, 'missing_value', ivland, 'th')
	    call set_attribute(ncid, 'scale_factor', 0.01, 'windx')
	    call set_attribute(ncid, 'scale_factor', 0.01, 'windy')
	    call set_attribute(ncid, 'scale_factor', 0.01, 'hs')
	    call set_attribute(ncid, 'scale_factor', 0.01, 'tp')
	    call set_attribute(ncid, 'scale_factor', 0.01, 'tz')
	    call set_attribute(ncid, 'scale_factor', 0.1, 'th')
	    call set_attribute(ncid, 'units', 'm/s', 'windx')
	    call set_attribute(ncid, 'units', 'm/s', 'windy')
	    call set_attribute(ncid, 'units', 'm'  , 'hs')
	    call set_attribute(ncid, 'units', 's'  , 'tp')
	    call set_attribute(ncid, 'units', 's'  , 'tz')
	    call set_attribute(ncid, 'units', 'deg', 'th')
	    call set_attribute(ncid, 'longname', 'Zonal Wind Velocity '     , 'windx')
	    call set_attribute(ncid, 'longname', 'Meridional Wind Velocity ', 'windy')
	    call set_attribute(ncid, 'longname', 'Significant wave height'  , 'hs')
	    call set_attribute(ncid, 'longname', 'Mean wave direction'      , 'th')
	    call set_attribute(ncid, 'longname', 'Spectrum peak wave period', 'tp')
	    call set_attribute(ncid, 'longname', 'Zero-crossing wave period', 'tz')
	    call end_define(ncid)
	    call writenc(ncid, 'lon', lon)
	    call writenc(ncid, 'lat', lat)
	    call close_nc(ncid)
	  else
	    call open_nc_self(ncid, filename, 'w')
	    timerec = get_dimension_len(ncid, 'time')
	    allocate(timealready(timerec))
	    call readnc(ncid, 'time', timealready)
	    outrecord = 1
	    do ittt = 1, timerec !-1
	    	if(dtime-jd > timealready(ittt))then 
	    	  outrecord = ittt + 1
	    	endif
	    enddo
	    deallocate(timealready)
	    call close_nc(ncid)
	  endif
	endif ! if(myid == 0)then

  call bcast_to_all(outrecord)

  if(myid==0)write(6, *)'outrecord = ', outrecord

  call open_nc(ncid, filename, 'w')
  call writenc(ncid, 'time', dtime-jd, outrecord)

  call setland_iv2(wx, 0.01)
  call writenc(ncid, 'windx', iv2(is:ie, js:je), locs=loc2, recnum=outrecord)
  if(ixoverlay /= 0 )then
	if(loc2(1) == 1)then
	  call writenc(ncid, 'windx', iv2(is:is+ixoverlay, js:je), &
	               locs = [im-ixoverlay, loc2(2)], recnum=outrecord)
	else
	  call writenc(ncid, 'windx', iv2(is:0, js:0), locs=[1,1])
	endif
  endif

  call setland_iv2(wy, 0.01)
  call writenc(ncid, 'windy', iv2(is:ie, js:je), locs=loc2, recnum=outrecord)
  if(ixoverlay /= 0 )then
	if(loc2(1) == 1)then
	  call writenc(ncid, 'windy', iv2(is:is+ixoverlay, js:je), &
	               locs = [im-ixoverlay, loc2(2)], recnum=outrecord)
	else
	  call writenc(ncid, 'windy', iv2(is:0, js:0), locs=[1,1])
	endif
  endif

  call setland_iv2(h1_3, 0.01)

  if(ixoverlay /= 0 )then
	if(loc2(1) == 1)then
	 ! write(*,*)"myid,loc2(2):",myid,loc2(2)
	  call writenc(ncid, 'hs', iv2(is:is+ixoverlay, js:je), &
	               locs = [im-ixoverlay, loc2(2)], recnum=outrecord)
	else
	  call writenc(ncid, 'hs', iv2(is:0, js:0), locs=[1,1])
	endif
  endif
  call writenc(ncid, 'hs', iv2(is:ie, js:je), locs=loc2, recnum=outrecord)

  call setland_iv2(tpf, 0.01)
  call writenc(ncid, 'tp', iv2(is:ie, js:je), locs=loc2, recnum=outrecord)
  if(ixoverlay /= 0)then
	if(loc2(1) == 1)then
	  call writenc(ncid, 'tp', iv2(is:is+ixoverlay, js:je), &
	               locs = [im-ixoverlay, loc2(2)], recnum=outrecord)
	else
	  call writenc(ncid, 'tp', iv2(is:0, js:0), locs=[1,1])
	endif
  endif

  call setland_iv2(ape, 0.01)
  call writenc(ncid, 'tz', iv2(is:ie, js:je), locs=loc2, recnum=outrecord)
  if(ixoverlay /= 0 )then
	if( loc2(1) == 1)then
	  call writenc(ncid, 'tz', iv2(is:is+ixoverlay, js:je), &
	               locs = [im-ixoverlay, loc2(2)], recnum=outrecord)
	else
	  call writenc(ncid, 'tz', iv2(is:0, js:0), locs=[1,1])
	endif
  endif
  
  call setland_iv2(aet, 0.1)
  call writenc(ncid, 'th', iv2(is:ie, js:je), locs=loc2, recnum=outrecord)
  if(ixoverlay /= 0 )then
	if(loc2(1) == 1)then
	  call writenc(ncid, 'th', iv2(is:is+ixoverlay, js:je), &
	               locs = [im-ixoverlay, loc2(2)], recnum=outrecord)
	else
	  call writenc(ncid, 'th', iv2(is:0, js:0), locs=[1,1])
	endif
  endif

  call close_nc(ncid)



!-------------------------------------------------------------------------------

  return

!-------------------------------------------------------------------------------

  end subroutine outwav_t

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
!*DeckYinxq: outrst

  subroutine outrst(filename)

  implicit none

  character(len=*), intent(in) :: filename

  integer :: ncid

!-------------------------------------------------------------------------------
! Modified by Zhenya.Song, 2016/04/03
!  if(mod(it-number, irstfreq) /= 0)return
  if(irstfreq == 0) then
    return
  else
    if(mod(it-number, irstfreq) /= 0)return
  endif

!-------------------------------------------------------------------------------

 
  if(myid == 0)then
	  call open_nc_self(ncid, filename, 'c')
	  call dimension_define(ncid, 'kk', kl, 'kk', nf_real)
	  call dimension_define(ncid, 'jj', jl, 'jj', nf_real)
	  call dimension_define(ncid, 'lon', im, 'lon', nf_real)
	  call dimension_define(ncid, 'lat', jm, 'lat', nf_real)
	  call variable_define(ncid, 'ee', nf_real, ['kk', 'jj', 'lon', 'lat'])
	  call set_attribute(ncid, 'units', 'degrees_north', 'lat')
	  call set_attribute(ncid, 'units', 'degrees_east', 'lon')
	  call set_attribute(ncid, 'modulo', '', 'lon')
	  call set_attribute(ncid, 'ctime', ctime)
	  call end_define(ncid)
 	  !  call writenc(ncid, 'kk', x) ! wk(1:kl)
	  !  call writenc(ncid, 'jj', x) ! 1:13
    	  call writenc(ncid, 'lon', lon)
    	  call writenc(ncid, 'lat', lat)
	  call close_nc(ncid)
  endif
  
  call mpi_barrier(mpi_comm_ympi,ierr)
  call open_nc(ncid, filename, 'w')
  call writenc(ncid, 'ee', ee(:, :, is:ie, js:je), locs=loc4)
  if(ixoverlay /= 0 )then
	if(loc4(3) == 1 ) then
	  call writenc(ncid, 'ee', ee(:, :, is:is+ixoverlay, js:je), &
	               locs = [loc4(1), loc4(2), im-ixoverlay, loc4(4)])
        else
	  call writenc(ncid, 'ee', ee(:0, :0, is:0, js:0), locs=[1,1,1,1])
        endif
  endif

  call close_nc(ncid)
  
!-------------------------------------------------------------------------------

  return

!-------------------------------------------------------------------------------

  end subroutine outrst
  

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
!*DeckYinxq: inprst

  subroutine inprst(filename, key)

  implicit none

  integer, intent(out) :: key
  character(len=*), intent(in) :: filename

  integer :: ncid
  logical :: ext

!-------------------------------------------------------------------------------

  inquire(file=filename, exist=ext)
  if(ext)then
    key = 1
  else
    key = 0
    return 
  endif

!-------------------------------------------------------------------------------

  call open_nc(ncid, filename, 'r')
  call readnc(ncid, 'ee', ee(:, :, is:ie, js:je), locs=loc4)
  call get_attribute(ncid, 'ctime', ctime)
  call close_nc(ncid)

  write(6, *)'Restart time is :', ctime
  read(ctime, '(i4.4,5i2.2)')istime
  istime(5:6) = 0

 !call mpi_barrier(mpi_comm_ympi,ierr)
  call updatev(ee, ixl, iyl, kl, jl, halosize)

!-------------------------------------------------------------------------------

  return

!-------------------------------------------------------------------------------

  end subroutine inprst

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
!*DeckYinxq: setland_iv2

  subroutine setland_iv2(var, scal)

  implicit none

  real, intent(in) :: var(:, :), scal
  integer :: i, j

  if(.not. allocated(iv2))allocate(iv2(ixl, iyl))

  do j = iys, iyl
  do i = ixs, ixl
!    if(nsp(i, j) == 0)then
    if(nsp(i, j) /= 1)then
      iv2(i, j) = nf_fill_int2
    else
      iv2(i, j) = var(i, j) / scal
    endif
  enddo
  enddo

  return

  end subroutine setland_iv2

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
!*DeckYinxq: setland_v2

  subroutine setland_v2(var)

  implicit none

  real, intent(in) :: var(:, :)
  integer :: i, j

  if(.not. allocated(v2))allocate(v2(ixl, iyl))

  do j = iys, iyl
  do i = ixs, ixl
    if(nsp(i, j) == 0)then
      v2(i, j) = nf_fill_real
    else
      v2(i, j) = var(i, j)
    endif
  enddo
  enddo

  return

  end subroutine setland_v2

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
!*DeckYinxq: setland_v3

  subroutine setland_v3(var)

  implicit none

  real, intent(in) :: var(:, :, :)

  integer :: i, j

  if(.not. allocated(v3))allocate(v3(ixl, iyl, kb))
  
  do j = iys, iyl
  do i = ixs, ixl
    if(nsp(i, j) == 0)then
      v3(i, j, :) = nf_fill_real
    else
      v3(i, j, :) = var(i, j, :)
    endif
  enddo
  enddo

  return

  end subroutine setland_v3

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------

  end module wammpi_mod

!-------------------------------------------------------------------------------
!###############################################################################
