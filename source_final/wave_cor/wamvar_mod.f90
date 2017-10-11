!###############################################################################
!-------------------------------------------------------------------------------

  module wamvar_mod

!-------------------------------------------------------------------------------

  implicit none

!-------------------------------------------------------------------------------

  public wamvar_mod_init

  public

!-------------------------------------------------------------------------------
  integer :: myid
  integer :: npe
  integer :: mpi_comm_ympi


  integer :: ixs
  integer :: ixl
  integer :: ix1
  integer :: ix2

  integer :: iys
  integer :: iyl
  integer :: iy1
  integer :: iy2

  integer :: kb

!-------------------------------------------------------------------------------

  integer, parameter :: kl    = 25
  integer, parameter :: kld   = 30
  integer, parameter :: klp1  = kl + 1
  integer, parameter :: kldp1 = kld + 1
  integer, parameter :: jl    = 12 
  integer, parameter :: jlp1  = jl + 1

!-------------------------------------------------------------------------------

  real, parameter :: acu       = 0.            ! --- Theoretical coefficient for wave-current interaction
!  real, parameter :: beta0     = 1.12         ! --- Coefficient for wind input.
  real, parameter :: beta0     = 1.0           ! --- Coefficient for wind input.
  real, parameter :: beta10    = beta0*0.25*1.25*0.001
  real, parameter :: rs        = 6367451.637   ! --- The global (Earth) radius.
  real, parameter :: pi        = 3.1415926     ! --- Pi.
  real, parameter :: pi2       = pi/2.0        ! --- Pi/2.
  real, parameter :: zpi       = 2.0*pi        ! --- 2*Pi.
  real, parameter :: pid180    = pi/180.       ! --- Pi/180.
 
  real, parameter :: p180p     = 180./pi 

  real, parameter :: g         = 9.81          ! --- Acceleration of gravity.
  real, parameter :: gc2       = 0.877**2 * g  ! --- Acceleration of gravity.
  real, parameter :: gg        = g * g         ! --- Acceleration of gravity.
  real, parameter :: tztz      = 1.099314      ! --- Coefficient for zero-crossing wave period.
  real, parameter :: d1        = 0.000132      ! --- Coefficient in wave-breaking dissipation formula
  real, parameter :: d2        = 2.61          ! --- Coefficient in wave-breaking dissipation formula
  real, parameter :: pwk       = 1.21          ! --- The constant for discretion of wave-number
  real, parameter :: alog10pwk = alog10(pwk)   ! --- alog10(pwk)
  real, parameter :: wkmax     = 0.6894        ! --- Maximum of wave-umber amplitude
  real, parameter :: wkmin     = 0.0071        ! --- Minimum of wave-number amplitude
  real, parameter :: wfmax     = 0.413         ! --- Maximum of wave frequency
  real, parameter :: wfmin     = 0.042         ! --- Minimum of wave frequency
  real, parameter :: ads       = 1.0           ! --- Theoretical coefficient for wave-breaking dissipation
  real, parameter :: abo       = 1.0           ! --- Theoretical coefficient for bottom dissipation
  real, parameter :: p         = 0.025         ! --- Coefficient for growth spectrum limiter
  real, parameter :: cksp      = 14.0          ! --- Coefficient for estimation of wave-number range
  real, parameter :: cksa      = 4.5           ! --- Coefficient for estimation of wave-number range
  real, parameter :: small     = 0.000001      ! --- Small value.

  character(len=12), parameter ::  rstfile = 'wave_rest.nc'

!-------------------------------------------------------------------------------

  real, allocatable :: x(:)                    ! --- For longitude: rang from 0~360.
  !dir$ attributes align:64 :: x
  real, allocatable :: rx(:)                   ! --- Real longitude.
  !dir$ attributes align:64 :: rx
  real, allocatable :: y(:)                    ! --- For latitude
  !dir$ attributes align:64 :: y
  real, allocatable :: rslat(:)                ! --- For rs * cos(lat)
  !dir$ attributes align:64 :: rslat
  real, allocatable :: d(:, :)                 ! --- For topography: the bottom depth (m) for topography
  !dir$ attributes align:64 :: d
  real, allocatable :: nsp(:, :)               ! --- Computing mask for scalar variables: 0 for land, 1 for water and 2 for open boundary.
  !dir$ attributes align:64 :: nsp             
  real, allocatable :: deltx(:), delty(:)      ! --- For grid interval.
  !dir$ attributes align:64 :: deltx
  !dir$ attributes align:64 :: delty 
  real, allocatable :: dddx(:, :), dddy(:, :)  ! --- For d(d)/d(x) and d(d) / d(y)
  !dir$ attributes align:64 :: dddx
  !dir$ attributes align:64 :: dddy
  real, allocatable :: e(:, :, :, :)           ! --- Wave spectrum, before the influence of source function & propagation.
  !dir$ attributes align:64 :: e
  real, allocatable :: ee(:, :, :, :)          ! --- Wave spectrum, after the influence of source function & propagation.
  !dir$ attributes align:64 :: ee
  real, allocatable :: ea(:, :, :, :)          ! --- Time averaged ee.
  !dir$ attributes align:64 :: ea  
  real, allocatable :: sein(:, :)              ! --- (kl, jl)
  !dir$ attributes align:64 :: sein  
  real, allocatable :: seds(:, :)              ! --- (kl, jl)
  !dir$ attributes align:64 :: seds  
  real, allocatable :: sebo(:, :)              ! --- (kl, jl)
  !dir$ attributes align:64 :: sebo




  real, pointer :: pein(:, :)                  ! --- (ixl, iyl)
  !dir$ attributes align:64 :: pein
  real, pointer :: peds(:, :)                  ! --- (ixl, iyl)
  !dir$ attributes align:64 :: peds
  real, pointer :: pebo(:, :)                  ! --- (ixl, iyl)
  !real, allocatable :: grids(:, :)             ! --- (ixl, iyl) 2013/4/11 18:07:13

!-------------------------------------------------------------------------------

  real, pointer :: wx(:, :)      ! --- The wind along longitude
  !dir$ attributes align:64 :: wx
  real, pointer :: wy(:, :)      ! --- The wind along latitude
  !dir$ attributes align:64 :: wy
  real, allocatable :: w(:, :)       ! --- The wind speed.
  !dir$ attributes align:64 :: w  
!-------------------------------------------------------------------------------

  real, allocatable :: grolim(:)               ! --- Growth spectrum limiter
  !dir$ attributes align:64 :: grolim
  real, allocatable :: thet(:)                 ! --- Directional discrete for 12 bands (300 resolution)
  !dir$ attributes align:64 :: thet
  real, allocatable :: wk(:)                   ! --- Wave-number discrete for 25 bands on a logarithmic scale
  !dir$ attributes align:64 :: wk
  real, allocatable :: wkh(:)                  ! --- coefficient for computing high-frenquency spectrum
  !dir$ attributes align:64 :: wkh
  real, allocatable :: dwk(:)                  ! --- Discrete bands of wave-number
  !dir$ attributes align:64 :: dwk                                             
  real, allocatable :: wp(:, :, :)             ! --- weighting factor of K+ for decomposing wave-wave transfer
  !dir$ attributes align:64 :: wp
  real, allocatable :: wm(:, :, :)             ! --- weighting factor of K- for decomposing wave-wave transfer
  !dir$ attributes align:64 :: wm
  real, allocatable :: wks17(:)                ! --- 17/2 exponential of wave-number, that is K**(17/2)
  !dir$ attributes align:64 :: wks17
  real, allocatable :: se(:, :)                ! --- Increment of spectrum (m4/s)
  !dir$ attributes align:64 :: se
  real, allocatable :: dse(:, :)               ! --- Deferential of se with respect to spectrum (s-1)
  !dir$ attributes align:64 :: dse
  real, allocatable :: enh(:, :)               ! --- shallow water factor for nonlinear wave-wave transfer
  !dir$ attributes align:64 :: enh
!-------------------------------------------------------------------------------

  real, allocatable :: ae(:, :)                ! --- The zero-order moment of the spectrum
  !dir$ attributes align:64 :: ae
  real, allocatable :: awf(:, :)               ! --- The frequency first-order moment of the spectrum
  !dir$ attributes align:64 :: awf
  real, allocatable :: asi(:, :)               ! --- The frequency negative first-order moment of the spectrum
  !dir$ attributes align:64 :: asi
  real, allocatable :: awk(:, :)               ! --- The frequency negative second-order moment of the spectrum
  !dir$ attributes align:64 :: awk
  real, allocatable :: ark(:, :)               ! --- spectral mean of wave-number
  !dir$ attributes align:64 :: ark
  real, allocatable :: hb(:,:)                 ! --- The Hs limiter through wave breaking
  !dir$ attributes align:64 :: hb
  real, allocatable :: hbb(:, :)               ! --- The Hs limiter through wave breaking
  !dir$ attributes align:64 :: hbb
  real, allocatable :: fconst0(:, :, :)        ! --- computing mask for threshold of wave-number 
  !dir$ attributes align:64 :: fconst0
  real, allocatable :: wf(:, :, :)             ! --- wave frequency
  !dir$ attributes align:64 :: wf
  real, allocatable :: ccg(:, :, :)            ! --- group velocity
  !dir$ attributes align:64 :: ccg
  real, allocatable :: dwf(:, :, :)            ! --- discrete bands of wave frequency multiplied by half of directional discrete
  !dir$ attributes align:64 :: dwf            
  integer, allocatable :: ks0(:, :)            ! --- threshold of wave-number determined by wind speed or spectral mean of wave-number
  !dir$ attributes align:64 :: ks0
  integer, allocatable :: kpmt0(:, :)          ! --- threshold of wave-number determined by wind speed 
  !dir$ attributes align:64 :: kpmt0
  integer, allocatable :: kakt0(:, :)          ! --- threshold of wave-number determined by spectral mean of wave-number
  !dir$ attributes align:64 :: kakt0         
  integer, allocatable :: jp1(:, :)            ! --- Discrete-interaction configurations of  nonlinear wave-wave transfer
  !dir$ attributes align:64 :: jp1
  integer, allocatable :: jp2(:, :)            ! --- Discrete-interaction configurations of  nonlinear wave-wave transfer
  !dir$ attributes align:64 :: jp2
  integer, allocatable :: jm1(:, :)            ! --- Discrete-interaction configurations of  nonlinear wave-wave transfer
  !dir$ attributes align:64 :: jm1
  integer, allocatable :: jm2(:, :)            ! --- Discrete-interaction configurations of  nonlinear wave-wave transfer
  !dir$ attributes align:64 :: jm2
                                               ! --- angular discrete-interaction configurations of nonlinear wave-wave transfer                                               
  integer, allocatable :: ikp(:)               ! --- wave-number discrete-interaction configurations of nonlinear wave-wave transfer 
  !dir$ attributes align:64 :: ikp
  integer, allocatable :: ikp1(:)              ! --- wave-number discrete-interaction configurations of nonlinear wave-wave transfer 
  !dir$ attributes align:64 :: ikp1
  integer, allocatable :: ikm(:)               ! --- wave-number discrete-interaction configurations of nonlinear wave-wave transfer 
  !dir$ attributes align:64 :: ikm
  integer, allocatable :: ikm1(:)              ! --- wave-number discrete-interaction configurations of nonlinear wave-wave transfer 
  !dir$ attributes align:64 :: ikm1

!-------------------------------------------------------------------------------

  real, pointer :: h1_3(:,:)     ! --- hs: significant wave height (m)
  !dir$ attributes align:64 :: h1_3
  real, pointer :: aet(:,:)      ! --- th: mean wave direction (Deg)
  !dir$ attributes align:64 :: aet
  real, pointer :: tpf(:,:)      ! --- tp: spectrum peak wave period (s)
  !dir$ attributes align:64 :: tpf
  real, pointer :: ape(:,:)      ! --- tz: zero-crossing wave period (s)
  !dir$ attributes align:64 :: ape



!-------------------------------------------------------------------------------

! --- Input parameters. ( in the file of ctlparams)

  character(len=100) :: wind_path  ! --- Path for wind.
  character(len=100) :: data_path  ! --- Path for model setting.
  character(len=14 ) :: title      ! --- Symbal for model output.

!-------------------------------------------------------------------------------

  real    :: deltt_p180p 

  real    :: delttm       ! --- Length of integral time step, in minutes.
  real    :: lonref       ! --- The real longitude for lon=0 used in model.
  integer :: istime(6)    ! --- Integral start time
  integer :: ietime(6)    ! --- Integral end time
  integer :: cools_days   ! --- The time (days) for cool start.
  integer :: wndfreq      ! --- The frequency of wind data, in hours.
  integer :: wndtype      ! --- Wind type: 0 for same grid with model
                          !                1 for GFS wind data, no interp.
  integer :: outflag      ! --- The method of output. 
                          !     0, only wave output; 
                          !     1 wave & mix together; 
                          !     2 for wave & mix seperately.
                          !     3 Only output wave variables into file 
                          !       multi-records, everyday 1 file.
                          !     4 Only output wave variables into file 
                          !       multi-records, one run 1 file
  integer :: wiofreq      ! --- The output frequency for wave results (hour).
  integer :: ciofreq      ! --- The output frequency for current coef.s (hour).
  integer :: rstfreq      ! --- The output frequency for model restart (hour).

!-------------------------------------------------------------------------------

  integer :: glbflag      ! --- global flag, 0 for global model, 1 for regional.
  integer :: mixflag      ! --- Flag for mixing method, 0 for none, 
                          !                             1 for unlimited, 
                          !                             2 for limited depth.
                          !                             3 for both methods.

  integer :: flageng, flagmpi = -1
                            
!-------------------------------------------------------------------------------

! --- For variables about time.

  integer :: itime(6)
  double precision :: dtime, dtime0, dtimeend
  double precision :: nxttim                      !jiangxj 2012.6.1
  character(len=14) :: ctime, cistime, cietime

! --- ia for x-direction, ic for y-direction, it for time steps, itend for
!     number of all steps, iprint for print interval.

  integer :: iwiofreq, iciofreq, irstfreq
  integer :: it, itend, ia, ic, j, k, key, number

  real :: cong, al11, al21, al31, al12, al22, al13, al23
  real :: deltt, deltt5, deltth

!-------------------------------------------------------------------------------

  namelist/ctlparams/data_path, wind_path, title, cistime, cietime, cools_days, &
                     delttm, wndfreq, wndtype, outflag, wiofreq, ciofreq, rstfreq

  namelist/ctlparams1/title, istime, ietime, cools_days, delttm, lonref,       &
                      glbflag, wndfreq, wndtype, outflag, wiofreq, iwiofreq,   &
                      ciofreq, iciofreq, rstfreq, irstfreq, ixs, ixl, iys, iyl
                       
!-------------------------------------------------------------------------------

  contains
  
!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
!*DeckYinxq: wamvar_mod_init

  subroutine wamvar_mod_init

!-------------------------------------------------------------------------------
  
  implicit none

!-------------------------------------------------------------------------------

  ixs=1
  ix1=ixs+1
  ix2=ixl-1

  iys=1
  iy1=iys+1
  iy2=iyl-1

!-------------------------------------------------------------------------------

  allocate(x(ixs:ixl))
  allocate(rx(ixs:ixl))
  allocate(y(iys:iyl))
  allocate(rslat(iys:iyl))
  allocate(d(ixs:ixl, iys:iyl))
  allocate(nsp(ixs:ixl, iys:iyl))

  allocate(deltx(ixs:ixl), delty(iys:iyl))
  allocate(dddx(ixs:ixl, iys:iyl), dddy(ixs:ixl, iys:iyl))

  allocate(e(kl, jl, ixs:ixl, iys:iyl))
  allocate(ee(kl, jl, ixs:ixl, iys:iyl))

  allocate(sein(kl, jl), seds(kl, jl), sebo(kl, jl))
  allocate(pein(ixl, iyl), peds(ixl, iyl), pebo(ixl, iyl)) 
  !2013/4/11 18:05:44, grids(ixl, iyl))

!-------------------------------------------------------------------------------

! --- For wind data.

  allocate(wx(ixs:ixl, iys:iyl))
  allocate(wy(ixs:ixl, iys:iyl))
  allocate(w(ixs:ixl, iys:iyl))

!-------------------------------------------------------------------------------

  allocate(grolim(kl)  )
  allocate(thet(jlp1)  )
  allocate(wk(kldp1)   )
!  allocate(wkh(kld)    ) ! yinxq
!  allocate(dwk(kld)    ) ! yinxq
  allocate(wkh(kldp1)    ) ! yinxq
  allocate(dwk(kldp1)    ) ! yinxq
  allocate(wp(kl, 2, 2)  )
  allocate(wm(kl, 2, 2)  )
  allocate(wks17(kl)   )
  allocate(se(klp1, jl) )
  allocate(dse(klp1, jl))
  allocate(enh(ixs:ixl, iys:iyl))

!-------------------------------------------------------------------------------

  allocate(ae(ixs:ixl, iys:iyl)        )
  allocate(awf(ixs:ixl, iys:iyl)       )
  allocate(asi(ixs:ixl, iys:iyl)       )
  allocate(awk(ixs:ixl, iys:iyl)       )
  allocate(ark(ixs:ixl, iys:iyl)       )
  allocate(hb(ixs:ixl, iys:iyl)        )
  allocate(hbb(ixs:ixl, iys:iyl)       )
  allocate(fconst0(kl,ixs:ixl, iys:iyl))
  allocate(wf(kldp1,ixs:ixl, iys:iyl) )
  allocate(ccg(kldp1,ixs:ixl, iys:iyl))
  allocate(dwf(kldp1,ixs:ixl, iys:iyl))

  allocate(ks0(ixs:ixl, iys:iyl))
  allocate(kpmt0(ixs:ixl, iys:iyl))
  allocate(kakt0(ixs:ixl, iys:iyl))
  allocate(jp1(2,jl)     )
  allocate(jp2(2,jl)     )
  allocate(jm1(2,jl)     )
  allocate(jm2(2,jl)     )
  allocate(ikp(kl)       )
  allocate(ikp1(kl)      )
  allocate(ikm(kl)       )
  allocate(ikm1(kl)      )

!-------------------------------------------------------------------------------

! --- hs: significant wave height (m)
  allocate(h1_3(ixs:ixl, iys:iyl))
  allocate(aet(ixs:ixl, iys:iyl) )
  allocate(tpf(ixs:ixl, iys:iyl) )
  allocate(ape(ixs:ixl, iys:iyl) )

!-------------------------------------------------------------------------------

  e = 0.0
  ee = 0.0

  h1_3 = 0.0
  aet = 0.0
  tpf = 0.0
  ape = 0.0

  pein = 0.0
  pebo = 0.0
  peds = 0.0

!-------------------------------------------------------------------------------

  return

!-------------------------------------------------------------------------------
  
  end subroutine wamvar_mod_init

!-------------------------------------------------------------------------------

  end module wamvar_mod

!-------------------------------------------------------------------------------
!###############################################################################
