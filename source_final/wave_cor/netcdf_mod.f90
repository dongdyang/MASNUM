!###############################################################################
!-------------------------------------------------------------------------------
!*Deckyinxq: netcdf_mod 2.1

  module netcdf_mod

!-------------------------------------------------------------------------------
! ******************************************************************************
!-------------------------------------------------------------------------------
!                                                Copyright (C) 2005 Xunqiang Yin
!                                                MODULE NAME : netcdf_mod
!                                                History : netcdf_mod 2.1
!                                                History : netcdf_mod 2.0
!                                                History : NcFileData 2.0
!                                                Current VERSION : 2008/03/31
!
! --- USAGE : To output/input nc files in convenience.
! --- DEPEND: The package of netcdf for FORTRAN.
!
! --- NOTE for describing of subroutine / function :
!  A. The parameters bracketed with [], means optional parameter.
!  B. The describe for the parameters of subroutine / function, started with:
!   * It means input prameter;
!   # It means output prameter;
!   @ It means input and output prameter(it will be changed inside).
!
!-------------------------------------------------------------------------------
! ******************************************************************************
! ***                       INTERFACE DESCRIBE                               ***
! ******************************************************************************
!-------------------------------------------------------------------------------
!
!  1. subroutine open_nc(NcID, FileName, action)
!     # integer :: NcID = unit number of opened netcdf file.
!     * character(len=*) :: FileName = the name of netcdf file.
!     * character :: action = 'create', 'define', 'write' or 'read'.
!
!-------------------------------------------------------------------------------
!
!  2. subroutine close_nc(NcID)
!     * integer :: NcID = unit number of opened netcdf file.
!
!-------------------------------------------------------------------------------
!
!  3. subroutine end_define(NcID)
!     * integer :: NcID = unit number of opened netcdf file.
!
!-------------------------------------------------------------------------------
!
!  4. subroutine dimension_define
!     - dimension_define(NcID, DimName, DimLen, DimVarName, DimVarType [, &
!                             DimID, DimVarID])
!     * integer :: NcID = unit number of opened netcdf file.
!     * character :: Dimname = the name of this dimension.
!     * integer :: DimLen = the size of this dimension.
!     * character :: DimVarName = the name of dimension variable name.
!     * integer :: DimVarType = the type of dimension variable (use integer).
!          ( nf_int1 = 1, nf_char = 2, nf_int2 = 3, nf_int = 4,
!            nf_real = 5, nf_double = 6 )
!     # integer :: DimID = the record number of this dimension.
!     # integer :: DimVarID = the record number of dimension variable.
!     ( Note: for the unlimited dimension, 'DimLen = 0 '.)
!
!-------------------------------------------------------------------------------
!
!  5. function get_dimension_len(NcID, DimName)
!     * integer :: NcID = unit number of opened netcdf file.
!     * character(len=*) :: DimName = the name of dimension need to get length.
!     # integer :: get_dimension_len = the needed dimension length.
!
!-------------------------------------------------------------------------------
!
!  6. subroutine variable_define
!
!     --- 2 interfaces:
!
!    6.1 variable_define(NcID, VarName, VarType, VarDimsName [, VarID])
!     * integer :: NcID = unit number of opened netcdf file.
!     * character :: VarName = the name of this variable.
!     * integer :: VarType = the type of variable (use integer).
!          ( nf_int1 = 1, nf_char = 2, nf_int2 = 3, nf_int = 4,
!            nf_real = 5, nf_double = 6 )
!     * character(len=*) :: VarDimsName = the name list to which this variable
!                           related.
!     # integer :: VarID = the record number of this variable.
!
!    6.2 variable_define(NcID, VarName, VarType, VarDims [, VarID])
!     * integer :: NcID = unit number of opened netcdf file.
!     * character :: VarName = the name of this variable.
!     * integer :: VarType = the type of variable (use integer).
!          ( nf_int1 = 1, nf_char = 2, nf_int2 = 3, nf_int = 4,
!            nf_real = 5, nf_double = 6 )
!     * integer :: VarDims = the related DimID of this variable.
!     # integer :: VarID = the record number of this variable.
!
!-------------------------------------------------------------------------------
!
!  7. subroutine set_attribute(NcID, AttName, Att [, VarName])
!     * integer :: NcID = unit number of opened netcdf file.
!     * character(len=*) :: AttName = the attribute name of netcdf file.
!     * character(len=*)/integer/integer*1/integer*2/real/double presion ::
!                           Att = attribute of opened netcdf file.
!     * character(len=*) :: VarName = the owner(variable) of attribute.
!     ( Note: for the globle attribute, VarName shouldn't be given.)
!
!-------------------------------------------------------------------------------
!
!  8. subroutine get_attribute(NcID, AttName, Att [, VarName])
!     * integer :: NcID = unit number of opened netcdf file.
!     * character(len=*) :: AttName = the attribute name of netcdf file.
!     # character(len=*)/integer/integer*1/integer*2/real/double presion ::
!                           Att = attribute of opened netcdf file.
!     * character(len=*) :: VarName = the owner(variable) of attribute.
!     ( Note: for the globle attribute, VarName shouldn't be given.)
!
!-------------------------------------------------------------------------------
!
!  9. subroutine writenc(NcID, VarName, Var [, RecNum, locs])
!     * integer :: NcID = unit number of opened netcdf file.
!     * character(len=*) :: VarName = the name of variable in netcdf file.
!     * character(len=*)/char/integer/integer*1/integer*2/real/double presion ::
!                          Var = the variable needed to be output.
!     * integer :: RecNum = the record number at unlimited dimension.
!     * integer :: locs = the start locations for output.
!     ( Note: for the variable without unlimited dimension,
!             RecNum shouldn't be given.)
!
!-------------------------------------------------------------------------------
!
!  10. subroutine readnc(NcID, VarName, Var [, RecNum, locs])
!     * integer :: NcID = unit number of opened netcdf file.
!     * character(len=*) :: VarName = the name of variable in netcdf file.
!     # character(len=*)/char/integer/integer*1/integer*2/real/double presion ::
!                         Var = the variable needed to be input.
!     * integer :: RecNum = the record number at unlimited dimension.
!     * integer :: locs = the start locations for output.
!     ( Note: for the variable without unlimited dimension,
!             RecNum shouldn't be given.)
!
!-------------------------------------------------------------------------------
!
!                                                --- Xunqiang Yin, 2007/04/01
!                                                 E-Mail: XunqiangYin@gmail.com
!
!-------------------------------------------------------------------------------
! ******************************************************************************
!-------------------------------------------------------------------------------

  use wamvar_mod

  implicit none

!-------------------------------------------------------------------------------

  public nf_int1, nf_char, nf_int2, nf_int, nf_real, nf_double
  public nf_fill_byte, nf_fill_int1, nf_fill_char, nf_fill_short, nf_fill_int2
  public nf_fill_int, nf_fill_float, nf_fill_real, nf_fill_double

  public open_nc, close_nc, end_define, readnc, writenc, open_nc_self
  public variable_define, set_attribute, get_attribute
  public dimension_define, get_dimension_len

  private

!-------------------------------------------------------------------------------

  interface variable_define
    module procedure variable_define1, variable_define2
  end interface variable_define

!-------------------------------------------------------------------------------

  interface set_attribute
    module procedure set_attribute_character,               &
                     set_attribute_int1,                    &
                     set_attribute_int2,                    &
                     set_attribute_int ,                    &
                     set_attribute_real,                    &
                     set_attribute_double
  end interface set_attribute

!-------------------------------------------------------------------------------

  interface get_attribute
    module procedure get_attribute_character,               &
                     get_attribute_int1,                    &
                     get_attribute_int2,                    &
                     get_attribute_int,                     &
                     get_attribute_real,                    &
                     get_attribute_double
  end interface get_attribute

!-------------------------------------------------------------------------------

  interface writenc
    module procedure &
    writenc_0d_int1, writenc_0d_int2,   writenc_0d_int,      &
    writenc_0d_real, writenc_0d_double, writenc_0d_text,     &
    writenc_1d_int1, writenc_1d_int2,   writenc_1d_int,      &
    writenc_1d_real, writenc_1d_double, writenc_1d_text,     &
    writenc_2d_int1, writenc_2d_int2,   writenc_2d_int,      &
    writenc_2d_real, writenc_2d_double, writenc_2d_text,     &
    writenc_3d_int1, writenc_3d_int2,   writenc_3d_int,      &
    writenc_3d_real, writenc_3d_double, writenc_3d_text,     &
    writenc_4d_int1, writenc_4d_int2,   writenc_4d_int,      &
    writenc_4d_real, writenc_4d_double, writenc_4d_text
  end interface writenc

!-------------------------------------------------------------------------------

  interface readnc
    module procedure &
    readnc_0d_int1, readnc_0d_int2,   readnc_0d_int,        &
    readnc_0d_real, readnc_0d_double, readnc_0d_text,       &
    readnc_1d_int1, readnc_1d_int2,   readnc_1d_int,        &
    readnc_1d_real, readnc_1d_double, readnc_1d_text,       &
    readnc_2d_int1, readnc_2d_int2,   readnc_2d_int,        &
    readnc_2d_real, readnc_2d_double, readnc_2d_text,       &
    readnc_3d_int1, readnc_3d_int2,   readnc_3d_int,        &
    readnc_3d_real, readnc_3d_double, readnc_3d_text,       &
    readnc_4d_int1, readnc_4d_int2,   readnc_4d_int,        &
    readnc_4d_real, readnc_4d_double, readnc_4d_text
  end interface readnc

!-------------------------------------------------------------------------------
  include 'mpif.h'
  include 'pnetcdf.inc'
  !include 'netcdf.inc'
  integer :: status, VarID
  character(len=80) :: version = 'netcdf_mod 2.1, '         &
                       // 'by Xunqiang Yin, 2008-3-31 17:15.'
  !integer myid,mpi_comm_ympi,ncID
!-------------------------------------------------------------------------------

  contains

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- To handle errors for NCFile input or output.
!*DeckYinxq: check_err

  subroutine check_err(status)
  implicit none
  integer, intent(in) :: status

  if (status .ne. NF_NOERR) then
    write(*, *)nfmpi_strerror(status); stop
  endif

  return
  end subroutine check_err

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- To open a NcFile. (By create, if new; By redefine, if old.)

  subroutine open_nc_self(NcID, FileName, action)
  implicit none
  character(len=*), intent(in) :: FileName, action
  integer, intent(in) :: NcID
  logical alive
  select case(action(1:1))
  case('c', 'C')
    !status = NF_CREATE(trim(FileName), NF_CLOBBER, ncID)
    !!IOR(NF_CLOBBER, NF_64BIT_DATA)
    status = nfmpi_create(MPI_COMM_SELF,trim(FileName),IOR(NF_CLOBBER,NF_64BIT_OFFSET),MPI_INFO_NULL,NcID)
    IF(status.NE.NF_NOERR) CALL check_err(status)
  case('r', 'R')
    inquire(file=trim(FileName), exist = alive)
    if(.not.alive)then
      write(*, *)trim(FileName), ' is not exist!'; stop
    endif
    status = nfmpi_open(MPI_COMM_SELF, trim(FileName), NF_NOWRITE, MPI_INFO_NULL, NcID)
    IF(status.NE.NF_NOERR) CALL check_err(status)
  case('w', 'W')
    inquire(file=trim(FileName), exist = alive)
    if(.not.alive)then
      write(*, *)trim(FileName), ' is not exist!'; stop
    endif
    status = nfmpi_open(MPI_COMM_SELF, trim(FileName), NF_WRITE, MPI_INFO_NULL, NcID)
    IF(status.NE.NF_NOERR) CALL check_err(status)


  end select

  return
  end subroutine open_nc_self



  subroutine open_nc(NcID, FileName, action)
  implicit none
  character(len=*), intent(in) :: FileName, action
  integer, intent(in) :: NcID
  logical alive

  select case(action(1:1))
  case('c', 'C')
    !status = NF_CREATE(trim(FileName), NF_CLOBBER, ncID)
    !IOR(NF_CLOBBER, NF_64BIT_DATA)
    status = nfmpi_create(mpi_comm_ympi,trim(FileName),IOR(NF_CLOBBER,NF_64BIT_OFFSET),MPI_INFO_NULL,NcID)
    IF(status.NE.NF_NOERR) CALL check_err(status)
  case('d', 'D')
    inquire(file=trim(FileName), exist = alive)
    if(.not.alive)then
      write(*, *)trim(FileName), ' is not exist!'; stop
    endif
    status = nfmpi_open(mpi_comm_ympi,trim(FileName), NF_WRITE, MPI_INFO_NULL, NcID)
    IF(status.NE.NF_NOERR) CALL check_err(status)
    status = NFMPI_REDEF(NcID)
    IF(status.NE.NF_NOERR) CALL check_err(status)
  case('w', 'W')
    inquire(file=trim(FileName), exist = alive)
    if(.not.alive)then
      write(*, *)trim(FileName), ' is not exist!'; stop
    endif
    status = nfmpi_open(mpi_comm_ympi, trim(FileName), NF_WRITE, MPI_INFO_NULL, NcID)
    IF(status.NE.NF_NOERR) CALL check_err(status)
  case('r', 'R')
    inquire(file=trim(FileName), exist = alive)
    if(.not.alive)then
      write(*, *)trim(FileName), ' is not exist!'; stop
    endif
   
    status = nfmpi_open(mpi_comm_ympi, trim(FileName), NF_NOWRITE, MPI_INFO_NULL, NcID)
    IF(status.NE.NF_NOERR) CALL check_err(status)

  end select

  return
  end subroutine open_nc

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Close a openning NcFile.
!*DeckYinxq: WriteNC1DInt2

  subroutine close_nc(NcID)
  implicit none
  integer, intent(in) :: NcID

  status=nfmpi_close(NcID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine close_nc

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- To end define.
!*DeckYinxq: end_define

  subroutine end_define(NcID)
  implicit none
  integer, intent(in) :: NcID

  character(len=80) :: lib_version
  character(len=80) :: version = 'netcdf_mod 2.1, '                    &
                               // 'by Xunqiang Yin, 2008-3-31 23:15.'

  character(len=10) :: dd, tt
  character(len=19) :: ts

  call date_and_time(dd, tt)

  ts = dd(1:4)//'/'//dd(5:6)//'/'//dd(7:8)// &
       'T'//tt(1:2)//':'//tt(3:4)//':'//tt(5:10)

! --- Output sorftware versions.

  call set_attribute_character(NcID, 'Creattime', ts)

  lib_version = 'netcdf '//nfmpi_inq_libvers()
  call set_attribute_character(NcID, 'Sorftware1', lib_version)
  call set_attribute_character(NcID, 'Sorftware2', version)

! --- end of defination.

  status = nfmpi_enddef(NcID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine end_define

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- To define dimension for a NcFile.
!*DeckYinxq: dimension_define_notype

  subroutine dimension_define(NcID, DimName, DimLen, DimVarName, DimVarType, &
                               DimID, DimVarID)
  implicit none

  character(len=*), intent(in) :: DimName, DimVarName
  integer, intent(in) :: NcId, DimLen, DimVarType
  integer, optional, intent(out) :: DimID, DimVarID

  integer :: DimID1, DimVarID1

  integer(8) :: DimLen_tmp

  DimLen_tmp=DimLen
  status = nfmpi_def_dim(NcID, DimName, DimLen_tmp, DimID1)
  IF(status.NE.NF_NOERR) CALL check_err(status)
  status = nfmpi_def_var(ncid, trim(DimVarName), DimVarType, 1, DimID1, DimVarID1)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(DimID))DimID = DimID1
  if(present(DimVarID))DimVarID = DimVarID1

  return
  end subroutine dimension_define

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Get the dimension length,
!*DeckYinxq: get_dimension_len

  integer(8) function get_dimension_len(NcID, DimName)

  implicit none

  integer, intent(in) :: NcID
  character(len=*), intent(in) :: DimName

  integer :: DimID

  status = nfmpi_inq_dimid(NcID, trim(DimName), DimID)

  IF(status.NE.NF_NOERR) CALL check_err(status)

  status = nfmpi_inq_dimlen(NcID, DimID, get_dimension_len)

  IF(status.NE.NF_NOERR) CALL check_err(status)

  return

  end function get_dimension_len

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- To define variables.
!*DeckYinxq: variable_define1

  subroutine variable_define1(NcID, VarName, VarType, VarDims, VarID)
  implicit none
  integer, intent(in) :: NcID
  character(len=*), intent(in) :: VarName
  integer, intent(in) :: VarType, VarDims(:)
  integer, optional, intent(out) :: VarID

  integer :: VarRank, VarID1

  status=nfmpi_inq_varid(NcID, trim(VarName), VarID1)
  if(status.ne.NF_NOERR)then
    VarRank = size(VarDims)
    status = nfmpi_def_var(ncid, trim(VarName), VarType, VarRank, VarDims, VarID1)
    IF(status.NE.NF_NOERR) CALL check_err(status)
  endif
  if(present(VarID))VarID = VarID1

  return
  end subroutine variable_define1

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- To define variables.
!*DeckYinxq: variable_define2

  subroutine variable_define2(NcID, VarName, VarType, VarDimsName, VarID)
  implicit none
  integer, intent(in) :: NcID
  character(len=*), intent(in) :: VarName
  integer, intent(in) :: VarType
  character(len=*), intent(in) :: VarDimsName(:)
  integer, optional, intent(out) :: VarID

  integer :: VarRank, VarID1, i
  integer, allocatable :: VarDims(:)

  status=nfmpi_inq_varid(NcID, trim(VarName), VarID1)
  if(status.ne.NF_NOERR)then
    VarRank = size(VarDimsName); allocate(VarDims(VarRank))
    do i = 1, VarRank
      status = nfmpi_inq_dimid(NcID, trim(VarDimsName(i)), VarDims(i))
      IF(status.NE.NF_NOERR) CALL check_err(status)
    enddo
    status = nfmpi_def_var(ncid, trim(VarName), VarType, VarRank, VarDims, VarID1)
    IF(status.NE.NF_NOERR) CALL check_err(status)
    deallocate(VarDims)
  endif
  if(present(VarID))VarID = VarID1

  return
  end subroutine variable_define2

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- To define attribute: character
!*DeckYinxq: set_attribute_character

  Subroutine set_attribute_character(NcID, AttName, attribute, VarName)
  implicit none
  integer, intent(in) :: NcID
  character(len=*), intent(in) :: AttName, attribute
  character(len=*), optional, intent(in) :: VarName

  integer(8) :: att_len
  VarID = 0
  
  if(present(VarName))then
    status = nfmpi_inq_varid(NcID, trim(VarName), VarID)
    
    
    IF(status.NE.NF_NOERR) CALL check_err(status)
  endif

  att_len=len_trim(attribute)
  status = nfmpi_put_att_text(NcID, VarID, AttName, att_len , trim(attribute))

  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine set_attribute_character

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- To define attribute: int1
!*DeckYinxq: set_attribute_int1

  Subroutine set_attribute_int1(NcID, AttName, attribute, VarName)
  implicit none
  integer, intent(in) :: NcID
  character(len=*), intent(in) :: AttName
  character(len=*), optional, intent(in) :: VarName
  integer*1, intent(in) :: attribute
  integer(8) :: tmp

  VarID = 0
  tmp = 1
  if(present(VarName))then
    status = nfmpi_inq_varid(NcID, trim(VarName), VarID)
    IF(status.NE.NF_NOERR) CALL check_err(status)
  endif
  status = nfmpi_put_att_int1(NcID, VarID, AttName, nf_int1, tmp, attribute)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine set_attribute_int1

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- To define attribute: int2
!*DeckYinxq: set_attribute_int2

  Subroutine set_attribute_int2(NcID, AttName, attribute, VarName)
  implicit none
  integer, intent(in) :: NcID
  character(len=*), intent(in) :: AttName
  character(len=*), optional, intent(in) :: VarName
  integer*2, intent(in) :: attribute
  integer(8) :: tmp  !------------------------debug

  VarID = 0
  
  if(present(VarName))then
    status = nfmpi_inq_varid(NcID, trim(VarName), VarID)
    IF(status.NE.NF_NOERR) CALL check_err(status)
  endif
  !write(*,*)"-----------------debug---------"
  tmp=1
  status = nfmpi_put_att_int2(NcID, VarID, AttName, nf_int2, tmp , attribute)
  !write(*,*)"-----------------debug----2-----status:",status
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine set_attribute_int2

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- To define attribute: int
!*DeckYinxq: set_attribute_int

  Subroutine set_attribute_int(NcID, AttName, attribute, VarName)
  implicit none
  integer, intent(in) :: NcID
  character(len=*), intent(in) :: AttName
  character(len=*), optional, intent(in) :: VarName
  integer, intent(in) :: attribute
  integer(8) :: tmp
  
  VarID = 0
  tmp = 1
  if(present(VarName))then
    status = nfmpi_inq_varid(NcID, trim(VarName), VarID)
    IF(status.NE.NF_NOERR) CALL check_err(status)
  endif
  status = nfmpi_put_att_int(NcID, VarID, AttName, nf_int, tmp, attribute)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine set_attribute_int

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- To define attribute: real
!*DeckYinxq: set_attribute_real

  Subroutine set_attribute_real(NcID, AttName, attribute, VarName)
  implicit none
  integer, intent(in) :: NcID
  character(len=*), intent(in) :: AttName
  character(len=*), optional, intent(in) :: VarName
  real, intent(in) :: attribute
  integer(8) :: tmp  

  VarID = 0
  tmp = 1
  if(present(VarName))then
    status = nfmpi_inq_varid(NcID, trim(VarName), VarID)
    IF(status.NE.NF_NOERR) CALL check_err(status)
  endif
  status = nfmpi_put_att_real(NcID, VarID, AttName, nf_real, tmp, attribute)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine set_attribute_real

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- To define attribute: double
!*DeckYinxq: set_attribute_double

  Subroutine set_attribute_double(NcID, AttName, attribute, VarName)
  implicit none
  integer, intent(in) :: NcID
  character(len=*), intent(in) :: AttName
  character(len=*), optional, intent(in) :: VarName
  double precision, intent(in) :: attribute
  integer(8) :: tmp
  VarID = 0

  tmp = 1

  if(present(VarName))then
    status = nfmpi_inq_varid(NcID, trim(VarName), VarID)
    IF(status.NE.NF_NOERR) CALL check_err(status)
  endif
  status = nfmpi_put_att_double(NcID, VarID, AttName, nf_double, tmp, attribute)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine set_attribute_double

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- To define attribute: character
!*DeckYinxq: get_attribute_character

  Subroutine get_attribute_character(NcID, AttName, attribute, VarName)
  implicit none
  integer, intent(in) :: NcID
  character(len=*), intent(in) :: AttName
  character(len=*), intent(out) :: attribute
  character(len=*), optional, intent(in) :: VarName

  VarID = 0
  if(present(VarName))then
    status = nfmpi_inq_varid(NcID, trim(VarName), VarID)
    IF(status.NE.NF_NOERR) CALL check_err(status)
  endif
  status = nfmpi_get_att_text(NcID, VarID, AttName, attribute)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine get_attribute_character

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- To define attribute: int1
!*DeckYinxq: get_attribute_int1

  Subroutine get_attribute_int1(NcID, AttName, attribute, VarName)
  implicit none
  integer, intent(in) :: NcID
  character(len=*), intent(in) :: AttName
  character(len=*), optional, intent(in) :: VarName
  integer*1, intent(out) :: attribute

  VarID = 0
  if(present(VarName))then
    status = nfmpi_inq_varid(NcID, trim(VarName), VarID)
    IF(status.NE.NF_NOERR) CALL check_err(status)
  endif
  status = nfmpi_get_att_int1(NcID, VarID, AttName, attribute)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine get_attribute_int1

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- To define attribute: int2
!*DeckYinxq: get_attribute_int2

  Subroutine get_attribute_int2(NcID, AttName, attribute, VarName)
  implicit none
  integer, intent(in) :: NcID
  character(len=*), intent(in) :: AttName
  character(len=*), optional, intent(in) :: VarName
  integer*2, intent(out) :: attribute

  VarID = 0
  if(present(VarName))then
    status = nfmpi_inq_varid(NcID, trim(VarName), VarID)
    IF(status.NE.NF_NOERR) CALL check_err(status)
  endif
  status = nfmpi_get_att_int2(NcID, VarID, AttName, attribute)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine get_attribute_int2

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- To define attribute: int
!*DeckYinxq: get_attribute_int

  Subroutine get_attribute_int(NcID, AttName, attribute, VarName)
  implicit none
  integer, intent(in) :: NcID
  character(len=*), intent(in) :: AttName
  character(len=*), optional, intent(in) :: VarName
  integer, intent(out) :: attribute

  VarID = 0
  if(present(VarName))then
    status = nfmpi_inq_varid(NcID, trim(VarName), VarID)
    IF(status.NE.NF_NOERR) CALL check_err(status)
  endif
  status = nfmpi_get_att_int(NcID, VarID, AttName, attribute)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine get_attribute_int

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- To define attribute: real
!*DeckYinxq: get_attribute_real

  Subroutine get_attribute_real(NcID, AttName, attribute, VarName)
  implicit none
  integer, intent(in) :: NcID
  character(len=*), intent(in) :: AttName
  character(len=*), optional, intent(in) :: VarName
  real, intent(out) :: attribute

  VarID = 0
  if(present(VarName))then
    status = nfmpi_inq_varid(NcID, trim(VarName), VarID)
    IF(status.NE.NF_NOERR) CALL check_err(status)
  endif
  status = nfmpi_get_att_real(NcID, VarID, AttName, attribute)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine get_attribute_real

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- To define attribute: double
!*DeckYinxq: get_attribute_double

  Subroutine get_attribute_double(NcID, AttName, attribute, VarName)
  implicit none
  integer, intent(in) :: NcID
  character(len=*), intent(in) :: AttName
  character(len=*), optional, intent(in) :: VarName
  double precision, intent(out) :: attribute

  VarID = 0
  if(present(VarName))then
    status = nfmpi_inq_varid(NcID, trim(VarName), VarID)
    IF(status.NE.NF_NOERR) CALL check_err(status)
  endif
  status = nfmpi_get_att_double(NcID, VarID, AttName, attribute)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine get_attribute_double

!===================================== 0-d =====================================
!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write data of 0D or unlimite dims var into NC file.
!*DeckYinxq: writenc_0d_int1

  subroutine writenc_0d_int1(ncid, VarName, Var, RecNum)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer*1, intent(in) :: Var
  integer, intent(in), optional :: RecNum

  integer(8) :: tmp
  integer(8) :: RecNum_tmp
  tmp = 1
  RecNum_tmp = RecNum


  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if (.not.present(RecNum))then
    status=nfmpi_put_var_int1_all(ncID, VarID, Var)
  else
    status =nfmpi_put_vara_int1_all(ncID, VarID, RecNum_tmp, tmp, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_0d_int1

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write data of 0D or unlimite dims var into NC file.
!*DeckYinxq: writenc_0d_int2

  subroutine writenc_0d_int2(ncid, VarName, Var, RecNum)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer*2, intent(in) :: Var
  integer, intent(in), optional :: RecNum
  integer(8) :: tmp
  integer(8) :: RecNum_tmp
  tmp = 1
  RecNum_tmp = RecNum

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if (.not.present(RecNum))then
    status=nfmpi_put_var_int2_all(ncID, VarID, Var)
  else
    status = nfmpi_put_vara_int2_all(ncID, VarID, RecNum_tmp, tmp, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_0d_int2

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write data of 0D or unlimite dims var into NC file.
!*DeckYinxq: writenc_0d_int

  subroutine writenc_0d_int(ncid, VarName, Var, RecNum)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer, intent(in) :: Var
  integer, intent(in), optional :: RecNum
  integer(8) :: tmp
  integer(8) :: RecNum_tmp
  tmp = 1
  RecNum_tmp = RecNum


  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if (.not.present(RecNum))then
    status=nfmpi_put_var_int_all(ncID, VarID, Var)
  else
    status =nfmpi_put_vara_int_all(ncID, VarID, RecNum_tmp, tmp, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_0d_int

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write data of 0D or unlimite dims var into NC file.
!*DeckYinxq: writenc_0d_real

  subroutine writenc_0d_real(ncid, VarName, Var, RecNum)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  real, intent(in) :: Var
  integer, intent(in), optional :: RecNum
  integer(8) :: tmp
  integer(8) :: RecNum_tmp
  tmp = 1
  RecNum_tmp = RecNum  

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)


  if (.not.present(RecNum))then
    status=nfmpi_put_var_real_all(ncID, VarID, Var)
  else
    status = nfmpi_put_vara_real_all(ncID, VarID, RecNum_tmp, tmp, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_0d_real

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write data of 0D or unlimite dims var into NC file.
!*DeckYinxq: writenc_0d_double

  subroutine writenc_0d_double(ncid, VarName, Var, RecNum)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  double precision, intent(in) :: Var
  integer, intent(in), optional :: RecNum

  integer(8) :: tmp
  integer(8) :: RecNum_tmp
  tmp = 1
  RecNum_tmp = RecNum

  status=nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if (.not.present(RecNum))then
    status=nfmpi_put_var_double_all(ncID, VarID, Var)
  else
    status=nfmpi_put_vara_double_all(ncID, VarID, RecNum_tmp, tmp, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_0d_double

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write data of 0D or unlimite dims var into NC file.
!*DeckYinxq: writenc_0d_text

  subroutine writenc_0d_text(ncid, VarName, Var, RecNum)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  character(len=*), intent(in) :: Var
  integer, intent(in), optional :: RecNum

  integer(8) :: starts(2), cont(2)

  status=nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if (.not.present(RecNum))then
    status=nfmpi_put_var_text_all(ncID, VarID, Var)
  else
    starts(1) = 1; starts(2) = RecNum
    cont(1) = len_trim(var); cont(2) = 1 
    status=nfmpi_put_vara_text_all(ncID, VarID, starts, cont, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_0d_text

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read data of 0D or unlimite dims var from NC file.
!*DeckYinxq: readnc_0d_int1

  subroutine readnc_0d_int1(ncid, VarName, Var, RecNum)
  implicit none

  integer, intent(in) :: NcID
  character(len=*), intent(in) :: VarName
  integer*1, intent(out) :: Var
  integer, optional, intent(in) :: RecNum
  integer(8) :: tmp
  integer(8) :: RecNum_tmp
  tmp = 1
  RecNum_tmp = RecNum

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status .NE.NF_NOERR) CALL check_err(status)

  if (.not.present(RecNum))then
    status = nfmpi_get_var_int1_all(ncID, varID, Var)
  else
    status = nfmpi_get_vara_int1_all(ncID, varID, RecNum_tmp, tmp, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_0d_int1

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read data of 0D or unlimite dims var from NC file.
!*DeckYinxq: readnc_0d_int2

  subroutine readnc_0d_int2(ncid, VarName, Var, RecNum)
  implicit none

  integer, intent(in) :: NcID
  character(len=*), intent(in) :: VarName
  integer*2, intent(out) :: Var
  integer, optional, intent(in) :: RecNum
  integer(8) :: tmp
  integer(8) :: RecNum_tmp
  tmp = 1
  RecNum_tmp = RecNum


  status = NFMPI_INQ_VARID(ncID, trim(VarName), varID)
  IF(status .NE.NF_NOERR) CALL check_err(status)
  if (.not.present(RecNum))then
    status = nfmpi_get_var_int2_all(ncID, varID, Var)
  else
    status = nfmpi_get_vara_int2_all(ncID, varID, RecNum_tmp, tmp, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_0d_int2

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read data of 0D or unlimite dims var from NC file.
!*DeckYinxq: readnc_0d_int

  subroutine readnc_0d_int(ncid, VarName, Var, RecNum)
  implicit none

  integer, intent(in) :: NcID
  character(len=*), intent(in) :: VarName
  integer, intent(out) :: Var
  integer, optional, intent(in) :: RecNum

  integer(8) :: tmp
  integer(8) :: RecNum_tmp
  tmp = 1
  RecNum_tmp = RecNum

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status .NE.NF_NOERR) CALL check_err(status)

  if (.not.present(RecNum))then
    status = nfmpi_get_var_int_all(ncID, varID, Var)
  else
    status = nfmpi_get_vara_int_all(ncID, varID, RecNum_tmp, tmp, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_0d_int

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read data of 0D or unlimite dims var from NC file.
!*DeckYinxq: readnc_0d_real

  subroutine readnc_0d_real(ncid, VarName, Var, RecNum)
  implicit none

  integer, intent(in) :: NcID
  character(len=*), intent(in) :: VarName
  real, intent(out) :: Var
  integer, optional, intent(in) :: RecNum
  integer(8) :: tmp
  integer(8) :: RecNum_tmp
  tmp = 1
  RecNum_tmp = RecNum


  status = NFMPI_INQ_VARID(ncID, trim(VarName), varID)
  IF(status .NE.NF_NOERR) CALL check_err(status)
  if (.not.present(RecNum))then
    status = nfmpi_get_var_real_all(ncID, varID, Var)
  else
    status = nfmpi_get_vara_real_all(ncID, varID, RecNum_tmp, tmp, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_0d_real

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read data of 0D or unlimite dims var from NC file.
!*DeckYinxq: readnc_0d_double

  subroutine readnc_0d_double(ncid, VarName, Var, RecNum)
  implicit none

  integer, intent(in) :: NcID
  character(len=*), intent(in) :: VarName
  double precision, intent(out) :: Var
  integer, optional, intent(in) :: RecNum
  integer(8) :: tmp
  integer(8) :: RecNum_tmp
  tmp = 1
  RecNum_tmp = RecNum


  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status .NE.NF_NOERR) CALL check_err(status)
  if (.not.present(RecNum))then
    status = nfmpi_get_var_double_all(ncID, varID, Var)
  else
    status = nfmpi_get_vara_double_all(ncID, varID, RecNum_tmp, tmp, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_0d_double

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read data of 0D or unlimite dims var from NC file.
!*DeckYinxq: readnc_0d_text

  subroutine readnc_0d_text(ncid, VarName, Var, RecNum)
  implicit none

  integer, intent(in) :: NcID
  character(len=*), intent(in) :: VarName
  character(len=*), intent(out) :: Var
  integer, intent(in), optional :: RecNum

  integer(8) :: starts(2), cont(2)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status .NE.NF_NOERR) CALL check_err(status)

  if (.not.present(RecNum))then
    status = nfmpi_get_var_text_all(ncID, varID, Var)
  else
    starts(1) = 1; starts(2) = RecNum
    cont(1) = len_trim(var); cont(2) = 1 
    status = nfmpi_get_vara_text_all(ncID, varID, starts, cont, Var)
  endif

  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_0d_text

!===================================== 1-d =====================================
!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 1D data into NC file: int1
!*DeckYinxq: writenc_1d_int1

  subroutine writenc_1d_int1(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer*1, intent(in) :: Var(:)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 1
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var)
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_put_vara_int1_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_int1_all(ncID, varID, starts(1:DimNum),       &
                                counts(1:DimNum), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_put_vara_int1_all(ncID, varID, starts, counts, Var)
  else
    status = nfmpi_put_var_int1_all(ncID, varID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_1d_int1

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 1D data into NC file: int2
!*DeckYinxq: writenc_1d_int2

  subroutine writenc_1d_int2(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer*2, intent(in) :: Var(:)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 1
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var)
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_put_vara_int2_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_int2_all(ncID, varID, starts(1:DimNum),       &
                                counts(1:DimNum), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_put_vara_int2_all(ncID, varID, starts, counts, Var)
  else
    status = nfmpi_put_var_int2_all(ncID, varID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_1d_int2

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 1D data into NC file: int
!*DeckYinxq: writenc_1d_int

  subroutine writenc_1d_int(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer, intent(in) :: Var(:)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 1
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var)
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_put_vara_int_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_int_all(ncID, varID, starts(1:DimNum),       &
                                counts(1:DimNum), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_put_vara_int_all(ncID, varID, starts, counts, Var)
  else
    status = nfmpi_put_var_int_all(ncID, varID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_1d_int

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 1D data into NC file: real
!*DeckYinxq: writenc_1d_real

  subroutine writenc_1d_real(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  real, intent(in) :: Var(:)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 1
  integer(8) :: starts(DimNum+1), counts(DimNum+1)
  integer(8) :: starts_2,counts_2


  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)


  if(present(locs))then
    counts(1:DimNum) = shape(Var)
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_put_vara_real_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_real_all(ncID, varID, starts(1:DimNum),  counts(1:DimNum), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_put_vara_real_all(ncID, varID, starts, counts, Var)
  else   
    
    status =  nfmpi_put_var_real_all(ncID, varID, Var)
    !write(*,*) " writenc_1d_real----status:",status
    IF(status.NE.NF_NOERR) CALL check_err(status)

  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_1d_real

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 1D data into NC file: double
!*DeckYinxq: writenc_1d_double

  subroutine writenc_1d_double(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  double precision, intent(in) :: Var(:)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 1
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var)
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_put_vara_double_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_double_all(ncID, varID, starts(1:DimNum),       &
                                counts(1:DimNum), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_put_vara_double_all(ncID, varID, starts, counts, Var)
  else
    status = nfmpi_put_var_double_all(ncID, varID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_1d_double

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 1D data into NC file: double
!*DeckYinxq: writenc_1d_text

  subroutine writenc_1d_text(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  character(len=*), intent(in) :: Var(:)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 1
  integer(8) :: starts(DimNum+2), counts(DimNum+2)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(2:DimNum+1) = shape(Var); counts(1) = len(var);
    starts(2:DimNum+1) = locs(1:DimNum); starts(1) = 1
    if(present(RecNum))then
      counts(DimNum+2) = 1; starts(DimNum+2) = RecNum
      status = nfmpi_put_vara_text_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_text_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(2:DimNum+1) = shape(Var); counts(1) = len(var); counts(DimNum+2) = 1
    starts = 1; starts(DimNum+2) = RecNum
    status = nfmpi_put_vara_text_all(ncID, varID, starts, counts, Var)
  else
    status = nfmpi_put_var_text_all(ncID, varID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_1d_text

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 1D data from NC file: int1
!*DeckYinxq: readnc_1d_int1

  subroutine readnc_1d_int1(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer*1, intent(out) :: Var(:)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 1
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var);
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_get_vara_int1_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_int1_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_get_vara_int1_all(ncID, VarID, starts, counts, Var)
  else
    status = nfmpi_get_var_int1_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_1d_int1

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 1D data from NC file: int2
!*DeckYinxq: readnc_1d_int2

  subroutine readnc_1d_int2(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer*2, intent(out) :: Var(:)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 1
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var);
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_get_vara_int2_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_int2_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_get_vara_int2_all(ncID, VarID, starts, counts, Var)
  else
    status = nfmpi_get_var_int2_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_1d_int2

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 1D data from NC file: int
!*DeckYinxq: readnc_1d_int

  subroutine readnc_1d_int(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer, intent(out) :: Var(:)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 1
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var);
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_get_vara_int_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_int_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_get_vara_int_all(ncID, VarID, starts, counts, Var)
  else
    status = nfmpi_get_var_int_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_1d_int

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 1D data from NC file: real
!*DeckYinxq: readnc_1d_real

  subroutine readnc_1d_real(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  real, intent(out) :: Var(:)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 1
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  ! status = NF_INQ_VarID(ncID,trime(VarName),VarID)
  ! status = nfmpi_inq_varid(ncid, trim(VarName), VarID)
 
  status = nfmpi_inq_varid(ncid, trim(VarName), VarID)

  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var);
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_get_vara_real_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_real_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif
  if(present(RecNum))then
    
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_get_vara_real_all(ncID, VarID, starts, counts, Var)
    
  else
    status = nfmpi_get_var_real_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)
  return
  end subroutine readnc_1d_real

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 1D data from NC file: double
!*DeckYinxq: readnc_1d_double

  subroutine readnc_1d_double(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  double precision, intent(out) :: Var(:)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 1
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var);
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_get_vara_double_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_double_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_get_vara_double_all(ncID, VarID, starts, counts, Var)
  else
    status = nfmpi_get_var_double_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_1d_double

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 1D data from NC file: double
!*DeckYinxq: readnc_1d_text

  subroutine readnc_1d_text(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  character(len=*), intent(out) :: Var(:)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 1
  integer(8) :: starts(DimNum+2), counts(DimNum+2)

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(2:DimNum+1) = shape(Var); counts(1) = len(var);
    starts(2:DimNum+1) = locs(1:DimNum); starts(1) = 1
    if(present(RecNum))then
      counts(DimNum+2) = 1; starts(DimNum+2) = RecNum
      status = nfmpi_get_vara_text_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_text_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(2:DimNum+1) = shape(Var); counts(1) = len(var); counts(DimNum+2) = 1
    starts = 1; starts(DimNum+2) = RecNum
    status = nfmpi_get_vara_text_all(ncID, VarID, starts, counts, Var)
  else
    status = nfmpi_get_var_text_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_1d_text

!===================================== 2-d =====================================
!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 2D data into NC file: int1
!*DeckYinxq: writenc_2d_int1

  subroutine writenc_2d_int1(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer*1, intent(in) :: Var(:, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 2
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var)
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_put_vara_int1_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_int1_all(ncID, varID, starts(1:DimNum),       &
                                counts(1:DimNum), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_put_vara_int1_all(ncID, varID, starts, counts, Var)
  else
    status = nfmpi_put_var_int1_all(ncID, varID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_2d_int1

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 2D data into NC file: int2
!*DeckYinxq: writenc_2d_int2

  subroutine writenc_2d_int2(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer*2, intent(in) :: Var(:, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 2
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var)
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_put_vara_int2_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_int2_all(ncID, varID, starts(1:DimNum),       &
                                counts(1:DimNum), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_put_vara_int2_all(ncID, varID, starts, counts, Var)
  else
    status = nfmpi_put_var_int2_all(ncID, varID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_2d_int2

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 2D data into NC file: int
!*DeckYinxq: writenc_2d_int

  subroutine writenc_2d_int(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer, intent(in) :: Var(:, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 2
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var)
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_put_vara_int_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_int_all(ncID, varID, starts(1:DimNum),       &
                                counts(1:DimNum), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_put_vara_int_all(ncID, varID, starts, counts, Var)
  else
    status = nfmpi_put_var_int_all(ncID, varID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_2d_int




!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 2D data into NC file: real
!*DeckYinxq: writenc_2d_real

  subroutine writenc_2d_real(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  real, intent(in) :: Var(:, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 2
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var)
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_put_vara_real_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_real_all(ncID, varID, starts(1:DimNum),       &
                                counts(1:DimNum), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_put_vara_real_all(ncID, varID, starts, counts, Var)
  else
    status = nfmpi_put_var_real_all(ncID, varID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_2d_real

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 2D data into NC file: double
!*DeckYinxq: writenc_2d_double

  subroutine writenc_2d_double(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  double precision, intent(in) :: Var(:, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 2
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var)
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_put_vara_double_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_double_all(ncID, varID, starts(1:DimNum),       &
                                counts(1:DimNum), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_put_vara_double_all(ncID, varID, starts, counts, Var)
  else
    status = nfmpi_put_var_double_all(ncID, varID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_2d_double

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 2D data into NC file: double
!*DeckYinxq: writenc_2d_text

  subroutine writenc_2d_text(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  character(len=*), intent(in) :: Var(:, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 2
  integer(8) :: starts(DimNum+2), counts(DimNum+2)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(2:DimNum+1) = shape(Var); counts(1) = len(var);
    starts(2:DimNum+1) = locs(1:DimNum); starts(1) = 1
    if(present(RecNum))then
      counts(DimNum+2) = 1; starts(DimNum+2) = RecNum
      status = nfmpi_put_vara_text_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_text_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(2:DimNum+1) = shape(Var); counts(1) = len(var); counts(DimNum+2) = 1
    starts = 1; starts(DimNum+2) = RecNum
    status = nfmpi_put_vara_text_all(ncID, varID, starts, counts, Var)
  else
    status = nfmpi_put_var_text_all(ncID, varID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_2d_text

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 2D data from NC file: int1
!*DeckYinxq: readnc_2d_int1

  subroutine readnc_2d_int1(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer*1, intent(out) :: Var(:, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 2
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var);
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_get_vara_int1_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_int1_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_get_vara_int1_all(ncID, VarID, starts, counts, Var)
  else
    status = nfmpi_get_var_int1_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_2d_int1

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 2D data from NC file: int2
!*DeckYinxq: readnc_2d_int2

  subroutine readnc_2d_int2(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer*2, intent(out) :: Var(:, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 2
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var);
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_get_vara_int2_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_int2_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_get_vara_int2_all(ncID, VarID, starts, counts, Var)
  else
    status = nfmpi_get_var_int2_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_2d_int2

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 2D data from NC file: int
!*DeckYinxq: readnc_2d_int

  subroutine readnc_2d_int(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer, intent(out) :: Var(:, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 2
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var);
    
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_get_vara_int_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_int_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_get_vara_int_all(ncID, VarID, starts, counts, Var)
  else
    status = nfmpi_get_var_int_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_2d_int

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 2D data from NC file: real
!*DeckYinxq: readnc_2d_real

  subroutine readnc_2d_real(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  real, intent(out) :: Var(:, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 2
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  integer :: i

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)


  if(present(locs))then
    counts(1:DimNum) = shape(Var);
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_get_vara_real_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_real_all(ncID, varID, starts(1), counts(1), Var)
    endif

    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_get_vara_real_all(ncID, VarID, starts, counts, Var)
  else
    status = nfmpi_get_var_real_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_2d_real

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 2D data from NC file: double
!*DeckYinxq: readnc_2d_double

  subroutine readnc_2d_double(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  double precision, intent(out) :: Var(:, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 2
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var);
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_get_vara_double_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_double_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_get_vara_double_all(ncID, VarID, starts, counts, Var)
  else
    status = nfmpi_get_var_double_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_2d_double

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 2D data from NC file: double
!*DeckYinxq: readnc_2d_text

  subroutine readnc_2d_text(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  character(len=*), intent(out) :: Var(:, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 2
  integer(8) :: starts(DimNum+2), counts(DimNum+2)

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(2:DimNum+1) = shape(Var); counts(1) = len(var);
    starts(2:DimNum+1) = locs(1:DimNum); starts(1) = 1
    if(present(RecNum))then
      counts(DimNum+2) = 1; starts(DimNum+2) = RecNum
      status = nfmpi_get_vara_text_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_text_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(2:DimNum+1) = shape(Var); counts(1) = len(var); counts(DimNum+2) = 1
    starts = 1; starts(DimNum+2) = RecNum
    status = nfmpi_get_vara_text_all(ncID, VarID, starts, counts, Var)
  else
    status = nfmpi_get_var_text_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_2d_text

!===================================== 3-d =====================================
!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 3D data into NC file: int1
!*DeckYinxq: writenc_3d_int1

  subroutine writenc_3d_int1(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer*1, intent(in) :: Var(:, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 3
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var)
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_put_vara_int1_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_int1_all(ncID, varID, starts(1:DimNum),       &
                                counts(1:DimNum), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_put_vara_int1_all(ncID, varID, starts, counts, Var)
  else
    status = nfmpi_put_var_int1_all(ncID, varID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_3d_int1

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 3D data into NC file: int2
!*DeckYinxq: writenc_3d_int2

  subroutine writenc_3d_int2(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer*2, intent(in) :: Var(:, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 3
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var)
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_put_vara_int2_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_int2_all(ncID, varID, starts(1:DimNum),       &
                                counts(1:DimNum), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_put_vara_int2_all(ncID, varID, starts, counts, Var)
  else
    status = nfmpi_put_var_int2_all(ncID, varID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_3d_int2

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 3D data into NC file: int
!*DeckYinxq: writenc_3d_int

  subroutine writenc_3d_int(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer, intent(in) :: Var(:, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 3
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var)
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_put_vara_int_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_int_all(ncID, varID, starts(1:DimNum),       &
                                counts(1:DimNum), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_put_vara_int_all(ncID, varID, starts, counts, Var)
  else
    status = nfmpi_put_var_int_all(ncID, varID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_3d_int

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 3D data into NC file: real
!*DeckYinxq: writenc_3d_real

  subroutine writenc_3d_real(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  real, intent(in) :: Var(:, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 3
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var)
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_put_vara_real_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_real_all(ncID, varID, starts(1:DimNum),       &
                                counts(1:DimNum), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_put_vara_real_all(ncID, varID, starts, counts, Var)
  else
    status = nfmpi_put_var_real_all(ncID, varID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_3d_real

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 3D data into NC file: double
!*DeckYinxq: writenc_3d_double

  subroutine writenc_3d_double(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  double precision, intent(in) :: Var(:, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 3
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var)
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_put_vara_double_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_double_all(ncID, varID, starts(1:DimNum),       &
                                counts(1:DimNum), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_put_vara_double_all(ncID, varID, starts, counts, Var)
  else
    status = nfmpi_put_var_double_all(ncID, varID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_3d_double

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 3D data into NC file: double
!*DeckYinxq: writenc_3d_text

  subroutine writenc_3d_text(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  character(len=*), intent(in) :: Var(:, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 3
  integer(8) :: starts(DimNum+2), counts(DimNum+2)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(2:DimNum+1) = shape(Var); counts(1) = len(var);
    starts(2:DimNum+1) = locs(1:DimNum); starts(1) = 1
    if(present(RecNum))then
      counts(DimNum+2) = 1; starts(DimNum+2) = RecNum
      status = nfmpi_put_vara_text_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_text_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(2:DimNum+1) = shape(Var); counts(1) = len(var); counts(DimNum+2) = 1
    starts = 1; starts(DimNum+2) = RecNum
    status = nfmpi_put_vara_text_all(ncID, varID, starts, counts, Var)
  else
    status = nfmpi_put_var_text_all(ncID, varID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_3d_text

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 3D data from NC file: int1
!*DeckYinxq: readnc_3d_int1

  subroutine readnc_3d_int1(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer*1, intent(out) :: Var(:, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 3
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var);
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_get_vara_int1_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_int1_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_get_vara_int1_all(ncID, VarID, starts, counts, Var)
  else
    status = nfmpi_get_var_int1_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_3d_int1

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 3D data from NC file: int2
!*DeckYinxq: readnc_3d_int2

  subroutine readnc_3d_int2(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer*2, intent(out) :: Var(:, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 3
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var);
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_get_vara_int2_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_int2_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_get_vara_int2_all(ncID, VarID, starts, counts, Var)
  else
    status = nfmpi_get_var_int2_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_3d_int2

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 3D data from NC file: int
!*DeckYinxq: readnc_3d_int

  subroutine readnc_3d_int(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer, intent(out) :: Var(:, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 3
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var);
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_get_vara_int_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_int_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_get_vara_int_all(ncID, VarID, starts, counts, Var)
  else
    status = nfmpi_get_var_int_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_3d_int

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 3D data from NC file: real
!*DeckYinxq: readnc_3d_real

  subroutine readnc_3d_real(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  real, intent(out) :: Var(:, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 3
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var);
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_get_vara_real_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_real_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_get_vara_real_all(ncID, VarID, starts, counts, Var)
  else
    status = nfmpi_get_var_real_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_3d_real

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 3D data from NC file: double
!*DeckYinxq: readnc_3d_double

  subroutine readnc_3d_double(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  double precision, intent(out) :: Var(:, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 3
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var);
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_get_vara_double_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_double_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_get_vara_double_all(ncID, VarID, starts, counts, Var)
  else
    status = nfmpi_get_var_double_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_3d_double

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 3D data from NC file: double
!*DeckYinxq: readnc_3d_text

  subroutine readnc_3d_text(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  character(len=*), intent(out) :: Var(:, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 3
  integer(8) :: starts(DimNum+2), counts(DimNum+2)

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(2:DimNum+1) = shape(Var); counts(1) = len(var);
    starts(2:DimNum+1) = locs(1:DimNum); starts(1) = 1
    if(present(RecNum))then
      counts(DimNum+2) = 1; starts(DimNum+2) = RecNum
      status = nfmpi_get_vara_text_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_text_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(2:DimNum+1) = shape(Var); counts(1) = len(var); counts(DimNum+2) = 1
    starts = 1; starts(DimNum+2) = RecNum
    status = nfmpi_get_vara_text_all(ncID, VarID, starts, counts, Var)
  else
    status = nfmpi_get_var_text_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_3d_text

!===================================== 4-d =====================================
!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 4D data into NC file: int1
!*DeckYinxq: writenc_4d_int1

  subroutine writenc_4d_int1(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer*1, intent(in) :: Var(:, :, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 4
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var)
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_put_vara_int1_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_int1_all(ncID, varID, starts(1:DimNum),       &
                                counts(1:DimNum), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_put_vara_int1_all(ncID, varID, starts, counts, Var)
  else
    status = nfmpi_put_var_int1_all(ncID, varID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_4d_int1

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 4D data into NC file: int2
!*DeckYinxq: writenc_4d_int2

  subroutine writenc_4d_int2(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer*2, intent(in) :: Var(:, :, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 4
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var)
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_put_vara_int2_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_int2_all(ncID, varID, starts(1:DimNum),       &
                                counts(1:DimNum), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_put_vara_int2_all(ncID, varID, starts, counts, Var)
  else
    status = nfmpi_put_var_int2_all(ncID, varID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_4d_int2

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 4D data into NC file: int
!*DeckYinxq: writenc_4d_int

  subroutine writenc_4d_int(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer, intent(in) :: Var(:, :, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 4
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var)
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_put_vara_int_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_int_all(ncID, varID, starts(1:DimNum),       &
                                counts(1:DimNum), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_put_vara_int_all(ncID, varID, starts, counts, Var)
  else
    status = nfmpi_put_var_int_all(ncID, varID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_4d_int

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 4D data into NC file: real
!*DeckYinxq: writenc_4d_real

  subroutine writenc_4d_real(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  real, intent(in) :: Var(:, :, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 4
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var)
    starts(1:DimNum) = locs(1:DimNum)
    !write(*,*)" myid,counts(1):",myid,counts(1)
    !write(*,*)" myid,counts(2):",myid,counts(2)
    !write(*,*)" myid,counts(2):",myid,counts(3)
    !write(*,*)" myid,counts(2):",myid,counts(4)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_put_vara_real_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_real_all(ncID, varID, starts(1:DimNum),       &
                                counts(1:DimNum), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_put_vara_real_all(ncID, varID, starts, counts, Var)
  else
    status = nfmpi_put_var_real_all(ncID, varID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_4d_real

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 4D data into NC file: double
!*DeckYinxq: writenc_4d_double

  subroutine writenc_4d_double(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  double precision, intent(in) :: Var(:, :, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 4
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var)
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_put_vara_double_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_double_all(ncID, varID, starts(1:DimNum),       &
                                counts(1:DimNum), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_put_vara_double_all(ncID, varID, starts, counts, Var)
  else
    status = nfmpi_put_var_double_all(ncID, varID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_4d_double

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Write 4D data into NC file: double
!*DeckYinxq: writenc_4d_text

  subroutine writenc_4d_text(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  character(len=*), intent(in) :: Var(:, :, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 4
  integer(8) :: starts(DimNum+2), counts(DimNum+2)

  status = nfmpi_inq_varid(ncID, trim(VarName), varID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(2:DimNum+1) = shape(Var); counts(1) = len(var);
    starts(2:DimNum+1) = locs(1:DimNum); starts(1) = 1
    if(present(RecNum))then
      counts(DimNum+2) = 1; starts(DimNum+2) = RecNum
      status = nfmpi_put_vara_text_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_put_vara_text_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(2:DimNum+1) = shape(Var); counts(1) = len(var); counts(DimNum+2) = 1
    starts = 1; starts(DimNum+2) = RecNum
    status = nfmpi_put_vara_text_all(ncID, varID, starts, counts, Var)
  else
    status = nfmpi_put_var_text_all(ncID, varID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine writenc_4d_text

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 4D data from NC file: int1
!*DeckYinxq: readnc_4d_int1

  subroutine readnc_4d_int1(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer*1, intent(out) :: Var(:, :, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 4
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var);
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_get_vara_int1_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_int1_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_get_vara_int1_all(ncID, VarID, starts, counts, Var)
  else
    status = nfmpi_get_var_int1_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_4d_int1

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 4D data from NC file: int2
!*DeckYinxq: readnc_4d_int2

  subroutine readnc_4d_int2(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer*2, intent(out) :: Var(:, :, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 4
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var);
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_get_vara_int2_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_int2_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_get_vara_int2_all(ncID, VarID, starts, counts, Var)
  else
    status = nfmpi_get_var_int2_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_4d_int2

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 4D data from NC file: int
!*DeckYinxq: readnc_4d_int

  subroutine readnc_4d_int(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  integer, intent(out) :: Var(:, :, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 4
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var);
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_get_vara_int_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_int_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_get_vara_int_all(ncID, VarID, starts, counts, Var)
  else
    status = nfmpi_get_var_int_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_4d_int

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 4D data from NC file: real
!*DeckYinxq: readnc_4d_real

  subroutine readnc_4d_real(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  real, intent(out) :: Var(:, :, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 4
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var);
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_get_vara_real_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_real_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_get_vara_real_all(ncID, VarID, starts, counts, Var)
  else
    status = nfmpi_get_var_real_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_4d_real

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 4D data from NC file: double
!*DeckYinxq: readnc_4d_double

  subroutine readnc_4d_double(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  double precision, intent(out) :: Var(:, :, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 4
  integer(8) :: starts(DimNum+1), counts(DimNum+1)

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(1:DimNum) = shape(Var);
    starts(1:DimNum) = locs(1:DimNum)
    if(present(RecNum))then
      counts(DimNum+1) = 1; starts(DimNum+1) = RecNum
      status = nfmpi_get_vara_double_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_double_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(1:DimNum) = shape(Var); counts(DimNum+1) = 1
    starts = 1; starts(DimNum+1) = RecNum
    status = nfmpi_get_vara_double_all(ncID, VarID, starts, counts, Var)
  else
    status = nfmpi_get_var_double_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)

  return
  end subroutine readnc_4d_double

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------
! --- Read 4D data from NC file: double
!*DeckYinxq: readnc_4d_text

  subroutine readnc_4d_text(ncid, VarName, Var, RecNum, locs)
  implicit none

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: VarName
  character(len=*), intent(out) :: Var(:, :, :, :)
  integer, optional, intent(in) :: RecNum
  integer, optional, intent(in) :: locs(:)

  integer, parameter :: DimNum = 4
  integer(8) :: starts(DimNum+2), counts(DimNum+2)

  status = nfmpi_inq_varid(ncID, trim(VarName), VarID)
  IF(status.NE.NF_NOERR) CALL check_err(status)

  if(present(locs))then
    counts(2:DimNum+1) = shape(Var); counts(1) = len(var);
    starts(2:DimNum+1) = locs(1:DimNum); starts(1) = 1
    if(present(RecNum))then
      counts(DimNum+2) = 1; starts(DimNum+2) = RecNum
      status = nfmpi_get_vara_text_all(ncID, varID, starts, counts, Var)
    else
      status = nfmpi_get_vara_text_all(ncID, varID, starts(1:DimNum+1),       &
                                counts(1:DimNum+1), Var)
    endif
    IF(status.NE.NF_NOERR) CALL check_err(status)
    return
  endif

  if(present(RecNum))then
    counts(2:DimNum+1) = shape(Var); counts(1) = len(var); counts(DimNum+2) = 1
    starts = 1; starts(DimNum+2) = RecNum
    status = nfmpi_get_vara_text_all(ncID, VarID, starts, counts, Var)
  else
    status = nfmpi_get_var_text_all(ncID, VarID, Var)
  endif
  IF(status.NE.NF_NOERR) CALL check_err(status)
	
  return
  end subroutine readnc_4d_text

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------------------------------------------------------------------------

  end module netcdf_mod

!-------------------------------------------------------------------------------
!###############################################################################

