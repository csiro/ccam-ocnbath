Program ocnbath

! This code creates CCAM ocean depth data using the ETOPO2 dataset

Implicit None

Character*80, dimension(:,:), allocatable :: options
Character*80, dimension(2) :: fname
Character*80 topofile,bathout
Integer binlimit, nopts
Logical fastocn

Namelist/ocnnml/ topofile,bathout,fastocn,binlimit

Write(6,*) 'OCNBATH - ETOPO 2km to CC grid (JUN-09)'

! Read switches
nopts=1
Allocate (options(nopts,2))
options(:,1) = (/ '-s' /)
options(:,2) = ''

Call readswitch(options,nopts)
Call defaults(options,nopts)

! Read namelist
Write(6,*) 'Input &ocnnml namelist'
Read(5,NML=ocnnml)
Write(6,*) 'Namelist accepted'

! Generate veg data
fname(1)=topofile
fname(2)=bathout

Call createocn(options,nopts,fname,fastocn,binlimit)

Deallocate(options)

Stop
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine displays the help message
!

Subroutine help()

Implicit None

Write(6,*)
Write(6,*) "Usage:"
Write(6,*) "  ocnbath -s size < ocnbath.nml"
Write(6,*)
Write(6,*) "Options:"
Write(6,*) "  -s size      size of array used for reading ETOPO data"
Write(6,*) "               (typically =500).  The larger the array, the"
Write(6,*) "               faster and more accurate the output."
Write(6,*)
Write(6,*) "Namelist:"
Write(6,*) "  The namelist specifies what data to store and the filenames"
Write(6,*) "  to use.  For example:"
Write(6,*)
Write(6,*) '  &ocnnml'
Write(6,*) '    topofile="topout"'
Write(6,*) '    bathout="bath"'
Write(6,*) '    fastocn=t'
Write(6,*) '    binlimit=2'
Write(6,*) '  &end'
Write(6,*)
Write(6,*) '  where:'
Write(6,*) '    topofile      = topography (input) file'
Write(6,*) '    bathout       = Depth filename'
Write(6,*) '    fastocn       = Turn on fastocn mode (see notes below)'
Write(6,*) '    binlimit      = The minimum ratio between the grid'
Write(6,*) '                    length scale and the length scale of'
Write(6,*) '                    the aggregated ETOPO data (see notes'
Write(6,*) '                    below).'
Write(6,*)
Write(6,*) 'NOTES: fastocn mode will speed up the code by aggregating'
Write(6,*) '       ETOPO data at a coarser resolution before'
Write(6,*) '       processing.  The degree of aggregation is determined'
Write(6,*) '       by the avaliable memory (i.e., -s switch).   Usually,'
Write(6,*) '       fastocn is used to test the output and then the'
Write(6,*) '       dataset is subsequently regenerated with fastocn=f.'
Write(6,*)
Write(6,*) '       During the binning of ETOPO data, the length scale'
Write(6,*) '       eventually becomes sufficently small so that binlimit'
Write(6,*) '       can no longer be satisfied.  Under these circumstances'
Write(6,*) '       the code will use the minimum length scale of the'
Write(6,*) '       ETOPO dataset (e.g., 2km) for all data that is'
Write(6,*) '       subsequently binned.  In the case where the grid scale'
Write(6,*) '       is less than the minimum length scale of the ETOPO'
Write(6,*) '       dataset, the code will use the nearest grid point'
Write(6,*) '       instead of binning.'
Write(6,*)
Stop

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine determins the default values for the switches
!

Subroutine defaults(options,nopts)

Implicit None

Integer nopts
Character(len=*), dimension(nopts,2), intent(inout) :: options
Integer siz
Integer locate

siz=locate('-s',options(:,1),nopts)

If (options(siz,2).EQ.'') then
  options(siz,2)='500'
End if

Return
End


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine processes the sib data
!

Subroutine createocn(options,nopts,fname,fastocn,binlimit)

Use ccinterp

Implicit None

Logical, intent(in) :: fastocn
Integer, intent(in) :: nopts,binlimit
Character(len=*), dimension(1:nopts,1:2), intent(in) :: options
Character(len=*), dimension(1:2), intent(in) :: fname
Character*80 returnoption,csize
Character*45 header
Character*9 formout
real, dimension(:,:,:), allocatable :: rlld
Real, dimension(:,:), allocatable :: gridout,lsdata,ocndata,topdata,depth
Real, dimension(1:2) :: lonlat
Real schmidt,dsx,ds
Integer, dimension(1:2) :: sibdim
Integer sibsize,tunit,i,j,k,ierr,ilout

csize=returnoption('-s',options,nopts)
Read(csize,FMT=*,IOSTAT=ierr) sibsize
If (ierr.NE.0) then
  Write(6,*) 'ERROR: Invalid array size.  Must be an integer.'
  Stop
End if

! Read topography file
tunit=1
Open(tunit,FILE=fname(1),FORM='formatted',STATUS='old',IOSTAT=ierr)
Read(tunit,*,IOSTAT=ierr) sibdim(1),sibdim(2),lonlat(1),lonlat(2),schmidt,dsx,header

Write(6,*) "Dimension : ",sibdim
Write(6,*) "lon0,lat0 : ",lonlat
Write(6,*) "Schmidt   : ",schmidt
Allocate(gridout(sibdim(1),sibdim(2)),rlld(sibdim(1),sibdim(2),2))
Allocate(ocndata(sibdim(1),sibdim(2)),topdata(sibdim(1),sibdim(2)))
Allocate(lsdata(sibdim(1),sibdim(2)),depth(sibdim(1),sibdim(2)))

ilout=Min(sibdim(1),30) ! To be compatiable with terread
Write(formout,'("(",i3,"f7.0)")',IOSTAT=ierr) ilout
Read(tunit,formout,IOSTAT=ierr) topdata ! read topography
Write(formout,'("(",i3,"f4.1)")',IOSTAT=ierr) ilout
Read(tunit,formout,IOSTAT=ierr) lsdata ! Read ls mask
Close(tunit)

! Determine lat/lon to CC mapping
Call ccgetgrid(rlld,gridout,sibdim,lonlat,schmidt,ds)

! Read ETOPO data
Call getdata(ocndata,lonlat,gridout,rlld,sibdim,sibsize,fastocn,binlimit)

! Calculate depth
where (lsdata.lt.0.5)
  depth=max(topdata/9.8-ocndata,10.)
elsewhere
  depth=0.
end where

Write(6,*) 'Write depth data'
Write(formout,'("(",i3,"f6.0)" )') sibdim(1)
Open(1,File=fname(2))
Write(1,'(i3,i4,2f8.3,f6.3,f8.0," ",a39)') sibdim(1),sibdim(2),lonlat(1),lonlat(2),schmidt,ds,'depth'
Write(1,formout) min(depth,9999.)
Close(1)

Deallocate(gridout,rlld,ocndata,topdata,lsdata,depth)

Return
End

