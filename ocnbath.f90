Program ocnbath

! This code creates CCAM ocean depth data using the ETOPO2 dataset

Implicit None

Character*80, dimension(:,:), allocatable :: options
Character*80, dimension(2) :: fname
Character*80 topofile,bathout
Integer binlimit, nopts
Logical fastocn

Namelist/ocnnml/ topofile,bathout,fastocn,binlimit

Write(6,*) 'OCNBATH - ETOPO 2km to CC grid (FEB-13)'

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

subroutine defaults(options,nopts)

implicit None

integer nopts
character(len=*), dimension(nopts,2), intent(inout) :: options
integer siz
integer locate

siz=locate('-s',options(:,1),nopts)

if (options(siz,2)=='') then
  options(siz,2)='500'
end if

return
end


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine processes the sib data
!

subroutine createocn(options,nopts,fname,fastocn,binlimit)

use ccinterp

implicit None

integer, intent(in) :: nopts,binlimit
integer, dimension(2) :: sibdim
integer, dimension(4) :: dimnum,dimid,dimcount
integer, dimension(6) :: adate
integer, dimension(0:4) :: ncidarr
integer sibsize,tunit,i,j,k,ierr
integer varid
real, dimension(:,:,:), allocatable :: rlld
real, dimension(:,:), allocatable :: gridout,lsdata,ocndata,topdata,depth
real, dimension(3,2) :: alonlat
real, dimension(2) :: lonlat
real, dimension(1) :: alvl
real, dimension(1) :: atime
real schmidt,dsx,ds
character(len=*), dimension(1:nopts,1:2), intent(in) :: options
character(len=*), dimension(1:2), intent(in) :: fname
character*80, dimension(1:3) :: outputdesc
character*80 returnoption,csize
character*45 header
character*10 formout
logical, intent(in) :: fastocn

csize=returnoption('-s',options,nopts)
read(csize,FMT=*,IOSTAT=ierr) sibsize
if (ierr/=0) then
  write(6,*) 'ERROR: Invalid array size.  Must be an integer.'
  stop
end if

! Read topography file
tunit=1
call readtopography(tunit,fname(1),sibdim,lonlat,schmidt,dsx,header)

write(6,*) "Dimension : ",sibdim
write(6,*) "lon0,lat0 : ",lonlat
write(6,*) "Schmidt   : ",schmidt
allocate(gridout(sibdim(1),sibdim(2)),rlld(sibdim(1),sibdim(2),2))
allocate(ocndata(sibdim(1),sibdim(2)),topdata(sibdim(1),sibdim(2)))
allocate(lsdata(sibdim(1),sibdim(2)),depth(sibdim(1),sibdim(2)))

Call gettopols(tunit,fname(1),lsdata,sibdim)

! Determine lat/lon to CC mapping
call ccgetgrid(rlld,gridout,sibdim,lonlat,schmidt,ds)

! Read ETOPO data
call getdata(ocndata,lonlat,gridout,rlld,sibdim,sibsize,fastocn,binlimit)

! Calculate depth
where (lsdata<0.5)
  depth=max(topdata/9.8-ocndata,1.)
elsewhere
  depth=0.
end where

! Prep netcdf output
Write(6,*) 'Write depth data'
dimnum(1:2)=sibdim(1:2) ! CC grid dimensions
dimnum(3)=1             ! Turn off level
dimnum(4)=1             ! Number months
adate=0                 ! Turn off date
adate(2)=1              ! time units=months
call ncinitcc(ncidarr,fname(2),dimnum(1:3),dimid,adate)
outputdesc=(/ 'depth', 'Bathymetry', 'm' /)
call ncaddvargen(ncidarr,outputdesc,5,2,varid,1.,0.)
call ncatt(ncidarr,'lon0',lonlat(1))
call ncatt(ncidarr,'lat0',lonlat(2))
call ncatt(ncidarr,'schmidt',schmidt)
call ncenddef(ncidarr)
alonlat(:,1)=(/ 1., real(sibdim(1)), 1. /)
alonlat(:,2)=(/ 1., real(sibdim(2)), 1. /)
alvl=1.
atime(1)=0.
call nclonlatgen(ncidarr,dimid,alonlat,alvl,atime,dimnum)

dimcount=(/ sibdim(1), sibdim(2), 1, 1 /)
call ncwritedatgen(ncidarr,depth,dimcount,varid)
call ncclose(ncidarr)

deallocate(gridout,rlld,ocndata,topdata,lsdata,depth)

Return
End

