! Conformal Cubic Atmospheric Model
    
! Copyright 2015-2018 Commonwealth Scientific Industrial Research Organisation (CSIRO)
    
! This file is part of the Conformal Cubic Atmospheric Model (CCAM)
!
! CCAM is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! CCAM is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with CCAM.  If not, see <http://www.gnu.org/licenses/>.

!------------------------------------------------------------------------------
    
program ocnbath

! This code creates CCAM ocean depth data using the ETOPO2 dataset

implicit None

include 'version.h'

character*1024, dimension(:,:), allocatable :: options
character*1024, dimension(4) :: fname
character*1024 topofile,bathout,bathdatafile,riverdatapath
integer :: binlimit = 4
integer nopts
logical :: fastocn = .true.
logical :: bathfilt = .false.

namelist/ocnnml/ topofile,bathout,fastocn,binlimit,bathfilt,bathdatafile,riverdatapath

! Start banner
write(6,*) "=============================================================================="
write(6,*) "CCAM: Starting ocnbath"
write(6,*) "=============================================================================="


#ifndef stacklimit
! For linux only - removes stacklimit on all processors
call setstacklimit(-1)
#endif 

Write(6,*) 'OCNBATH - ETOPO 2km to CC grid'
write(6,*) version

! Read switches
nopts=1
allocate (options(nopts,2))
options(:,1) = (/ '-s' /)
options(:,2) = ''

call readswitch(options,nopts)
call defaults(options,nopts)

bathdatafile="etopo1_ice_c.flt"
riverdatapath=""

! Read namelist
write(6,*) 'Input &ocnnml namelist'
read(5,NML=ocnnml)
write(6,*) 'Namelist accepted'

! Generate veg data
fname(1)=topofile
fname(2)=bathout
fname(3)=bathdatafile
fname(4)=riverdatapath

call createocn(options,nopts,fname,fastocn,bathfilt,binlimit)

deallocate(options)

! Complete
write(6,*) "CCAM: ocnbath completed successfully"
call finishbanner

stop
end

subroutine finishbanner

implicit none

! End banner
write(6,*) "=============================================================================="
write(6,*) "CCAM: Finished ocnbath"
write(6,*) "=============================================================================="

return
end

    
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
Write(6,*) '    bathdatafile="etopo1_ice_c.flt"'
Write(6,*) '    riverdatapath=""'
Write(6,*) '    fastocn=t'
Write(6,*) '    bathfilt=t'
Write(6,*) '    binlimit=4'
Write(6,*) '  &end'
Write(6,*)
Write(6,*) '  where:'
Write(6,*) '    topofile      = topography (input) file'
Write(6,*) '    bathout       = Depth filename'
Write(6,*) '    fastocn       = Turn on fastocn mode (see notes below)'
Write(6,*) '    bathfilt      = Filter bathymetry'
Write(6,*) '    bathdatafile  = Location of input bathymetry data'
Write(6,*) '    riverdatapath = Location of input river accumulation data'
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
Write(6,*) '       bathfilt will smooth the bathymetry for better'
Write(6,*) '       agreement with the assumtions of the semi-Lagrangian'
Write(6,*) '       advection'
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
call finishbanner
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

subroutine createocn(options,nopts,fname,fastocn,bathfilt,binlimit)

use ccinterp

implicit None

integer, intent(in) :: nopts,binlimit
integer, dimension(2) :: sibdim
integer, dimension(4) :: dimnum,dimid,dimcount
integer, dimension(2) :: varid
integer, dimension(6) :: adate
integer, dimension(0:4) :: ncidarr
integer, dimension(:,:), allocatable :: in,ie,is,iw
integer sibsize,tunit,i,j,k,ierr
integer inx,iny,isx,isy,iex,iey,iwx,iwy
integer nfilt,dum_count
integer, parameter :: nfiltmax = 1
real, dimension(:,:,:), allocatable :: rlld
real, dimension(:,:), allocatable :: gridout,lsdata,ocndata,topdata,depth,dum
real, dimension(:,:), allocatable :: riveracc
real, dimension(3,2) :: alonlat
real, dimension(2) :: lonlat
real, dimension(1) :: alvl
real, dimension(1) :: atime
real schmidt,dsx,ds
real rmax,lmax,dum_sum,dum_ave
character(len=*), dimension(1:nopts,1:2), intent(in) :: options
character(len=*), dimension(1:4), intent(in) :: fname
character*80, dimension(1:3) :: outputdesc
character*1024 returnoption,csize
character*45 header
character*10 formout
logical, intent(in) :: fastocn, bathfilt

csize=returnoption('-s',options,nopts)
read(csize,FMT=*,IOSTAT=ierr) sibsize
if (ierr/=0) then
  write(6,*) 'ERROR: Invalid array size.  Must be an integer.'
  call finishbanner
  stop -1
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
allocate(in(sibdim(1),sibdim(2)),ie(sibdim(1),sibdim(2)))
allocate(is(sibdim(1),sibdim(2)),iw(sibdim(1),sibdim(2)))
allocate(riveracc(sibdim(1),sibdim(2)))
allocate(dum(sibdim(1),sibdim(2)))

Call gettopohgt(tunit,fname(1),topdata,lsdata,sibdim)
lsdata = 1. - lsdata

! Determine lat/lon to CC mapping
call cgg2(rlld,gridout,sibdim,lonlat,schmidt,ds,in,ie,is,iw)

! Read ETOPO data
call getdata(ocndata,lonlat,gridout,rlld,sibdim,sibsize,fastocn,binlimit,fname(3),'bath')

! Calculate depth
where ( lsdata<0.5 )
  depth = max(topdata/9.8-ocndata, 1.)
elsewhere
  depth = 0.
end where

! Filter bathymetry
if (bathfilt) then

  rmax = 1.
  do while ( rmax>0.2 )

    do nfilt=1,nfiltmax
      dum=depth
      do j=1,sibdim(2)
        do i=1,sibdim(1)
          iny=(in(i,j)-1)/sibdim(1)+1
          inx=in(i,j)-(iny-1)*sibdim(1)
          isy=(is(i,j)-1)/sibdim(1)+1
          isx=is(i,j)-(isy-1)*sibdim(1)
          iey=(ie(i,j)-1)/sibdim(1)+1
          iex=ie(i,j)-(iey-1)*sibdim(1)
          iwy=(iw(i,j)-1)/sibdim(1)+1
          iwx=iw(i,j)-(iwy-1)*sibdim(1)
          if (dum(i,j)>0.01) then
            dum_sum = 0.
            dum_count = 0
            if ( dum(inx,iny)>0.01 ) then
              dum_sum = dum_sum + dum(inx,iny)
              dum_count = dum_count + 1
            end if
            if ( dum(isx,isy)>0.01 ) then
              dum_sum = dum_sum + dum(isx,isy)
              dum_count = dum_count + 1
            end if
            if ( dum(iex,iey)>0.01 ) then
              dum_sum = dum_sum + dum(iex,iey)
              dum_count = dum_count + 1
            end if
            if ( dum(iwx,iwy)>0.01 ) then
              dum_sum = dum_sum + dum(iwx,iwy)
              dum_count = dum_count + 1
            end if
            if ( dum_count>0 ) then
              dum_ave = dum_sum/real(dum_count)
              depth(i,j)=0.5*dum_ave+0.5*dum(i,j)
            end if  
          end if
        end do
      end do
    end do

    ! check r factor
    rmax = 0.
    do j = 1,sibdim(2)
      do i = 1,sibdim(1)
        if ( depth(i,j)>0.01 ) then  
          iny=(in(i,j)-1)/sibdim(1)+1
          inx=in(i,j)-(iny-1)*sibdim(1)
          iey=(ie(i,j)-1)/sibdim(1)+1
          iex=ie(i,j)-(iey-1)*sibdim(1)
          if ( depth(inx,iny)>0.01 .and. depth(iex,iey)>0.01 ) then
            lmax = max( abs(depth(inx,iny)-depth(i,j))/(depth(inx,iny)+depth(i,j)), abs(depth(iex,iey)-depth(i,j))/(depth(iex,iey)+depth(i,j)) )
            rmax = max( rmax, lmax )
            !if ( lmax>0.2 ) then
            !  write(6,*) "Exceed rmax = ",lmax
            !  write(6,*) "depth(n),depth(e),depth ",depth(inx,iny),depth(iex,iey),depth(i,j)
            !end if
          end if  
        end if  
      end do
    end do
    write(6,*) "rmax = ",rmax

  end do
  
end if

! calculate river routing directions
call getdata(riveracc,lonlat,gridout,rlld,sibdim,sibsize,fastocn,binlimit,fname(4),'river')

! Prep netcdf output
Write(6,*) 'Write depth data'
dimnum(1:2)=sibdim(1:2) ! CC grid dimensions
dimnum(3)=1             ! Turn off level
dimnum(4)=1             ! Number months
adate=0                 ! Turn off date
adate(2)=1              ! time units=months
call ncinitcc(ncidarr,fname(2),dimnum(1:3),dimid,adate)
outputdesc(1)='depth'
outputdesc(2)='Bathymetry'
outputdesc(3)='m'
call ncaddvargen(ncidarr,outputdesc,5,2,varid(1),1.,0.)
outputdesc(1)='riveracc'
outputdesc(2)='River accumulation'
outputdesc(3)='none'
call ncaddvargen(ncidarr,outputdesc,5,2,varid(2),1.,0.)
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
call ncwritedatgen(ncidarr,depth,dimcount,varid(1))
call ncwritedatgen(ncidarr,riveracc,dimcount,varid(2))

call ncclose(ncidarr)

deallocate(gridout,rlld,ocndata,topdata,lsdata,depth)
deallocate(in,is,ie,iw)
deallocate(riveracc)
deallocate(dum)
  
Return
End subroutine
