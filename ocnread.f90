! Conformal Cubic Atmospheric Model
    
! Copyright 2015-2016 Commonwealth Scientific Industrial Research Organisation (CSIRO)
    
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
    
! This subroutine is to extract (in memory) data from the ETOPO dataset.
!

Subroutine getdata(dataout,glonlat,grid,tlld,sibdim,sibsize,fastocn,binlimit,bathdatafile,datatype)

Use ccinterp

Implicit None

Integer, intent(in) :: sibsize,binlimit
Integer, dimension(2), intent(in) :: sibdim
Integer, dimension(sibdim(1),sibdim(2)) :: countn
Integer, dimension(2) :: lldim,lldim_x,llstore,pxy
Integer nscale,nscale_x,nface,subsec,mode,tmp
Integer i,j,k,lci,lcj,nx,ny,netcount
Integer basesize,scalelimit,minscale
Integer iadj,jadj
Real, dimension(sibdim(1),sibdim(2)), intent(out) :: dataout
Real, dimension(sibdim(1),sibdim(2)), intent(in) :: grid
Real, dimension(sibdim(1),sibdim(2),2), intent(in) :: tlld
Real, dimension(sibdim(1),sibdim(2),2) :: rlld
Real, dimension(sibdim(1),sibdim(2)) :: zsum
Real, dimension(2), intent(in) :: glonlat
Real, dimension(:,:), allocatable :: coverout
Real, dimension(2) :: latlon
Real, dimension(2,2) :: sll
Real, dimension(2,2) :: rdat
Real aglon,aglat,alci,alcj,serlon,serlat,slonn,slatx,elon,elat,tscale,baselon
Real ipol,callon,callat,indexlon,indexlat
Logical, intent(in) :: fastocn
Logical, dimension(:,:), allocatable :: sermask
logical, dimension(sibdim(1),sibdim(2)) :: ctest
character(len=*), intent(in) :: bathdatafile, datatype

dataout=0.
countn=0

! Determine scale limits
nscale=999

baselon=real(int(glonlat(1)-180.))
rlld=tlld
Do While (Any(rlld(:,:,1).LT.baselon))
  Where (rlld(:,:,1).LT.baselon)
    rlld(:,:,1)=rlld(:,:,1)+360.
  End where
End do
Do While (Any(rlld(:,:,1).GT.(baselon+360.)))
  Where (rlld(:,:,1).GT.(baselon+360.))
    rlld(:,:,1)=rlld(:,:,1)-360.
  End where
End do

select case(datatype)
  case('bath')
    Write(6,*) 'Process ETOPO1 dataset'
    scalelimit=2
  case('river')
    write(6,*) 'Process Hydrosheds dataset'
    scalelimit=1
  case default
    write(6,*) "ERROR: Unknown dataset ",trim(datatype)
    call finishbanner
    Stop -1
end select

If (fastocn) then

  ! Step over scales
  mode=0
  Do While (Any(countn.EQ.0).AND.(nscale.GT.scalelimit))

    latlon=(/ baselon, 90. /)
    ctest = countn==0
    Call findsmallscale(nscale,scalelimit,latlon,llstore,grid,ctest,rlld,subsec,sll,sibsize,sibdim)

    slonn=sll(1,1)
    slatx=sll(2,2)

    minscale=nscale*binlimit

    Write(6,*) 'Bin'
    Write(6,*) 'nscale       = ',nscale
    Write(6,*) 'subsec       = ',subsec
    Write(6,*) 'sll          = ',sll
    Write(6,*) 'llstore      = ',llstore

    If (subsec.NE.0) then

      Do nx=1,subsec
        Do ny=1,subsec

          Write(6,*) 'nx,ny,subsec = ',nx,ny,subsec
      
          lldim=llstore
          ! Determine top corner lat/lon
          Call latlonconvert(nscale,latlon,lldim,slonn,slatx,nx,ny)
      
          Write(6,*) 'orig latlon  = ',latlon
          Write(6,*) 'orig lldim   = ',lldim

          ! Check if there are any points of interest on this tile
          ctest = countn==0
          Call searchdim(mode,sll,nscale,real(nscale),latlon,lldim,grid,ctest,rlld,sibdim)
          Call scaleconvert(nscale,tmp,lldim,sll,sibsize)
          mode=2
      
          latlon(1)=sll(1,1)
          latlon(2)=sll(2,2)

          Write(6,*) 'mod latlon   = ',latlon
          Write(6,*) 'mod lldim    = ',lldim

          ! Bin
          If (All(lldim.GT.0)) then

            Allocate(coverout(lldim(1),lldim(2)))
	  
            select case(datatype)
              case('bath')
	            Call kmconvert(nscale,nscale_x,lldim,lldim_x,2)
                Call ocnread(latlon,nscale_x,lldim_x,coverout,bathdatafile)
              case('river')
                call riverread(latlon,nscale,lldim,coverout,bathdatafile)
              case default
                Write(6,*) 'ERROR: Cannot find data ',trim(datatype)
                call finishbanner
                Stop -1
            end select

            Write(6,*) 'Start bin'
            if ( datatype=='river' ) then
              Do i=1,lldim(1)
                Do j=1,lldim(2)
                  aglon=callon(latlon(1),i,nscale)
                  aglat=callat(latlon(2),j,nscale)
                  Call lltoijmod(aglon,aglat,alci,alcj,nface)
                  lci = nint(alci)
                  lcj = nint(alcj)
                  lcj = lcj+nface*sibdim(1)
                  If (grid(lci,lcj).GE.real(minscale)) then
                    dataout(lci,lcj)=max(dataout(lci,lcj),coverout(i,j))
                    countn(lci,lcj)=1
                  End if
                End Do
              End Do
            else ! usual
              Do i=1,lldim(1)
                Do j=1,lldim(2)
                  aglon=callon(latlon(1),i,nscale)
                  aglat=callat(latlon(2),j,nscale)
                  Call lltoijmod(aglon,aglat,alci,alcj,nface)
                  lci = nint(alci)
                  lcj = nint(alcj)
                  lcj = lcj+nface*sibdim(1)
                  If (grid(lci,lcj).GE.real(minscale)) then
                    dataout(lci,lcj)=dataout(lci,lcj)+coverout(i,j)
                    countn(lci,lcj)=countn(lci,lcj)+1
                  End if
                End Do
              End Do
            end if
            Write(6,*) 'Bin complete'

            Deallocate(coverout)

          Else
            Write(6,*) 'No points in valid range'
          End If
      
        End Do
      End Do

    Else
      Write(6,*) 'Skip'
    End If
  
  End Do

Else

  select case(datatype)
    case('bath')
      Call ocnstream(sibdim,dataout,countn,bathdatafile)
    case('river')
      call riverstream(sibdim,dataout,countn,bathdatafile)
    Case DEFAULT
      Write(6,*) 'ERROR: Cannot find data ',trim(datatype)
      call finishbanner
      Stop -1
  end select

End If

! Interpolate
Write(6,*) 'Interpolate'
Allocate(sermask(2,2))
nscale=scalelimit

latlon=(/ baselon, 90. /)
llstore=(/ 43200/nscale , 21600/nscale /)
ctest = countn==0
Call searchdim(4,sll,nscale,0.,latlon,llstore,grid,ctest,rlld,sibdim)
Call scaleconvert(nscale,subsec,llstore,sll,sibsize)
slonn=sll(1,1)
slatx=sll(2,2)

Write(6,*) 'nscale       = ',nscale
Write(6,*) 'subsec       = ',subsec
Write(6,*) 'sll          = ',sll
Write(6,*) 'llstore      = ',llstore

If (subsec.NE.0) then
  Do nx=1,subsec
    Do ny=1,subsec

      Write(6,*) 'nx,ny,subsec = ',nx,ny,subsec

      lldim=llstore
      Call latlonconvert(nscale,latlon,lldim,slonn,slatx,nx,ny)

      Write(6,*) 'orig latlon  = ',latlon
      Write(6,*) 'orig lldim   = ',lldim

      ! Check if there are any points of interest on this tile
      ctest = countn==0
      Call searchdim(4,sll,nscale,0.,latlon,lldim,grid,ctest,rlld,sibdim)
      Call scaleconvert(nscale,tmp,lldim,sll,sibsize)
      If (Any(lldim(:).EQ.1)) lldim=0
      
      latlon(1)=sll(1,1)
      latlon(2)=sll(2,2)

      Write(6,*) 'mod latlon   = ',latlon
      Write(6,*) 'mod lldim    = ',lldim

      If ((lldim(1).GT.0).AND.(lldim(2).GT.0)) then

        Allocate(coverout(lldim(1),lldim(2)))
	
        select case(datatype)
          case('bath')
            Call kmconvert(nscale,nscale_x,lldim,lldim_x,2)	
            Call ocnread(latlon,nscale_x,lldim_x,coverout,bathdatafile)
          case('river')
            call riverread(latlon,nscale,lldim,coverout,bathdatafile)
          case default
            Write(6,*) 'ERROR: Cannot find data ',trim(datatype)
            call finishbanner
            Stop -1
        end select

        if ( datatype=='river' ) then
          Do lcj=1,sibdim(2)
            Do lci=1,sibdim(1)
              If (countn(lci,lcj)==0) then
                ! unassigned
                dataout(lci,lcj)=0
                countn(lci,lcj)=1
              End If
            End Do
          End Do
        else ! usual
          Do lcj=1,sibdim(2)
            Do lci=1,sibdim(1)
              If (countn(lci,lcj)==0) then
                aglon=rlld(lci,lcj,1)
                aglat=rlld(lci,lcj,2)
                serlon=indexlon(aglon,latlon(1),nscale)
                serlat=indexlat(aglat,latlon(2),nscale)
                i=nint(serlon)
                j=nint(serlat)
                if (i>0.and.i<=lldim(1).and.j>0.and.j<=lldim(2)) then
                  ! fill
                  !dataout(lci,lcj)=coverout(i,j)
                  !countn(lci,lcj)=1
                  ! interpolate
                  serlon = serlon - real(i)
                  serlat = serlat - real(j)                  
                  iadj = nint(sign(1.,serlon))
                  jadj = nint(sign(1.,serlat))
                  serlon = abs(serlon)
                  serlat = abs(serlat)
                  iadj = max(min(i+iadj,lldim(1)),1)
                  jadj = max(min(j+jadj,lldim(2)),1)
                  rdat(1,1) = coverout(i,   j   )
                  rdat(2,1) = coverout(iadj,j   )
                  rdat(1,2) = coverout(i,   jadj)
                  rdat(2,2) = coverout(iadj,jadj)
                  dataout(lci,lcj) = ipol(rdat,serlon,serlat)
                  countn(lci,lcj) = 1
                end if
              End If
            End Do
          End Do
        end if
        Deallocate(coverout)

      Else
        Write(6,*) 'No points in valid range'
      End If
    End Do
  End Do

Else
  Write(6,*) 'Skip'
End If

Deallocate(sermask)

dataout=dataout/Real(countn)

Write(6,*) "Task complete"

Return
End


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads bathymetry data down to nscale=2km resolution
!

Subroutine ocnread(latlon,nscale,lldim,coverout,bathdatafile)

Implicit None

Integer, intent(in) :: nscale
Real, dimension(2), intent(in) :: latlon
Integer, dimension(2), intent(in) :: lldim
Real, dimension(lldim(1),lldim(2)), intent(out) :: coverout
real rtmp
real, dimension(21600,nscale) :: databuffer
real, dimension(21600) :: datatemp
Integer, dimension(2,2) :: jin,jout
Integer ilat,ilon,jlat,recpos,i,j
Integer, dimension(2) :: llint
character(len=*), intent(in) :: bathdatafile

! Must be compiled using 1 byte record lengths
Open(10,FILE=bathdatafile,ACCESS='DIRECT',FORM='UNFORMATTED',RECL=86400,CONVERT='LITTLE_ENDIAN',STATUS='OLD')

Call solvejshift(latlon(1),jin,jout,60)

coverout=0.

Do ilat=1,lldim(2)

  if ((mod(ilat,10).eq.0).or.(ilat.eq.lldim(2))) then
    Write(6,*) 'ETOPO - ',ilat,'/',lldim(2)
  end if
  
  ! Read data
  llint(2)=nint((90.-latlon(2))*60.)+(ilat-1)*nscale
  Do jlat=1,nscale
    recpos=llint(2)+jlat
    Read(10,REC=recpos) datatemp
    ! Shift lon to zero
    databuffer(jin(1,1):jin(1,2),jlat)=datatemp(jout(1,1):jout(1,2))
    databuffer(jin(2,1):jin(2,2),jlat)=datatemp(jout(2,1):jout(2,2))
  End Do

  Do ilon=1,lldim(1)
    llint(1)=(ilon-1)*nscale
    coverout(ilon,ilat)=sum(databuffer(llint(1)+1:llint(1)+nscale,1:nscale))/real(nscale*nscale)
  End Do
 
End Do

close(10)

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads river accumulation data down to nscale=1km resolution
!

Subroutine riverread(latlon,nscale,lldim,coverout,riverdatapath)

Implicit None

integer, parameter :: maxidom = 7

Integer, intent(in) :: nscale
Real, dimension(2), intent(in) :: latlon
Integer, dimension(2), intent(in) :: lldim
Real, dimension(lldim(1),lldim(2)), intent(out) :: coverout
real rtmp, rlat, rlon
integer, dimension(43200,nscale) :: databuffer
integer(kind=4), dimension(43200) :: datatemp
real, dimension(maxidom,4) :: domll
Integer, dimension(2,2) :: jin,jout
Integer ilat,ilon,jlat,recpos,i,j
integer posx_beg, posx_end, recpos_local
integer idom
Integer, dimension(2) :: llint
integer, dimension(maxidom,2) :: domsize
character(len=*), intent(in) :: riverdatapath
character(len=20), dimension(maxidom) :: domname

domname(1) = "af_acc_30s.bil"
domsize(1,1) = 8880
domsize(1,2) = 8760
domll(1,1) = -19.
domll(1,2) = 55.
domll(1,3) = -35.
domll(1,4) = 38.
domname(2) = "as_acc_30s.bil"
domsize(2,1) = 14760
domsize(2,2) = 8760
domll(2,1) = 57.
domll(2,2) = 180.
domll(2,3) = -12.
domll(2,4) = 61.
domname(3) = "au_acc_30s.bil"
domsize(3,1) = 8160
domsize(3,2) = 5520
domll(3,1) = 112.
domll(3,2) = 180.
domll(3,3) = -56.
domll(3,4) = -10.
domname(4) = "ca_acc_30s.bil"
domsize(4,1) = 7080
domsize(4,2) = 4080
domll(4,1) = -119.
domll(4,2) = -60.
domll(4,3) = 5.
domll(4,4) = 39.
domname(5) = "eu_acc_30s.bil"
domsize(5,1) = 10080
domsize(5,2) = 6000
domll(5,1) = -14.
domll(5,2) = 70.
domll(5,3) = 12.
domll(5,4) = 62.
domname(6) = "na_acc_30s.bil"
domsize(6,1) = 10320
domsize(6,2) = 4440
domll(6,1) = -138.
domll(6,2) = -52.
domll(6,3) = 24.
domll(6,4) = 61.
domname(7) = "sa_acc_30s.bil"
domsize(7,1) = 7320
domsize(7,2) = 8520
domll(7,1) = -93.
domll(7,2) = -32.
domll(7,3) = -56.
domll(7,4) = 12.


! Must be compiled using 1 byte record lengths
do idom = 1,maxidom
  if ( trim(riverdatapath)/='' ) then
    Open(10+idom,FILE=trim(riverdatapath)//'/'//trim(domname(idom)),access='direct',form='unformatted',recl=domsize(idom,1)*4,convert='LITTLE_ENDIAN',status='old')
  else
    Open(10+idom,FILE=trim(domname(idom)),access='direct',form='unformatted',recl=domsize(idom,1)*4,convert='LITTLE_ENDIAN',status='old')  
  end if
end do

Call solvejshift(latlon(1),jin,jout,120)

coverout=0.

Do ilat=1,lldim(2)

  if ((mod(ilat,10).eq.0).or.(ilat.eq.lldim(2))) then
    Write(6,*) 'Hydrosheds - ',ilat,'/',lldim(2)
  end if
  
  ! Read data
  llint(2)=nint((90.-latlon(2))*120.)+(ilat-1)*nscale
  Do jlat=1,nscale
    recpos=llint(2)+jlat
    
    datatemp(:) = 0
    do idom = 1,maxidom
      rlat = 90. + (real(recpos)-0.5)*(-180./21600.)
      recpos_local = nint( 0.5 + (rlat-domll(idom,4))*(real(domsize(idom,2))/(domll(idom,3)-domll(idom,4))) )
      if ( recpos_local>=1 .and. recpos_local<=domsize(idom,2) ) then
        posx_beg = nint( (domll(idom,1)+180.)*(43200./360.) + 0.5001 )
        posx_end = nint( (domll(idom,2)+180.)*(43200./360.) - 0.5001 )
        Read(10+idom,REC=recpos_local) datatemp(posx_beg:posx_end)
      end if
    end do
    
    ! Shift lon to zero
    databuffer(jin(1,1):jin(1,2),jlat)=datatemp(jout(1,1):jout(1,2))
    databuffer(jin(2,1):jin(2,2),jlat)=datatemp(jout(2,1):jout(2,2))
  End Do

  Do ilon=1,lldim(1)
    llint(1)=(ilon-1)*nscale
    coverout(ilon,ilat)=real(maxval(databuffer(llint(1)+1:llint(1)+nscale,1:nscale)))
  End Do
 
End Do

do idom = 1,maxidom
  close(10+idom)
end do

Return
End
    
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads ETOPO data at nscale=2km resolution
! (i.e., no storage, simply read and bin)
!

Subroutine ocnstream(sibdim,coverout,countn,bathdatafile)

Use ccinterp

Implicit None

Integer, dimension(2), intent(in) :: sibdim
Real, dimension(sibdim(1),sibdim(2)), intent(out) :: coverout
Real aglon,aglat,alci,alcj
Real callon,callat
Integer, dimension(sibdim(1),sibdim(2)), intent(out) :: countn
Real, dimension(21600) :: databuffer
Integer ilat,ilon,lci,lcj,nface
character(len=*), intent(in) :: bathdatafile

coverout=0
countn=0

Write(6,*) "Read ETOPO data (stream)"

! Must be compiled using 1 byte record lengths
Open(10,FILE=bathdatafile,ACCESS='DIRECT',FORM='UNFORMATTED',RECL=86400,CONVERT='LITTLE_ENDIAN',STATUS='OLD')


Do ilat=1,10800

  if (mod(ilat,10).eq.0) then
    Write(6,*) 'ETOPO - ',ilat,'/ 10800'
  end if
  
  ! Read data
  Read(10,REC=ilat) databuffer
  aglat=callat(90.,ilat,2)
    
  Do ilon=1,21600
    
    aglon=callon(-180.,ilon,2)
    
    Call lltoijmod(aglon,aglat,alci,alcj,nface)
    lci = nint(alci)
    lcj = nint(alcj)
    lcj = lcj+nface*sibdim(1)
    
    coverout(lci,lcj)=coverout(lci,lcj)+databuffer(ilon)
    countn(lci,lcj)=countn(lci,lcj)+1
    
  End Do
End Do

Close(10)

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads Hydrosheds data at nscale=1km resolution
! (i.e., no storage, simply read and bin)
!
        
Subroutine riverstream(sibdim,coverout,countn,riverdatapath)

Use ccinterp

Implicit None

integer, parameter :: maxidom = 7

Integer, dimension(2), intent(in) :: sibdim
Real, dimension(sibdim(1),sibdim(2)), intent(out) :: coverout
Real aglon,aglat,alci,alcj
Real callon,callat
real rlat
real, dimension(maxidom,4) :: domll
Integer, dimension(sibdim(1),sibdim(2)), intent(out) :: countn
integer(kind=4), dimension(43200) :: databuffer
Integer ilat,ilon,lci,lcj,nface
integer recpos_local, posx_beg, posx_end
integer idom
integer, dimension(maxidom,2) :: domsize
character(len=*), intent(in) :: riverdatapath
character(len=20), dimension(maxidom) :: domname


coverout=0
countn=0

domname(1) = "af_acc_30s.bil"
domsize(1,1) = 8880
domsize(1,2) = 8760
domll(1,1) = -19.
domll(1,2) = 55.
domll(1,3) = -35.
domll(1,4) = 38.
domname(2) = "as_acc_30s.bil"
domsize(2,1) = 14760
domsize(2,2) = 8760
domll(2,1) = 57.
domll(2,2) = 180.
domll(2,3) = -12.
domll(2,4) = 61.
domname(3) = "au_acc_30s.bil"
domsize(3,1) = 8160
domsize(3,2) = 5520
domll(3,1) = 112.
domll(3,2) = 180.
domll(3,3) = -56.
domll(3,4) = -10.
domname(4) = "ca_acc_30s.bil"
domsize(4,1) = 7080
domsize(4,2) = 4080
domll(4,1) = -119.
domll(4,2) = -60.
domll(4,3) = 5.
domll(4,4) = 39.
domname(5) = "eu_acc_30s.bil"
domsize(5,1) = 10080
domsize(5,2) = 6000
domll(5,1) = -14.
domll(5,2) = 70.
domll(5,3) = 12.
domll(5,4) = 62.
domname(6) = "na_acc_30s.bil"
domsize(6,1) = 10320
domsize(6,2) = 4440
domll(6,1) = -138.
domll(6,2) = -52.
domll(6,3) = 24.
domll(6,4) = 61.
domname(7) = "sa_acc_30s.bil"
domsize(7,1) = 7320
domsize(7,2) = 8520
domll(7,1) = -93.
domll(7,2) = -32.
domll(7,3) = -56.
domll(7,4) = 12.


Write(6,*) "Read Hydroshed data (stream)"

! Must be compiled using 1 byte record lengths
do idom = 1,maxidom
  if ( riverdatapath/='' ) then
    Open(10+idom,FILE=trim(riverdatapath)//'/'//trim(domname(idom)),access='direct',form='unformatted',recl=domsize(idom,1)*4,convert='LITTLE_ENDIAN',status='old')
  else
    Open(10+idom,FILE=trim(domname(idom)),access='direct',form='unformatted',recl=domsize(idom,1)*4,convert='LITTLE_ENDIAN',status='old')  
  end if
end do

Do ilat=1,21600

  if (mod(ilat,10).eq.0) then
    Write(6,*) 'Hydroshed - ',ilat,'/ 21600'
  end if
  
  ! Read data
  databuffer(:) = 0
  do idom = 1,maxidom
    posx_beg = 1 + nint( (domll(idom,1)+180.)/360.*real(43200-1) )
    posx_end = 1 + nint( (domll(idom,2)+180.)/360.*real(43200-1) )  
    rlat = 90. + -180.*real(ilat-1)/real(21600-1)
    recpos_local = 1 + nint( (rlat-domll(idom,4))/(domll(idom,3)-domll(idom,4))*real(domsize(idom,2)-1) )
    if ( recpos_local>=1 .and. recpos_local<=domsize(idom,2) ) then
      Read(10+idom,REC=recpos_local) databuffer(posx_beg:posx_end)
    end if
  end do
  aglat=callat(90.,ilat,1)
    
  Do ilon=1,43200
    
    aglon=callon(-180.,ilon,1)
    
    Call lltoijmod(aglon,aglat,alci,alcj,nface)
    lci = nint(alci)
    lcj = nint(alcj)
    lcj = lcj+nface*sibdim(1)
    
    coverout(lci,lcj)=max(coverout(lci,lcj),real(databuffer(ilon)))
    countn(lci,lcj)=1
    
  End Do
End Do

do idom = 1,maxidom
  close(10+idom)
end do

Return
End
    
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine aligns the data with the requested lat/lon
!

Subroutine solvejshift(lonin,jin,jout,nscale)

Implicit None

Real, intent(in) :: lonin
Integer, intent(in) :: nscale
Integer, dimension(2,2), intent(out) :: jin,jout
Integer jshift

jshift=Mod(nint(lonin*real(nscale))+180*nscale,360*nscale)
If (jshift.LT.0) jshift=jshift+360*nscale
jin(1,1)=1
jout(1,1)=Mod(jin(1,1)+jshift,360*nscale)
If (jout(1,1).EQ.0) jout(1,1)=360*nscale
jout(1,2)=360*nscale
jin(1,2)=jout(1,2)-jout(1,1)+jin(1,1)
jin(2,1)=jin(1,2)+1
jin(2,2)=360*nscale
jout(2,1)=1
jout(2,2)=jout(1,1)-1
If (jin(2,1).GT.jin(2,2)) then
  jin(2,:)=jin(1,:)
  jout(2,:)=jout(1,:)
End if

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine converts from 1km nscale to adj km nscale
!

Subroutine kmconvert(nscale,nscale_x,lldim,lldim_x,adj)

Implicit None

Integer, intent(in) :: nscale,adj
Integer, intent(out) :: nscale_x
Integer, dimension(1:2), intent(in) :: lldim
Integer, dimension(1:2), intent(out) :: lldim_x
Integer i

nscale_x=Int(nscale/adj)
If (nscale_x.LT.1) nscale_x=1

Do i=1,2
  lldim_x(i)=Int(Real(lldim(i))*Real(nscale)/(Real(nscale_x)*real(adj)))
  If (lldim_x(i).LT.1) lldim_x(i)=1
End Do

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine determines how many subsections (blocks) the data
! needs to be broken into so as to fit into memory (i.e., sibsize)
!

Subroutine scaleconvert(nscale,subsec,lldim,sll,sibsize)

Implicit None

Integer, intent(out) :: subsec
Integer, intent(in) :: nscale,sibsize
Integer, dimension(1:2), intent(out) :: lldim
Real, dimension(1:2,1:2), intent(in) :: sll
Integer i,j

i=nint((sll(1,2)-sll(1,1))*120./Real(nscale))
j=nint((sll(2,2)-sll(2,1))*120./Real(nscale))

subsec=int(sqrt(real(i)*real(j)/(real(sibsize)**2)))+1
If (subsec.LT.1) subsec=1
  
lldim(1)=nint(real(i)/real(subsec))
lldim(2)=nint(real(j)/real(subsec))

If ((real(lldim(1)*nscale*subsec)).LT.((sll(1,2)-sll(1,1))*120.)) lldim(1)=lldim(1)+1
If ((real(lldim(2)*nscale*subsec)).LT.((sll(2,2)-sll(2,1))*120.)) lldim(2)=lldim(2)+1
If ((nint((90.-sll(2,2))*120.)+lldim(2)*nscale).GT.21600) lldim(2)=(21600-nint((90.-sll(2,2))*120.))/nscale
If ((lldim(1)*nscale).GT.43200) lldim(1)=43200/nscale

If ((lldim(1).LT.1).OR.(lldim(2).LT.1)) Then
  lldim=(/ 0, 0 /)
  subsec=0
End If

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine adjusts the latlon array for a specified subsection
!

Subroutine latlonconvert(nscale,latlon,lldim,slonn,slatx,nx,ny)

Implicit None

Integer, intent(in) :: nscale
Real, dimension(1:2), intent(out) :: latlon
Integer, dimension(1:2), intent(in) :: lldim
Real, intent(in) :: slonn,slatx
Integer, intent(in) :: nx,ny

latlon=(/ slonn+Real((nx-1)*lldim(1)*nscale)/120., slatx-Real((ny-1)*lldim(2)*nscale)/120. /)
if (latlon(2).LT.-90.) latlon(2)=-90.

Return
End


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This function calculates lon from an array index
!

Real function callon(latlon,i,nscale)

Implicit None

Real, intent(in) :: latlon
Integer, intent(in) :: i,nscale

callon=(Real(i-1)+0.5)*real(nscale)/120.+latlon

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This function rounds up
!

Integer Function rndup(x)

Implicit None

Real, intent(in) :: x

rndup=int(x)
if (x.GT.real(rndup)) rndup=rndup+1

Return
End


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This function calculates lat from an array index
!

Real function callat(latlon,i,nscale)

Implicit None

Real, intent(in) :: latlon
Integer, intent(in) :: i,nscale

callat=latlon-(Real(i-1)+0.5)*real(nscale)/120.

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This function calculates the array index for a specified lon
!

Real function indexlon(aglon,latlon,nscale)

Implicit None

Real, intent(in) :: aglon,latlon
Integer, intent(in) :: nscale

indexlon=(aglon-latlon)*120./real(nscale)+0.5
	    
Return
End	    

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This function calculates the array index for a specified lat
!

Real function indexlat(aglat,latlon,nscale)

Implicit None

Real, intent(in) :: aglat,latlon
Integer, intent(in) :: nscale
   
indexlat=(-aglat+latlon)*120./real(nscale)+0.5

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine determines the boundary of a subsection for a
! specified scale.
!

Subroutine searchdim(mode,sll,nscale,scalelimit,latlon,lldim,grid,maskn,rlld,sibdim)

Implicit None

Integer, intent(in) :: mode,nscale
Integer, dimension(1:2), intent(in) :: lldim,sibdim
Real, intent(in) :: scalelimit
Real, dimension(1:2,1:2), intent(out) :: sll
Real, dimension(1:2), intent(in) :: latlon
Real, dimension(1:sibdim(1),1:sibdim(2)), intent(in) :: grid
Real, dimension(1:sibdim(1),1:sibdim(2),1:2), intent(in) :: rlld
Real, dimension(1:sibdim(1),1:sibdim(2),1:2) :: tlld
Real, dimension(1:2,1:2) :: templl
Integer, dimension(1:2,1:2,1:2) :: posll
Integer i,j
Integer rndup
Logical, dimension(1:sibdim(1),1:sibdim(2)) :: sermask
Logical, dimension(1:sibdim(1),1:sibdim(2)), intent(in) :: maskn

tlld=rlld

templl(1,1)=latlon(1)
templl(1,2)=latlon(1)+real(lldim(1)*nscale)/120.
templl(2,2)=latlon(2)
templl(2,1)=latlon(2)-real(lldim(2)*nscale)/120.

Do i=1,2
  If (templl(2,i).LT.-90.) templl(2,i)=-90.
  If (templl(2,i).GT.90.) templl(2,i)=90.
End Do

sermask=(tlld(:,:,1).GE.templl(1,1)).AND.(tlld(:,:,1).LE.templl(1,2)) &
        .AND.(tlld(:,:,2).GE.templl(2,1)).AND.(tlld(:,:,2).LE.templl(2,2))
sermask=sermask.AND.maskn

Select Case(mode)
  Case(0)
    ! Use all grid points
    sll=templl
    Return
  Case(1)
    sermask=sermask.AND.(grid.LE.scalelimit)
  Case(2)
    sermask=sermask.AND.(grid.GE.scalelimit)
  Case(3)
    sermask=sermask.AND.(grid.EQ.scalelimit)
  Case(4)
    ! Do nothing
  Case Default
    Write(6,*) 'ERROR: Internal error.  Unsupported mode in searchdim'
    call finishbanner
    Stop -1
End Select

If (.NOT.Any(sermask)) then
  sll=0.
  Return
End if

sll(1,2)=Maxval(tlld(:,:,1),sermask)
sll(1,1)=Minval(tlld(:,:,1),sermask)
sll(2,2)=Maxval(tlld(:,:,2),sermask)
sll(2,1)=Minval(tlld(:,:,2),sermask)

posll(1,2,:)=Maxloc(tlld(:,:,1),sermask)
posll(1,1,:)=Minloc(tlld(:,:,1),sermask)
posll(2,2,:)=Maxloc(tlld(:,:,2),sermask)
posll(2,1,:)=Minloc(tlld(:,:,2),sermask)
Do i=1,2
  Do j=1,2
    ! 1.6 is assumed to span from the centre to the corner (i.e., sqrt(2) if
    ! using a square grid)
    sll(i,j)=sll(i,j)+Real(j*2-3)*grid(posll(i,j,1),posll(i,j,2))*1.6/240.
  End Do
End Do

sll(1,1)=real(int((sll(1,1)-latlon(1))*120./real(nscale)))*real(nscale)/120.+latlon(1)
sll(1,2)=real(rndup((sll(1,2)-latlon(1))*120./real(nscale)))*real(nscale)/120.+latlon(1)
sll(2,1)=-real(rndup((latlon(2)-sll(2,1))*120./real(nscale)))*real(nscale)/120.+latlon(2)
sll(2,2)=-real(int((latlon(2)-sll(2,2))*120./real(nscale)))*real(nscale)/120.+latlon(2)

! Check bounds
Do i=1,2
  If (sll(i,1).LT.templl(i,1)) sll(i,1)=templl(i,1)
  If (sll(i,2).GT.templl(i,2)) sll(i,2)=templl(i,2)
End Do

! Consistancy check
If ((sll(1,1).GT.sll(1,2)).OR.(sll(2,1).GT.sll(2,2))) then
  sll=0.
End If

Return
End


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine calculates the next scale to load.  The suboutine
! attempts to minimise the number of times the sibveg data is loaded.
!

Subroutine findsmallscale(nscale,scalelimit,latlon,llstore,grid,maskn,rlld,subsec,sll,sibsize,sibdim)

Implicit None

Integer, intent(in) :: sibsize,scalelimit
Integer, dimension(1:2), intent(in) :: sibdim
Integer, intent(inout) :: nscale
Integer, intent(out) :: subsec
Integer, dimension(1:2), intent(out) :: llstore
Real, dimension(1:sibdim(1),1:sibdim(2)), intent(in) :: grid
Real, dimension(1:sibdim(1),1:sibdim(2),1:2), intent(in) :: rlld
Real, dimension(1:2), intent(in) :: latlon
Real, dimension(1:2,1:2), intent(out) :: sll
Real tscale
Integer mode,maxscale,subsecmax
Integer findfact
Logical, dimension(1:sibdim(1),1:sibdim(2)), intent(in) :: maskn

tscale=Maxval(grid,maskn)

mode=1
If (nscale.EQ.999) mode=0

maxscale=Int(0.5*real(nscale)/Real(scalelimit))*scalelimit
maxscale=findfact(21600,maxscale,-scalelimit)
If (maxscale.LT.scalelimit) maxscale=scalelimit

llstore=(/ 43200/maxscale , 21600/maxscale /)
Call searchdim(mode,sll,maxscale,tscale,latlon,llstore,grid,maskn,rlld,sibdim)
Call scaleconvert(maxscale,subsecmax,llstore,sll,sibsize)

If (subsecmax.LT.1) Then
  Write(6,*) "WARN: Cannot locate unassigned points in findsmallscale"
  mode=0
  nscale=maxscale
Else
  nscale=Int(Minval(grid,maskn)/Real(scalelimit))*scalelimit
  nscale=findfact(21600,nscale,-scalelimit)
  If (nscale.LT.scalelimit) nscale=scalelimit
  subsec=subsecmax+1
  Do While (subsec.GT.subsecmax)
    ! Get estimate of array size
    llstore=(/ 43200/nscale , 21600/nscale /)
    ! Calculate domain for search
    Call searchdim(mode,sll,nscale,tscale,latlon,llstore,grid,maskn,rlld,sibdim)
    ! Define number of points in domain and subdivide into tiles if array is too big
    Call scaleconvert(nscale,subsec,llstore,sll,sibsize)
    If (subsec.GT.subsecmax) Then
      nscale=nscale+scalelimit
      nscale=findfact(21600,nscale,scalelimit)
    End If
  End Do
End If

If (nscale.GT.maxscale) nscale=maxscale
If (nscale.LT.scalelimit) nscale=scalelimit


llstore=(/ 43200/nscale , 21600/nscale /)
! Calculate domain for search
Call searchdim(mode,sll,nscale,tscale,latlon,llstore,grid,maskn,rlld,sibdim)
! Define number of points in domain and subdivide into tiles if array is too big
Call scaleconvert(nscale,subsec,llstore,sll,sibsize)

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This function finds the nearest factor of an integer
!

Integer Function findfact(x,y,delta)

Implicit None

Integer, intent(in) :: x,y,delta
Integer z

z=y

If (z.EQ.0) Then
  findfact=1
  Return
End If

Do While (Mod(x,z).NE.0.)
  z=z+delta
  If (z.LT.1) z=1
  If (z.GT.x) z=x
End Do

findfact=z

Return
End

