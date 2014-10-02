  program corr_cnv
!***********************************************************************
!* GNU General Public License *
!* This file is a part of cnv2nc. *
!* *
!* cnv2nc is free software; you can redistribute it and/or modify it and *
!* are expected to follow the terms of the GNU General Public License *
!* as published by the Free Software Foundation; either version 2 of *
!* the License, or (at your option) any later version. *
!* *
!* cnv2nc is distributed in the hope that it will be useful, but WITHOUT *
!* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *
!* or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public *
!* License for more details. *
!* *
!* For the full text of the GNU General Public License, *
!* write to: Free Software Foundation, Inc., *
!* 675 Mass Ave, Cambridge, MA 02139, USA. *
!* or see: http://www.gnu.org/licenses/gpl.html *
!***********************************************************************
  
  use cnv
  
  character*32  inputfile, outputfile, trim
!
  character*32 cnvlist  
  data  cnvlist /'cnv.cmd'/

 
  parameter (maxcnv=1000,kmcnv=1000,ismoothanz=2)
  integer ::iolst=10, icnv=11, ocnv=12, stdout=6
  real, allocatable :: buff(:)
  
  include 'netcdf.inc'
  integer           :: ncid, status
  integer           :: latdim, londim, presdim, vardims(4),rinfdims(3), timedim
  integer           :: latid, lonid, presid, timeid
  integer, allocatable :: varid(:), rinfid(:), cinfid(:)
  character*255     :: title  
  character*10      :: name  
!-----------------------------------------------------------------------
!     define allowable i/o units for MOM
!-----------------------------------------------------------------------
!      
  write(stdout,*)'Processing files in ',cnvlist
  open(iolst,file=cnvlist,form='formatted')
  do ncnv=1,maxcnv   
    read(iolst,'(a)',end=1111,err=5555,iostat=ierr) inputfile
!
! ...   read ASCII .cnv-files
!
!-----------------------------------------------------------------------
    write(stdout,*) 'reading ... '//trim(inputfile)
    outputfile = 'out'//trim(inputfile(1:index(inputfile,'.')))//'nc'       
    write(stdout,*) 'writing ... '//trim(outputfile)
    open(icnv,file=inputfile,form='formatted')
! -----------------------------------------------------------------------

    call process_header(icnv)
    
    call get_data(icnv)
    
    status = nf_create(outputfile, nf_clobber, ncid)
    write(stdout,*) 'file ',outputfile,' has been created with err ',status
! -----------------------------------------------------------------------
!   define axis
! -----------------------------------------------------------------------
    status = nf_def_dim(ncid, 'longitude', 1, londim)
    status = nf_def_dim(ncid, 'latitude', 1, latdim)
    status = nf_def_dim(ncid, 'pressure', ngood, presdim)
    status = nf_def_dim(ncid, 'time', nf_unlimited, timedim)
    status = nf_def_var(ncid, 'longitude', nf_real, 1, londim, lonid)
    status = nf_def_var(ncid, 'latitude', nf_real, 1, latdim, latid)
    status = nf_def_var(ncid, 'pressure', nf_real, 1, presdim, presid)
    status = nf_def_var(ncid, 'time', nf_real, 1, timedim, timeid)
   
    status = nf_put_att_text(ncid, lonid, 'units', 12, 'degrees_east')
    status = nf_put_att_text(ncid, latid, 'units', 13, 'degrees_north')
    status = nf_put_att_text(ncid, presid, 'units', 4, 'dBar')
    status = nf_put_att_text(ncid, timeid, 'units',30, 'days since 2000-01-01 00:00:00')
    status = nf_put_att_text(ncid, timeid, 'time_origin', 19, '1-jan-2000 00:00:00')
 
    vardims(1) = londim
    vardims(2) = latdim
    vardims(3) = presdim
    vardims(4) = timedim
    rinfdims(1) = londim
    rinfdims(2) = latdim
    rinfdims(3) = timedim
! -----------------------------------------------------------------------
!   define variables
! -----------------------------------------------------------------------
    allocate(rinfid(nrinf))  
    do n=1, nrinf
      status = nf_def_var(ncid, trim(rinfname(n)), nf_real, 3, rinfdims, rinfid(n))
      status = nf_put_att_text(ncid, rinfid(n), 'units', len_trim(rinfunit(n)),trim(rinfunit(n)))
      status = nf_put_att_text(ncid, rinfid(n), 'long_name', len_trim(rinflongname(n)),trim(rinflongname(n)))
    enddo
    allocate(varid(ivar))  
    do n=1, ivar
      status = nf_def_var(ncid, trim(varshort(n)), nf_real, 4, vardims, varid(n))
      status = nf_put_att_text(ncid, varid(n), 'units', len_trim(varunit(n)),trim(varunit(n)))
      status = nf_put_att_text(ncid, varid(n), 'long_name', len_trim(varname(n)),trim(varname(n)))
    enddo
    do n=1, ncinf
      name = 'title'//char(n+48)
      title = trim(cinfname(n))//' : '//trim(cinfvalue(n))
      status = nf_put_att_text(ncid, nf_global, trim(name), len_trim(title), trim(title))
    enddo

    status = nf_enddef(ncid)

   
    status = nf_put_var_real(ncid, latid, degnord)
    status = nf_put_var_real(ncid, lonid, degeast)
    status = nf_put_var_real(ncid, presid, datafield(:,1))
    
    status = nf_put_vara_double(ncid, timeid, 1,1, rdays)
    do n=1, nrinf
      status = nf_put_var_real(ncid, rinfid(n), rinfvalue(n))
    enddo
    do n=1, ivar
      status = nf_put_var_real(ncid, varid(n), datafield(:,n))
    enddo
!-----------------------------------------------------------------------
!       Ein File ist fertig
!-----------------------------------------------------------------------
    status = nf_close(ncid)
10  continue
    close(icnv)
    deallocate(varid)      
    deallocate(rinfid)
    deallocate(datafield)      
    deallocate(varshort)
    deallocate(varname)
    deallocate(varunit)

    write(stdout,*) 'file ',outputfile,' has been closed with err ',status
  enddo
1111 continue
5555 continue
  stop
  end
  
