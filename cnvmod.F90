  module cnv
  private
!  
  integer, parameter :: maxvar = 50   
  character*72, public, allocatable	  :: varname(:)
  character*20, public, allocatable	  :: varshort(:)
  character*20, public, allocatable	  :: varunit(:)
!!  character*72, public                    :: varname(maxvar)
!!  character*20, public                    :: varshort(maxvar)
!!  character*20, public                    :: varunit(maxvar)
  integer, save                           :: stdout=6
  real, public                            :: degnord, degeast, rhour
  real*8, public                          :: rdays
  real, public, allocatable               :: datafield(:,:)
  integer , public                        :: ivar, ngood, ncinf, nrinf
  character*20, public                    :: cinfname(10), cinfvalue(10) 
  real, public                            :: rinfvalue(10) 
  character*20, public                    :: rinfunit(10)
  character*72, public                    :: rinfname(10), rinflongname(10) 
  logical, save                           :: first = .true.
  character*20                            :: varn
  logical                                 :: only_once
!!  integer, allocatable, save              :: varindex(:)
  integer, save                           :: varindex(maxvar)
  character*72, save,   dimension(maxvar) :: varnametmp
  character*20, save,   dimension(maxvar) :: varshorttmp, varunittmp
  integer, save                           :: nquan, nvalues
 
  public process_header, get_data
      
  contains
      
      subroutine process_header(icnv)    
!-----------------------------------------------------------------------
!     This procedure analyzes the .cnv file header and writes the header
!     back to the output file 
!-----------------------------------------------------------------------
      character*255       :: linebuff
      integer, intent(in) :: icnv
      integer             :: in, k, i
      character*4         :: cnquan, cnvalues
!-----------------------------------------------------------------------
!     define allowable i/o units for MOM
!-----------------------------------------------------------------------
!      
! ...   read ASCII .cnv-files
!
      in = 0
!       
!-----------------------------------------------------------------------
!       Bearbeitung des files
!-----------------------------------------------------------------------
      k = 0
      ivar = 0
      nrinf = 0
      ncinf = 0
      linebuff = ' '
      do while(index(linebuff,'END').eq.0)
        k = k + 1
        read(icnv,'(a)',end=1000) linebuff
        if (index(linebuff,'#').gt.0.or.index(linebuff,'*').gt.0) then
!         Analyse, wo was steht	   
          if (index(linebuff,'# name').gt.0) then
            in = index(linebuff,'name')+4
	    call setup(linebuff)
          endif  
          if (index(linebuff,'# nquan').gt.0) then
            cnquan = linebuff(index(linebuff,'=')+2:len_trim(linebuff))
	    read(cnquan,*) nquan
          endif  
          if (index(linebuff,'# nvalues').gt.0) then
            cnvalues = linebuff(index(linebuff,'=')+2:len_trim(linebuff))
	    read(cnvalues,*) nvalues
          endif  
          if (index(linebuff,'** ').gt.0) then
            call setup_pos(linebuff)
          endif 
        endif
      enddo
 1000  continue      
      write(stdout,*) 'degnord = ',degnord	
      write(stdout,*) 'degeast = ',degeast
      write(stdout,*) 'rhour   = ',rhour	
      write(stdout,*) 'The file contains the following variables:'
      do i=1,ivar
        write(stdout,*) i,' :',trim(varshorttmp(i)), trim(varnametmp(i)),' [',trim(varunittmp(i)),']'
      enddo
      write(stdout,*) 'The file contains ', nvalues,' data lines!'
!     Test, ob benötigte Variable gefunden wurden
      if (nquan.ne.ivar) then
        write(stdout,*)'Something is wrong with the number of variables!'
	write(stdout,*)'nquan =  ',nquan,' but ',ivar,' variables found!'
	stop
      endif
!     Now eliminite double data (Peter Wlost uses this for some hidden reason)      
      write(stdout,*) 'Eliminate double data columnes'
      ivar = 0
!      allocate(varindex(nquan))
      do i=1, nquan
        varn = varshorttmp(i)
	only_once = .true.
        do ii=1, ivar               
	  if (trim(varn).eq.trim(varshorttmp(ii))) only_once = .false.
	enddo
!        print*,i,only_once 
	if(only_once) then
	  ivar = ivar +1
          varindex(ivar) = i
	endif
      enddo
      if(.not.allocated(varshort)) allocate(varshort(ivar))
      if(.not.allocated(varname))  allocate(varname(ivar))
      if(.not.allocated(varunit))  allocate(varunit(ivar))
      do i=1, ivar
	varshort(i) = trim(varshorttmp(varindex(i)))
	varname (i) = trim(varnametmp (varindex(i)))
	varunit (i) = trim(varunittmp (varindex(i)))
      enddo
      write(stdout,*) 'The following variables are written to file:'
      do i=1, ivar
        print*,i,' ',trim(varshort(i)) &
                    ,' ',trim(varname(i)) &
                    ,' ',trim(varunit(i))
      enddo
      if(index_of('pr').eq.0) then
        write(stdout,*)'=>Error, no pressure',' in file'
        stop
      endif
      if(index_of('sal00').eq.0) then
         write(stdout,*)'=>Error, no salinity',' in file'
         stop
      endif
      if(index_of('t090').eq.0) then
        write(stdout,*)'=>Error, no temperature',' in file'
        stop
      endif
      return
      end subroutine process_header
	
      
!!      function trim(trimstr)
!!      character (*) :: trimstr, trim
!!      trim = trimstr
!!      return
!!      end function trim
      subroutine setup_pos(line) 
      character*255       ::line
      character*3         :: cxgrd,cygrd
      character*7         :: cxmin,cymin
      character*1         :: cxnor,cyeas
      character*2         :: ch, cm, cs
      character*9         :: cd
      character*8         :: clot
      real                :: xgrd, ygrd, xmin, ymin
      integer             :: ih, im, is, iend
      if (index(line,'GPS_Posn').gt.0) then
        cxgrd = line(15:16)
        cxmin = line(18:24)
        cxnor = line(25:25)
        cygrd = line(27:29)
        cymin = line(31:37)
        cyeas = line(38:38)
	read(cxgrd,*) xgrd
	read(cygrd,*) ygrd
	read(cxmin,*) xmin
	read(cymin,*) ymin
	degnord = xgrd + xmin/60.
	degeast = ygrd + ymin/60.
	if (cxnor.eq.'S') degnord = - degnord
	if (cyeas.eq.'W') degeast = - degeast
      elseif (index(line,'Startzeit').gt.0) then
        ch = line(15:16)
        cm = line(18:19)
        cs = line(21:22)
        cd = line(24:32)
	read(ch,*) ih
	read(cm,*) im
	read(cs,*) is
	rhour = float(ih) + (float(im) + float(is)/60.)/60.
	rdays = days_since_2000(cd) + rhour/24.
      elseif (index(line,'Echolote').gt.0) then
        nrinf = nrinf + 1
	clot = line(25:30)
	print*,clot
	read(clot,*) rinfvalue(nrinf)  
        rinfvalue(nrinf) = rinfvalue(nrinf) + 4.4
        rinfname(nrinf) = 'Echolot'
        rinfunit(nrinf) = 'm'
        rinflongname(nrinf) = 'Echolot mit Schiffstiefe korr.'
      elseif (index(line,'Luftdruck').gt.0) then
        nrinf = nrinf + 1
	clot = line(15:21)
	print*,clot
	read(clot,*) rinfvalue(nrinf)  
        rinfvalue(nrinf) = rinfvalue(nrinf)
        rinfname(nrinf) = 'Luftdruck'
        rinfunit(nrinf) = 'mBar'
        rinflongname(nrinf) = 'Luftdruck'
      elseif (index(line,'ThermoSal').gt.0) then
        nrinf = nrinf + 1
	clot = line(15:22)
	print*,clot
	read(clot,*) rinfvalue(nrinf)  
        rinfvalue(nrinf) = rinfvalue(nrinf)
        rinfname(nrinf) = 'Temperature'
        rinfunit(nrinf) = 'Celsius'
        rinflongname(nrinf) = 'Temperature of Thermosalinograph'
        nrinf = nrinf + 1
	istart = index(line,'grdC')+4
	clot = line(istart:istart+8)
	print*,clot
	read(clot,*) rinfvalue(nrinf)  
        rinfvalue(nrinf) = rinfvalue(nrinf)
        rinfname(nrinf) = 'Salinity'
        rinfunit(nrinf) = 'PSU'
        rinflongname(nrinf) = 'Salinity of Thermosalinograph'
      elseif (index(line,'ReiseNr').gt.0) then
        ncinf = ncinf + 1
	cinfname(ncinf) = 'ReiseNr'
	cinfvalue(ncinf) = line(15:22)
      elseif (index(line,'StationNr').gt.0) then
        ncinf = ncinf + 1
	cinfname(ncinf) = 'StationNr'
	cinfvalue(ncinf) = line(15:18)
      elseif (index(line,'StatBez').gt.0) then
        ncinf = ncinf + 1
	cinfname(ncinf) = 'StatBez'
	clot = line(15:20)
	iend = index(clot,' ') 
   	cinfvalue(ncinf) = clot(1:iend)
      endif
      return
      end subroutine setup_pos
      
      subroutine setup(name) 
      character *255 ::name
      integer        :: isname, iename, isunit, ieunit, isshort, ieshort   
      isname = index(name,':')+1 
      if(index(name,'[').gt.0) then
        isunit = index(name,'[') + 1  
        ieunit = index(name,']') - 1  
	iename = isunit - 2
      else
        isunit = 0
        ieunit = 0 
	iename = len_trim(name)-1
      endif
      ivar = ivar +1
      isshort = index(name,'=')+2
      ieshort = index(name,':')-1
      if(index(name,'c0mS').ne.0) ieshort = isshort + 1
      if(index(name,'sbeox').ne.0) ieshort = isshort + 4
      varshorttmp(ivar) = name(isshort:ieshort)
      varnametmp(ivar) = name(isname:iename)
      if (isunit.gt.0) then 
        varunittmp(ivar) = name(isunit:ieunit)
      else
        varunittmp(ivar) = ''
      endif
      do while (index(varshorttmp(ivar),"/").ne.0) 
        isunit = index(varshorttmp(ivar),"/") 
        ieunit = len_trim(varshorttmp(ivar))
        varshorttmp(ivar) = varshorttmp(ivar)(1:isunit-1)//'_'//varshorttmp(ivar)(isunit+1:ieunit)
      enddo
      return
      end subroutine setup
	
      subroutine get_data(icnv) 
      character*512       :: linebuff
      integer, intent(in) :: icnv
      integer             :: i
      integer,allocatable :: badflag(:)
      real,allocatable    :: datatmp(:,:)
      allocate(datafield(nvalues,ivar))
      allocate(datatmp(nvalues,nquan))
      allocate(badflag(nvalues))
      badflag(:) = 0
      do i=1, nvalues	
	read(icnv,*,end=100) (datatmp(i,j),j=1,nquan)
      enddo
100   continue
      write(stdout,*) nvalues,' read!'
!     get the index of the pressure field and remove lines 
!     where pressure is decreasing      
      ipres      = index_of('pr')
      pres_upper = -1.
      ngood = 0
      do i=1, nvalues
        if(pres_upper.lt.datatmp(i,ipres)) then
	  ngood = ngood + 1
	  badflag(i) = 1
          pres_upper = datatmp(i,ipres)
	endif
      enddo
      ngood = 0
      do n=1, nvalues
        if(badflag(n).eq.1) then
	  ngood = ngood + 1
	  do i=1, ivar
	    datafield(ngood,i) = datatmp(n,varindex(i))
	  enddo
 	endif
      enddo
      deallocate(badflag)
      deallocate(datatmp)
      return
      end subroutine get_data
      
      function index_of(name) 
      character *(*) ,name
      integer index_of
      index_of = 0
      do i=1,ivar
        if (index(varshort(i),name).ne.0) then
          index_of = i
        endif
      enddo
      return
      end function index_of
      
      function days_since_1900(cd) 
      integer             :: mon, iday, imon, iyr, inc
      integer             :: days
      character*9         :: cd
      character*2         :: cday, cyr
      character*3         :: cmon
      integer             :: monlen(12)
      data (monlen(i),i=1,12) /31,28,31,30,31,30,31,31,30,31,30,31/
      cday = cd(1:2)
      cmon = cd(4:6)
      cyr  = cd(8:9)
      read(cday,*) iday
      read(cyr,*)  iyr
      if (iyr.lt.50) then
        iyr = iyr + 2000
      else
        iyr = iyr + 1900
      endif
      print*,iday, iyr, cmon
      if(cmon.eq.'Jan') then
        imon = 1
      elseif (cmon.eq.'Feb') then
        imon = 2
      elseif (cmon.eq.'Mar') then
        imon = 3
      elseif (cmon.eq.'Apr') then
         imon =4
      elseif (cmon.eq.'Mai') then
        imon = 5
      elseif (cmon.eq.'Jun') then
        imon = 6
      elseif (cmon.eq.'Jul') then
        imon = 7
      elseif (cmon.eq.'Aug') then
        imon = 8
      elseif (cmon.eq.'Sep') then
        imon = 9
      elseif (cmon.eq.'Okt') then
        imon = 10
      elseif (cmon.eq.'Nov') then
        imon = 11
      elseif (cmon.eq.'Dez') then
        imon = 12
      else
        write(stdout,*) 'Unknown month ', cmon,' in Startzeit!'
      endif
      days = 0
      do n=1900, iyr - 1
        inc=365
        if(int(n/4)*4.eq.n) inc = 366
	if(n.eq.1900) inc = 365
	days=days+inc
      enddo 
      monlen(2) =  28
      if(int(iyr/4)*4.eq.iyr) monlen(2) = 29
      if(iyr.eq.1900) monlen(2) =  28 
      do n=1,imon-1
        days = days + monlen(n)
      enddo
      days_since_1900 = days + iday - 1
      return
      end function days_since_1900 
      
      function days_since_2000(cd) 
      integer             :: mon, iday, imon, iyr, inc
      integer             :: days
      character*9         :: cd
      character*2         :: cday, cyr
      character*3         :: cmon
      integer             :: monlen(12)
      data (monlen(i),i=1,12) /31,28,31,30,31,30,31,31,30,31,30,31/
      cday = cd(1:2)
      cmon = cd(4:6)
      cyr  = cd(8:9)
      read(cday,*) iday
      read(cyr,*)  iyr
      if (iyr.lt.50) then
        iyr = iyr + 2000
      else
        iyr = iyr + 1900
      endif
      print*,iday, iyr, cmon
      if(cmon.eq.'Jan') then
        imon = 1
      elseif (cmon.eq.'Feb') then
        imon = 2
      elseif (cmon.eq.'Mar') then
        imon = 3
      elseif (cmon.eq.'Apr') then
         imon =4
      elseif (cmon.eq.'Mai') then
        imon = 5
      elseif (cmon.eq.'Jun') then
        imon = 6
      elseif (cmon.eq.'Jul') then
        imon = 7
      elseif (cmon.eq.'Aug') then
        imon = 8
      elseif (cmon.eq.'Sep') then
        imon = 9
      elseif (cmon.eq.'Okt') then
        imon = 10
      elseif (cmon.eq.'Nov') then
        imon = 11
      elseif (cmon.eq.'Dez') then
        imon = 12
      else
        write(stdout,*) 'Unknown month ', cmon,' in Startzeit!'
      endif
      days = 0
      do n=2000, iyr - 1
        inc=365
        if(int(n/4)*4.eq.n) inc = 366
	if(n.eq.1900) inc = 365
	days=days+inc
      enddo 
      monlen(2) =  28
      if(int(iyr/4)*4.eq.iyr) monlen(2) = 29
      do n=1,imon-1
        days = days + monlen(n)
      enddo
      days_since_2000 = days + iday - 1
      return
      end function days_since_2000 
   
  end module cnv
