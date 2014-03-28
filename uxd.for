******************************************************************************
*     UXD to RAW, EPF(popLA),PFG(Rigaku) converting program
*     The other way around is not considered here.
*     The purpose of this code is to compare the popLA and the other method
*     in obtaining discritized grains
*     The code must be complied for KOCKS nomenclature. (Not clear yet)
*     VPSC library for converting can be done easily. 
*     Refer to the subroutine for texture input
*     of the VPSC code.
*
******************************************************************************
*           Young Ung Jeong Ph.D. Candadate (2010-06-20)
*                                 Materials Mechanics Laboratory
*                                 Graduate Institute of Ferrous Technology
*                                 Pohang University of Science and Technology
*                                 
*            email: youngung.jeong@gmail.com
******************************************************************************
*
*  2012 - 12 - 29 
*       The code was modified to be compatible with g95 compiler
*
*******************************************************************************



c
      program converter
c
      parameter(min_step=5)
      parameter(maxfile=100)
      parameter(maxpf=10)
      parameter(step_chi=int(real(90 - 0)/real(min_step))+1)
      parameter(step_phi=int(real(360-min_step)/real(min_step))+1)
c
      integer ur1,ur2,iopt,max_i,iformat ! iformat=0(RAW), 1(PFG)
      character*256 dummy
      character*8 infile
      character*8 inf(maxfile)
      character*50 outfile
      character*50 outf(maxfile)
      character*3 cdex(maxpf)
      character*20 datemeasured
      dimension range_chi(2),range_phi(2),range_intensity(2)
      dimension pf_intensity(maxpf,int(step_chi),int(step_phi))
      dimension pf_2theta(maxpf)
      dimension bg_2theta(2,maxpf)  ! two backgrounds (left & right)
      real step_size,avg
      integer npf, nbg
      integer n_line_pfindex, iheadnum
      integer ibg_mode !no bg(1), single bg(2),full bg(3)
      !write(*,*) 'step_chi',step_chi
      !write(*,*) 'step_phi',step_phi
c     file identifiers
c     ur1- infile (UXD extension)
c     ur2- outfile (RAW extension)      
      ur1=1
      ur2=2
      write(*,'(a)') '*************************************************'
      write(*,'(a)') '*      UXD -> RAW,EPF, and PFG converter        *'
      write(*,'(a)') '*************************************************'
      write(*,*)
      write(*,'(a)') 'ENVIRONMENT? (default:0, via inp.env:1, manual:2)'
      read (*, * ) ienv
c
      if (ienv.eq.0) then !default
       write(*,'(a)') '************************************************'
       write(*,'(a)') '*         Default environment settings         *'
       write(*,'(a)') '************************************************'
       write(*,'(a)') '# of file?'
       read (*,    *)  nfile
       write(*,'(a)') 'input file names in a row'
       do ifile=1,nfile
          read(*,'(a)') inf(ifile)
       enddo
       write(*,'(a)') 'output file names in a row, w/o extension'
       do ifile=1,nfile
          read(*,'(a)') outf(ifile)
       enddo
       npf=3
       nbg=2
       avg=0.1
       n_line_pfindex = npf * (nbg+1)
       n_line_dummy   = 23 
       ibg_mode = 3  !Full background measurement is hardwired       
       range_chi(1)   = 0
       range_chi(2)   = 75  !GIFT bruker D8-discovery defaults
       range_phi(1)   = 0
       range_phi(2)   = 355
       step_size= 5.0
       iformat=0      !RAW output format is default
       write(*,'(a)')
      else if(ienv.eq.1) then !via 'inp.env' file
         call input_env(nfile,inf,outf,npf,nbg,
     $        range_chi,range_phi,step_size,avg,ibg_mode,0,iformat)
         n_line_pfindex=npf*(nbg+1)
      else if(ienv.eq.2) then   !via manual way
         call input_env(nfile,inf,outf,npf,nbg,
     $        range_chi,range_phi,step_size,avg,ibg_mode,1,iformat)
         n_line_pfindex=npf*(nbg+1)
      else
         write(*,'(a)') 'ERR: Your environment flag is wrong'
         pause
         stop
      endif
c
ccc   file loop      
      do ifile=1, nfile
!     RESULT=SYSTEMQQ('cls')
         write(*,'(a)')'***********************************************'
         write(*,'(a1,a15,a8,i2,a21,a)') '*',' ',
     $        'for file',ifile,' ',' *'
         write(*,'(a)')'***********************************************'
c
*************************************************************
*     scanning the file to detect the range of intensity    *
*************************************************************
         infile = inf(ifile)
         outfile= outf(ifile)
!     open(ur2,file=outfile,status='unknown')
c     Scan and find the largest intensity
         
         open (ur1,file=trim(infile), status='old')
         iop =1                 ! for the first run of scan
         call scan(iop,ur1,infile,n_line_pfindex,step_size,range_phi,
     $        range_chi,range_intensity, ibg_mode, npf, nbg,iheadnum,
     $        pf_2theta,bg_2theta,datemeasured,cdex)
         close(ur1)
c
c     Resulting file formats (RAW, PFG)
c
         open (ur1,file=infile,status='old')
c
         if     (iformat.eq.0) then ! UXD
            open(ur2, file=trim(outfile)//'.EPF', status='unknown')
         elseif(iformat.eq.1) then ! PFG
            do kpf=1,npf
               open(ur2+(kpf-1),file=trim(outfile)//'_'
     $              //trim(cdex(kpf))//'.PFG',status='unknown')
            enddo
         endif
c
c      converting file!
c      the core of the code
c
         call converting (ur1,ur2,n_line_pfindex,step_size,range_phi,
     $        range_chi,range_intensity,ibg_mode,npf,nbg,pf_intensity,
     $        cdex,ifile,datemeasured,avg,pf_2theta,bg_2theta,iformat)
*     infile closure     
         close(ur1) 
*     outfile closure      
         if (iformat.eq.0) then ! UXD
            open(ur2, file=trim(outfile)//'.EPF',status='unknown')
         elseif(iformat.eq.1) then ! PFG
            do kpf=1,npf
               close(ur2+(kpf-1)) 
            enddo
         endif
c
      enddo         ! Loop over nfile

ccc   file loop

      !close(ur2)  ! outfile (RAW)
c
      write(*, *   )
      write(*,'(a)') '*************************************************'
      write(*,'(a)') '*             End of the execution              *'
      write(*,'(a)') '*************************************************'
      write(*,*    )
      pause
      end program
c
c




***   subroutines
***************************************************************************
      subroutine converting(ur1,ur2,n_line_pfindex,step_size,range_phi,
     $     range_chi,range_intensity, ibg_mode,npf,nbg,pf_intensity,
     $     cdex,ifile,datemeasured,avg,pf_2theta,bg_2theta,iformat)
*     
*     Core subroutine. Based on the variables from subroutine scan
*     and settings either from deault, env. file and manual, fit the UXD
*     intensities into EPF file format.
*     This subroutine includes the normalization subroutine     
***************************************************************************     
c     independent parameters     
      parameter(min_step = 5)
      parameter(max_chi  = 90)
      parameter(max_phi  = 360)
c     dependent parameters      
      parameter(phi_step=int(real(max_phi)         /real(min_step)) + 1)
      parameter(chi_step=int(real(max_chi-min_step)/real(min_step)) + 1)
c
      integer ur1,ur2,npf,n_line_pfindex, ibg_mode,nbg, ifile,iformat
      integer max_intensity, min_intensity
      real step_size,stepsize_check, theta2,avg
      dimension pf_2theta(npf), bg_2theta(2,npf)
      dimension range_phi(2), range_chi(2), range_intensity(2)
      dimension pf_intensity (npf,int(((range_chi(2)-range_chi(1))
     $     /step_size) + 1), 
     $     int(((range_phi(2)-range_phi(1))/step_size+1)))
      dimension bg_intensity (npf,nbg,int(((range_chi(2)-range_chi(1))
     $     /step_size) + 1) ,
     $     int(((range_phi(2)-range_phi(1))/step_size+1)))
c     the first number, 18, is based on the convention of several pole figure file formats
      dimension int_list (18,int(((range_phi(2)-range_phi(1))
     $     /step_size+1)/
     $     18*((range_chi(2)-range_chi(1))/step_size+1)))
      ! nbg stands for fore and post peak backgrounds
      character*256 dummy, dum, header
      character*3   cdex(npf)
      character*20  datemeasured
c
      write(*,*) 'phi_step', phi_step
      write(*,*) 'chi_step', chi_step
      if (nbg.ne.0) then
         write(*,'(a)') 'The UXD file includes background measured'
         write(*,'(a)') 'Do you want bg to be taken into account?'
         write(*,'(a)') 'Yes (0), or No(1)'
         read (*, *   ) iflagbg
      elseif (iflagbg.eq.1 .or. ibg_mode.eq.1) then
         write(*,'(a)') 'No background consideration'
      endif
*      
*       Removing the header (please mimic the header removal from the scan subroutine)
      read (ur1, '(a)') dummy
c     write(*, '(a)'  ) trim(dummy)
      do j=1, npf
         read(ur1,'(a1,a3)') dummy,cdex(j)
         write(*,'(a)') cdex(j)
         if (nbg.ne.0) then
            do k=1, nbg
               read(ur1,'(a)') dummy
            enddo
         endif
      enddo 
      iquit=0
      do while(iquit.eq.0)
         read (ur1, '(a6)') dum
         if(trim(dum).eq.'_ANODE') iquit=1
      enddo
*       Detecting the structure of the UXD file
*         -How many pole figures? (3? e.g.) - npf
*         -How many background measurements does the file have for each pole figure? nbg
*            & Was it fully measured or partially? or even not measured at all?
      do   ipf =1, npf
         do  ichi=1, int((range_chi(2)-range_chi(1))/step_size)+1
            read(ur1,'(a)') dummy
            read(ur1,'(a)') dummy
            read(ur1,'(a)') dummy
            read(ur1,'(a10,f10.3)') dummy, stepsize_check
            if (step_size.ne.stepsize_check) then
               write(*,'(a)') '*************************************'
               write(*,'(a)') '*  ERR: Stepsize is not consistent  *'
               write(*,'(a)') '*************************************'
               pause
               stop
            endif
            iquit=0
            do while(iquit.eq.0)
               read(ur1,'(a1,a12)') dummy,dum
               if(trim(dum).eq.'2THETACOUNTS') iquit=1
            enddo
            do iphi=1, int((range_phi(2)-range_phi(1))/step_size)+1
               read (ur1, *)  theta2, pf_intensity(ipf, ichi, iphi)
c     write(*,   *)  theta2, pf_intensity(ipf, ichi, iphi)
            enddo               !phi
         enddo                  !chi
c
         if(nbg.gt.0) then
            do i_bg=1, nbg
               do ichistep=1, int((range_chi(2)-range_chi(1))
     $              /step_size)+1
                  read(ur1,'(a)') dummy
                  read(ur1,'(a)') dummy
                  read(ur1,'(a)') dummy
                  read(ur1,'(a10,f10.3)') dummy, stepsize_check
c
                  if (step_size.ne.stepsize_check) then
                     write(*,'(a)') '**********************************'
                     write(*,'(a)') 'ERR: Stepsize is not consistentd *'
                     write(*,'(a)') '**********************************'
                     pause
                     stop
                  endif
                  iquit=0
                  do while(iquit.eq.0)
                     read(ur1,'(a1,a12)') dummy,dum
                     if(trim(dum).eq.'2THETACOUNTS') iquit=1
                  enddo
c     write(*,'(a)') dummy
c
                  if(ibg_mode.eq.3) then !Full background
                     do iphistep=1, int((range_phi(2)-range_phi(1))/
     $                    step_size)+1
                        read (ur1, *) theta2,bg_intensity(ipf,
     $                       i_bg,ichistep,iphistep)
                     enddo
                  elseif(ibg_mode.eq.2) then !Single background
                     read (ur1,*) theta2,
     $                    bg_intensity(ipf,i_bg,ichistep,1 )
                     do iphistep=2, int((range_phi(2)-range_phi(1))
     $                    /step_size)+1
                        bg_intensity(ipf,i_bg,ichistep,iphistep) = 0.
                     enddo
                  endif         ! BG_intensity readings dependending upon ibg_mode (either 2 or 3)
               enddo            ! ichistep
            enddo               ! i_bg
         endif                  ! for nbg>0
c     for two background measured
c
      enddo                     !ipf
c
*     if (one wants to take into account of background) then
*     taking care of back ground iflagbg 0 (YES)
      if(iflagbg.eq.0 ) then    ! .and. ibg_mode.gt.1) then       
c     
         if (ibg_mode.gt.1) then
            call background_subtract(pf_intensity,bg_intensity,
     $           range_phi, range_chi,step_size,npf,nbg,avg,
     $           ibg_mode,iopt,pf_2theta,bg_2theta)
        
            if (iopt.eq.1) then
               write(*,'(a)') 'Warning : Negative PF intensity!'
               pause
            endif
         endif
         call normalize(pf_intensity,range_phi, range_chi,
     $        range_intensity,step_size,npf,avg)

c
c       The position of normalization subroutine is in question.
c       Perhaps, it has to be called regardless of the presense of
c       background measurement. Moreover, normalizing needs be carried out
c       in prior to the call of background_subtract
c
      else if (max_intensity.gt.9999) then
         write(*,'(a)') '*********************************************'
         write(*,'(a)') '*Caution: Your intensity is larger than 9999*'
         write(*,'(a)') '*   The intensities have to be normalized   *'
         write(*,'(a)') '*      to fit in the RAW, EPF formats.      *'
         write(*,'(a)') '*********************************************'
         pause
         call normalize(pf_intensity,range_phi,range_chi,range_intensity
     $        ,step_size,npf,avg)
c     stop
      endif
      !result = systemqq('cls')
      write(*,'(a)')'*************************************************'
      write(*,'(a)')'*              Writing activity                 *'
      write(*,'(a)')'*************************************************'
      write(*,'(a)') 'sample name(8 bytes)'
      read (*,'(a8)') header
      ibg=1                     !arbitrarily given (Don't know what it is exactly.)
      do ipf=1,npf
c      
c     output file category starts
c        
         if (iformat.eq.0) then ! RAW
            write(ur2,'(a6,a20)'            ) header,trim(datemeasured)
            write(ur2,'(a5,4f5.1,5i2,2i5,2a5)') '('//cdex(ipf)//')',
     $           step_size,range_chi(2),step_size,
     $           range_phi(2)+step_size,
     $           1,1,2,-1,3,int(avg*100),int(ibg),' ',' '        
         elseif(iformat.eq.1) then ! PFG
            write(ur2+(ipf-1),'(a6,a20)'    )header,trim(datemeasured)
            write(ur2+(ipf-1),'(a5,4f5.1,5i2,2i5,2a5)')
     $           '('//cdex(ipf)//')'
     $           ,90.-range_chi(2),90.,range_phi(1),range_phi(2),
     $           1,1,2,-1,3,int(avg*100),int(ibg),' ',' '
         endif
c
c     output file category ends
c        
         kount  = 0             !phi 
         ikount = 1             !phi
         do  ichi=1, int((range_chi(2)-range_chi(1))/step_size)+1 !16
            do iphi=1, int((range_phi(2)-range_phi(1))/step_size)+1 !72
               kount = kount +1
               int_list(kount,ikount) = pf_intensity(ipf,ichi,iphi)
               if (kount.eq.18) then
                  kount  = 0
                  ikount = ikount+1
               endif
            enddo               ! Loop over iphi
         enddo                  ! Loop over ichi
c
c
c
         do ikount =1, int((range_phi(2)-range_phi(1))/step_size+1)/
     $        18*((range_chi(2)-range_chi(1))/step_size+1)
            if (iformat.eq.0)    then !RAW
               write(ur2,'(1x,18i4)')(int_list(kount,ikount),kount=1,18)
            elseif(iformat.eq.1) then !PFG
               write(ur2+(ipf-1),'(1x,18i4)')
     $              (int_list(kount,ikount),kount=1,18)
            endif
         enddo
         if (iformat.eq.0) then !RAW
            do ikount =1, int((range_phi(2)-range_phi(1))/step_size+1)/
     $           18*(( 90- range_chi(2) )/step_size )
               write(ur2,'(1x,18i4)') (0, kount=1,18)
            enddo
         endif
c     
c    intensity list files
c
c     open(87,file='intensity'//char(ipf+48)//'_'//char(ifile+48)//
c     #                                      '.int',status='unknown')
c     do ich =1, int((range_chi(2)-range_chi(1))/step_size)+1
c     write (87,*) 'KHI = ', int((ich-1)*step_size)
c     do iph =1, int((range_phi(2)-range_phi(1))/step_size)+1
c     write (87,*) int(pf_intensity(ipf, ich, iph))
c     enddo
c     enddo
c     close(87)
         if     (iformat.eq.0) then
            write(ur2,'(a)')
         elseif (iformat.eq.1) then
            write(ur2+ipf-1,'(a)')
         endif
      enddo                     ! Loop over ipf
c
      return
      end subroutine
c
***************************************************************************
      subroutine background_subtract(pf_intensity,bg_intensity,range_phi
     $             ,range_chi,step_size,npf,nbg,avg,ibg_mode,iopt
     $             ,pf_2theta,bg_2theta)
*     Subroutine background_subtract is for ripping off the background 
*     properly depending upon the background measurement mode.
*     ibg_mode = 2 (single background)
*     ibg_mode = 3 (full   background)
***************************************************************************     
      real step_size,avg,mult
      integer npf,ibg_mode,iopt,nbg,ineg,iquit
      dimension range_phi(2), range_chi(2)
      dimension pf_intensity (npf,int(((range_chi(2)-range_chi(1))
     $     /step_size)+1),int((range_phi(2)-range_phi(1))/step_size+1))
      dimension bg_intensity (npf,nbg,int(((range_chi(2)-range_chi(1))
     $    /step_size)+1),int(((range_phi(2)-range_phi(1))/step_size+1)))
      dimension pf_2theta(npf),bg_2theta(2,npf)
      dimension pf_intensity_temp(npf,int(((range_chi(2)-range_chi(1))
     $     /step_size)+1),
     $     int(((range_phi(2)-range_phi(1))/step_size+1)))
      character*25 dum
c
      ineg=1
      iquit=0
      mult=1
      do while(ineg.eq.1)       !   .or. ineg.eq.1)
         ineg=0
         do ipf=1,npf
            do ichi=1,int((range_chi(2)-range_chi(1))/step_size)+1
               do iphi=1,int((range_phi(2)-range_phi(1))/step_size)+1
c     
                  if(ibg_mode.eq.3) then ! Full background
                     pf_intensity_temp(ipf,ichi,iphi) = 
     $                    pf_intensity(ipf,ichi,iphi)  - mult * (
     $                    (bg_intensity(ipf,2,ichi,iphi)-
     $                    bg_intensity(ipf,1,ichi,iphi))/
     $                    (bg_2theta(2,ipf) - bg_2theta(1,ipf)) *
     $                    (pf_2theta(ipf)   - bg_2theta(1,ipf)) +
     $                    bg_intensity(ipf,1,ichi,iphi) )
                     if(pf_intensity_temp(ipf,ichi,iphi).lt.0) ineg=1
                  else if(ibg_mode.eq.2) then ! Single background
                     pf_intensity_temp(ipf,ichi,iphi) = 
     $                    pf_intensity(ipf,ichi,iphi)  - mult * (
     $                    (bg_intensity(ipf,2,ichi,1   )-
     $                    bg_intensity(ipf,1,ichi,1   ))/
     $                    (bg_2theta(2,ipf) - bg_2theta(1,ipf)) *
     $                    (pf_2theta(ipf)   - bg_2theta(1,ipf)) +
     $                    bg_intensity(ipf,1,ichi,1   ))
                     if(pf_intensity_temp(ipf,ichi,iphi).lt.0) ineg=1
                  endif
               enddo 
            enddo
            if (ineg.eq.1) then
               write(*,'(a)')
               write(*,'(a)')'*****************************************'
               write(*,'(a)')'*WARNING) Neg intensity resulted from   *'
               write(*,'(a)')'* subroutine BACKGROUND_SUBTRACTION     *'
               write(*,'(a1,11x,a2,2x,i3,a14,11x,a1)')'*','at',ipf,
     $              'th pole figure','*'
               write(*,'(a)')'*****************************************'
               write(*,'(a)')
               ineg = 1
            endif
         enddo                  ! do over pole figure
c     
         if     (ineg.eq.0) then
            iquit=1
         elseif (ineg.eq.1) then
            write(*,'(a)')       
            write(*,'(a)')'********************************************'
            write(*,'(a)')'*Do you want a subtraction factor than 1.0 *'
            write(*,'(a)')'*    and to do the subtraction again?      *'
            write(*,'(a)')'*      a factor (YES) or 1.(NO)            *'
            write(*,'(a)')'********************************************'
            write(*,'(a)')
            read (*, *   ) mult
            if     (mult.eq.1) then
               ineg=-1
            elseif (mult.gt.1) then
               do while(mult.gt.1)
                  write(*,'(a)')'Mult factor must be smaller than 1'
                  write(*,'(a)')'Please type a factor again'
                  read (*, *   ) mult
               enddo
            elseif (mult.lt.1) then
               write(*,'(a,f4.2)') 'Entered factor is ', mult
            endif
         endif
      enddo                     ! do while upon iquit
ccc   Assign the temp PF intensity (all positive) to the real intensity
      do ipf=1,npf
         do ichi=1,int((range_chi(2)-range_chi(1))/step_size)+1
            do iphi=1,int((range_phi(2)-range_phi(1))/step_size)+1
c               pf_intensity(ipf,ichi,iphi) =
c     $              pf_intensity_temp(ipf,ichi,iphi)
            enddo
         enddo
      enddo
ccc
      return
      end subroutine
c
***************************************************************************
      subroutine scan(ioption,ur,infile,npfline,stepsize,range_phi,
     $              range_chi,range_intensity,ibg,npf,nbg,iheadnum,
     $              pf_2theta,bg_2theta,datemeasured,cdex)
*     subroutine scan is like a pre-processing subroutine
*     analyze the given UXD file, provides data & variables to
*     subroutine converting
***************************************************************************
      !parameter(maxpf=10)
      integer ioption,npfline,ur, iheadnum
      integer intensity,ibg,npf,nbg,max_intensity,min_intensity
      real theta2
      character*256 dummy,dum
      character*25  infile
      character*20  datemeasured
      character*3 cdex(npf)
      real stepsize, stepsize_check
      dimension range_chi(2), range_phi(2),range_intensity(2)
      !dimension pf_2theta(maxpf), bg_2theta(2,maxpf)
      dimension pf_2theta(npf), bg_2theta(2,npf)
!     save range_intensity
      integer ipeakmax !iflag for peak having maximum peak
      integer ipeakmin !iflag for peak having minimum peak
c
c     Note that ipeak (either max or min) > 100 means the peak is at Background intensity region 
c
c
c     intensity range initialization
c
      if (ioption.eq.1) then
         range_intensity(1) =99999
         range_intensity(2) =0.
      endif
c
****  HEADER starts
      iquit=0
**    CDEX (indecies for PF)
      read(ur,'(a)') dummy
      do ipf=1, npf
         read(ur,'(a1,a3)') dum,cdex(ipf)
         do kbg=1,nbg
            read(ur,'(a)') dum
         enddo
      enddo
c
      do while(iquit.eq.0)
         read(ur,'(a14,a1,a20)') dum,dummy,datemeasured
         if (trim(dum).eq.'_DATEMEASURED=') iquit=1
      enddo
      iquit=0
      do while(iquit.eq.0)
         read(ur,'(a6)') dum
         if (trim(dum).eq.'_ANODE') iquit=1
      enddo
****  HEADER ends
c
*     Phi2(DRIVE) Intensity DATA SET starts
      do 2000 ipf=1, npf
c     For pf & background (can be mixed up if background was measured)
c     Needs to tell background measurement from PF measurement
c
         do ichi=1, int((range_chi(2)-range_chi(1))/stepsize)+1
c     Checking the step_size      
            iquit=0
            do while(iquit.eq.0)
               read(ur,'(a10)') dum
               if(trim(dum).eq.'_STEPTIME=') then
                  read(ur,'(a10,f10.1)') dum,stepsize_check
                  if (trim(dum).ne.'_STEPSIZE=') then
                     write(*,*) 'ERR: position at SUBROUTINE SCAN'
                     pause
                     stop
                  endif
                  iquit = 1
               endif
            enddo
            if (stepsize.ne.stepsize_check) then
               write(*,*)
               write(*,'(a)') '*************************************'
               write(*,'(a)') '*  ERR: Stepsize is not consistent  *'
               write(*,'(a)') '*************************************'
               write(*,*)
               pause
               stop
            endif
c     Step_size consistency checking ends
c
c     Sub-header starts
            iquit=0
            do while(iquit.eq.0)
               read(ur,'(a7)' ) dum
               if(trim(dum).eq.'_THETA=') then
                  read(ur,'(a8,f8.3)' ) dum, pf_2theta(ipf)
                  if (trim(dum).ne.'_2THETA=') then
                     write(*,*) 'ERR: position at SUBROUTINE SCAN'
                     pause
                     stop
                  endif
                  iquit=1
               endif
            enddo
            iquit=0
            do while(iquit.eq.0)
               read(ur,'(a13)') dum
               if(trim(dum).eq.'_2THETACOUNTS') iquit=1
            enddo
c     Sub-header ends
c
c     PHI drive mode.       
            do iphi=1, int((range_phi(2)-range_phi(1))/stepsize)+1
               read (ur,*) theta2, intensity
               if      (intensity.le.range_intensity(1)) then
                  range_intensity(1)=intensity ! Seeking for the current min.
                  ipeakmin = ipf ! Identify the current peak
               elseif (intensity.ge.range_intensity(2)) then
                  range_intensity(2)=intensity ! Seeking for the current max.
                  ipeakmax = ipf ! Identify the current peak
               endif
            enddo               ! Loop over phi
         enddo                  ! Loop over chi
c     
         if (ibg.eq.2 .or.ibg.eq.3) then
            do 1000 n_bg=1,2
               do 500 ichi=1, int((range_chi(2)-range_chi(1))
     $              /stepsize)+1
c
ccc   stepsize checking
                  iquit=0
                  do while(iquit.eq.0)
                     read(ur,'(a10)') dum
                     if(trim(dum).eq.'_STEPTIME=') then
                        read(ur,'(a10,f8.3)') dum,stepsize_check
                        if (trim(dum).ne.'_STEPSIZE=') then
                           write(*,*) 'ERR: position at SUBROUTINE SCAN'
                           pause
                           stop
                        endif
                        iquit = 1
                     endif 
                  enddo         !step size checking
c     
                  if (stepsize.ne.stepsize_check) then
                     write(*,'(a)') 'Err: stepsize is not consistent'
                     pause
                     stop
                  endif
ccc   stepsize checking
c
c     bg_2theta input
                  iquit=0
                  do while(iquit.eq.0)
                     read(ur,'(a7)') dum
                     if(trim(dum).eq.'_THETA=') then
                        read(ur,'(a8,f8.3)') dum, bg_2theta(n_bg,ipf)
                        if(trim(dum).ne.'_2THETA=') then
                           write(*,'(a)')'ERR:'
                           write(*,'(a)')'position at subroutine scan'
                           pause
                           stop
                        endif
                        iquit = 1
                     endif
                  enddo 
c
c
c     Sub-header starts
                  iquit=0.
                  do while(iquit.eq.0)
                     read(ur,'(a13)') dum
                     if(trim(dum).eq.'_2THETACOUNTS') then
                        iquit=1
                     endif
                  enddo
c     Sub-header ends
c
                  if (ibg.eq.3) then ! full background
                     do iphi=1, int((range_phi(2)-range_phi(1)
     $                    )/stepsize)+1
                        read(ur,*) dum1, dum2
                        write(*,*) dum1, dum2                        
                        theta2 = dum1
                        intensity = dum2
c                        read(ur,*) theta2, intensity
                        if    (intensity.le.range_intensity(1)) then
                           range_intensity(1)=intensity
                           ipeakmin = ipf+100*n_bg
                        elseif(intensity.ge.range_intensity(2)) then
                           range_intensity(2)=intensity
                           ipeakmax = ipf+ 100*n_bg
                        endif
                     enddo      !iphi
                  else if(ibg.eq.2) then ! partial background
                     read(ur,*) theta2, intensity   
                     if     (intensity.le.range_intensity(1)) then
                        range_intensity(1)=intensity
                        ipeakmin= ipf+100*n_bg
                     elseif (intensity.ge.range_intensity(2)) then
                        range_intensity(2)=intensity
                        ipeakmax= ipf+100*n_bg
                     endif
                  endif         ! if ibg.eq.2 or ibg.eq.3 
c     (Respectively for single background and full background)
c
c     
 500           enddo            !ichi
 1000       enddo               ! loop over two background measures
            
         else if (ibg.eq.1 ) then
            write(*,'(a)') 'No background was measured'
         endif
c
 2000 enddo                     !Loop over pole figures
*     End of PHI2 vs. Intensity probe
c     
      write(*,'(1x,a32,i10)')'Max intensity:', int(range_intensity(2))
      write(*,'(1x,a32,i10)')'Min intensity:', int(range_intensity(1))
      write(*,'(1x,a32,i10)')'Max polefigure intensity is at:',ipeakmax
      if (ipeakmin.gt.100) then
         write(*,'(1x,a32,i10,a12)')'Min intensity is at:',
     $        ipeakmin,' PFs BG peak'
      else
         write(*,'(1x,a32,i10,a8 )')'Min intensity is at:',
     $        ipeakmin,' PF peak'
      endif

! max_intensity = range_intensity(2)
! min_intensity = range_intensity(1)
! min_i = min_intensity
! max_i = max_intensity
      return
      end subroutine
c
***************************************************************************
      subroutine normalize(pf_intensity,range_phi,range_chi
,
     $ range_intensity, step_size, npf,avg)
*
*     subroutine normazlize works for normalize the experimentally obtained
*     polefigures. Since EPF format allows only maximum 4 digits of 
*     intensity, normalization is necessary.
***************************************************************************    
      dimension range_phi(2), range_chi(2),range_intensity(2)
      real step_size,avg
      integer max_intensity
c      dimension pf_intensity(npf,chi_step,phi_step)
      dimension pf_intensity(npf,int(((range_chi(2)-range_chi(1))
     $     /step_size+1)) ,
     $     int(((range_phi(2)-range_phi(1))/step_size)+1))
c
      !write(*,*) 'chi_step:',chi_step
      !write(*,*) 'phi_step:',phi_step
c
      iquit=0
      do while(iquit.eq.0)
c
         max_intensity=0
         do ipf=1, npf
            do ich=1, (range_chi(2)-range_chi(1))/step_size+1
               do iph=1, (range_phi(2)-range_phi(1))/step_size+1
                  if (pf_intensity(ipf,ich,iph).gt.max_intensity)
     $                 max_intensity = pf_intensity(ipf,ich,iph)
               enddo
            enddo
         enddo
c
         if(max_intensity.gt.9999) then
            do ipf=1, npf
               do ich=1, (range_chi(2)-range_chi(1))/step_size+1
                  do iph=1, (range_phi(2)-range_phi(1))/step_size+1
                     pf_intensity(ipf,ich,iph) = 
     $                    pf_intensity(ipf,ich,iph) * avg
                  enddo
               enddo
            enddo
         else
            iquit=1
         endif
c
      enddo                     ! do-while loop
c
      return
      end subroutine
c
***************************************************************************
      subroutine input_env (nfile,inf,outf,npf,nbg,
     $     range_chi,range_phi,step_size,avg,ibg_mode,iopt,iformat)
*     subroutine input_env is for input environment settings 
*     from 'inp.env'.
***************************************************************************
      integer nfile,npf,nbg,ibg_mode,ur3,iopt,iformat
      character*25 inf(100),outf(100)
      character*256 dummy
      dimension range_chi(2), range_phi(2)
      real step_size,avg
c
      if (iopt.eq.0) then
         ur3=3                  ! for in.env file
         open(ur3,file='in', status='old')
         read (ur3,'(a)') dummy
         read (ur3, *   ) nfile
         read (ur3,'(a)') dummy
         do i=1, nfile
            read (ur3,'(a)') inf(i)
         enddo
         read (ur3,'(a)') dummy
         do i=1, nfile
            read (ur3,'(a)') outf(i)
         enddo
         read (ur3,'(a)') dummy
         read (ur3, *   ) npf
         read (ur3,'(a)') dummy
         read (ur3, *   ) nbg
         read (ur3,'(a)') dummy
         read (ur3, *   ) (range_chi(i),i=1,2)
         read (ur3,'(a)') dummy
         read (ur3, *   ) (range_phi(i),i=1,2)
         read (ur3,'(a)') dummy
         read (ur3, *   ) step_size
         read (ur3,'(a)') dummy
         read (ur3, *   ) ibg_mode
         read (ur3,'(a)') dummy
         read (ur3, *   ) avg
         read (ur3,'(a)') dummy
         read (ur3, *   ) iformat
      else if (iopt.eq.1) then
         write(*,'(a)') 'Default environment setting'
         write(*,'(a)') '* # of files'
         read (*,    *) nfile
         write(*,'(a)') '* input  FILE names'
         do ifile=1,nfile
            read(*,'(a)') inf(ifile)
         enddo
         write(*,'(a)') '* output FILE names, without extension'
         do ifile=1,nfile
            read(*,'(a)') outf(ifile)
         enddo
c     
         write(*,'(a)') '* # of pole figures in a file (npf)'
         read (*,*    ) npf
         write(*,'(a)') '* # of background measurements in a pole (nbg)'
         read (*,*    ) nbg
         write(*,'(a)') '* range_chi(1) for min, (2) for max'
         read (*,*    ) (range_chi(i),i=1,2)
         write(*,'(a)') '* range_phi(1) for min, (2) for max'
         read (*,*    ) (range_phi(i),i=1,2)
         write(*,'(a)') '*step_size? (default=5.0)'
         read (*,*    ) step_size
         write(*,'(a)') '*ibg_mode(1:no background,2:partial, 3:Full)'
         read (*,*    ) ibg_mode
         write(*,'(a)') '* AVG ? (default : 0.1) '
         read (*,*    ) avg
         write(*,'(a)') '* iformat? (0:EPF, 1:PFG)'
         read (*,*    ) iformat
      endif
c
      return
      end subroutine
