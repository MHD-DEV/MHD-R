SUBROUTINE CALIBRA
!
!  SHUFFLED COMPLEX EVOLUTION METHOD FOR GLOBAL OPTIMIZATION
!     -- Version 2.1
!
!  by QINGYUN DUAN
!  DEPARTMENT OF HYDROLOGY & WATER RESOURCES
!  UNIVERSITY OF ARIZONA, TUCSON, AZ 85721
!  (602) 621-9360, email: duan@hwr.arizona.edu
!
!  WRITTEN IN OCTOBER 1990.
!  REVISED IN AUGUST 1991
!  REVISED IN APRIL 1992
!
!  STATEMENT BY AUTHOR:
!  --------------------
!
!     This general purpose global optimization program is developed at
!     the Department of Hydrology & Water Resources of the University
!     of Arizona.  Further information regarding the SCE-UA method can
!     be obtained from Dr. Q. Duan, Dr. S. Sorooshian or Dr. V.K. Gupta
!     at the address and phone number listed above.  We request all
!     users of this program make proper reference to the paper entitled
!     'Effective and Efficient Global Optimization for Conceptual
!     Rainfall-runoff Models' by Duan, Q., S. Sorooshian, and V.K. Gupta,
!     Water Resources Research, Vol 28(4), pp.1015-1031, 1992.
!
 	USE VARS_MAIN
	USE VARS_CALIB
	IMPLICIT NONE
	INTEGER I,J,K,JSEED(10),IB 
    INTEGER ierror,iwarn,ideflt
    INTEGER nrun
    character*10 pcntrl,deflt,usrsp
    character*4 reduc,initl,ysflg,noflg
    data deflt/' DEFAULT  '/
    data usrsp/'USER SPEC.'/
    data ysflg/'YES '/
    data noflg/'NO  '/

    data JSEED/2,3,5,7,11,13,17,19,23,29/
    !
    ! ABRE O ARQUIVO COM PARAMETROS DE CALIBRACAO
    !
	OPEN(FILCAL,FILE=DIR_DADOS//'Entrada/Calibra.hig',STATUS='OLD')    
    !
    !   READ THE SCE CONTROL PARAMETERS
    !
    !  LIST OF INPUT ARGUMENT VARIABLES
    !
    !     a(.) = initial parameter set
    !     BLOW(.) = lower bound on parameters
    !     BUPP(.) = upper bound on parameters
    !     nopt = number of parameters to be optimized
    !     POS(I) = loc(i) = index on the order of parameters to be optimized
    !
    !
    !  LIST OF SCE ALGORITHMIC CONTROL PARAMETERS:
    !
    !     ngs = number of complexes in the initial population
    !     npg = number of points in each complex
    !     npt = total number of points in initial population (npt=ngs*npg)
    !     nps = number of points in a sub-complex
    !     nspl = number of evolution steps allowed for each complex before
    !         complex shuffling
    !     mings = minimum number of complexes required, if the number of
    !         complexes is allowed to reduce as the optimization proceeds
    !     iseed = initial random seed
    !     iniflg = flag on whether to include the initial point in population
    !         = 0, not included
    !         = 1, included
    !     iprint = flag for controlling print-out after each shuffling loop
    !         = 0, print information on the best point of the population
    !         = 1, print information on every point of the population   
    !
   
    !
    !  CONVERGENCE CHECK PARAMETERS
    !
    !     maxn = max no. of trials allowed before optimization is terminated
    !     kstop = number of shuffling loops in which the criterion value must
    !         chang by the given percentage before optimization is terminated
    !     pcento = percentage by which the criterion value must change in
    !         given number of shuffling loops
    !     ipcnvg = flag indicating whether parameter convergence is reached
    !         (i.e., check if gnrng is less than 0.001)
    !         = 0, parameter convergence not satisfied
    !         = 1, parameter convergence satisfied
    !

    READ(FILCAL,*)
    read(FILCAL,*) maxn,kstop,pcento,ngs,iseed,ideflt
    if (iseed .eq. 0) iseed = 1969
!
!   IF ideflt IS EQUAL TO 1, READ THE SCE CONTROL PARAMETERS
!
    read(FILCAL,*)
    read(FILCAL,*)
    if (ideflt == 1) Then
        read(FILCAL,*) npg,nps,nspl,mings,iniflg,iprint
        pcntrl = usrsp
    else
        read(FILCAL,*)
        pcntrl = deflt
    endif
    
    READ(FILCAL,*)
    READ(FILCAL,*)
	READ(FILCAL,*) IBCAL,NBVFO ! INDICA QUE BACIA SERA CALIBRADA E O NUMERO DE POSTO ASSOCIADO A BACIA USADO PARA CALIBRAR
	
	CALL DOMINIO
	
	!
	! POS INDICA QUAIS VARIAVEIS SERAO CALIBRADAS =1 CALIBRA; =0 NAO CALIBRA
	!
	READ(FILCAL,*)
	READ(FILCAL,*)(POS(I),I=1,NPAR)
	READ(FILCAL,*)
	
!
! CALCULA O NUMERO DE PARAMETROS A SEREM OTIMIZADOS
!
    NOPT=0
    DO I=1,NPAR
        IF(POS(I) > 0) THEN
            NOPT=NOPT+1
            POS(NOPT)=I ! POS PASSA A SER UM VETOR DE POSICAO
        ENDIF
    ENDDO
!
! ALOCA ESPACO DE MEMORIA
!
    ALLOCATE(A(NOPT),blow(NOPT),bupp(NOPT)) 
!
!   READ THE PARAMETER BOUNDS
!
    READ(FILCAL,*) (VPAR(I),I=1,NPAR)
    DO I=1,NOPT
        BLOW(I)=VPAR(POS(I)) ! VALOR MINIMO
    ENDDO
    
    READ(FILCAL,*) (VPAR(I),I=1,NPAR)
    DO I=1,NOPT
        BUPP(I)=VPAR(POS(I)) ! VALOR MAXIMO
    ENDDO
	CLOSE(FILCAL)  ! FIM LEITURA ARQUIVO DE CALIBRACAO
    
	OPEN(FILAJUSTE,FILE=DIR_DADOS//'Entrada/ParAjuste.hig',STATUS='OLD')
    READ(FILAJUSTE,*) 
        
        
    ! LE FATORES DE AJUSTE ORIGINAIS DO ARQUIVO FILEAJUSTE.HIG E OS TRANSFORMA EM FUNCAO DOS FATORES DO PARCALIBRA.HIG
    ! COMO JÁ PASSOU PELA ROTINA LECELL, OS PARAMETROS ESTAO ATUALIZADOS
    DO IB=1,NB
        IF(IB == IBCAL) THEN
            READ(FILAJUSTE,'(A10,10G10.0)') TITULO,(VPAR(I),I=1,NPAR) ! LE OS VALORES DE CALIBRACAO INICIAIS E OS INSERE NO VETOR VPAR
        ELSE
            READ(FILAJUSTE,*)
        ENDIF
    ENDDO
	CLOSE (FILAJUSTE)

    ! CALCULA O VALOR DE CB DE REFERENCIA EM DIAS PARA ATUALIZACAO DE PARAMETROS
    CB_REF=CB(IBCAL)/(VPAR(NPAR)*86400.)
!
!   ABRE ARQUIVO EVOLUCAO
!
    OPEN(FILEVOL,FILE=DIR_DADOS//'Saida/Evolucao.hig',STATUS='UNKNOWN')
    WRITE(FILEVOL,'(10X,A46,/,10x,46(1h=))' ) 'SHUFFLED COMPLEX EVOLUTION GLOBAL OPTIMIZATION'
    WRITE(*,'(A46,/,46(1h=))' ) 'SHUFFLED COMPLEX EVOLUTION GLOBAL OPTIMIZATION'
    WRITE(FILEVOL,'(//,10X,A59,/,10x,59(1h=))') 'READ AND WRITE THE INPUT INFORMATION FOR HYDROLOGICAL MODEL'
    WRITE(*,'(//,A59,/,59(1h=))') 'READ AND WRITE THE INPUT INFORMATION FOR HYDROLOGICAL MODEL'
    WRITE(FILEVOL,'(//,A25,5X,I10,//,A30,I10)') 'SUBBACIA A SER OTIMIZADA:',IBCAL,'POSTO UTILIZADO NA OTIMIZACAO:',NBVFO
    WRITE(*,'(//,A25,5X,I10,//,A30,I10)') 'SUBBACIA A SER OTIMIZADA:',IBCAL,'POSTO UTILIZADO NA OTIMIZACAO:',NBVFO    
    WRITE(FILEVOL,'(//,A25,//,<NPAR>(4X,A6),/,<NPAR>(F10.4))') 'INITIAL PARAMETER VALUES:',(XNAME(J),J=1,NPAR),(VPAR(J),J=1,NPAR)
    WRITE(*,'(//,A25,//,<NPAR>(4X,A6),/,<NPAR>(F10.4))') 'INITIAL PARAMETER VALUES:',(XNAME(J),J=1,NPAR),(VPAR(J),J=1,NPAR)
    WRITE(FILEVOL,'(//,A31,//,<nopt>(4x,a6))') 'THE PARAMETERS TO BE OPTIMIZED:',(XNAME(POS(j)),j = 1, nopt)
    WRITE(*,'(//,A31,//,<nopt>(4x,a6))') 'THE PARAMETERS TO BE OPTIMIZED:',(XNAME(POS(j)),j = 1, nopt)
!
!   IF ideflt IS EQUAL TO 0, SET THE SCE CONTROL PARAMETERS TO THE DEFAULT VALUES
!
    if (ideflt .eq. 0) then
        npg = 2*nopt + 1
        nps = nopt + 1
        nspl = npg
        mings = ngs
        iniflg = 0
        iprint = 0
    endif
    
!
!   CHECK IF THE SCE CONTROL PARAMETERS ARE VALID
!
    ierror = 0
    iwarn = 0
    if (ngs .lt. 1 .or. ngs .ge. 1320) then
        write(FILEVOL,900) ngs
        write(*,900) ngs
900     format(//,1x,'**ERROR** NUMBER OF COMPLEXES IN INITIAL POPULATION ',&
     &  i5,' IS NOT A VALID CHOICE')
        ierror = ierror + 1
    endif
!
    if (kstop .lt. 0 .or. kstop .ge. 20) then
        write(FILEVOL,901) kstop
        write(*,901) kstop
901     format(//,1x,'**WARNING** THE NUMBER OF SHUFFLING LOOPS IN',&
     &  ' WHICH THE CRITERION VALUE MUST CHANGE ',/,13x,'SHOULD BE',&
     &  ' GREATER THAN 0 AND LESS THAN 10.  ','kstop = ',i2,&
     &  ' WAS SPECIFIED.'/,13x,'BUT kstop = 5 WILL BE USED INSTEAD.')
        iwarn = iwarn + 1
        kstop=5
    endif
!
    if (mings .lt. 1 .or. mings .gt. ngs) then
        write(FILEVOL,902) mings
        write(*,902) mings
902     format(//,1x,'**WARNING** THE MINIMUM NUMBER OF COMPLEXES ',&
     &         i2,' IS NOT A VALID CHOICE. SET IT TO DEFAULT')
        iwarn = iwarn + 1
        mings = ngs
    endif
!
    if (npg .lt. 2 .or. npg .gt. 1320/max(ngs,1)) then
        write(FILEVOL,903) npg
        write(*,903) npg
903     format(//,1x,'**WARNING** THE NUMBER OF POINTS IN A COMPLEX ',&
     &         I4,' IS NOT A VALID CHOICE, SET IT TO DEFAULT')
        iwarn = iwarn + 1
        npg = 2*nopt+1
    endif
!
    if (nps.lt.2 .or. nps.gt.npg .or. nps.gt.50) then
        write(FILEVOL,904) nps
        write(*,904) nps
904     format(//,1x,'**WARNING** THE NUMBER OF POINTS IN A SUB-','COMPLEX ',&
     &  i4,' IS NOT A VALID CHOICE, SET IT TO DEFAULT')
        iwarn = iwarn + 1
        nps = nopt + 1
    endif
!
    if (nspl .lt. 1) then
        write(FILEVOL,905) nspl
        write(*,905) nspl
905     format(//,1x,'**WARNING** THE NUMBER OF EVOLUTION STEPS TAKEN IN EACH COMPLEX BEFORE SHUFFLING ',I4,/,13x,&
     &         'IS NOT A VALID CHOICE, SET IT TO DEFAULT')
        iwarn = iwarn + 1
        nspl = npg
    endif
!
!   COMPUTE THE TOTAL NUMBER OF POINTS IN INITIAL POPULATION
!
    npt = ngs * npg
!
    if (npt .gt. 1320) then
        write(FILEVOL,906) npt
        write(*,906) npt
906     format(//,1x,'**WARNING** THE NUMBER OF POINTS IN INITIAL POPULATION ',i5,' EXCEED THE POPULATION LIMIT,',/,13x,&
     &         'SET NGS TO 2, AND NPG, NPS AND NSPL TO DEFAULTS')
        iwarn = iwarn + 1
        ngs = 2
        npg = 2*nopt + 1
        nps = nopt + 1
        nspl = npg
    endif
!
!   ALOCA OS PARAMETROS A SEREM OTIMIZADOS NO VETOR A
!   VERIFIQUE SE OS PARAMETROS ESTAO DENTRO DO INTERVALO ADMISSIVEL
!

    DO I=1,NOPT
        A(I)=VPAR(POS(I)) 
        IF(A(I) < BLOW(I) .OR. A(I) > BUPP(I)) THEN
            IERROR=IERROR+1
            WRITE(FILEVOL,917) XNAME(POS(I))
            WRITE(*,917) XNAME(POS(I))
917         format(//,8x,'*** PARAMETROS ',A6,' FORA DO INTERVALO ADMISSIVEL ****')            
        ENDIF
    ENDDO
!
!  PRINT OUT THE TOTAL NUMBER OF ERROR AND WARNING MESSAGES
!
    if (ierror .ge. 1) write(FILEVOL,907) ierror
907 format(//,1x,'*** TOTAL NUMBER OF ERROR MESSAGES IS ',i2)
!
    if (iwarn .ge. 1) write(FILEVOL,908) iwarn
908 format(//,1x,'*** TOTAL NUMBER OF WARNING MESSAGES IS ',i2)
!
    if (mings .lt. ngs) then
        reduc = ysflg
    else
        reduc = noflg
    endif
!
    if (iniflg .ne. 0) then
        initl = ysflg
    else
        initl = noflg
    endif
!
!   PRINT SHUFFLED COMPLEX EVOLUTION OPTIMIZATION OPTIONS
!
    write(FILEVOL,910)
910 format(//,2x,'SCE CONTROL',5x,'MAX TRIALS',5x,'REQUIRED IMPROVEMENT',5x,'RANDOM',/,3x,'PARAMETER',8x,&
   &'ALLOWED',6x,'PERCENT',4x,'NO. LOOPS',6x,'SEED',/,2x,11(1h-),5x,10(1H-),5x,7(1h-),4x,9(1h-),5x,6(1h-))
    write(FILEVOL,912) pcntrl,maxn,pcento*100.,kstop,iseed
912 format(3x,a10,7x,i5,10x,f3.1,9x,i2,9x,i5)
    write(FILEVOL,914) ngs,npg,npt,nps,nspl
914 format(//,18x,'SCE ALGORITHM CONTROL PARAMETERS',/,18x,32(1H=),//,2x,'NUMBER OF',5x,'POINTS PER',5x,'POINTS IN',6x,'POINTS PER',&
   &4x,'EVOL. STEPS',/,2x,'COMPLEXES',6X,'COMPLEX',6x,'INI. POPUL.',5x,'SUB-COMPLX',4x,'PER COMPLEX',/,2x,9(1h-),5x,10(1h-),4x,&
   &11(1h-),5x,10(1h-),4x,11(1h-),5x,/,2x,5(i5,10x))
    write(FILEVOL,915) reduc,mings,initl
915 format(//,15x,'COMPLX NO.',5x,'MIN COMPLEX',5x,'INI. POINT',/,15x,'REDUCTION',6x,'NO. ALLOWED',6x,'INCLUDED',/,&
   &15x,10(1h-),5x,11(1h-),5x,10(1h-),/,18x,a4,6x,i8,13x,a4)
    write(FILEVOL,916)
916 format(//,8x,'INITIAL PARAMETER VALUES AND PARAMETER BOUNDS',/,8x,45(1h=),//,2x,'PARAMETER',5x,'INITIAL VALUE',5x,&
   &'LOWER BOUND',5x,'UPPER BOUND',/,2x,9(1h-),5x,13(1h-),5x,11(1h-),5x,11(1h-))
    do i = 1, nopt
        write(FILEVOL,'(2x,a6,4x,3(6x,f10.4))') xname(POS(I)),a(i),BLOW(i),BUPP(i)
    enddo
    if (ierror .ge. 1) then
        write(FILEVOL,922)
922     format(//,'*** THE OPTIMIZATION SEARCH IS NOT CONDUCTED BECAUSE OF INPUT DATA ERROR ***')
        stop
    endif
!
!   TRANSFORMACAO LOGARITMICA DAS VARIAVEIS DE OTIMIZACAO
!    
    A(1:NOPT)=LOG(A(1:NOPT))
    BLOW(1:NOPT)=LOG(MAX(BLOW(1:NOPT),0.0001))
    BUPP(1:NOPT)=LOG(MAX(BUPP(1:NOPT),0.0001)) 
!
!   OTIMIZA
!
   if (iseed > 0) then
        nrun = min(iseed,10)
   else
        nrun = 1
   endif
   j=npar
   do i=1, nrun
        if (nrun .ne. 1) iseed = -jseed(i)
        write (*,*) '@ SCE-UA Run Number',i,' Random Seed Value',iseed
        call sceua(FILEVOL)
    enddo
!
!  END OF PROGRAM
!
    DEALLOCATE(A,blow,bupp)
    RETURN
end SUBROUTINE CALIBRA
!
!
!   
subroutine sceua(FILEVOL)
    USE VARS_CALIB
    USE VARS_MAIN, ONLY: R2,NASH,LNASH
    implicit real*8 (a-h,o-z)
    !
    !
    !  LIST OF LOCAL VARIABLES
    !     x(.,.) = coordinates of points in the population
    !     xf(.) = function values of x(.,.)
    !     xx(.) = coordinates of a single point in x
    !     cx(.,.) = coordinates of points in a complex
    !     cf(.) = function values of cx(.,.)
    !     s(.,.) = coordinates of points in the current simplex
    !     sf(.) = function values of s(.,.)
    !     bestx(.) = best point at current shuffling loop
    !     bestf = function value of bestx(.)
    !     worstx(.) = worst point at current shuffling loop
    !     worstf = function value of worstx(.)
    !     xnstd(.) = standard deviation of parameters in the population
    !     gnrng = normalized geometric mean of parameter ranges
    !     lcs(.) = indices locating position of s(.,.) in x(.,.)
    !     bound(.) = bound on ith variable being optimized
    !     ngs1 = number of complexes in current population
    !     ngs2 = number of complexes in last population
    !     iseed1 = current random seed
    !     criter(.) = vector containing the best criterion values of the last
    !         10 shuffling loops
    !
 
    !
    !  ARRAYS FROM THE INPUT DATA
    !
     INTEGER FILEVOL
    !
    !  LOCAL ARRAYS
    !
    dimension xx(nopt),bestx(NOPT),worstx(NOPT),xf(ngs*npg)
    dimension s(nps,nopt),sf(nps),lcs(nps)
    dimension criter(20),unit(NOPT)

    allocate (x(ngs*npg,NOPT),xnstd(nopt),bound(nopt))
    allocate (cx(ngs*npg,nopt),cf(ngs*npg))

    !
    write (*,*) ' ENTER THE SCEUA SUBROUTINE --- '     
    !
    !  INITIALIZE VARIABLES
    !
    nloop = 0
    loop = 0
    igs = 0
    !
    !  INITIALIZE RANDOM SEED TO A NEGATIVE INTEGER
    !
    iseed1 = -abs(iseed)
    !
    !  COMPUTE THE TOTAL NUMBER OF POINTS IN INITIAL POPUALTION
    !
    npt = ngs * npg
    ngs1 = ngs
    npt1 = npt
    !
    write(FILEVOL,400)
400 format(//,2x,50(1h=),/,2x,'ENTER THE SHUFFLED COMPLEX EVOLUTION GLOBAL SEARCH',/,2x,50(1h=))
    write (*,*) ' ***  Evolution Loop Number ',nloop
    !
    !  COMPUTE THE BOUND FOR PARAMETERS BEING OPTIMIZED
    !
    bound = bupp - blow
    unit = 1.0 
    !
    !  COMPUTE THE FUNCTION VALUE OF THE INITIAL POINT
    !
    fa = functn(a)
    !
    !  PRINT THE INITIAL POINT AND ITS CRITERION VALUE
    !
    write(FILEVOL,500)
500 format(//,'*** PRINT THE INITIAL POINT AND ITS CRITERION VALUE ***')
    write(FILEVOL,510) (xname(POS(j)),j=1,nopt)
510 format(/,' CRITERION',<NOPT>(4x,a6),'      R2','    NASH','   LNASH',/1x,<10*NOPT+9+24>(1h-))
    write(FILEVOL,'(g10.3,<NOPT>F10.4,3F8.2)') fa,DEXP(a(1:nopt)),R2(IBCAL),NASH(IBCAL),LNASH(IBCAL)
    !
    !  GENERATE AN INITIAL SET OF npt1 POINTS IN THE PARAMETER SPACE
    !  IF iniflg IS EQUAL TO 1, SET x(1,.) TO INITIAL POINT a(.)
    !

!!!  AQUI
    if (iniflg == 1) then
        x(1,1:nopt)=a(1:nopt)
        xf(1) = fa
    !
    !  ELSE, GENERATE A POINT RANDOMLY AND SET IT EQUAL TO x(1,.)
    !
    else  
        call getpnt(1,iseed1,xx,unit,blow)
        x(1,1:nopt) = xx(1:nopt)
        xf(1) = functn(xx)
    endif
    icall = 1
    if (icall >= maxn) go to 900
    !
    !  GENERATE npt1-1 RANDOM POINTS DISTRIBUTED UNIFORMLY IN THE PARAMETER
    !  SPACE, AND COMPUTE THE CORRESPONDING FUNCTION VALUES
    !
    do i = 2, npt1
        call getpnt(1,iseed1,xx,unit,blow)
        x(i,1:nopt) = xx(1:nopt)
        xf(i) = functn(xx)
        icall = icall + 1
        if (icall >= maxn) then
            npt1 = i
            exit
        endif
    enddo
    !
    !  ARRANGE THE POINTS IN ORDER OF INCREASING FUNCTION VALUE
    !
    call SORT1(npt,npt1,nopt,x,xf)
    !
    !  RECORD THE BEST AND WORST POINTS
    !
    bestx(1:nopt) = x(1,1:nopt)
    worstx(1:nopt) = x(npt1,1:nopt)
    bestf = xf(1)
    worstf = xf(npt1)
    !
    !  COMPUTE THE PARAMETER RANGE FOR THE INITIAL POPULATION
    !
    call parstt(npt1,gnrng,ipcnvg)
    !
    !  PRINT THE RESULTS FOR THE INITIAL POPULATION
    !
    write(FILEVOL,600)
600 format(//,1x,'*** PRINT THE RESULTS OF THE SCE SEARCH ***')
    write(FILEVOL,610) (xname(POS(j)),j=1,nopt)
610 format(/,1x,'LOOP',1x,'TRIALS',1x,'COMPLXS',2x,'BEST F',3x,'WORST F',2x,'PAR RNG',2x,<nopt>(4x,a6),'      R2','    NASH','   LNASH')
    write(FILEVOL,'(i5,1x,i5,3x,i5,3g10.3,<nopt>(f10.4),3F8.2)') nloop,icall,ngs1,bestf,worstf,gnrng,DEXP(bestx(1:nopt)),R2(IBCAL),NASH(IBCAL),LNASH(IBCAL)
    if (iprint == 1) then
        write(FILEVOL,650) nloop
        do i = 1, npt1
            write(FILEVOL,'(15x,g10.3,20x,<nopt>(f10.4),3F8.2)') xf(i),DEXP(x(i,1:nopt)),R2(IBCAL),NASH(IBCAL),LNASH(IBCAL)
        enddo
    endif
    !
    if (icall >= maxn) go to 900
    if (ipcnvg == 1) go to 920
    !
    !  BEGIN THE MAIN LOOP ----------------
1000 continue
    nloop = nloop + 1
    !
    write (*,*) ' ***  Evolution Loop Number ',nloop
    !
    !  BEGIN LOOP ON COMPLEXES
    do igs = 1, ngs1
        !
        !  ASSIGN POINTS INTO COMPLEXES
        do k1 = 1, npg
            k2 = (k1-1) * ngs1 + igs
            cx(k1,1:nopt) = x(k2,1:nopt)
            cf(k1) = xf(k2)
        enddo
        !
        !  BEGIN INNER LOOP - RANDOM SELECTION OF SUB-COMPLEXES ---------------
        !
        do loop = 1,nspl
            !
            !  CHOOSE A SUB-COMPLEX (nps points) ACCORDING TO A LINEAR
            !  PROBABILITY DISTRIBUTION
            !
            if (nps .eq. npg) then
                lcs(1:nps) = (/ 1:nps /)
            else
                ! acho que está errado javier
                rand = ran1(iseed1)
                lcs(1) = 1 + dint(npg + 0.5 - dsqrt( (npg+.5)**2 - npg * (npg+1) * rand ))
                do k = 2, nps
    60              rand = ran1(iseed1)
                    lpos = 1 + dint(npg + 0.5 - dsqrt((npg+.5)**2 - npg * (npg+1) * rand ))
                    do k1 = 1, k-1
                        if (lpos == lcs(k1)) go to 60
                    enddo
                    lcs(k) = lpos
                enddo
                !
                !  ARRANGE THE SUB-COMPLEX IN ORDER OF INCEASING FUNCTION VALUE
                !
                call SORT2(nps,lcs)
            endif
            !
            !  CREATE THE SUB-COMPLEX ARRAYS
            !
            do k = 1, nps
                s(k,1:nopt) = cx(lcs(k),1:nopt)
                sf(k) = cf(lcs(k))
            enddo
            !
            !  USE THE SUB-COMPLEX TO GENERATE NEW POINT(S)
            !
            call cce(s,sf,icall,iseed1)
            !
            !  IF THE SUB-COMPLEX IS ACCEPTED, REPLACE THE NEW SUB-COMPLEX
            !  INTO THE COMPLEX
            !
            do k = 1, nps
                cx(lcs(k),1:nopt) = s(k,1:nopt)
                cf(lcs(k)) = sf(k)
            enddo
            !
            !  SORT THE POINTS
            !
            call SORT1(npt,npg,nopt,cx,cf)
            !
            !  IF MAXIMUM NUMBER OF RUNS EXCEEDED, BREAK OUT OF THE LOOP
            !
            if (icall >= maxn) go to 2222
            !
            !  END OF INNER LOOP ------------
            !
        enddo
2222    continue
        !
        !  REPLACE THE NEW COMPLEX INTO ORIGINAL ARRAY x(.,.)
        do k1 = 1, npg
            k2 = (k1-1) * ngs1 + igs
            x(k2,1:nopt) = cx(k1,1:nopt)
            xf(k2) = cf(k1)
        end do
        if (icall .ge. maxn) go to 3333
    !
    !  END LOOP ON COMPLEXES
    !
    enddo
    !
    !  RE-SORT THE POINTS
    !
3333 call SORT1(npt,npt1,nopt,x,xf)
    !
    !  RECORD THE BEST AND WORST POINTS
    !
    bestx(1:nopt) = x(1,1:nopt)
    worstx(1:nopt) = x(npt1,1:nopt)
    bestf = xf(1)
    worstf = xf(npt1)
    !
    !  TEST THE POPULATION FOR PARAMETER CONVERGENCE
    !
    call parstt(npt1,gnrng,ipcnvg)
    !
    !  PRINT THE RESULTS FOR CURRENT POPULATION
    !
    if (mod(nloop,5) == 0) then
        write(FILEVOL,610) (xname(POS(j)),j=1,nopt)
    endif
    write(FILEVOL,'(i5,1x,i5,3x,i5,3g10.3,<nopt>(F10.4),3F8.2)') nloop,icall,ngs1,bestf,worstf,gnrng,DEXP(bestx(1:nopt)),R2(IBCAL),NASH(IBCAL),LNASH(IBCAL)
    if (iprint .eq. 1) then
        write(FILEVOL,650) nloop
650     format(/,1x,'POPULATION AT LOOP ',i3,/,1x,22(1h-))
        do i = 1, npt1
            write(FILEVOL,'(15x,g10.3,20x,<nopt>(F10.4),3F8.2)') xf(i),DEXP(x(i,1:nopt)),R2(IBCAL),NASH(IBCAL),LNASH(IBCAL)
        enddo
    endif
    !
    !  TEST IF MAXIMUM NUMBER OF FUNCTION EVALUATIONS EXCEEDED
    if (icall >= maxn) go to 900
    !
    !  COMPUTE THE COUNT ON SUCCESSIVE LOOPS W/O FUNCTION IMPROVEMENT
    criter(20) = bestf
    if (nloop >= (kstop+1)) then
        denomi = dabs(criter(20-kstop) + criter(20)) / 2.
        timeou = dabs(criter(20-kstop) - criter(20)) / denomi
        if (timeou .lt. pcento) go to 910
    endif
    criter(1:19) = criter(2:20)
    !
    !  IF POPULATION IS CONVERGED INTO A SUFFICIENTLY SMALL SPACE
    if (ipcnvg == 1) go to 920
    !
    !  NONE OF THE STOPPING CRITERIA IS SATISFIED, CONTINUE SEARCH
    !
    !  CHECK FOR COMPLEX NUMBER REDUCTION
    if (ngs1 > mings) then
        ngs2 = ngs1
        ngs1 = ngs1 - 1
        npt1 = ngs1 * npg        
        call comp(npt1,ngs1,ngs2,xf)
    endif
    !
    !  END OF MAIN LOOP -----------
    go to 1000
    !
    !  SEARCH TERMINATED
900 write(FILEVOL,800) maxn,loop,igs,nloop
800 format(//,1x,'*** OPTIMIZATION SEARCH TERMINATED BECAUSE THE',&
    &       ' LIMIT ON THE MAXIMUM',/,5x,'NUMBER OF TRIALS ',i5,&
    &       ' EXCEEDED.  SEARCH WAS STOPPED AT',/,5x,'SUB-COMPLEX ',&
    &       i3,' OF COMPLEX ',i3,' IN SHUFFLING LOOP ',i3,' ***')
    go to 999
910 write(FILEVOL,810) pcento*100.,kstop
    810 format(//,1x,'*** OPTIMIZATION TERMINATED BECAUSE THE CRITERION',&
    &       ' VALUE HAS NOT CHANGED ',/,5x,f5.2,' PERCENT IN',i3,&
    &       ' SHUFFLING LOOPS ***')
    go to 999
920 write(FILEVOL,820) gnrng*100.
820 format(//,1x,'*** OPTIMIZATION TERMINATED BECAUSE THE POPULATION',&
    &       ' HAS CONVERGED INTO ',/,4x,f5.2,' PERCENT OF THE',&
    &       ' FEASIBLE SPACE ***')
    !
    !  PRINT THE FINAL PARAMETER ESTIMATE AND ITS FUNCTION VALUE
    !
999 write(FILEVOL,830)
830 format(//,'*** PRINT THE FINAL PARAMETER ESTIMATE AND ITS',&
    &       ' CRITERION VALUE ***')
    write(FILEVOL,510) (xname(POS(j)),j=1,nopt)
    write(FILEVOL,'(g10.3,<nopt>F10.4,3F8.2)') bestf,DEXP(bestx(1:nopt)),R2(IBCAL),NASH(IBCAL),LNASH(IBCAL)
    !
    !  END OF SUBROUTINE SCEUA
    deallocate (x,xnstd,bound)
    deallocate (cx,cf)
    return
end
!
!  ALGORITHM TO GENERATE A NEW POINT(S) FROM A SUB-COMPLEX
!====================================================================
subroutine cce(s,sf,icall,iseed)
    USE VARS_CALIB, ONLY: nopt,nps,blow,bupp,maxn,xnstd
    !
    !
    !  SUB-COMPLEX VARIABLES
    implicit real*8 (a-h,o-z)
    parameter (c1=0.8,c2=0.4)
    dimension s(nps,nopt),sf(nps)
    !
    !  LIST OF LOCAL VARIABLES
    !    sb(.) = the best point of the simplex
    !    sw(.) = the worst point of the simplex
    !    w2(.) = the second worst point of the simplex
    !    fw = function value of the worst point
    !    ce(.) = the centroid of the simplex excluding wo
    !    snew(.) = new point generated from the simplex
    !    iviol = flag indicating if constraints are violated
    !          = 1 , yes
    !          = 0 , no
    !
    dimension sw(nopt),sb(nopt),ce(nopt),snew(nopt)
    !
    !  EQUIVALENCE OF VARIABLES FOR READABILTY OF CODE
    !
    n = nps
    m = nopt
    alpha = 1.0
    beta = 0.5
    !
    !  IDENTIFY THE WORST POINT wo OF THE SUB-COMPLEX s
    !  COMPUTE THE CENTROID ce OF THE REMAINING POINTS
    !  COMPUTE step, THE VECTOR BETWEEN wo AND ce
    !  IDENTIFY THE WORST FUNCTION VALUE fw
    sb(1:m) = s(1,1:m)
    sw(1:m) = s(n,1:m)
    do j = 1, m
        ce(j) = 0.0
        ce(j) = (ce(j) + sum(s(1:n-1,j)))  /dble(n-1)
    enddo
    fw = sf(n)
    !
    !  COMPUTE THE NEW POINT snew
    !
    !  FIRST TRY A REFLECTION STEP
    snew(1:m) = ce(1:m) + alpha * (ce(1:m) - sw(1:m))
    !
    !  CHECK IF snew SATISFIES ALL CONSTRAINTS
    call chkcst(nopt,snew,blow,bupp,ibound)
    !
    !  snew IS OUTSIDE THE BOUND,
    !  CHOOSE A POINT AT RANDOM WITHIN FEASIBLE REGION ACCORDING TO
    !  A NORMAL DISTRIBUTION WITH BEST POINT OF THE SUB-COMPLEX
    !  AS MEAN AND STANDARD DEVIATION OF THE POPULATION AS STD
    if (ibound .ge. 1) call getpnt(2,iseed,snew,xnstd,sb)
    !
    !  COMPUTE THE FUNCTION VALUE AT snew
    !
    fnew = functn(snew)
    icall = icall + 1
    !
    !  COMPARE fnew WITH THE WORST FUNCTION VALUE fw
    !
    !  fnew IS LESS THAN fw, ACCEPT THE NEW POINT snew AND RETURN
    !
    if (fnew .le. fw) go to 20
    if (icall .ge. maxn) return
    !
    !  fnew IS GREATER THAN fw, SO TRY A CONTRACTION STEP
    !
    snew(1:m) = ce(1:m) - beta * (ce(1:m) - sw(1:m))
    !
    !  COMPUTE THE FUNCTION VALUE OF THE CONTRACTED POINT
    !
    fnew = functn(snew)
    icall = icall + 1
    !
    !  COMPARE fnew TO THE WORST VALUE fw
    !  IF fnew IS LESS THAN OR EQUAL TO fw, THEN ACCEPT THE POINT AND RETURN
    if (fnew .le. fw) go to 20
    if (icall .ge. maxn) return
    !
    !  IF BOTH REFLECTION AND CONTRACTION FAIL, CHOOSE ANOTHER POINT
    !  ACCORDING TO A NORMAL DISTRIBUTION WITH BEST POINT OF THE SUB-COMPLEX
    !  AS MEAN AND STANDARD DEVIATION OF THE POPULATION AS STD
    !
    call getpnt(2,iseed,snew,xnstd,sb)
    !
    !  COMPUTE THE FUNCTION VALUE AT THE RANDOM POINT
    !
    fnew = functn(snew)
    icall = icall + 1
    !
    !  REPLACE THE WORST POINT BY THE NEW POINT
    !
20  continue
    s(n,1:m) = snew(1:m)
    sf(n) = fnew
    return

end subroutine cce
!
!===================================================================
subroutine getpnt(idist,iseed,x,std,xi)
    use vars_calib, only: nopt,blow,bupp
    !
    !     This subroutine generates a new point within feasible region
    !
    !     x(.) = new point
    !     xi(.) = focal point
    !     blow(.) = lower bound
    !     bupp(.) = upper bound
    !     std(.) = standard deviation of probability distribution
    !     idist = probability flag
    !           = 1 - uniform distribution
    !           = 2 - Gaussian distribution
    !
    implicit real*8 (a-h,o-z)
    dimension x(nopt),std(nopt),xi(nopt)
    !
    ibound=1
    do while (ibound == 1)
        do j=1,nopt
            ibound=1
            do while (ibound == 1)
                if (idist == 1) then
                    rand = ran1(iseed)
                elseif (idist == 2) then
                    rand = gasdev(iseed)
                endif
                x(j) = xi(j) + std(j) * rand * (bupp(j) - blow(j))
                !
                !     Check explicit constraints
                !        
                call chkcst(1,x(j),blow(j),bupp(j),ibound)              
            enddo
        enddo
        !
        !     Check implicit constraints
        !      
        call chkcst(nopt,x,blow,bupp,ibound)
    enddo
!    if (ibound .ge. 1) go to 1
    !
    return
end subroutine getpnt
!
!  CHECKING FOR PARAMETER CONVERGENCE
!===================================================================
subroutine parstt(npt,gnrng,ipcnvg)
    use vars_calib, only: x,xnstd,bound,nopt,nptmax => npt
    implicit real*8 (a-h,o-z)
    dimension xmax(nopt),xmin(nopt)
    dimension xmean(nopt)
    parameter (delta = 1.0d-20,peps=1.0d-3)
    !
    !  COMPUTE MAXIMUM, MINIMUM AND STANDARD DEVIATION OF PARAMETER VALUES
    !
    gsum = 0.d0
    do k = 1, nopt
        xmean(k) = sum(x(1:npt,k)) / dble(npt)
        xnstd(k) = sum(x(1:npt,k)*x(1:npt,k))  / dble(npt) - xmean(k)*xmean(k)
        xmax(k) = maxval(x(1:npt,k))
        xmin(k) = minval(x(1:npt,k))
        if (xnstd(k) .le. delta) xnstd(k) = delta 
        xnstd(k) = dsqrt(xnstd(k)) / bound(k)
        gsum = gsum + dlog( delta + (xmax(k)-xmin(k))/bound(k) )
    enddo
    gnrng = dexp(gsum/dble(nopt))
    !
    !  CHECK IF NORMALIZED STANDARD DEVIATION OF PARAMETER IS <= eps
    !
    ipcnvg = 0
    if (gnrng .le. peps) then
        ipcnvg = 1
    endif
    return
end subroutine parstt
!
!  THIS SUBROUTINE REDUCE INPUT MATRIX a(n,ngs2*npg) TO MATRIX
!  b(n,ngs1*npg) AND VECTOR af(ngs2*npg) TO VECTOR bf(ngs1*npg)
!====================================================================
subroutine comp(npt1,ngs1,ngs2,af)  
    use vars_calib, only: npg,a => x,n => nopt,npt,b => cx, bf => cf

    implicit real*8 (a-h,o-z)
    dimension af(npt)
    do igs=1, ngs1
        do ipg=1, npg
          k1=(ipg-1)*ngs2 + igs
          k2=(ipg-1)*ngs1 + igs
          b(k2,1:n) = a(k1,1:n)
          bf(k2) = af(k1)
        end do
    end do
    !
    do j=1, npt1
        a(j,1:n) = b(j,1:n)
        af(j) = bf(j)
    end do
    return
end subroutine comp
!
!  SORTING SUBROUTINE ADAPTED FROM "NUMERICAL RECIPES"
!  BY W.H. PRESS ET AL., pp. 233-234
!===================================================================

subroutine SORT1(nmax,n,m,rb,ra)      
    !  LIST OF VARIABLES
    !     ra(.) = array to be sorted
    !     rb(.,.) = arrays ordered corresponding to rearrangement of ra(.)
    !     wk(.,.), iwk(.) = local varibles
    !
    implicit real*8 (a-h,o-z)
    dimension ra(nmax),rb(nmax,m),wk(nmax,m),iwk(nmax)
    !
    call indexx(nmax,n, ra, iwk)
    wk(1:n,1) = ra(1:n)
    ra(1:n) = wk(iwk(1:n),1)
    do j = 1, m
        wk(1:n,j) = rb(1:n,j)
    enddo
    do j = 1, m
        rb(1:n,j) = wk(iwk(1:n),j)
    enddo
    return
end subroutine SORT1
!
!  SORTING SUBROUTINE ADAPTED FROM "NUMERICAL RECIPES"
!  BY W.H. PRESS ET AL., pp. 231
!===========================================================
subroutine SORT2(n,ra)     
    !
    !  LIST OF VARIABLES
    !     ra(.) = integer array to be sorted
    !
    implicit real*8 (a-h,o-z)
    dimension ra(n)
    integer ra, rra
    !
    l = (n / 2) + 1
    ir = n
10  continue
    if (l .gt. 1) then
        l = l - 1
        rra = ra(l)
    else
        rra = ra(ir)
        ra(ir) = ra(1)
        ir = ir - 1
        if (ir .eq. 1) then
            ra(1) = rra
            return
        endif
    endif
    i = l
    j = l + l
20  continue
    if (j .le. ir) then
        if (j .lt. ir) then
            if (ra(j) .lt. ra(j + 1)) j = j + 1
        endif
        if (rra .lt. ra(j)) then
            ra(i) = ra(j)
            i = j
            j = j + j
        else
            j = ir + 1
        endif
        goto 20
    endif
    ra(i) = rra
    goto 10
    
end subroutine SORT2
!
!=======================================================
subroutine indexx(nmax,n, arrin, indx)     
    !
    !  THIS SUBROUTINE IS FROM "NUMERICAL RECIPES" BY PRESS ET AL.
    !
    implicit real*8 (a-h,o-z)
    dimension arrin(nmax), indx(nmax)
    !
    do j = 1, n
        indx(j) = j
    enddo
    l = (n / 2) + 1
    ir = n
10  continue
    if (l .gt. 1) then
        l = l - 1
        indxt = indx(l)
        q = arrin(indxt)
    else
        indxt = indx(ir)
        q = arrin(indxt)
        indx(ir) = indx(1)
        ir = ir - 1
        if (ir .eq. 1) then
            indx(1) = indxt
            return
        end if
    end if
    i = l
    j = l + l
20  continue
    if (j .le. ir) then
        if (j .lt. ir) then
            if (arrin(indx(j)) .lt. arrin(indx(j + 1))) j = j + 1
        endif
        if (q .lt. arrin(indx(j))) then
            indx(i) = indx(j)
            i = j
            j = j + j
        else
            j = ir + 1
        endif
        goto 20
    endif
    indx(i) = indxt
    goto 10

end subroutine indexx
    !==============================================================
    !  real*8 function ran1(idum)
    !
    !  THIS SUBROUTINE IS FROM "NUMERICAL RECIPES" BY PRESS ET AL.
    !
    !  implicit real*8 (a-h,o-z)
    !  dimension r(97)
    !  parameter (m1 = 259200, ia1 = 7141, ic1 = 54773, rm1 = 3.8580247e-6)
    !  parameter (m2 = 134456, ia2 = 8121, ic2 = 28411, rm2 = 7.4373773e-6)
    !  parameter (m3 = 243000, ia3 = 4561, ic3 = 51349)
    !  save
    !  data iff / 0 /
    !  if ((idum .lt. 0) .or. (iff .eq. 0)) then
    !      iff = 1
    !      ix1 = mod(ic1 - idum,m1)
    !      ix1 = mod((ia1 * ix1) + ic1,m1)
    !      ix2 = mod(ix1,m2)
    !      ix1 = mod((ia1 * ix1) + ic1,m1)
    !      ix3 = mod(ix1,m3)
    !      do j = 1, 97
    !        ix1 = mod((ia1 * ix1) + ic1,m1)
    !        ix2 = mod((ia2 * ix2) + ic2,m2)
    !        r(j) = (dble(ix1) + (dble(ix2) * rm2)) * rm1
    !      enddo
    !      idum = 1
    !  end if
    !  ix1 = mod((ia1 * ix1) + ic1,m1)
    !  ix2 = mod((ia2 * ix2) + ic2,m2)
    !  ix3 = mod((ia3 * ix3) + ic3,m3)
    !  j = 1 + ((97 * ix3) / m3)
    !  if ((j .gt. 97) .or. (j .lt. 1)) pause
    !  ran1 = r(j)
    !  r(j) = (dble(ix1) + (dble(ix2) * rm2)) * rm1
    !
    !  END OF SUBROUTINE RAN1
    !
    !      return
    !      end      

    
!
!  THIS SUBROUTINE IS FROM "NUMERICAL RECIPES" BY PRESS ET AL.    
!==============================================================
real*8 function ran1(idum)
    implicit real*8 (a-h,o-z)
    !INTEGER*8 IA,IM,IQ,IR,NTAB,NDIV
    !INTEGER IDUM
    !REAL*8 AM,EPS,RNMX
    PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836,NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
    !INTEGER*8 j,k,iv(NTAB),iy
    DIMENSION iv(NTAB)
    SAVE iv,iy
    DATA iv /NTAB*0/, iy /0/
    if (idum.le.0.or.iy.eq.0) then
        idum=max(-idum,1)
        do j=NTAB+8,1,-1
            k=idum/IQ
            idum=IA*(idum-k*IQ)-IR*k
            if (idum.lt.0) idum=idum+IM
            if (j.le.NTAB) iv(j)=idum
        enddo
        iy=iv(1)
    endif
    k=idum/IQ
    idum=IA*(idum-k*IQ)-IR*k
    if (idum.lt.0) idum=idum+IM
    j=1+iy/NDIV
    iy=iv(j)
    iv(j)=idum
    ran1=min(AM*iy,RNMX)
    return
END function ran1       
!
!  THIS SUBROUTINE IS FROM "NUMERICAL RECIPES" BY PRESS ET AL.
!===============================================================
real*8 function gasdev(idum)
    implicit real*8 (a-h,o-z)
    common /gasblk/ iset
    data iset / 0 /
    if (iset .eq. 0) then
1       v1 = (2. * ran1(idum)) - 1.
        v2 = (2. * ran1(idum)) - 1.
        r = (v1 ** 2) + (v2 ** 2)
        if (r .ge. 1.) goto 1
        fac = sqrt(- ((2. * log(r)) / r))
        gset = v1 * fac
        gasdev = v2 * fac
        iset = 1
        else
        gasdev = gset
        iset = 0
    end if
    return
end function gasdev
!
!     This subroutine check if the trial point satisfies all
!     constraints.
!==================================================================
!      
subroutine chkcst(nopt,x,blow,bupp,ibound)
!
!     ibound - violation indicator
!            = -1 initial value
!            = 0  no violation
!            = 1  violation
!     nopt = number of optimizing variables
!     i = the ii'th variable of the arrays x, bl, and bu
!
implicit real*8 (a-h,o-z)
dimension x(nopt),blow(nopt),bupp(nopt)      
!
ibound = -1
!
!     Check if explicit constraints are violated
!
do i=1, nopt
    if (x(i) .lt. blow(i) .or. x(i) .gt. bupp(i)) then
        ibound = 1
        return  
    endif
enddo
if (nopt .eq. 1) then
    ibound = 0
    return
endif
!
!     Check if implicit constraints are violated
!     (Note - Implicit constraints are commented out, 
!      you can activate these constraints by removing
!      "!" in the first columns)
!      c1 = 5.0*x(1) + 2.0*x(3) - 100.0
!      c2 = x(3) + 50.*x(4) - 50.0
!      c3 = 10.*x(5) - x(6) - 2.0
!      c4 = -10.*x(5) + x(6) - 2.0
!      if (c1 .gt. 0.) go to 10
!      if (c2 .gt. 0.) go to 10
!      if (c3 .gt. 0.) go to 10
!      if (c4 .gt. 0.) go to 10
!
!     No constraints are violated
!
return
end subroutine chkcst
!
!  ESTA funcao AJEITA OS DADOS E CHAMA O MODELO
!==================================================================
real*8 function functn(xpar)
USE VARS_MAIN
USE VARS_CALIB
implicit none
REAL*8 xpar(nopt)
REAL PAR(NPAR)
INTEGER  IU,I,IC,IC2,K,IAUX
!
!  ASSIGN x TO APPROPRIATE PARAMETERS IN MODEL
!
PAR=VPAR ! VETOR ORIGINAL DE PARAMETROS

do k = 1, nopt
    iaux=DEXP(xpar(k))*10000.
    par(pos(k))=float(iaux)/10000. ! LIMITA O VALOR DE PARA 4 DIGITOS SIGNIFICATIVOS
enddo

! KSAT CONDUTIVIDADE HIDRAULICA SATURADA (MM/H)
! B PARAMETRO DA CURVA DE RETENCAO
! PSIB PRESSAO DE ENTRADA DO AR (KPA)
! THS UMIDADE VOLUMETRICA NA SATURACAO
! THR UMIDADE VOLUMETRICA RESIDUAL     

!DO IU=1,NU
!    IF(SOLO(IU) /= 14) THEN
!        ! CALCULA OS PARAMETROS DE CADA SUB-BACIA
!        SSMAX(IBCAL,IU)=(THS(SOLO(IU))-THR(SOLO(IU)))*1000*PAR(1) ! ARMAZENAMENTO MAXIMO CAMADA SUPERIOR DO SOLO
!        SRMAX(IBCAL,IU)=(THS(SOLO(IU))-THR(SOLO(IU)))*1000*PAR(2) ! ARMAZENAMENTO MAXIMO EM MM NA SEGUNDA CAMADA
!        SMAX(IBCAL,IU)=(THS(SOLO(IU))-THR(SOLO(IU)))*1000*PAR(3) ! ARMAZENAMENTO MAXIMO EM MM NA TERCEIRA CAMADA
!        ALPHA(IBCAL,IU)=ALPHA_REF*PAR(7) ! COEFICIENTE DE ANISOTROPIA
!        KSS(IBCAL,IU)=(KSAT(SOLO(IU))*24./1000.)*PAR(4) ! CONDUTIVIDADE HIDRAULICA SATURADA CAMADA SUPERIOR M/DIA
!        TSUB(IBCAL,IU)=TSUB_REF*PAR(5) ! TRANSMISSIVIDADE MAXIMA
!        MU(IBCAL,IU)=MU_REF*PAR(6) ! 
!        CSI(IBCAL,IU)=MIN(CSI_REF(IU)*PAR(8),1.) ! CAPACIDADE DE CAMPO           
!    ENDIF
!ENDDO
D1(IBCAL)=PAR(1)
D2(IBCAL)=PAR(2)
D3(IBCAL)=PAR(3)
CS(IBCAL)=CS_REF*PAR(9)
CB(IBCAL)=CB_REF*PAR(10)*86400. ! CB EM DIAS FOI MULTIPLICADO POR 86400 PARA CB FICAR EM SEG
!
!
DO IC2=1,NCDOM
    IC=ICDOM(IC2)
    IF(IBAC(IC) == IBCAL) THEN ! CELULA QUE PERTENCE A BACIA SENDO OTIMIZADA
!        DO IU=1,NU
!	        LAMBDAM(IC,IU)=0.0  ! ATUALIZA O VALOR DE LAMBDAM
!		    IF(SMAX(IBCAL,IU).GT.0) THEN ! PULA AGUA
!		        DO K=2,50
!				    LAMBDAM(IC,IU)=LAMBDAM(IC,IU)+0.5*(LAMBDAI(IC,K)**(1./MU(IBCAL,IU))+LAMBDAI(IC,K-1)**(1./MU(IBCAL,IU)))*(AC(IC,K)-AC(IC,K-1))
!			    ENDDO
!			    LAMBDAM(IC,IU)=LAMBDAM(IC,IU)**MU(IBCAL,IU)
!		    ENDIF
!	    ENDDO
	    !ATUALIZA OS COEFICIENTES DO RLS 
   		TKB(IC)=EXP(-DTP/(CB(IBCAL)))	    !CB DEFINIDO EM SEGUNDOS EM LESOLO.F90
    	TKS(IC)=EXP(-DTP/(CS(IBCAL)*TCON(IC)))   !CS DEFINIDO EM LESOLO.F90	
	ENDIF
ENDDO

! CHAMA O MODELO
CALL MODELO
CALL FOBJ

IF(ISNAN(FOB) == .TRUE.) THEN ! CASO A COMBINACAO DE PARAMETROS GERE UM NAN
    FOB=1.
    FOB=HUGE(FOB) ! GERA UM NUMERO GRANDE
ENDIF

functn = FOB
return

end function functn
