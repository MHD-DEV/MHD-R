MODULE VARS_CALIB
!DECLARA VARI�VEIS DO M�TODO DE CALIBRA��O
IMPLICIT NONE
SAVE
INTEGER, PARAMETER:: NPAR=10 ! NUMERO MAXIMO DE PARAMETROS A SEREM OTIMIZADOS
REAL*8,ALLOCATABLE:: a(:),blow(:),bupp(:),x(:,:),xnstd(:),bound(:),cx(:,:),cf(:)
REAL*8:: pcento
REAL,ALLOCATABLE:: CSI_REF(:) ! VALOR DE REFERENCIA DO CSI
INTEGER,ALLOCATABLE:: SOLO(:),VEG(:) ! INDICA TIPO DE VEGETACAO E DE SOLO NO ARQUIVO BLOCOS.HIG 
REAL,ALLOCATABLE:: QJCAL(:,:,:) !ARMAZENA AS VAZOES DE JUSANTE DAS BACIAS A MONTANTE DA BACIA A SER CALIBRADA
REAL MU_REF,CS_REF,ALPHA_REF,TSUB_REF,CB_REF
REAL THS(13),THR(13),PSI(13),KSAT(13),B(13),VPAR(NPAR)
INTEGER IBCAL,POS(NPAR),NQCAL
INTEGER,ALLOCATABLE:: IQCAL(:) ! VETOR COM AS CELULAS DOS EXUTORIOS DAS BACIAS A MONTANTE E AS BACIA A SER CALIBRADA
INTEGER nopt,maxn,kstop,iseed,ngs,npg,nps,nspl,mings,npt
INTEGER iniflg,iprint  
character*10 xname(NPAR)
data xname /'    D1','    D2','    D3','  DKSS',' DTSUB','   DMU','DALPHA', &
&'  DCSI','   DCS','   DCB'/ 
END MODULE VARS_CALIB