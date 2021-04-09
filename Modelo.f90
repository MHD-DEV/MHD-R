SUBROUTINE MODELO
!Esta subrotina comanda o loop do tempo do modelo hidrol�gico e chama as 
!rotinas de balanco e propaga��o nas c�lulas e de propaga��o na rede de drenagem
USE VARS_MAIN
IMPLICIT NONE 
INTEGER,ALLOCATABLE:: NAUX(:) ! NUMERO DE SUBTRECHOS NO ARQUIVO DE CONDICOES INICIAIS 
REAL,ALLOCATABLE:: QAUX(:,:) ! VAZOES QRIOINI NO ARQUIVO DE CONDICOES INICIAIS 
INTEGER IMAPAS,IC,IC2,IB,IU,ITR1,ITR2 !CONTADORES
INTEGER JC,JU,IS,K 
REAL TEMPO ! TEMPO DE SIMULACAO EM DIAS
REAL SOMAD,SOMAAR,PMEDDIA
LOGICAL EXISTE
REAL VBX,AUX,DX ! 

! CONDICAO INICIAL USO DA TERRA
! ITMAP INDICA O NUMERO DO INTERVALO DE TEMPO ONDE MUDA O MAPA DE USO DA TERRA
! SE A DATA INICIAL DA PREVISAO FOR POSTERIOR A ULTIMA DATA DO USO DA TERRA, ITMAP < 0

!ASAT=0.0

! CONDICAO INICIAL DE ARMAZENAMENTO DO SISTEMA: CORRESPONDE A DATA INICIAL -1
! CRIA DATA PARA A CONDICAO INICIAL
TEMPO=IHORAINI/24.-DTP/86400
IF(TEMPO < 0) THEN
    TEMPO=TEMPO+1.
	JDIA=IDINI+INT(TEMPO)-1 ! DIA JULIANO DO CALEND�RIO
ELSE
	JDIA=IDINI+INT(TEMPO) ! DIA JULIANO DO CALEND�RIO
ENDIF

IHORA=NINT((TEMPO-INT(TEMPO))*24.) ! HORA DA SIMULACAO
IF(CALENDAR .EQ. 360) then
    CALL CALDAT360(JDIA,IMES,IDIA,IANO)
ELSE
    CALL CALDAT366(JDIA,IMES,IDIA,IANO)
ENDIF
WRITE(CDATA(1:2),'(I2.2)') IDIA
WRITE(CDATA(3:4),'(I2.2)') IMES
WRITE(CDATA(5:8),'(I4)') IANO    
WRITE(CDATA(9:10),'(I2.2)') IHORA

DO IC2=1,NCDOM
    IC=ICDOM(IC2)
    IB=IBAC(IC)
    VSUB(IC)=QESP(IB)*ACEL(IC)*CB(IB) ! CB EM SEGUNDOS
    VBX=VSUB(IC)*EXP(-DTP/TKB(IC))
    QSUB(IC)=(VSUB(IC)-VBX)/DTP
    VSSU(IC)=0.0
    VSUP(IC)=0.0
	QSUP(IC)=0.0
	QSSU(IC)=0.0
ENDDO

! DA PLANICIE DE ALAGAMENTO
IF(NFP > 0) THEN ! PLANICIE DE ALAGAMENTO SECA
    AFP=0.0
    VFP=0.0
    ZFP=ZFPB
    QRFP=0.0
ENDIF
!DA REDE DE DRENAGEM
CALL REDEINI
!****************************FIM DAS CONDI��ES INICIAIS*********************************

!	INICIO DO LOOP DO TEMPO.
!GAROFOLO
!OPEN(15000, FILE=trim(dir_dados)//'Saida/escoamento_Qs.txt', status='unknown',form='formatted')
!WRITE(15000,'(A40)') '      QSUP      QSUB      VSUP      VSUB'
IT=0

DO WHILE (IT < NT)
    IT=IT+1
    ! ARRUMA AS DATAS
    TEMPO=IHORAINI/24.+FLOAT(IT-1)*DTP/86400
    IHORA=NINT((TEMPO-INT(TEMPO))*24.) ! HORA DA SIMULACAO
    JDIA=IDINI+INT(TEMPO) ! DIA JULIANO DO CALEND�RIO
    ! ARRUMA DATAS EM FORMAT CARACTER PARA A SIMULACAO
IF(CALENDAR .EQ. 360) then
    CALL CALDAT360(JDIA,IMES,IDIA,IANO)
ELSE
    CALL CALDAT366(JDIA,IMES,IDIA,IANO)
ENDIF

    WRITE(CDATA(1:2),'(I2.2)') IDIA
    WRITE(CDATA(3:4),'(I2.2)') IMES
    WRITE(CDATA(5:8),'(I4)') IANO
    WRITE(CDATA(9:10),'(I2.2)') IHORA
    CDATAG(IT)=CDATA

	!SUBROTINA DA CELULA
	CALL CELULA
	
!    WRITE(15000,'(4F20.10)')QSUP(1000),QSUB(1000),VSUP(1000),VSUB(1000)
    
	!SUBROTINA DA REDE DE DRENAGEM
	CALL REDE

	!ARMAZENA DADOS DE VAZ�O DAS C�LULAS EM QUE EXISTE VAZ�O OBSERVADA 
	
    !IQOBS(IB) CELULAS QUE CORRESPONDEM AOS POSTOS FLU COM DADOS
    DO IB=1,NB
	    !ARMAZENA VAZOES DAS SUB-BACIAS
	    QRB(IB,IT)=QJ2(IEXUT(IB)) ! IEXUT(IB) CELULA DO EXUTORIO DA SUB-BACIA IB
    ENDDO
    DO IB=1,NOBS
	    QR(IB,IT)=QJ2(IQOBS(IB)) ! POSTOS COM VAZOES OBSERVADAS
    ENDDO
    DO IB=1,NHIDG !GUARDA DADOS PARA GRAVAR HIDROGRAMAS EM LOCAIS DEFINIDOS NO ARQUIVO PARHIG
		QRG(IB,IT)=QJ2(IHIDG(IB)) !QRG ARMAZENA OS HIDROGRAMAS NOS LOCAIS DESEJADOS
    ENDDO

!************** NOSOLO.HIG ***************************************************************
!As linhas abaixo servem para gravar algumas vari�veis detalhadas de um bloco e de
!uma c�lula. Os valores de JB e JC podem ser alterados, JU indica o bloco e JC a
!c�lula em que se desejam os dados. Os dados s�o gravados num arquivo chamado NOSOLO.HIG.
		
!	JC=10
!	WRITE(FILSOL,'(I6,3F10.3)')IT,QSUB(JC),QSSU(JC),QSUP(JC)
!Fim da saida de dados para o arquivo NOSOLO.HIG
!******************************************************************************************
!ARMAZENA VAZ�ES SEGUNDO A ORIGEM PARA UMA C�LULA
! IHIDG(NBVFO) C�LULA EM QUE SE DESEJAM OS RESULTADOS DE HIDROGRAMA SEPARADO POR ORIGEM
	QBSUB(IT)=QJ2(IHIDG(NBVFO))*PJSUB(IHIDG(NBVFO))
	QBSSU(IT)=QBSSU(IT)+QJ2(IHIDG(NBVFO))*PJSSU(IHIDG(NBVFO))
	QBSUP(IT)=QBSUP(IT)+QJ2(IHIDG(NBVFO))*PJSUP(IHIDG(NBVFO))

ENDDO !FIM DO LOOP DO TEMPO
close(15000)

RETURN
END
