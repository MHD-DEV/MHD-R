SUBROUTINE DOMINIO
!
!  ESTA ROTINA E CHAMADA NA OPCAO CALIBRA E SELECIONA COMO DOMINIO
!  A BACIA A SER CALIBRADA, PARA DIMINUIR O TEMPO DE CALIBRACAO
!
!
USE VARS_MAIN, ONLY: CELJUS,IEXUT,IDREN,ICDOM,NC,NCDOM,ADREN,ICELL,ICELLFP,ZFPT,AFPT,ZVERT,ZFPB,VFPT,CELLW,CELLN,NFP,&
& NB,IQOBS,NOBS,IBAC,NT,NBVFO,ICALIB
USE VARS_CALIB, ONLY: IBCAL,QJCAL,NQCAL,IQCAL
IMPLICIT NONE
INTEGER IC,IC2,IFP,IFP2,NFP2,K,IB
REAL AUX(NC)

!VERIFICAR o que acontece quando o posto encontra-se a jusante do exutorio

NQCAL=0 ! INDICA O NUMERO DE POSTOS A MONTANTE
ALLOCATE(IQCAL(NB-1)) !VETOR QUE INDICA AS CELULAS DOS EXUTORIOS A MONTANTE DA BACIA A SER CALIBRADA
DO IB=1,NB-1 ! JAVIER: VERIFICAR O QUE ACONTECE QUANDO CALIBRA A ULTIMA SUBBACIA
	IF(IBAC(CELJUS(IEXUT(IB))) == IBCAL) THEN ! BACIA A JUSANTE DO EXUTORIO DA SUB-BACIA IB
        NQCAL=NQCAL+1
        IQCAL(NQCAL)=IEXUT(IB) 
    ENDIF
ENDDO

IF(NQCAL > 0) THEN ! EXISTEM CC A MONTANTE DA BACIA A SER CALIBRADA
    ALLOCATE(QJCAL(NQCAL,25,NT))
    NCDOM=NC
    DO IC=1,NC
        ICDOM(IC)=IC ! SIMULA TODO O DOMINIO NA ROTINA SIMULACAO 
    ENDDO
    !VETOR COM A SEQUENCIA DE CELULAS COM AREAS DRENADA CRESCENTE USADOS NA RESOLUCAO NA ROTINA REDE
    CALL SORT(NC,ADREN,IDREN)
    ICALIB = -1 ! FLAG PARA INDICAR QUE PRECISA GRAVAR CC DE MONTANTE 
    ! FAZ UMA PRIMEIRA RODADA EM TODO O DOMINIO DE SIMULACAO PARA CRIAR AS CONDICOES DE MONTANTE 
    ! E GUARDA-LAS EM QCONTCAL(IB,NTRC,IT)
    CALL MODELO
    ICALIB = 1
ENDIF

NCDOM=0 ! VETOR QUE IDENTIFICA AS CELULAS DA BACIA A SER CALIBRADA
DO IC=1,NC
    IF(IBAC(IC) == IBCAL) THEN
        NCDOM=NCDOM+1
        ICDOM(NCDOM) = IC ! INDICA AS CELULAS DENTRO DO DOMINIO
    ENDIF
ENDDO

! CRIA VETOR AUXILIAR DE AREA DRENADA
AUX=1.0E15 ! NUMERO MUITO GRANDE, PARA QUE AS CELULAS FORA DO DOMINIO FIQUEM NO FINAL
DO IC=1,NCDOM
    AUX(ICDOM(IC))=ADREN(ICDOM(IC)) ! ASSINA OS VALORES VERDADEIROS DE AREA DE DRENAGEM APENAS NO DOMINIO
ENDDO
!VETOR COM A SEQUENCIA DE CELULAS DO DOMINIO COM AREAS DRENADA CRESCENTE USADOS NA RESOLUCAO NA ROTINA REDE
CALL SORT(NC,AUX,IDREN)

! ELIMINA AS CELULAS DE PLANICIE FORA DO DOMINIO
IF(NFP > 0) THEN ! EXISTEM CELULAS DE PLANICIE
    DO IFP = 1,NFP ! LOOP ENTRE AS CELULAS DE PLANICIE
        ! ICELLFP(IFP) = X INDICA QUE A CELULA IFP DO ARQUIVO FLOODPLAIN.HIG CORRESPONDE
        ! `A X-ESIMA CELULA DO ARQUIVO CELL.HIG
        IC = ICELLFP(IFP) 
        DO IC2= 1,NCDOM ! LOOP ENTRE AS CELULAS DO DOMINIO
            IF(ICDOM(IC2) == IC) THEN ! A CELULA DE PLANICIE ESTA DENTRO DO DOMINIO 
                EXIT
            ENDIF
        ENDDO
        IF(IC2 > NCDOM) THEN ! A CELULA DE PLANICE ESTA FORA DO DOMINIO E EH ZERADA
            ! SE ICELL(IC) = 0 A CELULA IC DE CELL.HIG NAO PERTENCE A PLANICIE
            ICELL(ICELLFP(IFP))=0 ! ICELL INDICA A POSICAO DAS CELULAS DA PLANICIE DO ARQUIVO CELL.HIG NO ARQUIVO FLOODPLAIN.HIG
            ICELLFP(IFP)=0 
        ENDIF
    ENDDO
	
    ! ARRUMA O ARQUIVO DAS CELULAS DE PLANICIE EM SEQUENCIA COM AS CELULAS QUE SOBRARAM
    NFP2 = 0
    DO IFP=1,NFP
        IC = ICELLFP(IFP)
        IF(IC == 0) CYCLE
        NFP2=NFP2+1
        ICELLFP(NFP2) = ICELLFP(IFP) ! MODIFICA O ARQUIVO DE PLANICIE DESLOCANDO AS CELULAS
	    ZVERT(NFP2) = ZVERT(IFP)
        ZFPB(NFP2) = ZFPB(IFP)
        DO K=1,50
            ZFPT(NFP2,K) = ZFPT(IFP,K)
            AFPT(NFP2,K) = AFPT(IFP,K)
            VFPT(NFP2,K) = VFPT(IFP,K)
        ENDDO
        CELLN(NFP2)=CELLN(IFP)
        CELLW(NFP2)=CELLW(IFP)
        ICELL(ICELLFP(NFP2))=NFP2
    ENDDO
    NFP = NFP2 ! ARQUIVO DE PLANICIE AGORA EH MENOR
ENDIF

RETURN
END
