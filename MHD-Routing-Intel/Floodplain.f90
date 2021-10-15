SUBROUTINE FLOODPLAIN
!DEC$ OPTIMIZE:3 	                 
!esta subrotina PROPAGA NA PLANICIE
USE VARS_MAIN
IMPLICIT NONE
INTEGER IC,IU,IB,IFP,K
REAL QN(NFP),QW(NFP),QS(NFP),QE(NFP) ! VAZAO DE TROCA ENTRE CELULAS DE PLANICIE NAS DIRECOES CARDINAIS
REAL ZR(NFP) ! COTA DO RIO
REAL PPU,DUSO,AUX,AREA,HF
REAL QMX,QJX,C
INTEGER NDTFP,IDTFP

NDTFP=DTP/DTFP ! NUMERO DE SUBINTERVALOS DA ROTINA FLOODPLAIN EM SEG

QRFP=0.
DO IDTFP=1,NDTFP
    QN=0.
    QW=0.
    QS=0.
    QE=0.
    ! DETERMINA A ALTURA DA ÁGUA EM CADA CELULA DO RIO NA PLANICIE
    DO IFP=1,NFP
        IC=ICELLFP(IFP)
        IF(NTRC(IC) == 0)CYCLE !CELULA SEM RIO
        
        !INTERPOLA AS VAZOES DE MONTANTE E JUSANTE ENTRE T E T+1
        QMX=QM1(IC)+IDTFP*(QM2(IC)-QM1(IC))/NDTFP
        QJX=QJ1(IC)+IDTFP*(QJ2(IC)-QJ1(IC))/NDTFP        
        
        ! ESTIMA A COTA MEDIA DE AGUA DO RIO, ZR, USANDO MANNING, PARA SECAO RETANGULAR
!        ZR(IFP)=0.5*((QJX*RUGMAN/BRIO(IC))**0.6/DECL(IC)**0.3+(QMX*RUGMAN/BRIO(IC))**0.6/DECL(IC)**0.3)        
        ZR(IFP)=0.5*(QJX**0.6+QMX**0.6)*(RUGMAN/BRIO(IC))**0.6/DECL(IC)**0.3
        ZR(IFP)=ZR(IFP)+0.5*(HCEL(IC)+LCEL(IC))
        
        IF(ZR(IFP) <= ZVERT(IFP)) THEN ! O NIVEL DO RIO ESTA ABAIXO DA COTA DE VERTIMENTO
            IF(ZFP(IFP) > ZVERT(IFP)) THEN !PLANICIE VERTE NO RIO COMO VERTEDOR LIVRE
                QRFP(IFP)=CVL*BVL*(ZFP(IFP)-ZVERT(IFP))**1.5
            ENDIF
        ELSE ! RIO ACIMA DA COTA DE VERTIMENTO
            IF(ZFP(IFP) > ZVERT(IFP)) THEN ! VERTEDOR AFOGADO, O SINAL DE AUX DEFINE O SENTIDO
                ! AUX > 0 PLANICIE -> RIO; AUX < 0 RIO -> PLANICIE
                AUX=ZFP(IFP)-ZR(IFP)
                QRFP(IFP)=CVA*BVA*AUX*ABS(AUX)**0.5
            ELSE  ! VERTEDOR LIVRE, O RIO VERTE NA PLANICIE
                AUX =ZR(IFP)-ZVERT(IFP)
                QRFP(IFP)=-CVL*BVL*(AUX)**1.5
            ENDIF
            ! AMORTECE A VAZAO QUE SAI DO RIO PARA EVITAR ESVAZIAMENTOS BRUSCOS
            IF(QRFP(IFP) < 0.) THEN ! VAZAO DO RIO PARA A PLANICIE
                AUX = ABS(AUX)*BRIO(IC)*SRIO(IC) ! VOLUME DO RIO ACIMA DA COTA DE VERTIMENTO OU DA COTA NA PLANICIE
                C=AUX/(QRFP(IFP)*DTFP) ! COEFICIENTE DE REDUCAO
                IF(C < 1. ) THEN ! VERTEU MAIS DO QUE DEVIA
                    QRFP(IFP)=QRFP(IFP)*C
                ENDIF
            ENDIF
        ENDIF
        
     !   decl(ic)=1.e-5
        
     !   decl(ic)=(HCEL(IC)-LCEL(IC))/(1000*srio(ic))
        
       ! aux=7000*adren(ic)/233812.6
       ! write(*,*) aux
       ! aux=(aux*RUGMAN/BRIO(IC))**0.6/DECL(IC)**0.3
       ! aux=aux+0.5*(HCEL(IC)+LCEL(IC))
       ! write(*,*) aux,zvert(ifp),ICELL(ICELLFP(IFP)),ICELLFP(IFP),decl(ic)
        
        
        !AUX=1.4351*ADREN(IC)**0.1901        
        !WRITE(*,*) ZR(IFP),ZVERT(IFP),ZFP(IFP),QJ2(IC),QM2(IC),IC,IFP,QRFP(IFP),ZR(IFP)-0.5*(HCEL(IC)+LCEL(IC)),AUX
        !PAUSE
                
        
        
    ENDDO ! FIM DAS TROCAS RIO PLANICIE
    ! CALCULO DAS TROCAS ENTRE ELEMENTOS DA PLANICIE
    QN=0.
    QW=0.
    QS=0.
    QE=0.
    DO IFP=1,NFP
        ! CALCULA PRIMEIRAMENTE AO NORTE
        IF(CELLN(IFP) > 0) THEN ! EXISTE CELULA DE PLANICIE AO NORTE
            HF=MAX(ZFP(CELLN(IFP)),ZFP(IFP))-MAX(ZFPB(CELLN(IFP)),ZFPB(IFP))
            IF(HF > 0) THEN
                AUX=ZFP(CELLN(IFP))-ZFP(IFP)
                QN(IFP)=FCH*HF**1.67*SQRT(ABS(AUX)) ! VAZAO NA DIRECAO NORTE
                IF(AUX < 0) QN(IFP)=-QN(IFP) ! A CELULA PERDE AGUA
                QS(CELLN(IFP))=-QN(IFP) ! EQUIVALE A - A VAZAO NA DIRECAO DA CELULA AO NORTE
            ENDIF
        ENDIF
        ! CALCULA AO OESTE
        IF(CELLW(IFP) > 0) THEN ! EXISTE CELULA DE PLANICIE AO OESTE
            HF=MAX(ZFP(CELLW(IFP)),ZFP(IFP))-MAX(ZFPB(CELLW(IFP)),ZFPB(IFP))
            IF(HF > 0) THEN
                AUX=ZFP(CELLW(IFP))-ZFP(IFP)
                QW(IFP)=FCH*HF**1.67*SQRT(ABS(AUX)) ! VAZAO NA DIRECAO OESTE
                IF(AUX < 0) QW(IFP)=-QW(IFP) ! A CELULA PERDE AGUA
                QE(CELLW(IFP))=-QW(IFP) ! EQUIVALE A - (MENOS) A VAZAO NA DIRECAO DA CELULA AO OESTE
            ENDIF
        ENDIF   
    ENDDO
    ! VERIFICA SE AS CELULAS DE PLANICIE NAO ESVAZIARAM ALEM DO QUE DEVIAM
    DO IFP =1,NFP
        AUX=QN(IFP)+QW(IFP)+QS(IFP)+QE(IFP)-QRFP(IFP)
        IF(AUX < 0.) THEN ! A CELULA PERDE AGUA
            C = -(ZFP(IFP)-ZFPB(IFP))*AFP(IFP)/(AUX*DTFP) ! COEFICIENTE LINEAR DE ESVAZIAMENTO
            IF(C < 1.) THEN ! ESVAZIOU ALEM DO VOLUME ARMAZENADO NA CELULA
                QN(IFP)=QN(IFP)*C
                QW(IFP)=QW(IFP)*C
                QE(IFP)=QE(IFP)*C
                QS(IFP)=QS(IFP)*C
                QRFP(IFP)=QRFP(IFP)*C
            ENDIF
            ! COMPATIBILIZA AS VAZOES
            IF(CELLN(IFP) > 0) THEN ! EXISTE CELULA DE PLANICIE AO NORTE
                IF(QN(IFP) < 0. ) THEN ! SE FOR MENOR QUE ZERO ESCOLHE A MAIOR VAZAO
                    QN(IFP)=MAX(QN(IFP),-QS(CELLN(IFP)))
                ELSE ! SE FOR MAIOR QUE ZERO ESCOLHE A VAZAO MINIMA
                    QN(IFP)=MIN(QN(IFP),-QS(CELLN(IFP)))
                ENDIF
                QS(CELLN(IFP))=-QN(IFP)
            ENDIF
            IF(CELLW(IFP) > 0) THEN ! EXISTE CELULA DE PLANICIE AO OESTE
                IF(QW(IFP) < 0.) THEN
                    QW(IFP)=MAX(QW(IFP),-QE(CELLW(IFP)))
                ELSE
                    QW(IFP)=MIN(QW(IFP),-QE(CELLW(IFP)))
                ENDIF
                QE(CELLW(IFP))=-QW(IFP)
            ENDIF
        ENDIF
    ENDDO
    ! BALANCO HIDRICO DE CADA ELEMENTO DE CELULA
    DO IFP=1,NFP
        VFP(IFP)=VFP(IFP)+(QN(IFP)+QW(IFP)+QS(IFP)+QE(IFP)-QRFP(IFP))*DTFP ! VOL ARMAZENADO NO FLOODPLAIN
        ! ESTIMATIVA PRELIMINAR DE AREA ALAGADA
        IF(VFP(IFP) <= VFPT(IFP,1)) THEN
            AREA=0.
        ELSE
            AREA=100.
            DO K=2,50
                IF(VFP(IFP) < VFPT(IFP,K)) THEN
                    AREA=AFPT(IFP,K-1)+(VFP(IFP)-VFPT(IFP,K-1))*(AFPT(IFP,K)-AFPT(IFP,K-1))/(VFPT(IFP,K)-VFPT(IFP,K-1))
                    EXIT
                ENDIF
            ENDDO
        ENDIF
        IC=ICELLFP(IFP)
        IB=IBAC(IC)
        ! CORRECOES NA CELULA EM DECORRENCIA DA MUDANCA DE AREA ALAGADA
        DO IU=1,NU
            PPU=PUSO(IC,IU)/100.*(1.-AFP(IFP)/100.) ! PERCENTAGEM DE USO IU NO TEMPO T CORRIGIDO PELA AREA ALAGADA
            DUSO=PUSO(IC,IU)/100.*(AFP(IFP)-AREA)/100. ! EXPANSAO/CONTRACAO DO USO IU no tempo t+1 EM FUNCAO DA MUDANCA DE AREA ALAGADA
            IF(DUSO > 0) THEN ! EXPANSAO DA AREA ALAGADA
                ! CORRIGE O VOLUME ALAGADO EM FUNCAO DA DEMANDA DE AGUA NECESSARIA PARA SATURAR A PRIMEIRA CAMADA DAS NOVA AREAS ALAGADAS
                VFP(IFP)=VFP(IFP)-DUSO*ACEL(IC)*(SSMAX(IB,IU)-SSSU(IC,IU))/1000.
            ELSE ! CONTRACAO DE AREA SATURADA
                ! CORRIGE O ARMAZENAMENTO NA CAMADA SUPERIOR DO SOLO NAS AREAS QUE AGORA NAO ESTAO ALAGADAS 
                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                ! aqui da um nun quando ppu e duso sao iguais a zero
                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                SSSU(IC,IU)=(SSSU(IC,IU)*PPU+SSMAX(IB,IU)*DUSO)/(PPU+DUSO)
            ENDIF
        ENDDO
        ! VERIFICAR SE EH O MELHOR
        IF(VFP(IFP) < 0.) THEN
            ! ERRO DE BALANCO EH ALOCADO TOTALMENTE NO FLUXO PLANICIE RIO, POIS ELIMINA A NECESSIDADE DE CORRIGIR TODA A PLANICIE
            QRFP(IFP)=QRFP(IFP)+VFP(IFP)/86400.
            VFP(IFP)=0.
        ENDIF
        ! AREA ALAGADA EH REAJUSTADA E CALCULA-SE A ALTURA DE ALAGAMENTO 
        IF(VFP(IFP) <= VFPT(IFP,1)) THEN
            AFP(IFP)=0.
            ZFP(IFP)=ZFPB(IFP)
        ELSE
            AFP(IFP)=100.
            ZFP(IFP)=ZFPT(IFP,50)
            DO K=2,50
                IF(VFP(IFP) < VFPT(IFP,K-1)) THEN
                    AFP(IFP)=AFPT(IFP,K-1)+(VFP(IFP)-VFPT(IFP,K-1))*(AFPT(IFP,K)-AFPT(IFP,K-1))/(VFPT(IFP,K)-VFPT(IFP,K-1))
                    ZFP(IFP)=ZFPT(IFP,K-1)+(VFP(IFP)-VFPT(IFP,K-1))*(ZFPT(IFP,K)-ZFPT(IFP,K-1))/(VFPT(IFP,K)-VFPT(IFP,K-1))
                    EXIT
                ENDIF
            ENDDO
        ENDIF
    ENDDO ! FIM DO LOOP CELULAS DE PLANICIE
ENDDO ! FIM DO LOOP DE SUBINTERVALOS DE TEMPO
RETURN
END
