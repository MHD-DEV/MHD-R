SUBROUTINE MUSK(QJX,NTR,NTC,NTCLAST,IC)
!ROTINA QUE CALCULA MUSKINGUM CUNGE
USE VARS_MAIN
IMPLICIT NONE
INTEGER ITC,ITR,IC,ITCLAST
REAL C1,C2,C3 !COEFICIENTES MUSKINGHUM
REAL DTC,DTCLAST1,DTCLAST2 !VARIAVEIS AUXILIARES
REAL QJX
INTEGER NTR,NTC,NTCLAST !NUMERO DE TRECHOS, DE INTERVALOS DE TEMPO DA CELULA, E DA ULTIMA CELULA
!VAZOES INTERNAS DO TRECHO	
REAL,ALLOCATABLE:: QC(:,:)
REAL QPERDAS ! PERDAS DEVIDO A IRRIGACAO, CONSUMO HUMANO, ETC
REAL DTCAL,DX,KMC,XMC,DEN ! DELTA T E DELTA X, PARAMETROS K E X DE MC  
ALLOCATE (QC(NTR+1,NTC+1))

! CALCULA OS COEFICIENTES DE MC
DTCAL=DT(IC)	
DX=SRIO(IC)*1000./NTRC(IC)
KMC=DX/CEL(IC)
XMC=0.5*(1.0-(QREF(IC)/(BRIO(IC)*DECL(IC)*CEL(IC)*DX)))
DEN=2.*KMC*(1.-XMC)+DTCAL
C1=(2.*KMC*XMC+DTCAL)/DEN
C2=(DTCAL-2.*KMC*XMC)/DEN
C3=(2.*KMC*(1.-XMC)-DTCAL)/DEN

QC=0.0 !ZERA MATRIZ

IF(IC == IEXUT(IBAC(IC))) THEN
    QPERDAS=QCONS(IBAC(IC)) ! PERDAS TOTAIS DEVIDO A IRRIGACAO, CONS HUMANO, ETC
ELSE
    QPERDAS=0.
ENDIF

!CONDICOES DE CONTORNO DE MONTANTE EXTRAIDA A EVAPORACAO E A PERDA POR IRRIGACAO
IF(NTC == NTCLAST) THEN		
    DO ITC=1,NTC+1
	    QC(1,ITC)=MAX(QCONTORM(IC,ITC)-EVQ(IC)-QPERDAS,0.0)
!	    IF(IC==172) WRITE(*,*) '1,',QC(1,ITC),QCONTORM(IC,ITC),EVQ(IC),QPERDAS
    ENDDO
ELSE
    ! COMENTARIO JAVIER: ACHO QUE TINHA UM BUG. SE DT(NC) FOR MAIOR QUE DT(IC) TERIA DE HAVER 
    ! UMA INTERPOLACAO POIS NTC SERIA MAIOR QUE NTCLAST E AS CONDICOES INICIAIS SERIAM AFETADAS
    DO ITC=1,NTC+1
        DTC=FLOAT(ITC-1)*DT(IC)
        DTCLAST1=0.
        DO ITCLAST=2,NTCLAST+1
            DTCLAST2=FLOAT(ITCLAST-1)*DT(NC)
            IF(DTC <= DTCLAST2) THEN
                QC(1,ITC)=QCONTORM(IC,ITCLAST-1)+(DTC-DTCLAST1)*(QCONTORM(IC,ITCLAST)-QCONTORM(IC,ITCLAST-1))/(DTCLAST2-DTCLAST1)
                
                QC(1,ITC)=MAX(QC(1,ITC)-EVQ(IC)-QPERDAS,0.0)
!                IF(IC==172) WRITE(*,*) '2,',QC(1,ITC),EVQ(IC),QPERDAS
                EXIT
            ENDIF
            DTCLAST1=DTCLAST2
        ENDDO
    ENDDO
ENDIF
!CONDICOES INICIAIS	EM CADA SUBTRECHO DA CELULA
DO ITR=1,NTR+1
	QC(ITR,1)=QRIOINI(IC,ITR)
!	IF(IC==172) WRITE(*,*) '3,',QC(ITR,1),QRIOINI(IC,ITR)
ENDDO

DO ITC=1,NTC
	DO ITR=1,NTR
	    ! PROPAGA HIDROGRAMA EM CADA SUBTRECHO DA CELULA
		QC(ITR+1,ITC+1)=C1*QC(ITR,ITC)+C2*QC(ITR,ITC+1)+C3*QC(ITR+1,ITC) 
		QC(ITR+1,ITC+1)=MAX(QC(ITR+1,ITC+1),0.0) !EVITA VAZ�ES NEGATIVAS
!		IF(IC==172) WRITE(*,*) '4,',QC(ITR+1,ITC+1)
	ENDDO		
	!GUARDA HIDROGRAMA DE SA�DA COMPLETO DO ULTIMO SUBTRECHO EM CADA INTERVALO DE TEMPO DE MC 
	IF(NTC == NTCLAST) THEN
		QCONTORJ(IC,ITC+1)=QC(NTR+1,ITC+1)
	ELSE
        ! COMENTARIO JAVIER: ACHO QUE TEM OUTRO UM BUG. SE DT(NC) FOR MAIOR QUE DT(IC) TERIA DE HAVER UMA INTERPOLACAO
        ! POIS NTC SERIA MAIOR QUE NTCLAST E AS CONDICOES DE CONTORNO SERIAM AFETADAS. EH NECESSARIO QUE O INTERVALO
        ! DE TEMPO DE CONTORNO DE MONTANTE E JUSANTE ESTEJAM EM UM MESMO INTERVALO DE TEMPO POIS ESSA INFORMACAO EH
        ! TRANSFERIDA DE CELULA EM CELULA
        DTC=FLOAT(ITC)*DT(IC)
        DTCLAST1=0.
        DO ITCLAST=2,NTCLAST+1
            DTCLAST2=FLOAT(ITCLAST-1)*DT(NC)
            IF(DTC <= DTCLAST2) THEN
                QCONTORJ(IC,ITCLAST)=QC(NTR+1,ITC+1)+(DTC-DTCLAST1)*(QC(NTR+1,ITC+1)-QC(NTR+1,ITC))/(DTCLAST2-DTCLAST1)
                EXIT
            ENDIF
            DTCLAST1=DTCLAST2
        ENDDO
    ENDIF
ENDDO

!GUARDA VALORES DA CONDI��O INICIAL PARA A PROXIMA VEZ QUE VAI CALCULAR MUSKINGUM CUNGE
QRIOINI(IC,1)=QCONTORM(IC,NTC+1)
DO ITR=2,NTR+1
	QRIOINI(IC,ITR)=QC(ITR,NTC+1)
ENDDO

!GUARDA VALOR DO FINAL DO PER�ODO DE PROPAGA��O NO EXTREMO DE JUSANTE DO TRECHO
QJX=QC(NTR+1,NTC+1) !ESTE VALOR VOLTA PARA PROGRAMA PRINCIPAL
!COMENT�RIOS GAROFOLo
!IF(IC==172)THEN
! WRITE(*,*) QC(NTR+1,NTC+1),QJX
!ENDIF
DEALLOCATE (QC)
RETURN
END
	