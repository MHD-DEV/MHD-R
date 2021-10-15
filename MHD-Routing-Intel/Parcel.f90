SUBROUTINE PARCEL

!CALCULA ALGUNS PAR�METROS DA C�LULA E DO RIO

USE VARS_MAIN
IMPLICIT NONE
INTEGER IC
REAL XLADO !COMPRIMENTO DO LADO DA C�LULA

!CALCULA TEMPO DE CONCENTRACAO NA CELULA USANDO KIRPICH
DO IC=1,NC
	XLADO=ACEL(IC)**0.5 !ESTIMATIVA DO COMPRIMENTO
	TCON(IC)=3600.*((0.868*XLADO**3.0)/(HCEL(IC)-LCEL(IC)))**0.385	! KIRPICH EM SEGUNDOS
ENDDO

!CALCULA OS COEFICIENTES DO RLS DA CELULA
DO IC=1,NC
	TKB(IC)=EXP(-DTP/(CB(IBAC(IC))))       !CB DEFINIDO EM SEGUNDOS EM LESOLO.F90
!	WRITE(*,*) CB(IBAC(IC)),TKB(IC),-DTP
	TKS(IC)=EXP(-DTP/(CS(IBAC(IC))*TCON(IC)))   !CS DEFINIDO EM LESOLO.F90	
!	WRITE(*,*) CS(IBAC(IC)),TKS(IC),-DTP,TCON(IC)
ENDDO

!CALCULA LARGURA DO RIO E VAZAO DE REFERENCIA
DO IC=1,NC
	IF(ADREN(IC).GT.1.0)THEN	 
	    IF(BC1 == 0.) THEN
	        BRIO(IC)=BC2*ADREN(IC)**BC3	 !LARGURA DO RIO EM M
	    ELSE
		    BRIO(IC)=BC1*ADREN(IC)*ADREN(IC)+BC2*ADREN(IC)+BC3	 !LARGURA DO RIO
		ENDIF
		QREF(IC)=QMESP*ADREN(IC) !VAZAO DE REFERENCIA EM M3/S
	ELSE !AREAS FONTE, SEM RIO, NAO INTERESSAM
		BRIO(IC)=0.0
    	QREF(IC)=0.0
	ENDIF
ENDDO
!FIM DO CALCULO DA LARGURA E VAZAO DE REFERENCIA
RETURN
END
