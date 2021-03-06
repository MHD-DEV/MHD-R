SUBROUTINE REDE	   
!ESTA ROTINA FAZ A PROPAGA��O NA REDE DE DRENAGEM
USE VARS_MAIN
!USE VARS_CALIB, ONLY: QJCAL,NQCAL,IQCAL 
IMPLICIT NONE
REAL QJX,QAUX,VT 	!VARIAVEIS AUXILIARES
INTEGER JUS
INTEGER NTR,NTC !NUMERO DE SUBTRECHOS, NUMERO DE SUBINTERVALOS
INTEGER NTCLAST,LT
INTEGER K,IS,IB,IC,IC2,IFP !CONTADORES

NTCLAST=DTP/DT(NC) ! NUMERO DE INTERVALO DE TEMPO PARA RESOLVER MC DA ULTIMA CELULA 
! E USADO PARA PROPAGAR DE UMA CELULA A OUTRA

! COMENTARIO JAVIER ACHO NAO PRECISA EXISTIR QM1 E QJ1, BASTA QM2(IC) E QJ2(IC)
! TALVEZ EM UM MODELO HIDRODINAMICO SEJA NECESSARIO
QCONTORM=0.0
!CC DE JUSANTE NO PRIMEIRO TRECHO EH A VAZAO DE JUSANTE DO INTERVALO DE TEMPO ANTERIOR
!QCONTORJ=0.0
QCONTORJ(1:NC,1)=QJ2(1:NC)
!EM CADA INTERVALO DE TEMPO O QUE ERA T+1 VIRA T
QM1=QM2
QJ1=QJ2
QM2=0.0
QJ2=0.0
QCEL1=QCEL2

DO IC2=1,NCDOM ! LOOP SOBRE AS CELULAS DO DOMINIO
    IC=IDREN(IC2)
	IB=IBAC(IC)
	IFP=ICELL(IC)

	IF(IFP > 0 ) THEN ! CELULA DE PLANICE, SOMA/SUBSTRAI A VAZAO DE TROCA RIO - PLANICIE
	    QSUP(IC)=QSUP(IC)+QRFP(IFP) ! A TROCA RIO-PLANICIE EH TRATADA COMO FLUXO SUPERFICIAL NA CELULA
		IF(QSUP(IC) < 0. ) THEN ! FOI RETIRADA AGUA A MAIS NO RIO
	        WRITE(*,*) 'QSUP < 0 EM REDE'
	        PAUSE
	    ENDIF
	ENDIF
	QCEL2(IC)=MAX(QSUB(IC)+QSSU(IC)+QSUP(IC),0.000001)  !SOMA VAZOES GERADAS NA CELULA NO TEMPO T+1

	IF(NTRC(IC) > 0)THEN !CELULA COM RIO

		!AS VAZOES QUE SAO GERADAS NAS CELULAS COM RIO ENTRAM A MONTANTE DA PROPRIA CELULA E SAO PROPAGADAS
		QAUX = QM2(IC)+QCEL2(IC) ! VAZAO TOTAL DE MONTANTE
		
		!ATUALIZA PROPOR��ES DAS VAZOES DE MONTANTE
		PMSUB(IC)=(PMSUB(IC)*QM2(IC)+QSUB(IC))/QAUX !VAZAO SUBTERRANEA
		PMSSU(IC)=(PMSSU(IC)*QM2(IC)+QSSU(IC))/QAUX !VAZAO SUB-SUPERFICIAL
		PMSUP(IC)=(PMSUP(IC)*QM2(IC)+QSUP(IC))/QAUX !VAZ�O SUPERFICIAL
		
		QM2(IC) = QAUX ! ATUALIZA VAZAO TOTAL DE MONTANTE
					
		!ATUALIZA PROPOR��ES NO VOLUME DO RIO
		VRSUB(IC)=VRSUB(IC)+PMSUB(IC)*QM2(IC)*DTP
		VRSSU(IC)=VRSSU(IC)+PMSSU(IC)*QM2(IC)*DTP
		VRSUP(IC)=VRSUP(IC)+PMSUP(IC)*QM2(IC)*DTP	
		
		!CONDI��O DE CONTORNO MUSKINGUM-CUNGE DE MONTANTE: AS VAZOES GERADAS NA CELULA NO TEMPO T+1 (QCEL2) 
		!SAO INTERPOLADAS LINEARMENTE ENTRE T E T+1 USANDO VAZAO NO TEMPO T (QCEL1)
		
		DO LT=1,NTCLAST+1
			QCONTORM(IC,LT)=QCONTORM(IC,LT)+QCEL1(IC)+(LT-1)*(QCEL2(IC)-QCEL1(IC))/NTCLAST
		ENDDO

		NTR=NTRC(IC) !NUMERO DE SUBTRECHOS MC NA CELULA
		NTC=DTP/DT(IC)	!NUMERO DE SUBINTERVALOS DE TEMPO MC NA CELULA
		IF(ICODMUSK(IC)==0)THEN !MUSKINGUN CUNGE LINEAR
			CALL MUSK(QJX,NTR,NTC,NTCLAST,IC)
		ELSEIF(ICODMUSK(IC)==1)THEN !MUSKINGUN CUNGE NAO LINEAR
			CALL MUSK_NL(QJX,NTR,NTC,NTCLAST,IC) 
		ENDIF
		QJ2(IC)=QJX

		!ATUALIZA PROPOR��ES NO VOLUME DO RIO USANDO VAZAO PROPAGADA NA CELULA
		VT=VRSUB(IC)+VRSSU(IC)+VRSUP(IC)
		PJSUB(IC)=VRSUB(IC)/VT
		PJSSU(IC)=VRSSU(IC)/VT
		PJSUP(IC)=VRSUP(IC)/VT
		VRSUB(IC)=MAX(0.0,VRSUB(IC)-PJSUB(IC)*QJ2(IC)*DTP)	
		VRSSU(IC)=MAX(0.0,VRSSU(IC)-PJSSU(IC)*QJ2(IC)*DTP)	
		VRSUP(IC)=MAX(0.0,VRSUP(IC)-PJSUP(IC)*QJ2(IC)*DTP)	

		!AS VAZOES PROPAGADAS NA CELULA ENTRAM COMO CONDICAO DE CONTORNO
		!DE MONTANTE DA CELULA QUE ESTA A JUSANTE
		JUS=CELJUS(IC)
		QAUX=MAX(QM2(JUS)+QJ2(IC),0.000001) ! VAZAO TOTAL DE MONTANTE DA CEL A JUSANTE
		DO LT=1,NTCLAST+1
		    !O CONTORNO DE MONTANTE DA CELULA JUS RECEBE A SAIDA DO HIDROGRAMA RECEM CALCULADO
			QCONTORM(JUS,LT)=QCONTORM(JUS,LT)+QCONTORJ(IC,LT)
		ENDDO

		! ATUALIZA PROPOR��ES DA CELULA A JUSANTE
		PMSUB(JUS)=(PMSUB(JUS)*QM2(JUS)+PJSUB(IC)*QJ2(IC))/QAUX  
		PMSSU(JUS)=(PMSSU(JUS)*QM2(JUS)+PJSSU(IC)*QJ2(IC))/QAUX
		PMSUP(JUS)=(PMSUP(JUS)*QM2(JUS)+PJSUP(IC)*QJ2(IC))/QAUX
		QM2(JUS)=QAUX ! ATUALIZA A VAZAO DE MONTANTE DA CELULA A JUSANTE
		
	ELSE  !CELULA SEM RIO
		
		QJ2(IC)=QCEL2(IC) ! VAZAO TOTAL A JUSANTE DA CELULA EM T+1 EH A VAZAO DA CELULA
		! PROPORCOES A JUSANTE
		PJSUB(IC)=QSUB(IC)/QJ2(IC)
		PJSSU(IC)=QSSU(IC)/QJ2(IC)
		PJSUP(IC)=QSUP(IC)/QJ2(IC)
		
		! AS VAZOES QUE SAO GERADAS NAS CELULAS FONTE ENTRAM A MONTANTE DO TRECHO DE RIO QUE EST� A JUSANTE
		JUS=CELJUS(IC)
		QAUX=MAX(QM2(JUS)+QJ2(IC),0.000001)
!        IF(IC==172) WRITE(*,*) 'REDE:',QAUX,QM2(JUS),QJ2(IC)
		!CONDI��O DE CONTORNO MUSKINGUM-CUNGE DISTRIBUIDA EM CADA INTERVALO DE TEMPO DA CEL DE JUSANTE
        !COMENTARIO JAVIER NAO VERIFIQUEI SE PRECISA CONSIDERAR O CASO DE VAZAO A SER SUBSTITUIDA
		DO LT=1,NTCLAST+1
			QCONTORM(JUS,LT)=QCONTORM(JUS,LT)+(QCEL1(IC)+(LT-1)*(QCEL2(IC)-QCEL1(IC))/NTCLAST)
		ENDDO

		!ATUALIZA AS PROPOR��ES
		PMSUB(JUS)=(PMSUB(JUS)*QM2(JUS)+QSUB(IC))/QAUX !VAZ�O SUBTERRANEA
		PMSSU(JUS)=(PMSSU(JUS)*QM2(JUS)+QSSU(IC))/QAUX !VAZ�O SUB-SUPERFICIAL
		PMSUP(JUS)=(PMSUP(JUS)*QM2(JUS)+QSUP(IC))/QAUX !VAZ�O SUPERFICIAL
		!ATUALIZA A VAZAO DE MONTANTE DA CELULA DE JUSANTE
		QM2(JUS)=QAUX
	ENDIF
ENDDO

RETURN
END
