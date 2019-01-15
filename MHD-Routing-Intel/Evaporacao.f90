SUBROUTINE EVAPORACAO(IC,IU)
!ESTA SUBROTINA CALCULA A EVAPORACAO DO DOSSEL DE ACORDO COM VAN DIJK 2001 (JH) - MODELO DE GASH
USE VARS_MAIN
!USE VARS_MAIN, ONLY: JDIA,TA,TDEW,PATM,VV,PRE,EC,ZVEG,DVEG,Z0,IAF,COVER,SCMAX,XCEL,YCEL,GAMMA,DELTA,IMES
IMPLICIT NONE
!CALCULA A EVAPORA��O DA �GUA INTERCEPTADA
INTEGER IU,IC,LONMC
REAL RN !RADIA��O LIQUIDA (MJ/m2/dia)	
REAL G  !FLUXO DE CALOR DO SOLO
REAL SN !RADIACAO LIQUIDA DE ONDAS CURTAS
!	REAL LN !RADIA��O LIQUIDA DE ONDAS LONGAS
REAL CLATE !CALOR LATENTE DE VAPORIZA��O - em MJ/K, temperatura em C
REAL ES		!PRESS�O DE SATURA��O DO VAPOR DA �GUA EM KPA
REAL ED		!PRESS�O AUAL DE VAPOR REAL EM KPA
REAL RHOCP ! DENSIDADE DO AR A PRESSAO CONSTANTE EM KG/M3 VEZES O CALOR ESPECIFICO DO AR UMIDO MJ/kg/C
REAL Z0M   ! RUGOSIDADE PARA TRANSFERENCIA DE MOMENTUM, RESISTENCIA AERODINAMICA DE MAIDMENT E CHOU
REAL Z0V   ! RUGOSIDADE PARA TRANSFERENCIA DE VAPOR
REAL D0    ! PLANO DE DESLOCAMENTO ZERO
REAL PL    ! CHUVA NECESSARIA PARA SATURAR O DOSSEL
REAL EPR   ! RAZAO ENTRE EVAPORACAO �MIDA E INTENSIDADE MEDIA (0.039)
REAL PINT  ! INTERCEPTACAO DO DOSSEL
REAL ROL   ! RADIACAO LIQUIDA DE ONDA LONGA
REAL RSBS !PARAMETRO RESISTENCIA DO SOLO NU
REAL DECLIN,WS,LAT,RAE,ROC0,DR,INSOL,AUX,Z,VZ,W1,W2,W,SC,T,DTH
REAL RNAG,RAAG
REAL Ecc,Scc,R !mohor
!VARIAVEIS DE ENTRADA
! DR	: DISTANCIA REL. TERRA - SOL
! ALB	: ALBEDO
! ROC	: RADIACAO GLOBAL INCIDENTE
! VV	: VENTO A 2 METROS
! PATM	: PRESSAO ATMOSFERICA
! TA	: TEMPERATURA MEDIA DIARIA
! TONTEM: TEMPERATURA MEDIA DIA ANTERIOR 
! ZVEG	: ALTURA DA VEGETACAO
! SCMAX : PARAMETRO - CAPACIDADE MAXIMA DE ARMAZENAMENTO NO DOSSEL
! COVER : FRACAO DE COBERTURA DO DOSSEL

!VARIAVEIS DE SAIDA

! EC	: EVAPORACAO ATUAL DO ARMAZENAMENTO NO DOSSEL
! E0	: EVAPORACAO DE SUPERFICIE DE AGUA LIVRE
! RA	: RESISTENCIA AERODINCAMICA	S M-1

!VARIAVEIS INTERNAS

! EPR	: PARAMETRO - RAZAO ENTRE EVAPORACAO �MIDA E INTENSIDADE MEDIA (0.039)
! CLATE	: CALOR LATENTE DE VAPORIZACAO MJ M-2 DIA-1
! ES	: PRESSAO DE SATURACAO kPA
! ED	: PRESSAO DE VAPOR kPA
! GAMMA	: CONSTANTE PSICOMETRICA EM KPA/C (EQ. 4.2.28) 
! DELTA : DECLIVIDADE DA CURVA DE PRESSAO DE VAPOR (EQ. 4.2.3)
! MSPA	: DENSIDADE DO AR �MIDO
! Z0M   : RUGOSIDADE S M-1
! DVEG  : PLANO DE DESLOCAMENTO ZERO
! RA	: RESISTENCIA AERODINAMICA
! MESP	: MASSA ESPECIFICA DA AGUA
! PL	: CAPACIDADE DE ARMAZENAMENTO ACTUAL DA VEGETACAO MM
! PINT	: INTERCEPTACAO NO DOSSEL MM

ES=0.6108*EXP((17.27*TA(IC))/(237.3+TA(IC))) ! PRESSAO DE VAPOR DE SATURACAO
ED=0.6108*EXP((17.27*TDEW(IC))/(237.3+TDEW(IC))) ! PRESSAO DE VAPOR 
CLATE=(2.501-0.002361*TA(IC)) ! CALOR LATENTE MJ/C
GAMMA=0.0016286*PATM(IC)/CLATE ! CONSTANTE PSICROMETRICA
DELTA=(4098.0*ES)/((237.3+TA(IC))**2.) ! DECLIVIDADE DA CURVA DE PRESSAO DE VAPOR DE SATURACAO
RHOCP=0.00353128*PATM(IC)/(CLATE*(275.+TA(IC))) ! RHO	: MASSA ESPECIFICA DO AR
IF(ZVEG(IU,IMES) > 10.) THEN ! VEGETACAO MAIS ALTA QUE O ANEMOMETRO
	! LEVA A VELOCIDADE DO VENTO A ALTURA DO DOSSEL ASSUMINDO GRAMA COM RUGOSIDADE = 0.078 M E DESLOCAMENTO = 2.65 M 
    VZ=VV(IC)*LOG(12.82*ZVEG(IU,IMES)-3.4)/4.85+0.01
    Z=ZVEG(IU,IMES)
ELSE
    VZ=VV(IC)+0.01
    Z=10.
ENDIF
!AUX=LOG((Z-DVEG(IU,IMES))/Z0(IU,IMES))
!RA=AUX*AUX/(0.41*0.41*VZ)
AUX=LOG((Z-DVEG(IU,IMES))/Z0(IU,IMES))*LOG((Z-DVEG(IU,IMES))/(Z0(IU,IMES))) !mudei, ERA 0.1Z0
RA=AUX/(0.41*0.41*VZ) ! RESISTENCIA AERODINAMICA
AUX=LOG((Z-DVEG(NU+1,IMES))/Z0(NU+1,IMES))*LOG((Z-DVEG(NU+1,IMES))/(Z0(NU+1,IMES))) !mudei 05022014, ERA 0.1Z0
RAAG=AUX/(0.41*0.41*VZ) ! RESISTENCIA AERODINAMICA DA AGUA
! CALCULA RADIACAO DE ONDA LONGA
DR=1+0.033*COS(2*PI*JDIA/365) ! DR	: DISTANCIA TERRA SOL
DECLIN=0.4093*SIN(2*PI*JDIA/365-1.405) ! DECLINACAO
LAT=YCEL(IC)*PI/180. ! LATITUDE EM RADIANES
WS=ACOS(-TAN(LAT)*TAN(DECLIN)) 	! ANGULO SOLAR
IF(DTP >= 86400.) THEN	! DELTA T MAIOR OU IGUAL A 1 DIA
    INSOL=24*WS/PI ! HORAS DE INSOLACAO
    RAE=37.586*DR*(WS*SIN(LAT)*SIN(DECLIN)+COS(LAT)*COS(DECLIN)*SIN(WS)) ! RADIACAO EXTRATERRESTRE MJ/M2/DIA 
    ROC0=MAX(0.75*RAE,ROC(IC)) ! RADIACAO DE ONDA CURTA INCIDENTE EM DIA DE CEU CLARO
    ROL=STEFAN*(TA(IC)+273.16)**4*(0.34-0.14*SQRT(ED))*(1.35*ROC(IC)/ROC0-0.35) ! RADIACAO LIQUIDA DE ONDA LONGA
    SN=ROC(IC)*(1.-ALB(IU,IMES)) ! RADICAO LIQUIDA DE ONDA CURTA
    RN=SN-ROL ! RADIACAO LIQUIDA
    SN=ROC(IC)*(1.-ALB(NU+1,IMES)) ! RADICAO LIQUIDA DE ONDA CURTA DA AGUA
    RNAG=SN-ROL ! RADIACAO LIQUIDA DA AGUA
    G=0.38*(TA(IC)-TONTEM(IC)) ! FLUXO DE CALOR DO SOLO do Handbook of Hydrology
	E0=(DELTA*(RN+G)+86400.*RHOCP*(ES-ED)/RA)/(DELTA+GAMMA)*DTP/(86400.*CLATE) !mm NO DELTAT   	
    E0AG=(DELTA*RNAG+86400.*RHOCP*(ES-ED)/RAAG)/(DELTA+GAMMA)*DTP/(86400.*CLATE) !mm NO DELTAT    
    ! OS VALORES DE RADIACAO E TEMPERATURA SAO VALORES DO PERIODO DIURNO NOS DADOS DO INMET
    ! POR ISSO MULTIPLICO A EVAPORACAO POR INSOL/24.  
    !TESTE CECILIA E JAVIER  
    !E0=E0*INSOL/24.
    !E0AG=E0AG*INSOL/24.
ELSE ! DELTA T MENOR A UM DIA
    AUX = 2*PI*(JDIA-81)/364.
    SC = 0.1645*SIN(2*AUX)-0.1255*COS(AUX)-0.025*SIN(AUX) ! CORRECAO SAZONAL PARA HORA SOLAR (HORAS)
    AUX=XCEL(IC)/15.
    LONMC=INT(AUX)
    IF((AUX-LONMC) > 0.5 ) LONMC=LONMC-1
    LONMC=LONMC*15. ! LONGITUDE AT THE CENTRE OF THE LOCAL TIMEZONE (DEGREES WEST OF GREENWHICH)
    DTH=DTP/3600. ! INTERVALO DE TEMPO EM HORAS
    T=FLOAT(IHORA)-0.5*DTH ! STANDARD CLOCK TIME AT THE MIDPOINT OF THE PERIOD (HS)
    IF(T < 0.) T = T + 24.
    W=(PI/12.)*(T+0.06667*(LONMC-XCEL(IC))+SC-12.) ! SOLAR TIME ANGLE AT MIDPOINT OF HOURLY OR SHORTER PERIOD (RAD)
    W1=W-PI*DTH/48. ! DIVIDI POR 48, ERA 24
    IF(W1 < -WS) THEN
        W1=-WS
    ELSEIF(W1 > WS) THEN
        W1=WS
    ENDIF
    W2=W+PI*DTH/48. ! DIVIDI POR 48, ERA 24
    IF(W2 < -WS) THEN
        W2=-WS
    ELSEIF(W2 > WS) THEN
        W2=WS
    ENDIF
	RAE=18.793*DR*((W2-W1)*SIN(LAT)*SIN(DECLIN)+COS(LAT)*COS(DECLIN)*(SIN(W2)-SIN(W1))) ! RADIACAO EXTRATERRESTRE EM MJ/M2/HORA
	IF(RAE <= 0.) THEN
	    ROC0=0.5 ! RAZAO ASSUMIDA PARA HORARIO NOTURNO (FA0 56) for humid conditions, 0.8 or arid conditions
	ELSE
        ROC0=ROC(IC)/MAX(0.75*RAE,ROC(IC)) ! relative solar radiation
    ENDIF
    ROL=(STEFAN/24.)*(TA(IC)+273.16)**4*(0.34-0.14*SQRT(ED))*(1.35*ROC0-0.35) ! RADIACAO LIQUIDA DE ONDA LONGA  EM MJ/M2/HORA
    SN=ROC(IC)*(1.-ALB(IU,IMES)) ! RADIACAO LIQUIDA DE ONDA CURTA
    RN=SN-ROL ! RADIACAO LIQUIDA
	IF(RAE <= 0.) THEN
	    G = -2.1 * EXP(-0.5 * IAF(IU,IMES)) * RN ! Choudhury (1989) soil heat flux density (G) under nightime conditions
	ELSE
        G = -0.4 * EXP(-0.5 * IAF(IU,IMES)) * RN ! Choudhury (1989) soil heat flux density (G) under daylight conditions
    ENDIF
    SN=ROC(IC)*(1.-ALB(NU+1,IMES)) ! RADICAO LIQUIDA DE ONDA CURTA DA AGUA
    RNAG=SN-ROL ! RADIACAO LIQUIDA DA AGUA
	E0=(DELTA*(RN-G)+3600.*RHOCP*(ES-ED)/RA)/(DELTA+GAMMA)*DTP/(3600.*CLATE) !mm NO DELTAT
    E0AG=(DELTA*RNAG+3600.*RHOCP*(ES-ED)/RAAG)/(DELTA+GAMMA)*DTP/(3600.*CLATE) !mm NO DELTAT
    
    
    !IF(E0>1) THEN
    !WRITE(*,*) DELTA*(RN-G)/((DELTA+GAMMA)*CLATE), 3600.*RHOCP*(ES-ED)/RA/((DELTA+GAMMA)*CLATE),E0,T,ROC(ic),ROL,RAE
    !PAUSE
    !ENDIF
    
ENDIF

!
! CALCULA INTERCEPTACAO
!
!IF (SCMAX(IU) > 0.0) THEN !EXCLUI O CASO DE AGUA LIVRE
	
    ! Ec = 0.21mm/h (Germer 2006; Lloyd,Gash,Shuttleworth 1988).
    ! R = 6.66 em Germer foi tirado de menos de 1 ano de medi��es, com 97 eventos.
    ! R =5.15 Lloyd foi tirado de 2 anos, 83 - 85, di�rios
    ! Nota-se que estamos adotando EPR fixo para qualquer area.
    ! pt=0.036 (Lloyd, Amazonian)
    ! Cuartas: St of 0.06 mm and pt = 0.013, embora n�o consideramos mais a parcela do tronco.
    	
    !EPR = 0.21/6.66 !razao Evaporacao umida/intensidade media (Parametro)- Lloyd,88 + GERMER ET AL 2006 		
    !EPR = 0.21/5.15 Lloyd, 88
    !Entendo que o SCMAX, retirado de Ubarana, � S, e o EPR de Lloyd � E, aplicados em Gash 95
    !EPR = EPR /COVER(IU,IMES)                !Fa�o estas duas convers�es pois s�o E, e S, de Gash 77
!    SCMAX(IU) = SCMAX(IU)/COVER(IU,IMES)     !convertidos agora para Ec e Sc, de Gash 95
    
    !Mantenho os valores do ParVeg desenvolvidos para todas as vegeta��es
!    PL= -(SCMAX(IU)/COVER(IU,IMES))/(EPR/COVER(IU,IMES)) *LOG(1- (EPR/COVER(IU,IMES)))
	    			    
!    EPR = 0.32/7.13 !Adriana Cuartas 2007.
    
!    Ecc=0.32/COVER(IU,IMES)
!    R=7.13
!    Scc=SCMAX(IU)/COVER(IU,IMES)
    
!    PL = -Scc * (R/Ecc) * LOG(1- Ecc/R)
    
!    IF (PRE(IC).LE.PL) THEN
!	    PINT = COVER(IU,IMES)*PRE(IC)
!    ELSE
!	    PINT = COVER(IU,IMES)*(PL + (Ecc/R)*(PRE(IC)-PL) )
!    ENDIF

	! RETIRA EVAPORA��O DO RESERVAT�RIO DE INTERCEPTA��O
    ! EC=EVAPORA��O DA �GUA INTERCEPTADA
    
!	PINT=MIN(PINT,E0,PRE(IC)) ! havia outra linha antes: IF(PINT.GT.E0) PINT=E0. D� no mesmo.
!	EC(IC,IU) = PINT ! Evapora��o � IGUAL � intercepta��o= 100%. Considera que em 1 dia inteiro, o que ficou no dossel evaporou.
	
!ENDIF !IF n�o agua livre
!
! CALCULA INTERCEPTACAO
!

IF (SCMAX(IU) > 0.0) THEN !EXCLUI O CASO DE AGUA LIVRE
	EPR =0.031 !razao Evaporacao umida/intensidade media (Parametro)- GERMER ET AL 2006
	PL=-SCMAX(IU)*COVER(IU,IMES)/EPR*LOG(1-EPR) ! CAPACIDADE DE SATURACAO DO DOSSEL
!    ! CALCULA A INTERCEPTACAO DO DOSSEL [MM/DIA]
	IF (PRE(IC).LE.PL) THEN
		PINT = COVER(IU,IMES)*PRE(IC)
	ELSE
		PINT = COVER(IU,IMES)*(PL + EPR*(PRE(IC)-PL))
	ENDIF
	
	! RETIRA EVAPORA��O DO RESERVAT�RIO DE INTERCEPTA��O
    ! EC=EVAPORA��O DA �GUA INTERCEPTADA
	
	IF(PINT.GT.E0) PINT=E0
	PINT=MIN(PINT,PRE(IC))
	EC(IC,IU) = PINT
ENDIF

RETURN
END
