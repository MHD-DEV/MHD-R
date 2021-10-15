SUBROUTINE CELULA
!esta subrotina comanda o loop das celulas
USE VARS_MAIN
IMPLICIT NONE
INTEGER IC,IC2,IU,IB,IFP,K
REAL VBX,VIX,VSX !,PPU,FNA
INTEGER JULDAY
! REAL VOLMAX

!VALORES MEDIOS NA CELULA DE ARMAZENAMENTOS NAS CAMADAS, DA EVAPORACAO DO DOSSEL E TOTAL. 
! EXCLUIDO - MHD-ROUTING GAROFOLO
!REAL SSSUC,SSUBC,SRADC,ECC,ETTC,ETPC,ASATC 

!VERIFICA A QUE MES CORRESPONDE O DIA JULIANO
CALL CALDAT(JDIA,IMES,IDIA,IANO)
JDIA=JDIA-JULDAY(1,1,IANO)+1

DO IC2=1,NCDOM	 	!LOOP NAS CELULAS DO DOMINIO
    IC=ICDOM(IC2)
! EXCLUIDO - MHD-ROUTING GAROFOLO
!	ECC=0.0 !ZERA VARIAVEIS TEMPORARIAS
!	ETTC=0.0
!	ETPC=0.
!	ASATC=0.
!	SSSUC=0.0
!	SSUBC=0.0
!	SRADC=0.0
	IB=IBAC(IC)
!    IFP=ICELL(IC)
!	EVQ(IC)=0.0 !EVAPORA��O DIRETA DA SUPERF�CIE L�QUIDA
!	FNA=1. !FRACAO DA CELULA SEM ALAGAMENTO
!	! AREA ALAGADA E TRATADA COM UM USO SEPARADO, COM PARAMETROS IGUAIS A AGUA
!	IF(IFP > 0) THEN ! CELULA DE PLANICIE
!        IF(AFP(IFP) > 0.001) THEN
!            ! AREA ALAGADA, A PRECIPITACAO MENOS EVAPORACAO ALTERA O VOLUME ARMAZENADO, E NAO GERA ESCOAMENTO
!            VFP(IFP)=VFP(IFP)+AFP(IFP)*(PRE(IC)-E0AG)/1000.
!            ! ATUALIZA A ALTURA E A AREA DE ALAGAMENTO DEVIDO AS TROCAS VERTICAIS
!            IF(VFP(IFP) <= VFPT(IFP,1)) THEN
!                ZFP(IFP)=ZFPB(IFP)
!                AFP(IFP)=0.
!            ELSE
!                ZFP(IFP)=ZFPT(IFP,50)
!                AFP(IFP)=1.
!                DO K=2,50
!                    IF(VFP(IFP) < VFPT(IFP,K-1)) THEN
!                        AFP(IFP)=AFPT(IFP,K-1)+(VFP(IFP)-VFPT(IFP,K-1))*(AFPT(IFP,K)-AFPT(IFP,K-1))/(VFPT(IFP,K)-VFPT(IFP,K-1))
!                        ZFP(IFP)=ZFPT(IFP,K-1)+(VFP(IFP)-VFPT(IFP,K-1))*(ZFPT(IFP,K)-ZFPT(IFP,K-1))/(ZFPT(IFP,K)-ZFPT(IFP,K-1))
!                        EXIT
!                    ENDIF
!                ENDDO
!            ENDIF
!            FNA=(1.-AFP(IFP)/100) ! FRACAO DE AREA NAO ALAGADA
!		    ! CONSIDERA QUE A �REA ALAG�VEL NAO GERA ESCOAMENTO SUP, SUBS OU DE BASE
!		    ! LOGO, DSUP=DSS=DSUB=DD=0.0
!		    ! TALVEZ SERIA NECESSARIO ASSSUMIR DSUB > 0
!        ENDIF
!    ENDIF

!	DO IU=1,NU !LOOP DOS USOS
	
	! EXCLUIDO - MHD-ROUTING GAROFOLO			    
	    ! AJUSTA PROPORCAO DE USO NO TEMPO T CORRIGIDO PELA AREA ALAGADA
!        PPU=PUSO(IC,IU)/100.*FNA
!		IF(PUSO(IC,IU) < 0.001) CYCLE !NAO TEM ESTE USO NESTA CELULA, PASSA PARA O PROXIMO USO
!		CALL EVAPORACAO(IC,IU)  !CALCULA LA RADIACION LIQUIDA E INTERCEPTACAO	
!	
!		IF(SMAX(IB,IU).GT.0.001) THEN !FAZ O BALAN�O H�DRICO DO SOLO, SE FOR �REA COBERTA POR �GUA (BLOCO AGUA, SMAX=0.0)
!			CALL ESCOAMENTOS(IC,IU,IB)
!			CALL TRANSPIRACAO(IC,IU,IB)
!    		IF(ICALIB == 0) THEN ! ESTA SIMULANDO
!			    SSSUC=SSSUC+SSSU(IC,IU)*PPU !ARMAZENAMENTO MEDIO NA CAMADA SUPERIOR NA CELULA
!			    SRADC=SRADC+SRAD(IC,IU)*PPU !ARMAZENAMENTO NA CAMADA INTERMEDIARIA NA CELULA
!			    SSUBC=SSUBC+SSUB(IC,IU)*PPU !ARMAZENAMENTO NA CAMADA INFERIOR NA CELULA
!			    ASATC=ASATC+ASAT(IC,IU)*PPU !AREA SATURADA MEDIA NA CELULA
!			    ECC=ECC+(EC(IC,IU))*PPU !INTERCEPTA��O M�DIA NA CELULA
!			    ETTC=ETTC+ETT(IC,IU)*PPU !EVAPOTRANSPIRA��O TOTAL M�DIA NA CELULA
!			    ETPC=ETPC+ETP(IC,IU)*PPU !TRANSPIRACAO POTENCIAL MEDIA NA CELULA
!            ELSEIF(IB == NBVFO) THEN
!			    ETTC=ETTC+ETT(IC,IU)*PPU !EVAPOTRANSPIRA��O TOTAL M�DIA NA CELULA            

!            ENDIF			
!		ELSE ! CORPO DE AGUA PERMANENTE DE SUPERFICIE ~ CTE: RESERVATORIO OU RIO DE GRANDE PORTE
!			DSUP=PRE(IC) ! GERA ESCOAMENTO SUPERFICIAL
!			DSS=0.0
!			DSUB=0
!			EVQ(IC)=(E0AG*1000.*ACEL(IC)*PPU)/DTP !EVAPORA��O DIRETA DAS SUPERF�CIES L�QUIDAS EM M3/S
!			ETTC=ETTC+E0AG*PPU
!		ENDIF

		!CORRIGE AS UNIDADES; MULTIPLICA PELA �REA DA CELULA (KM2); MULTIPLICA PELA PROPOR��O DE USO
		!OS VALORES DE DSUP, DSSU E DSUB ESTAO EM MM - CONVERTE PARA M3	
		!ATUALIZA VOLUMES (EM M3)
!		VSUB(IC)=VSUB(IC)+DSUB*ACEL(IC)*PPU*1000.
!		VSSU(IC)=VSSU(IC)+DSS*ACEL(IC)*PPU*1000.
!		VSUP(IC)=VSUP(IC)+DSUP*ACEL(IC)*PPU*1000.

!       GAROFOLO - ENTRANDO DIRETO COM ESCOAMENTO
    DSUP=DSUPALL(IC,IT)
    DSS=0.
    DSUB=DSUBALL(IC,IT)

	VSUB(IC)=VSUB(IC)+DSUB*ACEL(IC)*1000000
	VSSU(IC)=VSSU(IC)+DSS*ACEL(IC)*1000000
	VSUP(IC)=VSUP(IC)+DSUP*ACEL(IC)*1000000
		
!	    ENDDO !FIM DO LOOP DOS USOS

	!CALCULA VAZOES DAS CELULAS USANDO O RLS
	VBX=VSUB(IC)*TKB(IC)
	QSUB(IC)=(VSUB(IC)-VBX)/DTP
	VSUB(IC)=VBX
	VIX=VSSU(IC)*TKS(IC)
	QSSU(IC)=(VSSU(IC)-VIX)/DTP
	VSSU(IC)=VIX
	VSX=VSUP(IC)*TKS(IC)
	QSUP(IC)=(VSUP(IC)-VSX)/DTP
	VSUP(IC)=VSX
	
    !GUARDA VALORES MEDIOS PARA CADA BACIA
    !IB � O NUMERO DA BACIA A QUAL PERTENCE A CELULA IC
!	IF(ICALIB == 0) THEN ! ESTA SIMULANDO
!	    PREB(IB,IT)=PREB(IB,IT)+PRE(IC)/KCB(IB) !PRECIPITACAO
!	    ECB(IB,IT)=ECB(IB,IT)+ECC/KCB(IB) !LAMINA INTERCEPTADA
!	    ETTB(IB,IT)=ETTB(IB,IT)+ETTC/KCB(IB) !EVAPOTRANSPIRACAO TOTAL
!	    ETPB(IB,IT)=ETPB(IB,IT)+ETPC/KCB(IB) !TRANSPIRACAO POTENCIAL
!	    SSSUB(IB,IT)=SSSUB(IB,IT)+SSSUC/KCB(IB) !LAMINA ARMAZENADA MEDIA camada superior
!	    SSUBB(IB,IT)=SSUBB(IB,IT)+SSUBC/KCB(IB) !LAMINA ARMAZENADA MEDIA camada inferior
!	    SRADB(IB,IT)=SRADB(IB,IT)+SRADC/KCB(IB) !LAMINA ARMAZENADA MEDIA camada RADICULAR
!	    ASATB(IB,IT)=ASATB(IB,IT)+100.*ASATC/KCB(IB) !AREA SATURADA MEDIA EM %
!       GAROFOLO 
	    DSUPB(IB,IT)=DSUPB(IB,IT)+QSUP(IC)*(DTP/1000.)/(ACEL(IC)*KCB(IB)) !ESCOMENTO SUPERFICIAL DA BACIA EM MM NO DELTAT
	    DSSUB(IB,IT)=DSSUB(IB,IT)+QSSU(IC)*(DTP/1000.)/(ACEL(IC)*KCB(IB)) !ESCOMENTO INTERMEDI�RIO DA BACIA EM MM
	    DSUBB(IB,IT)=DSUBB(IB,IT)+QSUB(IC)*(DTP/1000.)/(ACEL(IC)*KCB(IB)) !ESCOMENTO SUBTERR�NEO DA BACIA	EM MM  
!	ENDIF
ENDDO !FIM DO LOOP DAS C�LULAS

RETURN
END
	
