SUBROUTINE ALLOCA_VARS(IOP)
!SUBROTINA DE ALLOCA��O DE MEM�RIA DAS VARI�VEIS PRINCIPAIS
USE VARS_MAIN
IMPLICIT NONE
INTEGER IOP
SAVE

ALLOCV_CASE: SELECT CASE (IOP) !VERIFICA SE ALLOCA OU DEALLOCA
CASE (0) ! ALLOCA
	ALLOCATE (ICODMUSK(NC)) !CODIGO QUE INDICA LINEAR OU NAO LINEAR
	ALLOCATE (BPLAN(NC)) !LARGURA DA PLAN�CIE DE INUNDA��O (INCLUI O PROPRIO RIO)
	ALLOCATE (HCALHA1(NC),HCALHA2(NC)) !PROF EM QUE INICIA E EM QUE A PLANICIE ESTA TOT INUNDADA
	ALLOCATE (QMUP(NC,NTMUP),AMUP(NC,NTMUP),BMUP(NC,NTMUP),CMUP(NC,NTMUP)) !TABELA MUSKINGUN NAO LINEAR
	ALLOCATE (EVQ(NC)) !EVAPORA��O DIRETA DA SUPERF�CIE L�QUIDA DA C�LULA EM M3/S
	ALLOCATE (QRIOINI(NC,NTRMAX+1)) !CONDI��O INCIAL DA PROPAGA��O MUSKINGUM CUNGE
	ALLOCATE (QCONTORM(NC+1,25),QCONTORJ(NC+1,25)) !VAZ�O DA CONDI��O DE CONTORNO DE MUSKINGUM CUNGE
!	ALLOCATE (PREBANUAL(NB),KBANUAL(NB)) !PRECIPITACAO POR BACIA ANUAL E CONTADOR
	ALLOCATE (QREF(NC)) !VAZAO DE REFERENCIA
!	ALLOCATE (PUSO(NC,NU))!PROPORCAO DE USOS NA CELULA
! 	ALLOCATE (LAMBDA(NC,NU),LAMBDAM(NC,NU),LAMBDAN(NC,NU),AC(NC,50),LAMBDAI(NC,50),TANB(NC)) !INDICES TOPOGRAFICOS E HISTOGRAMA NA CELULA
    ALLOCATE (LAMBDAI(NC,50),TANB(NC),AC(NC,50))
	ALLOCATE (ACEL(NC),ADREN(NC),SRIO(NC),DECL(NC),ICELL(NC),ICDOM(NC))
	ALLOCATE (IDREN(NC))
	ALLOCATE (XCEL(NC),YCEL(NC))!COORDENADAS DO CENTRO DA CELULA
	ALLOCATE (IBAC(NC),HCEL(NC),LCEL(NC),CELJUS(NC))!BACIA,HMAX,HMIN,CELULA DE JUSANTE
! 	ALLOCATE (PREC(NC)) !CHUVA MEDIA NA CELULA,
	ALLOCATE (BRIO(NC))!LARGURA DO RIO
	ALLOCATE (IEXUT(NB)) !INDICA CELULA DO EXUTORIO DA BACIA
!	ALLOCATE (NETA(NU),D1(NB),D2(NB),D3(NB))
!	ALLOCATE (SMAX(NB,NU),KSS(NB,NU),TSUB(NB,NU),MU(NB,NU),ALPHA(NB,NU),CSI(NB,NU),SSMAX(NB,NU),SRMAX(NB,NU))
	ALLOCATE (TCON(NC),CB(NB),CS(NB)) !TEMPO DE CONCENTRACAO E PARAMETROS DA PROPAGA��O NA C�LULA
!	ALLOCATE (SSUB(NC,NU))!ARMAZENAMENTO NA CAMADA INFERIOR DO SOLO
!	ALLOCATE (SRAD(NC,NU))!ARMAZENAMENTO NA ZONA RADICULAR DO SOLO
!	ALLOCATE (SECRBS(NU))!PARAMETRO DE TRANSPIRACAO DE SOLO NU
!	ALLOCATE (SECRJAL(NU),SECRJAH(NU))!PARAMETROS DE ESTRESSE DE JARVIS
	ALLOCATE (NTRC(NC)) !NUMERO DE SUBTRECHOS DE RIO PARA MUSKINGUM CUNGE (DIFERENTE PARA CADA C�LULA)
    ALLOCATE (DT(NC)) !INTERVALO DE TEMPO MUSKINGUM CUNGE (DIFERENTE PARA CADA C�LULA)
	ALLOCATE (CEL(NC),TKS(NC),TKB(NC)) !CELERIDADE NO RIO E COEFICIENTES DO RLS DA CELULA
	! NOS PARAMETROS DE VEGETACAO, O USO NU+1 � RESERVADO A AREA ALAGAVEL
!	ALLOCATE (ALB(NU+1,12),IAF(NU+1,12),ZVEG(NU+1,12),RC(NU+1)) ! ALBEDO, INDICE DE AREA FOLIAR, ALTURA DA VEG E RESISTENCIA MINIMA
!	ALLOCATE (DVEG(NU+1,12),Z0(NU+1,12)) ! PLANO DE DESLOCAMENTO ZERO E RUGOSIDADE			
!	ALLOCATE (COVER(NU+1,12),SCMAX(NU+1)) !
	ALLOCATE (QESP(NB),QCONS(NB)) !VAZ�O ESPECIFICA DE BASE (M3/S/KM2) E CONSUMO DE AGUA NO EXUTORIO DA BACIA (M3/SEG)
	ALLOCATE (QBSUB(NT),QBSSU(NT),QBSUP(NT))!VAZ�O DE ACORDO COM ORIGEM
	ALLOCATE (DSUPB(NB,NT),DSSUB(NB,NT),DSUBB(NB,NT))!MEDIAS BACIA

	ALLOCATE (CDATAG(NT)) ! VETOR COM DATAS EM FORMATO CHARACTER
	ALLOCATE (KCB(NB)) !NUMERO DE CELULAS EM CADA SUB-BACIA
	ALLOCATE (QR(NOBS,NT)) !VAZ�O CALCULADA E OBSERVADA NOS EXUTORIOS DAS BACIAS
	ALLOCATE (QRG(NHIDG,NT)) !HIDROGRAMAS PARA GRAVA��O
	ALLOCATE (QRB(NB,NT)) !HIDROGRAMAS DAS SUB-BACIAS

	ALLOCATE (QM1(NC+1),QJ1(NC),QM2(NC+1),QJ2(NC)) !VAZOES A MONTANTE E A JUSANTE EM I
	ALLOCATE (QCEL1(NC),QCEL2(NC)) !VAZ�ES ORIGINADAS NA CELULA NOS INSTANTES T E T+1 NA C�LULA I
	ALLOCATE (PMSUB(NC+1),PMSSU(NC+1),PMSUP(NC+1),PJSUB(NC+1),PJSSU(NC+1),PJSUP(NC+1)) !PROPOR��ES DE ORIGEM DAS VAZ�ES NO RIO
	ALLOCATE (VRSUB(NC),VRSSU(NC),VRSUP(NC)) !VOLUMES DAS PROPOR��ES NO TRECHO
!	ALLOCATE (ETT(NC,NU)) 	!EVAPOTRANSPIRACAO TOTAL
!	ALLOCATE (ETP(NC,NU)) 	!TRANSPIRACAO POTENCIAL
!	ALLOCATE (EC(NC,NU)) ! EVAPORACAO NO DOSSEL
!	ALLOCATE (ASAT(NC,NU)) 	!ARMAZENAMENTO DOSSEL
!	ALLOCATE (SSSU(NC,NU)) 	!ARMAZENAMENTO NA CAMAD SUPERIOR DO SOLO
	ALLOCATE (QSUB(NC),QSSU(NC),QSUP(NC))		!VAZAO NA CELULA
	ALLOCATE (VSUB(NC),VSSU(NC),VSUP(NC))		!VOLUME NA CELULA
!	ALLOCATE (TONTEM(NC))		!TEMPERATURA NO DIA ANTERIOR
!	ALLOCATE (PRE(NC)) !CHUVA NO INTERVALO NA CELULA
!	ALLOCATE (TA(NC),TDEW(NC),VV(NC),ROC(NC),PATM(NC))	!TEMP., UMIDADE, VENTO, INSOL., PRESSAO
!	ALLOCATE (PREALL(NC,NT),TAALL(NC,NT),TDEWALL(NC,NT),VVALL(NC,NT),ROCALL(NC,NT),PATMALL(NC,NT))! PARA TODO O PERIODO DE SIMULACAO  
!	ALLOCATE (TAMM(12,NC),TDEWMM(12,NC),VVMM(12,NC),ROCMM(12,NC),PRMM(12,NC))
	!ALLOCATE (RAUX(NRECMAX))
!	ALLOCATE (PRAD(NU+1,12)) !PROFUNDIDADE RADICULAR
	ALLOCATE (NASH(NB),LNASH(NB),R2(NB),ERRV(NB),ERRE(NB),R2L(NB))
    !MHD-ROUTING GAROFOLO
    ALLOCATE (DSUBALL(NC,NT),DSUPALL(NC,NT))
	
CASE (1) ! DEALLOCA
    DEALLOCATE (ICODMUSK) !CODIGO QUE INDICA LINEAR OU NAO LINEAR
	DEALLOCATE (BPLAN) !LARGURA DA PLAN�CIE DE INUNDA��O (INCLUI O PROPRIO RIO)
	DEALLOCATE (HCALHA1,HCALHA2) !PROF EM QUE INICIA E EM QUE A PLANICIE ESTA TOT INUNDADA
	DEALLOCATE (QMUP,AMUP,BMUP,CMUP) !TABELA MUSKINGUN NAO LINEAR
	DEALLOCATE (EVQ) !EVAPORA��O DIRETA DA SUPERF�CIE L�QUIDA DA C�LULA EM M3/S
	DEALLOCATE (QRIOINI) !CONDI��O INCIAL DA PROPAGA��O MUSKINGUM CUNGE
	DEALLOCATE (QCONTORM,QCONTORJ) !VAZ�O DA CONDI��O DE CONTORNO DE MUSKINGUM CUNGE
!	DEALLOCATE (PREBANUAL,KBANUAL) !PRECIPITACAO POR BACIA ANUAL E CONTADOR
	DEALLOCATE (QREF) !VAZAO DE REFERENCIA
! 	DEALLOCATE (PUSO)!PROPORCAO DE USOS NA CELULA
! 	DEALLOCATE (LAMBDA,LAMBDAM,LAMBDAN,AC,LAMBDAI,TANB) !INDICES TOPOGRAFICOS E HISTOGRAMA NA CELULA
 	DEALLOCATE (AC,LAMBDAI,TANB)
 	DEALLOCATE (ACEL,ADREN,SRIO,DECL,ICELL,ICDOM)
 	DEALLOCATE (IDREN)
	DEALLOCATE (XCEL,YCEL)!COORDENADAS DO CENTRO DA CELULA
	DEALLOCATE (IBAC,HCEL,LCEL,CELJUS)!BACIA,HMAX,HMIN,CELULA DE JUSANTE
! 	DEALLOCATE (PREC) !CHUVA MEDIA NA CELULA
	DEALLOCATE (BRIO)!LARGURA DO RIO
	DEALLOCATE (IEXUT) !INDICA CELULA DO EXUTORIO DA BACIA
!	DEALLOCATE (NETA,D1,D2,D3)
!	DEALLOCATE (SMAX,KSS,TSUB,MU,ALPHA,CSI,SSMAX,SRMAX) !PARAMETROS DE SOLO DA CELULA
    DEALLOCATE (TCON,CB,CS) !TEMPO DE CONCENTRACAO E PARAMETROS DA PROPAGA��O NA C�LULA
!   DEALLOCATE (SSUB)!ARMAZENAMENTO NA CAMADA INFERIOR DO SOLO
!	DEALLOCATE (SRAD)!ARMAZENAMENTO NA ZONA RADICULAR DO SOLO
!	DEALLOCATE (SECRBS)!PARAMETRO DE TRANSPIRACAO DE SOLO NU
!	DEALLOCATE (SECRJAL,SECRJAH)!PARAMETROS DE ESTRESSE DE JARVIS
	DEALLOCATE (NTRC,DT)
	DEALLOCATE (CEL,TKS,TKB)
!	DEALLOCATE (ALB,IAF,ZVEG,RC)
!	DEALLOCATE (DVEG,Z0) ! PLANO DE DESLOCAMENTO ZERO E RUGOSIDADE	
	DEALLOCATE (QESP,QCONS) !VAZ�O ESPECIFICA DE BASE (M3/S/KM2) E CONSUMO DE AGUA NOE XUTORIO DA BACIA (M3/SEG)
	DEALLOCATE (QBSUB,QBSSU,QBSUP)!VAZ�O DE ACORDO COM ORIGEM
	DEALLOCATE (DSUPB,DSSUB,DSUBB)!MEDIAS BACIA
!	DEALLOCATE (ECB,ETPB,ETTB,SSSUB,PREB,SSUBB,ASATB,SRADB)!MEDIAS BACIA
	DEALLOCATE (CDATAG)
	DEALLOCATE (KCB) !NUMERO DE CELULAS EM CADA SUB-BACIA
	DEALLOCATE (QR) !VAZ�O CALCULADA E OBSERVADA NOS EXUTORIOS DAS BACIAS
	DEALLOCATE (QRG) 	!VAZ�O NOS EXUTORIOS DAS BACIAS
	DEALLOCATE (QRB) !HIDROGRAMAS DAS SUB-BACIAS
	DEALLOCATE (QM1,QJ1,QM2,QJ2) !VAZOES A MONTANTE E A JUSANTE EM i
	DEALLOCATE (PMSUB,PMSSU,PMSUP,PJSUB,PJSSU,PJSUP) !PROPOR��ES DE ORIGEM DAS VAZ�ES NO RIO
	DEALLOCATE (VRSUB,VRSSU,VRSUP) !VOLUMES DAS PROPOR��ES NO TRECHO
!	DEALLOCATE (ETT) 	!EVAPOTRASPIRACAO TOTAL
!	DEALLOCATE (ETP) 	!TRANSPIRACAO POTENCIAL
!	DEALLOCATE (EC)		!EVAPORACAO NO DOSSEL
!	DEALLOCATE (ASAT) 	!ARMAZENAMENTO DOSSEL
!	DEALLOCATE (SSSU) 	!ARMAZENAMENTO NA CAMADA SUPERIOR DO SOLO
	DEALLOCATE (QSUB,QSSU,QSUP)		!VAZAO NA CELULA
	DEALLOCATE (VSUB,VSSU,VSUP)		!VOLUME NA CELULA
!	DEALLOCATE (TONTEM)		!TEMPERATURA NO DIA ANTERIOR
!	DEALLOCATE (PRE) !CHUVA NO INTERVALO NA CELULA
	DEALLOCATE (QCEL1,QCEL2) !VAZ�ES ORIGINADAS NA CELULA NOS INSTANTES t E t+1 NA C�LULA i
!	DEALLOCATE (QLIDO) !VAZAO DE SUBSTITUI�AO DA CALCULADA
!	DEALLOCATE (TA,TDEW,VV,ROC,PATM)	!TEMP., PONTO DE ORVALHO, VENTO, INSOL., PRESSAO
!	DEALLOCATE (PREALL,TAALL,TDEWALL,VVALL,ROCALL,PATMALL) 
!	DEALLOCATE (TAMM,TDEWMM,VVMM,ROCMM,PRMM)
!	DEALLOCATE (PRAD) !PROFUNDIDADE RADICULAR
	DEALLOCATE (NASH,LNASH,R2,ERRV,ERRE,R2L)
	
	DEALLOCATE (DSUPALL,DSUBALL)
CASE DEFAULT
	STOP ' ERRO: IOP DESCONHECIDO NO ROTINA ALLOCA_CALIB!!!'
END SELECT ALLOCV_CASE

END