    !PROGRAM MHD-FL
	!******************************************************************
	!*********MODELO HIDROL�GICO DISTRIBU�DO - ROUTING*****************
	!******************************************************************
	!*		    Baseado no modelo MGB do                              *
	!*			Instituto de Pesquisas Hidr�ulicas                    *
	!*          desenvolvido pelo INPE                                *
	!******************************************************************
	!USE PORTLIB !biblioteca para calcular o tempo de processamento
	USE VARS_MAIN !m�dulo que cont�m as vari�veis principais!
!	USE VARS_CALIB !m�dulo que cont�m as vari�veis da calibra��o
	IMPLICIT NONE
	INTEGER JULDAY, K, IMC
	INTEGER:: TEMPOI(3)=0, TEMPOF(3)=0
	REAL:: TEMPOT
	
    ! DIRETORIO DE TRABALHO (ESPECIFICADO EM VARS_MAIN)
    WRITE(*,*) 'DIRETORIO DE TRABALHO'
    WRITE(*,*) DIR_DADOS
    
	!____________LEITURA DE ARQUIVOS PRINCIPAIS________________
	CALL LEFIX 	!SUBROTINA DE LEITURA DE PARAMETROS FIXOS
    
	CALL ALLOCA_VARS(0) !ALLOCA VARI�VEIS PRINCIPAIS
    
	IDINI=JULDAY(IMES,IDIA,IANO,CALENDAR) !CONVERTE O DIA DE INICIO EM CALENDARIO JULIANO
	
    CALL LECELL !SUBROTINA DE LEITURA DO ARQUIVO DAS CELULAS
    CALL LEESC
    CALL LEBACIA
	!___________FIM DA LEITURA DOS ARQUIVOS PRINCIPAIS ______________

	!_______________PREPARACAO DE DADOS_____________________
	CALL PARCEL !CALCULA ALGUNS PAR�METROS DAS C�LULAS E DO RIO
	CALL PARCUNGE !CALCULA PARAMETROS PARA PROPAGACAO MUSKINGUM-CUNGE
	!____________FIM DA PREPARACAO DE DADOS_____________________
    !____________INICIO DA SIMULA��O_______________
	WRITE(*,*) "ICALIB:",ICALIB
 	CALIBRA_CASE: SELECT CASE(ICALIB)
	CASE(0)
    	WRITE(*,*)
		WRITE(*,*)' FAZENDO SIMULACAO'
		WRITE(*,*)
		CALL SIMULA
	CASE(1)
		WRITE(*,*)
		WRITE(*,*)' FAZENDO CALIBRACAO'
		WRITE(*,*)
		CALL LEQOBS	!LEITURA DE DADOS DE VAZAO OBSERVADA
		CALL CALIBRA
	CASE DEFAULT
		STOP "ERRO: ICALIB DESCONHECIDO NO PROGRAMA PRINCIAPL!"
	END SELECT CALIBRA_CASE
	
	CALL ALLOCA_VARS(1) !DEALLOCA VARI�VEIS PRINCIPAIS
	pause
	WRITE(*,*)'PROGRAMA TERMINOU'
!	PAUSE
!	STOP
	END
