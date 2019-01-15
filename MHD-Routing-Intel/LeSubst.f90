SUBROUTINE LESUBST
!SUBROTINA QUE L� DADOS DE VAZ�O PARA SUBSTITUIR VAZ�O CALCULADA
USE VARS_MAIN
IMPLICIT NONE
INTEGER K,JULDAY,JUL,IDIA1,IMES1,IANO1,IHORA1,IAUX

OPEN(FILSUBS,FILE=DIR_DADOS//ARQSUBST,STATUS='OLD')
READ(FILSUBS,71) TITULO
! LE PRIMEIRO REGISTRO E EXTRAI A DATA, QUE DEVE ESTAR EM FORMATO DD/MM/AAAAHH
READ(FILSUBS,71) TITULO
IAUX=INDEX(TITULO,'/') ! IDENTIFICA A PRIMEIRA BARRA DA DATA
READ(TITULO(IAUX-2:IAUX-1),'(I2)') IDIA1 
READ(TITULO(IAUX+1:IAUX+2),'(I2)') IMES1
READ(TITULO(IAUX+4:IAUX+7),'(I4)') IANO1
IF(TITULO(IAUX+8:IAUX+9) == '  ') THEN ! DATA NAO INCLUI HORA
    IHORA1=0
    IAUX=IAUX+7
ELSE ! DATA INCLUI HORA
    READ(TITULO(IAUX+8:IAUX+9),'(I2)') IHORA1
    IAUX=IAUX+9
ENDIF
BACKSPACE(FILSUBS)

! SE A SIMULACAO SE INICIA APOS A DATA DO INICIO DAS OBSERVACOES, O NSTEP, CALCULADO EM LEQOBS.PRN, UTILIZADO EH > 0 

IF(NSTEP > 0) THEN ! A SIMULACAO SE INICIA APOS O INICIO DOS DADOS
    ! SE NSTEP FOR MAIOR QUE ZERO DESPREZA OS PRIMEIROS NSTEP VALORES
    DO IT=1,NSTEP
        READ(FILSUBS,*)
    ENDDO
ENDIF

! LE O RESTANTE DO ARQUIVO
DO IT=1,NT
	READ(FILSUBS,'(A<IAUX>,1x,<NUMSUBST>G10.0)') TITULO(1:IAUX),(QOBS(K,IT),K=1,NUMSUBST)
ENDDO
WRITE(*,*)
WRITE(*,'(A20,/,A<IAUX>,<NUMSUBST>F10.3)') ' ULTIMA VAZAO LIDA  ',TITULO(1:IAUX),(QOBS(K,NT),K=1,NUMSUBST)
WRITE(*,*)
CLOSE (FILSUBS)

71	FORMAT(A80)
!FIM DA LEITURA DE DADOS DE VAZAO DE SUBSTITUI�AO DE VALORES CALCULADOS

RETURN
END
