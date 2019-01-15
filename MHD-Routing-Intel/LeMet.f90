SUBROUTINE LEMET
!SUBROTINA QUE L� DADOS METEOROLOGICOS DOS ARQUIVOS DE DADOS METEOROLOGICOS
USE VARS_MAIN
IMPLICIT NONE
INTEGER IC,ITAUX
REAL LIXO(NC)

!ABRE ARQUIVOS DE PRECIPITACAO E METEOROLOGICOS DE TODO O PERIODO DE DADOS
! O INICIO DA SIMULACAO PODE SER POSTERIOR AO INICIO DO PERIODO COM DADOS
! EH NECESSARIO QUE A DATA DE INCIO DAS OBSERVACOES NOS ARQUIVOS QOBS.PRN, PREC,BIN E MET.BIN SEJAM AS MESMAS	
OPEN(FILPLU,FILE=DIR_DADOS//'Entrada/prec_diaria_5km_2000_2013_corrigido.bin',STATUS='OLD',FORM='UNFORMATTED',RECORDTYPE='STREAM')  !dados de chuva interpolados	
OPEN(FILMET,FILE=DIR_DADOS//'Entrada/clima_2000_2013_5km_corrigido.bin', STATUS='OLD',FORM='UNFORMATTED',RECORDTYPE='STREAM') !dados DADOS METEOROLOGICOS PARA CALCULAR EVAPORACAO

! SE A SIMULACAO SE INICIA APOS A DATA DO INICIO DAS OBSERVACOES, O NSTEP, CALCULADO EM LEQOBS.PRN, UTILIZADO EH > 0 
IF(NSTEP > 0) THEN ! A SIMULACAO SE INICIA APOS O INICIO DOS DADOS
    ! DESPREZA OS NSTEP PRIMEIROS DADOS LENDO POR CIMA
    DO ITAUX=1,NSTEP
        READ(FILPLU)(LIXO(IC),IC=1,NC)
    ENDDO
    
    DO ITAUX=1,NSTEP
        READ(FILMET)(LIXO(IC),IC=1,NC) ! TEMPERATURA DO AR
        READ(FILMET)(LIXO(IC),IC=1,NC) ! TEMPERATURA PONTO DE ORVALHO
        READ(FILMET)(LIXO(IC),IC=1,NC) ! VELOCIDAE DO VENTO
        READ(FILMET)(LIXO(IC),IC=1,NC) ! PRESSAO ATMOSFERICA
        READ(FILMET)(LIXO(IC),IC=1,NC) ! RADIACAO GLOBAL INCIDENTE
    ENDDO
ENDIF
! LE O RESTANTE DO ARQUIVO
DO ITAUX=1,NT
	READ(FILPLU)(PREALL(IC,ITAUX),IC=1,NC)
ENDDO

CLOSE(FILPLU) !ARQUIVO DE DADOS DE CHUVA

DO ITAUX=1,NT
    READ(FILMET)(TAALL(IC,ITAUX),IC=1,NC) ! TEMPERATURA DO AR
    READ(FILMET)(TDEWALL(IC,ITAUX),IC=1,NC) ! TEMPERATURA PONTO DE ORVALHO
    READ(FILMET)(VVALL(IC,ITAUX),IC=1,NC) ! VELOCIDAE DO VENTO
    READ(FILMET)(PATMALL(IC,ITAUX),IC=1,NC) ! PRESSAO ATMOSFERICA
    READ(FILMET)(ROCALL(IC,ITAUX),IC=1,NC) ! RADIACAO GLOBAL INCIDENTE
ENDDO
CLOSE (FILMET) !ARQUIVO DE DADOS METEOROLOGICOS

!ic=128
!OPEN(1000,FILE=DIR_DADOS//'SAIDA/MET.TXT', STATUS='UNKNOWN',FORM='FORMATTED') 
!write(1000,'(6f10.2)') (preall(ic,itaux),taall(ic,itaux), &
!& tdewall(ic,itaux),vvall(ic,itaux),patmall(ic,itaux),rocall(ic,itaux),itaux=1,nt)
!stop

WRITE(*,*)
WRITE(*,*) ' LEU DADOS METEOROLOGICOS'

RETURN
END
	