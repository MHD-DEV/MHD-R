!GAROFOLO
SUBROUTINE LEESC
USE VARS_MAIN
IMPLICIT NONE
INTEGER ITAUX,IC
REAL LIXO(NC)

OPEN(FILSSUB, FILE='/Users/hidrologia/Desktop/Garofolo/UFF/Entradas-MHDRouting/IBGE+CARRIELO_PRESENTE/escoamento-subterraneo_presente_1970-2005.bin', status='old',form='unformatted',recordtype='STREAM')

OPEN(FILSSUP, FILE='/Users/hidrologia/Desktop/Garofolo/UFF/Entradas-MHDRouting/IBGE+CARRIELO_PRESENTE/escoamento-superficial_presente_1970-2005.bin', status='old',form='unformatted',recordtype='STREAM')

IF(NSTEP > 0) THEN
    DO ITAUX=1,NSTEP
        READ(FILSSUP) (LIXO(IC),IC=1,NC)
        READ(FILSSUB) (LIXO(IC),IC=1,NC)
    ENDDO
ENDIF

DO ITAUX=1,NT
    READ(FILSSUP) (DSUPALL(IC,ITAUX),IC=1,NC)
    READ(FILSSUB) (DSUBALL(IC,ITAUX),IC=1,NC)
ENDDO

! GAROFOLO
OPEN(15000, FILE=trim(dir_dados)//'Saida/escoamento_Eta.txt', status='unknown',form='formatted')
DO ITAUX=1,NT
    WRITE(15000,'(2F15.10)')DSUPALL(1000,ITAUX),DSUBALL(1000,ITAUX)
ENDDO
close(15000)

CLOSE(FILSSUB)
CLOSE(FILSSUP)
WRITE(*,*) 'LEU ESCOAMENTO'
RETURN
END
