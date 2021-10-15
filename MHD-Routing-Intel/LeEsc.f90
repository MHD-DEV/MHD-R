!GAROFOLO
SUBROUTINE LEESC
USE VARS_MAIN
IMPLICIT NONE
INTEGER ITAUX,IC
REAL LIXO(NC)

CHARACTER CDATAL(NT)*16

!OPEN(FILSSUP, FILE='C:\Users\Garofolo\entrada85-90\escoamento-superficial_1985-1990.bin', status='old',form='unformatted',recordtype='STREAM')
!OPEN(FILSSUB, FILE='C:\Users\Garofolo\entrada85-90\escoamento-subterraneo_1985-1990.bin', status='old',form='unformatted',recordtype='STREAM')
OPEN(FILSSUP, FILE='C:\Users\Proclima\Google Drive\UFF\Projeto\MHD-Routing\Dados-Eta-EtaHad-5km\escoamento-superficial_1961-2005.bin', status='old',form='unformatted',recordtype='STREAM')
OPEN(FILSSUB, FILE='C:\Users\Proclima\Google Drive\UFF\Projeto\MHD-Routing\Dados-Eta-EtaHad-5km\escoamento-subterraneo_1961-2005.bin', status='old',form='unformatted',recordtype='STREAM')

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
