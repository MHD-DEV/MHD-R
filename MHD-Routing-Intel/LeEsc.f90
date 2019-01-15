!GAROFOLO
SUBROUTINE LEESC
USE VARS_MAIN
IMPLICIT NONE
INTEGER ITAUX,IC
REAL LIXO(NC)

CHARACTER CDATAL(NT)*16

!OPEN(FILSSUP, FILE='C:\Users\Garofolo\entrada85-90\escoamento-superficial_1985-1990.bin', status='old',form='unformatted',recordtype='STREAM')
!OPEN(FILSSUB, FILE='C:\Users\Garofolo\entrada85-90\escoamento-subterraneo_1985-1990.bin', status='old',form='unformatted',recordtype='STREAM')
OPEN(FILSSUP, FILE='F:\DADOS-MAC\Entradas-MHDRouting\IBGE+CARRIELO_OTIM_2033-2057\escoamento-superficial_RCP45_otim_2033-2057.bin', status='old',form='unformatted',recordtype='STREAM')
OPEN(FILSSUB, FILE='F:\DADOS-MAC\Entradas-MHDRouting\IBGE+CARRIELO_OTIM_2033-2057\escoamento-subterraneo_RCP45_otim_2033-2057.bin', status='old',form='unformatted',recordtype='STREAM')

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
