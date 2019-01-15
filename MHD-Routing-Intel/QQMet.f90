SUBROUTINE QQMET
!SUBROTINA QUE VERIFICA A QUALIDADE DOS DADOS METEOROLOGICOS
USE VARS_MAIN
IMPLICIT NONE
INTEGER IC
REAL TEMPO ! TEMPO DE SIMULACAO EM DIAS

DO IT=1,NT
    ! ARRUMA AS DATAS
    TEMPO=IHORAINI/24.+FLOAT(IT-1)*DTP/86400
    IHORA=NINT((TEMPO-INT(TEMPO))*24.) ! HORA DA SIMULACAO
    JDIA=IDINI+INT(TEMPO) ! DIA JULIANO DO CALENDÁRIO 
    CALL CALDAT(JDIA,IMES,IDIA,IANO)
    WRITE(CDATA(1:2),'(I2.2)') IDIA
    WRITE(CDATA(3:4),'(I2.2)') IMES
    WRITE(CDATA(5:8),'(I4)') IANO
    WRITE(CDATA(9:10),'(I2.2)') IHORA
    DO IC=1,NC
	    !TEMPERATURA DO AR EM GRAUS CENTIGRADOS
	    IF(TAALL(IC,IT) < -25.0 .OR. TAALL(IC,IT) > 50.0)THEN 
		    WRITE(*,*)'ERRO NO VALOR DA TEMPERATURA NA CELULA:', IC, ' NA DATA: ', & 
		    & CDATA(1:2)//'/'//CDATA(3:4)//'/'//CDATA(5:8)//' '//CDATA(9:10)//':00',' VALOR: ',TAALL(IC,IT)
		    STOP			
		    !TAALL(IC,IT)=TAMM(MM,IC)	!USA MEDIA MENSAL DO POSTO
	    ENDIF

        !PONTO DE ORVALHO EM GRAUS CENTIGRADOS

	    IF(TDEWALL(IC,IT) < -25 .OR. TDEWALL(IC,IT) > 50.)THEN 
		    WRITE(*,*)'ERRO NO PONTO DE ORVALHO:', IC, ' NA DATA: ', & 
		    & CDATA(1:2)//'/'//CDATA(3:4)//'/'//CDATA(5:8)//' '//CDATA(9:10)//':00',' VALOR: ',TDEWALL(IC,IT)
		    STOP
		    !TDEWALL(IC,IT)=TDEWMM(MM,IC)	!USA MEDIA MENSAL DO POSTO
	    ENDIF
        IF(TDEWALL(IC,IT) > TAALL(IC,IT)) TDEWALL(IC,IT)=TAALL(IC,IT)
	    
	    !VELOCIDADE DO VENTO EM M/S
	    IF(VVALL(IC,IT).LT.0.0.OR.VVALL(IC,IT).GT.30.)THEN

		    WRITE(*,*) 'ERRO NA VELOCIDADE DO VENTO NA CELULA:', IC,' NA DATA: ', & 
		    & CDATA(1:2)//'/'//CDATA(3:4)//'/'//CDATA(5:8)//' '//CDATA(9:10)//':00',' VALOR: ',VVALL(IC,IT)
            !STOP
		    !VVALL(IC,IT)=VVMM(MM,IC) ! EM M/S
	    ENDIF	
    	
		!RADIACAO DE ONDA CURTA GLOBAL INCIDENTE									
	    IF(ROCALL(IC,IT).LT.0.0)THEN
		    WRITE(*,*)'ERRO NA RADICAO DE ONDA CURTA NA CELULA ', IC, ' NA DATA: ', & 
		    & CDATA(1:2)//'/'//CDATA(3:4)//'/'//CDATA(5:8)//' '//CDATA(9:10)//':00',' VALOR:',ROCALL(IC,IT)
		    STOP
		    !ROCALL(IC,IT)=ROCMM(MM,IC) !EM HORAS/DIA
	    ENDIF

        ! PRMD PRESSAO EM MILIBARES
	    IF(PATMALL(IC,IT)>1300 .OR. PATMALL(IC,IT)<400)THEN
		    WRITE(*,*)'ERRO NA PRESSAO NA CELULA:', IC, ' NA DATA:', & 
		    & CDATA(1:2)//'/'//CDATA(3:4)//'/'//CDATA(5:8)//' '//CDATA(9:10)//':00',' VALOR:',PATMALL(IC,IT)
		    !STOP
		    !PATMALL(IC,IT)=PRMM(MM,IC)
	    ELSE
	        !TRANSFORMA MB EM KPA
	        PATMALL(IC,IT)=0.1*PATMALL(IC,IT)
	    ENDIF
    ENDDO
	!pause
ENDDO
!IC=277
!WRITE(1000,'(A10,5F10.2)') CDATA,TAALL(IC,IT),TDEWALL(IC,IT),PATMALL(IC,IT)/0.1,VVALL(IC,IT),ROCALL(IC,IT)


RETURN
END