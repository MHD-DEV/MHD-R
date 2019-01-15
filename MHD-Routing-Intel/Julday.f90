     FUNCTION JULDAY(m,d,y)
	  !DETERMINA O DIA DO CALEND�RIO JULIANO CORRESPONDENTE AO DIA, M�S E ANO DADOS
     INTEGER d,y,m
	  julday=(y-1)*360+(m-1)*30+d
     return
    END    
    
!FUNCTION JULDAY(mm,id,iyyy)
	  !DETERMINA O DIA DO CALEND�RIO JULIANO CORRESPONDENTE AO DIA, M�S E ANO DADOS
      !INTEGER julday,id,iyyy,mm,IGREG
      !PARAMETER (IGREG=15+31*(10+12*1582))
      !INTEGER ja,jm,jy
      !jy=iyyy
      !if (jy.eq.0) pause 'julday: there is no year zero'
      !if (jy.lt.0) jy=jy+1
      !if (mm.gt.2) then
      !  jm=mm+1
      !else
      !  jy=jy-1
      !  jm=mm+13
      !endif
      !julday=int(365.25*jy)+int(30.6001*jm)+id+1720995
	  !
      !if (id+31*(mm+12*iyyy).ge.IGREG) then
      !  ja=int(0.01*jy)
      !  julday=julday+2-ja+int(0.25*ja)
      !endif
	  !
      !return
      !END