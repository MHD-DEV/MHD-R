	SUBROUTINE CALDAT360(JDIA,IMES,IDIA,IANO)
	AUX=FLOAT(JDIA)
	IF(MOD(AUX,360.).EQ.0) THEN
		IANO=INT(AUX/360.)
	ELSE
		IANO=INT(AUX/360.)+1
	ENDIF
	AUX=AUX-(IANO-1)*360
	IF(MOD(AUX,30.).EQ.0.) THEN
		IMES=INT(AUX/30.)
	ELSE
		IMES=INT(AUX/30)+1
	ENDIF
	IDIA=AUX-(IMES-1)*30
	RETURN
	END
