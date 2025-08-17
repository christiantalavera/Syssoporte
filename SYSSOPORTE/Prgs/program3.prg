a= encripta('1311')
WAIT WINDOW a

b=decodificar(a)
WAIT WINDOW b

FUNCTION encripta
lPARAMETERS pcTexto as String
LOCAL lnTemporalA, lnTemporalB, lnTemporalC, cResultado, lnContador

lnTemporalA=0
lnTemporalB=0
lnTemporalC=0
cResultado=""
	
FOR lnContador=1 TO LEN(pcTexto)
	
	lnTemporalA=ASC(SUBSTR(pcTexto,lnContador,1))
	lnTemporalB=FLOOR(lnTemporalA/16)
	lnTemporalC=lnTemporalA % 16

	IF	lnTemporalB<10
		lnTemporalB=lnTemporalB+48
	ELSE
		lnTemporalB=lnTemporalB+55
	ENDIF 
	
	IF	lnTemporalC<10 
		lnTemporalC=lnTemporalC+48
	ELSE 
		lnTemporalC=lnTemporalC+55
	ENDIF 
	
	cResultado=cResultado+CHR(lnTemporalB)+CHR(lnTemporalC)
ENDFOR 
RETURN cResultado
ENDFUNC 

FUNCTION codificar
lPARAMETERS pcTexto as String
LOCAL lnTemporalA, lnTemporalB, lnTemporalC, cResultado, lnContador

lnTemporalA=0
lnTemporalB=0
lnTemporalC=0
cResultado=""
	
FOR lnContador=1 TO LEN(pcTexto)
	
	lnTemporalA=ASC(SUBSTR(pcTexto,lnContador,1))
	lnTemporalB=FLOOR(lnTemporalA/16)
	lnTemporalC=lnTemporalA % 16

	IF	lnTemporalB<10
		lnTemporalB=lnTemporalB+48
	ELSE
		lnTemporalB=lnTemporalB+55
	ENDIF 
	
	IF	lnTemporalC<10 
		lnTemporalC=lnTemporalC+48
	ELSE 
		lnTemporalC=lnTemporalC+55
	ENDIF 
	
	cResultado=cResultado+CHR(lnTemporalB)+CHR(lnTemporalC)
ENDFOR 
RETURN cResultado
ENDFUNC 

FUNCTION decodificar
LPARAMETERS pcTexto as String

LOCAL lnTemporalA, lnTemporalB, lnTemporalC, cResultado, lnContador

lnTemporalA=0	
lnTemporalB=0
lnTemporalC=0
cResultado=""

FOR lnContador=1 TO LEN(pcTexto)


	
	lnTemporalB=ASC(SUBSTR(pcTexto,lnContador,1))
	lnContador=lnContador+1
	lnTemporalC=ASC(SUBSTR(pcTexto,lnContador,1))
				
	lnTemporalB=IIF(lnTemporalB=0,48,lnTemporalB)
		
	IF lnTemporalB<58 
		lnTemporalB=lnTemporalB-48
	ELSE 
		IF lnTemporalB>96
			lnTemporalB=lnTemporalB-87
		ELSE 
			lnTemporalB=lnTemporalB-55
		ENDIF 
	ENDIF 
	
	IF lnTemporalC<58
		lnTemporalC=lnTemporalC-48
	ELSE 
		IF lnTemporalC>96
			lnTemporalC=lnTemporalC-87
		ELSE 
			lnTemporalC=lnTemporalC-55
		ENDIF 
	ENDIF 
		
	lnTemporalA=(lnTemporalB*16)+lnTemporalC

	cResultado=cResultado+CHR(lnTemporalA)

ENDFOR 
	
RETURN cResultado
ENDFUNC 