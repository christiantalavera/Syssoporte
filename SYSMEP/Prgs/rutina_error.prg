PROCEDURE OnError
	LPARAMETERS lnerror As Integer, lcMetodo As String, lnLine As Integer, llReturn As Logical
	LOCAL loException As Exception
	LOCAL lnNivel As Integer
	*ON ERROR
	lnNivel = TXNLEVEL()
	FOR n = lnNivel TO 1 STEP -1
		ROLLBACK
	ENDFOR
	CLEAR
	loException = CREATEOBJECT("Exception")
	loException.ErrorNo = lnError
	loException.LineNo = lnLine
	loException.Message = MESSAGE()
	loException.Procedure = lcMetodo
	loException.Details = SYS(2018)
	loException.LineContents = MESSAGE(1)
	DO ShowError WITH loException
	*WAIT WINDOW lcMetodo
	IF llReturn
		RETURN TO lcMetodo
	ENDIF
ENDFUNC

PROCEDURE ShowError
	LPARAMETERS loError As Exception
	LOCAL CRLF As String
	CRLF = CHR(13)+CHR(10)
	cStr = "Error occurred in CATCH block" + CRLF + CRLF + ;
			"[  Número: ] " + STR(loError.ErrorNo) + CRLF + ;
    		"[  Línea: ] " + STR(loError.LineNo) + CRLF + ; 
	    	"[  Mensaje: ] " + loError.Message + CRLF + ; 
    		"[  Procedimiento: ] " + loError.Procedure + CRLF + ; 
	   		"[  Detalles: ] " + loError.Details + CRLF + ; 
	    	"[  Nivel de Pila: ] " + STR(loError.StackLevel) + CRLF + ; 
    		"[  Línea de Contenido: ] " + loError.LineContents + CRLF + ;
    		"[  Tabla : ] " + ALIAS() + CRLF + ;
	    	"[  Fecha : ] " + TTOC(DATETIME()) + CRLF + ;
    		"[======================================]" + CRLF + CRLF
	MESSAGEBOX(cStr, 'Error del Sistema ') && ,1+64+0
ENDPROC