FUNCTION Codificar_simple
LPARAMETERS pcTexto as String, pcPalabraClave as String 
LOCAL nDelta, lnTemporalA, cSalidaDato, i, j

nDelta=5
lnTemporalA=0
cSalidaDato=""
j=1
	
IF LEN(pcPalabraClave)=0
	cSalidaDato=this.Codificar(pcTexto)
ELSE 
	FOR i=1 TO LEN(pcTexto)
		IF j=LEN(pcPalabraClave)
			j=1
		ENDIF 
			
		lnTemporalA = ASC(SUBSTR(pcTexto,i,1))
		lnTemporalA = lnTemporalA + ASC(SUBSTR(pcPalabraClave,j,1))
		
		IF lnTemporalA > 255
			lnTemporalA=lnTemporalA-256
		ENDIF 
		
		IF lnTemporalA < nDelta
			cSalidaDato = cSalidaDato + CHR(nDelta)
			lnTemporalA = lnTemporalA+ nDelta
		ENDIF 
		
		cSalidaDato = cSalidaDato + CHR(lnTemporalA)
		j=j+1
	ENDFOR 
	cSalidaDato=Codificar(cSalidaDato)
ENDIF 
RETURN cSalidaDato

FUNCTION decodificar_simple 
LPARAMETERS pcTexto as String, pcPalabraClave as String 
LOCAL delta, lnTemporalA, cSalidaDato, i ,j 
	
nDelta=5
lnTemporalA=0
cSalidaDato=""
j=1
	
*WITH this 
	IF LEN(pcPalabraClave)=0
		cSalidaDato= .Decodificar(pcTexto) &&myunescape(pcTexto)
	ELSE 
		pcTexto=Decodificar(pcTexto) &&myunescape(pcTexto)
		
		FOR i=1 TO LEN(pcTexto)
	
			IF j=LEN(pcPalabraClave)
				j=1
			ENDIF 
		
			lnTemporalA = ASC(SUBSTR(pcTexto,i,1))
			IF lnTemporalA=nDelta
				i=i+1
				lnTemporalA = ASC(SUBSTR(pcTexto,i,1))
				slnTemporalA = lnTemporalA - nDelta
			ENDIF 
							
			lnTemporalA = lnTemporalA - ASC(SUBSTR(pcPalabraClave,j,1))

		
			IF lnTemporalA < 0
				lnTemporalA=lnTemporalA+256
			ENDIF 
		
			cSalidaDato = cSalidaDato + CHR(lnTemporalA)
			j=j+1
		ENDFOR	 
	ENDIF
*ENDWITH

RETURN cSalidaDato

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


PROCEDURE close_table
LOCAL lgndbfnumber, ladbfs, lcTable, lncount
LOCAL ARRAY ladbfs(10)
lgndbfnumber = AUSED(ladbfs)
FOR lncount = 1 TO lgndbfnumber  && Bucle para número de bases de datos
	lcTable = ladbfs(lncount,1)
	IF USED('&lcTable.')
		SELE (lcTable)
		USE
	ENDIF
ENDFOR
ENDPROC 


PROCEDURE CERRAR_TABLA
 DIMENSION aTablas(1)
   nTablas = AUSED(aTablas)

   IF nTablas > 0 
  FOR EACH oTabla IN aTablas
     SELECT (oTabla)
     USE 
  ENDFOR 
   ENDIF
ENDPROC 

PROCEDURE Marquesina
IF RTRIM(UPPER(gcEncriptaDesencripta.DecodificarBlowFish(gcClave,gcllave))) <> RTRIM(gcLlave) THEN 
	msg='Versión de Prueba --> Solo dispone de '+limiteAccesos - cvalue
	cl = LEN(msg)
	FOR x=1 TO cl
	SET MESSAGE TO RIGHT(msg,x)
	WAIT WIND '' TIME .1
	ENDFOR 
ENDIF 
ENDPROC 

*----------------------
FUNCTION FormIsObject()
*----------------------
*-- Return .T. if the active form is of type "O" and its baseclass
*-- is "Form".
RETURN (TYPE("_screen.activeform") == "O" AND ;
	UPPER(_SCREEN.ACTIVEFORM.BASECLASS) = "FORM")
ENDFUNC


**************************
FUNCTION LETREO()
**************************
PARAMETER parmonto,parmoneda
IF parmonto<.01
	RETURN ''
ENDIF
PRIVATE monto,moneda,centimos,importe,cientos,miles,millones,enletras,nlet
monto = parmonto
moneda= parmoneda
DECLARE nlet(42)
STORE 'UNO'           TO nlet(01)
STORE 'DOS'           TO nlet(02)
STORE 'TRES'          TO nlet(03)
STORE 'CUATRO'        TO nlet(04)
STORE 'CINCO'         TO nlet(05)
STORE 'SEIS'          TO nlet(06)
STORE 'SIETE'         TO nlet(07)
STORE 'OCHO'          TO nlet(08)
STORE 'NUEVE'         TO nlet(09)
STORE 'DIEZ'          TO nlet(10)
STORE 'ONCE'          TO nlet(11)
STORE 'DOCE'          TO nlet(12)
STORE 'TRECE'         TO nlet(13)
STORE 'CATORCE'       TO nlet(14)
STORE 'QUINCE'        TO nlet(15)
STORE 'DIEZ'          TO nlet(16)
STORE 'VEINTE'        TO nlet(17)
STORE 'TREINTA'       TO nlet(18)
STORE 'CUARENTA'      TO nlet(19)
STORE 'CINCUENTA'     TO nlet(20)
STORE 'SESENTA'       TO nlet(21)
STORE 'SETENTA'       TO nlet(22)
STORE 'OCHENTA'       TO nlet(23)
STORE 'NOVENTA'       TO nlet(24)
STORE 'DIECI'         TO nlet(25)
STORE 'VEINTI'        TO nlet(26)
STORE 'TREINTI'       TO nlet(27)
STORE 'CUARENTI'      TO nlet(28)
STORE 'CINCUENTI'     TO nlet(29)
STORE 'SESENTI'       TO nlet(30)
STORE 'SETENTI'       TO nlet(31)
STORE 'OCHENTI'       TO nlet(32)
STORE 'NOVENTI'       TO nlet(33)
STORE 'CIENTO'        TO nlet(34)
STORE 'DOSCIENTOS'    TO nlet(35)
STORE 'TRESCIENTOS'   TO nlet(36)
STORE 'CUATROCIENTOS' TO nlet(37)
STORE 'QUINIENTOS'    TO nlet(38)
STORE 'SEISCIENTOS'   TO nlet(39)
STORE 'SETECIENTOS'   TO nlet(40)
STORE 'OCHOCIENTOS'   TO nlet(41)
STORE 'NOVECIENTOS'   TO nlet(42)

centimos=(monto-INT(monto))*100
centimos=IIF(1<monto,'Y ','')+PADL(INT(centimos),2,'0')+'/100 '+IIF(moneda=1,'NUEVOS SOLES','DOLARES AMERICANOS')
importe=PADL(INT(monto),12,'0')
cientos=RIGHT(importe,3)
miles=SUBSTR(importe,7,3)
millones=SUBSTR(importe,4,3)
milesmillones=LEFT(importe,3)
enletras=''
IF cientos<>'000'
	enletras=letras(cientos,1)
ENDIF
IF miles<>'000'
	enletras=letras(miles,2)+'MIL '+enletras
ENDIF
IF millones<>'000'
	enletras=letras(millones,3)+IIF(VAL(millones)>1,'MILLONES ','MILLON ')+enletras
ENDIF
IF milesmillones<>'000'
	enletras=letras(milesmillones,3)+IIF(VAL(milesmillones)>1,'MIL ','MIL ')+enletras
ENDIF
RETURN enletras+centimos

FUNCTION letras
PARAMETER let,CENT
PRIVATE let,CENT,LETREO
LETREO=''
FOR i=1 TO 3
	x=IIF(i>1,VAL(SUBSTR(let,i-1,1)),0)
	Y=VAL(SUBSTR(let,i,1))
	z=IIF(i<3,VAL(SUBSTR(let,i+1,1)),0)
	DO porcientos
ENDFOR
DO CASE
	CASE 'UNO'$LETREO.AND.CENT>1
		LETREO=STRTRAN(LETREO,'UNO','UN')
	CASE 'CIENTO '=LETREO.AND.RIGHT(let,2)='00'
		LETREO='CIEN '
ENDCASE
RETURN LETREO

FUNCTION porcientos
DO CASE
	CASE i=3.AND.(Y>5.OR.(BETWEEN(Y,1,5).AND.x<>1))
		LETREO=LETREO+nlet(Y)+' '
	CASE i=3.AND.BETWEEN(Y,1,5).AND.x=1
		LETREO=LETREO+nlet(Y+10)+' '
	CASE i=2.AND.Y>0.AND.z=0
		LETREO=LETREO+nlet(Y+15)+' '
	CASE i=2.AND.(Y>0.AND.!(Y=1.AND.BETWEEN(z,1,5)))
		LETREO=LETREO+nlet(Y+24)
	CASE i=1.AND.Y>0
		LETREO=LETREO+nlet(Y+33)+' '
ENDCASE


FUNCTION obtiene_clasificador
PARAMETERS pAno, pIdClasificador
	USE siaf!especifica_det IN 0 ALIAS c_especifica_det AGAIN ORDER tag idclasif
	lClasificador = ''
	IF SEEK(pAno+pIdClasificador,'c_especifica_det') THEN 
		lClasificador = c_especifica_det.tipo_transaccion+'.'+c_especifica_det.generica+'.'+c_especifica_det.subgenerica+'.'+;
						c_especifica_det.subgenerica_det+'.'+c_especifica_det.especifica+'.'+c_especifica_det.especifica_det
	ENDIF 
	USE IN c_especifica_det 


RETURN lClasificador


FUNCTION obtiene_clasificador_new
PARAMETERS pClasificador
PRIVATE cTipo_transaccion, cGenerica, cSubGenerica, cSubGenerica_det, cEspecifica, cEspecifica_det
	STORE '' TO cTipo_transaccion, cGenerica, cSubGenerica, cSubGenerica_det, cEspecifica, cEspecifica_det
*	pClasificador = STRTRAN(pClasificador, '.', ' ') 
	pClasificador = RTRIM(pClasificador)
	CREATE CURSOR cur_posicion (posicion N(2))
	STORE 0 TO xPos_1, xPos_2, xPos_3
	x = 0
	FOR i = 1 TO LEN(RTRIM(pclasificador))
		IF INLIST(SUBSTR(pClasificador,i,1), '.',' ') THEN 
			INSERT INTO cur_posicion VALUES (i)
			x = x + 1
		ENDIF 
	ENDFOR 
	IF x = 3 THEN 
		cTipo_transaccion = LEFT(pClasificador,1)
		cGenerica 		  = SUBSTR(pClasificador,3,1)
		z = 0
		SELECT cur_posicion
		SCAN ALL 
			z = z + 1
			DO CASE 
				CASE z = 1
					xPos_1 = cur_posicion.posicion
				CASE z = 2
					xPos_2 = cur_posicion.posicion			
				CASE z = 3
					xPos_3 = cur_posicion.posicion				
			ENDCASE 
		ENDSCAN 
		
		lcCadena = SUBSTR(pClasificador,xPos_2 + 1, xPos_3 - 1 - xPos_2)
		DO CASE 
			CASE LEN(lcCadena) = 2
				cSubGenerica	  = PADL(SUBSTR(pClasificador,xPos_2 + 1, 1), 2, ' ')
				cSubGenerica_det  = PADL(SUBSTR(pClasificador,xPos_2 + 2, 1), 2, ' ')
			CASE LEN(lcCadena) = 3
				cSubGenerica	  = PADL(SUBSTR(pClasificador,xPos_2 + 1, 1), 2, ' ')
				cSubGenerica_det  = PADL(SUBSTR(pClasificador,xPos_2 + 2, 2), 2, ' ')
			CASE LEN(lcCadena) = 4					
				cSubGenerica	  = SUBSTR(pClasificador,xPos_2 + 1, 2)
				cSubGenerica_det  = SUBSTR(pClasificador,xPos_2 + 3, 2)
			
		ENDCASE 
		
		lcCadena = SUBSTR(pClasificador, xPos_3+1, LEN(pClasificador)- xPos_3)
		DO CASE 
			CASE LEN(lcCadena) = 2
				cEspecifica		  = PADL(SUBSTR(pClasificador,xPos_3 + 1, 1), 2, ' ')
				cEspecifica_det   = PADL(SUBSTR(pClasificador,xPos_3 + 2, 1), 2, ' ')
			CASE LEN(lcCadena) = 3
				cEspecifica		  = PADL(SUBSTR(pClasificador,xPos_3 + 1, 1), 2, ' ')
				cEspecifica_det   = PADL(SUBSTR(pClasificador,xPos_3 + 2, 2), 2, ' ')
			CASE LEN(lcCadena) = 4					
				cEspecifica	 	  = SUBSTR(pClasificador,xPos_3 + 1, 2)
				cEspecifica_det   = SUBSTR(pClasificador,xPos_3 + 3, 2)
			
		ENDCASE 
		
		
	ENDIF 

	USE IN cur_posicion
	lcClasificador = cTipo_transaccion + cGenerica + cSubGenerica + cSubGenerica_det + cEspecifica + cEspecifica_det

RETURN lcClasificador

FUNCTION bloqueo
PARAMETERS pTable, pTipo, pCaption
LOCAL flag_log
*- pTable 	: Tabla a bloquear
*- pTipo	: Tipo de bloqueo (R:Registro ; T:Tabla)
*- flag_log : F Desactiva el log_actividad
*- flag_log : T Activa el log_actividad

flag_log=.F.

gcip	= ''
llogico	= .T.

IF pTipo = 'T'
	IF FLOCK(ptable) THEN 
		lcMensaje	= 'Usuario bloqueó la tabla exitosamente.'
		llogico=.T.
	ELSE
		lcMensaje	= 'Usuario intentó bloquear la tabla sin éxito.'
		llogico=.F.
	ENDIF 
ELSE
	IF RLOCK(ptable) THEN 
		lcMensaje	= 'Usuario bloqueó el registro exitosamente.'
		llogico=.T.
	ELSE
		lcMensaje	= 'Usuario intentó bloquear el registro sin éxito.'
		llogico=.F.
	ENDIF 
ENDIF

*!*	IF EMPTY(gcUserid) then
*!*		gcUserid='INSTALADOR'
*!*	ENDIF

*!*	IF flag_log THEN
*!*		INSERT INTO log_actividad VALUES (gcUserid, gcHost, gcIp, pTipo , pTable, lcMensaje, DATETIME(),oSiafRutina.mRetornoSession(),pCaption)
*!*	ENDIF


RETURN llogico

*----------------------------------------------------
FUNCTION WriteFileIni(tcFileName,tcSection,tcEntry,tcValue)
*----------------------------------------------------
* Escribe un valor de un archivo INI.
* Si no existe el archivo, la sección o la entrada, la crea.
* Retorna .T. si tuvo éxito
* PARAMETROS:
* tcFileName = Nombre y ruta completa del archivo.INI
* tcSection = Sección del archivo.INI
* tcEntry = Entrada del archivo.INI
* tcValue = Valor de la entrada
* USO: WriteFileIni("C:MiArchivo.ini","Default","Port","2")
* RETORNO: Logico
*----------------------------------------------------
DECLARE INTEGER WritePrivateProfileString ;
IN WIN32API ;
STRING cSection,STRING cEntry,STRING cEntry,;
STRING cFileName

RETURN IIF(WritePrivateProfileString(tcSection,tcEntry,tcValue,tcFileName)=1, .T., .F.)
ENDFUNC

*----------------------------------------------------
FUNCTION ReadFileIni(tcFileName,tcSection,tcEntry)
*----------------------------------------------------
* Lee un valor de un archivo INI.
* Si no existe el archivo, la sección o la entrada, retorna .NULL.
* PARAMETROS:
* tcFileName = Nombre y ruta completa del archivo.INI
* tcSection = Sección del archivo.INI
* tcEntry = Entrada del archivo.INI
* USO: ReadFileIni("C:MiArchivo.ini","Default","Port")
* RETORNO: Caracter
*----------------------------------------------------
LOCAL lcIniValue, lnResult, lnBufferSize
DECLARE INTEGER GetPrivateProfileString ;
IN WIN32API ;
STRING cSection,;
STRING cEntry,;
STRING cDefault,;
STRING @cRetVal,;
INTEGER nSize,;
STRING cFileName
lnBufferSize = 255
lcIniValue = spac(lnBufferSize)
lnResult=GetPrivateProfileString(tcSection,tcEntry,"*NULL*",;
@lcIniValue,lnBufferSize,tcFileName)
lcIniValue=SUBSTR(lcIniValue,1,lnResult)
IF lcIniValue="*NULL*"
lcIniValue=.NULL.
ENDIF
RETURN lcIniValue
ENDFUNC

*!------------------------------------------------------------------------------
*! Procedure : ExcelToCursor
*! Parametros: pcSrcFile -> Nombre del libro de excel
*!             pcCursorName -> Nombre del cursor
*!------------------------------------------------------------------------------
PROCEDURE ExcelToCursor(pcSrcFile AS STRING, pcCursorName AS STRING)

  IF PCOUNT() = 0
    RETURN .F.
  ELSE
    IF VARTYPE("pcSrcFile")#"C"
      RETURN .F.
    ENDIF

    IF !FILE(pcSrcFile)
      MESSAGEBOX("Archivo no encontrado", 16)
      RETURN .F.
    ENDIF

    IF VARTYPE("pcCursorName")#"C"
      RETURN .F.
    ENDIF
  ENDIF

  *** Instanciar MS Excel
  LOCAL oExcel AS Excel.APPLICATION
  m.oExcel = CREATEOBJECT("Excel.application")

  IF VARTYPE(oExcel,.T.)!='O'
    MESSAGEBOX("No se puede procesar el archivo." ;
      + CHR(13) + "Microsoft Excel no está instalado en su ordenador.", 16)
    m.oExcel = NULL
    RELEASE oExcel
    RETURN .F.
  ENDIF

  *** Abrir archivo de Excel
  m.oExcel.Workbooks.OPEN(pcSrcFile)
  m.oExcel.Worksheets(1).ACTIVATE
  m.oExcel.DisplayAlerts = .F.

  LOCAL oSheet AS OBJECT
  m.oSheet = m.oExcel.ActiveSheet

  LOCAL aExcel(1), laStructure(1)
  LOCAL lnCol, lnRow, lnSize, lcCol, lcRow, lcValue, lcCmd

  *** Redimensionar aExcel de acuerdo a las filas y columnas que
  *** contiene el libro de excel abierto
  IF EVALUATE("ALEN(aExcel)") # m.oSheet.UsedRange.COLUMNS.COUNT
    DIMENSION aExcel [1, m.oSheet.UsedRange.Columns.Count]
  ENDIF

  m.lnCol = m.oSheet.UsedRange.COLUMNS.COUNT
  m.lnRow = m.oSheet.UsedRange.ROWS.COUNT

  *** Pasar los valores del libro de excel
  *** a la matriz redimensionada aExcel
  TEXT TO lcCmd TEXTMERGE NOSHOW PRETEXT 1+2
		aExcel = m.oExcel.ActiveWorkbook.ActiveSheet.Range(m.oSheet.Cells(1,1), m.oSheet.Cells(<<m.lnRow>>,<<m.lnCol>>)).value
  ENDTEXT
  &lcCmd

  *** Cerrar la instancia MS Excel
  m.oExcel.QUIT()
  m.oExcel = NULL
  RELEASE oExcel, oSheet

  *** Procedimiento para determinar los tipo de datos por
  *** columnas y crear la estructura del cursor
  m.lnRow = ALEN(aExcel,1)
  m.lnCol = IIF(ALEN(aExcel,2)>0, ALEN(aExcel,2), 1)

  *** La matriz laStructure bidimensional
  *** almacena la estructura del cursor
  *** Columna 1 -> Nombre de la columna
  *** Columna 2 -> Tipo de datos
  *** Columna 3 -> Largo
  *** Columna 4 -> Decimal
  *** Columna 5 -> Acepta valores null
  DIMENSION laStructure(m.lnCol,5)
  FOR i = 1 TO m.lnCol
    m.lnSize = 1
    m.lcCol = LTRIM(STR(i))
    laStructure(i,1) = aExcel(1,i)
    laStructure(i,2) = VARTYPE(aExcel(2,i))
    DO CASE
      CASE laStructure(i,2) = "C" && Character, Memo, Varchar, Varchar (Binary)
        FOR j = 1 TO m.lnRow
          m.lcValue = IIF(m.lnCol = 1, TRANSFORM(aExcel(j)), TRANSFORM(aExcel(j,i)))
          m.lnSize = MAX(m.lnSize, LEN(TRANSFORM(aExcel(j,i))))
          IF AT(CHR(13), m.lcValue) > 0
            laStructure(i,2) = "M" && Memo
          ENDIF
        ENDFOR

        IF laStructure(i,2) = "C"  && Character, Varchar
          IF lnSize < 10
            laStructure(i,3) = 10
          ELSE
            laStructure(i,3) = lnSize
          ENDIF
          laStructure(i,4) = 0
        ELSE 				       && Memo, Blob
          laStructure(i,3) = 4
          laStructure(i,4) = 0
        ENDIF

      CASE laStructure(i,2) = "D" OR laStructure(i,2) = "T" && Date, DateTime
      	laStructure(i,2) = "C"
*        laStructure(i,3) = 8
        laStructure(i,3) = 10
        laStructure(i,4) = 0

      CASE laStructure(i,2) = "L" && Logical
        laStructure(i,3) = 1
        laStructure(i,4) = 0

      CASE laStructure(i,2) = "N" && Numeric, Float, Double, o Integer
      	laStructure(i,2) = "C"      
        laStructure(i,3) = 19
        laStructure(i,4) = 0
	    laStructure(i,5) = .T.      	
*        laStructure(i,3) = 12
*        laStructure(i,4) = 2


      OTHERWISE
    ENDCASE
    laStructure(i,5) = .T.
  ENDFOR

  *** Crear el cursor
  CREATE CURSOR &pcCursorName FROM ARRAY laStructure

  *** Insertar en el cursor los valores desde aExcel
  LOCAL lCellValue
  m.lcRow = ""

  FOR i = 1 TO m.lnRow
    FOR j = 1 TO m.lnCol
      IF !EMPTY(m.lcRow)
        m.lcRow = m.lcRow + ", "
      ENDIF

      lCellValue = EVALUATE([aExcel(i,j)])
      DO CASE
        CASE VARTYPE(lCellValue) = "C" && Character, Memo, Varchar, Varchar (Binary)
          IF !EMPTY(lCellValue) OR lCellValue # ""
            m.lcRow = m.lcRow + ['] + EVALUATE([aExcel(i,j)]) + [']
          ELSE
            m.lcRow = m.lcRow + [Null]
          ENDIF

        CASE VARTYPE(lCellValue) = "D" OR VARTYPE(lCellValue) = "T" && Date, DateTime
*          m.lcRow = m.lcRow + [{] + EVALUATE([aExcel(i,j)]) + [}]
*          m.lcRow = m.lcRow +  '[' + dtoc(EVALUATE([aExcel(i,j)]))+' 12:00:00]'
            m.lcRow = m.lcRow + ['] + DTOC(EVALUATE([aExcel(i,j)])) + [']

        CASE VARTYPE(lCellValue) = "N" && Numeric, Float, Double, o Integer
          m.lcRow = m.lcRow + ['] + ALLTRIM(STR(EVALUATE([aExcel(i,j)]))) + ['] 

*          m.lcRow = m.lcRow + ALLTRIM(STR(EVALUATE([aExcel(i,j)]),12,2))

        OTHERWISE
          m.lcRow = m.lcRow + EVALUATE([aExcel(i,j)])
      ENDCASE
    ENDFOR

    IF i > 1
      TEXT TO cSQL TEXTMERGE NOSHOW PRETEXT 1+2
				Insert Into <<pcCursorName>> Values (<<lcRow>>)
      ENDTEXT
      EXECSCRIPT(cSQL)
    ENDIF

    m.lcRow = ""
  ENDFOR

  *** Liberar variables
  RELEASE pcSrcFile, laStructure, lnSize, lcValue, lcCmd
  RELEASE lCellValue, aExcel, lnCol, lnRow, lcCol, lcRow, cSQL, i, j

  SELECT &pcCursorName
  GO TOP

  *** Retornar el cursor
  RETURN SETRESULTSET(pcCursorName)
ENDPROC


PROCEDURE EXCELTOCURSOR_NEW 

PARAMETERS pcNameFile, pcNameHojaExcel, pcNameCursor
PUBLIC  oCA as CursorAdapter  
PUBLIC  oRS as ADODB.Recordset
PUBLIC  oConnDS  as ADODB.CONNECTION
PRIVATE lcArchivo,lcHojaXls,lcExt     

lcArchivo   = ALLTRIM(UPPER(pcNameFile))
lcExt       = UPPER(ALLTRIM(SUBSTR(lcArchivo, RAT(".",lcArchivo)+1)))
lcHojaXls   = ALLTRIM(UPPER(pcNameHojaExcel))
lcNameCursor= ALLTRIM(UPPER(pcNameCursor))
lbSalir     = .F.
OK          = .T.
DO CASE 
      CASE lcExt =="XLS"
            lcConnString= "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + lcArchivo+ ";Extended Properties=Excel 8.0;"
      CASE lcExt =="XLSX"
            lcConnString= "Provider=Microsoft.ACE.OLEDB.12.0;Data Source=" + lcArchivo+ ";Extended Properties=Excel 12.0;"
      OTHERWISE
            WAIT WINDOW "Formato no valido ..." NOWAIT 
            lbSalir = .T.
            Ok          = .F.
ENDCASE 
 
IF !lbSalir 
      oConnDS = CREATEOBJECT('ADODB.CONNECTION')
      TRY
            oConnDS.Open(lcConnString)
            csql = "SELECT * FROM ["+lcHojaXls +"$]"
            oRS = CREATEOBJECT("ADODB.Recordset")
          oRS.DataSource.CursorLocation = 3   &&adUseClient
          oRS.DataSource.LockType     = 3   &&adLockOptimistic
          oRS.CursorType                          = 3 &&adOpenStatic
          oRS.ActiveConnection = oConnDS
          oCA = CREATEOBJECT("CursorAdapter")
          oCA.DataSourceType = "ADO"
          oCA.DataSource            = oRS
          oCA.MapBinary             = .T.
          oCA.MapVarchar            = .T.
          oCA.Alias = lcNameCursor
            oCA.SelectCmd = csql
            oCA.BufferModeOverride= 5
          IF !oCA.CursorFill()
                  lcMensaje= 'Línea de código con error: ' + str(cLine) + CHR(13) +;
                                 'Nombre del Programa: ' + cProg + CHR(13) +;
                                 'Mensaje de error: ' + MESSAGE( ) + CHR(13) +;
                                 'Sentencia SQL: ' + CHR(13) + csql
              MESSAGEBOX("Error al leer Excel",16,"Aviso del Sistema")
              sqlreterr=.T.
          ELSE
            SELECT &lcNameCursor.
            GO bottom
              LOCAL laFlds,lcStr,lnFldCount,i
              DIMENSION laFlds[1]
              lnFldCount=AFIELDS(laFlds)
              lcStr=""
              FOR i = 1 TO lnFldCount
                lcStr = lcStr + laFlds[m.i,1] +  ","
              ENDFOR
              oCA.UpdatableFieldList = lcStr
          ENDIF
      CATCH TO oException
            OK = .F.       
      ENDTRY
ENDIF 
IF !OK THEN
      WAIT WINDOW "No se puede cargar información del archivo "+lcArchivo NOWAIT 
ENDIF 
RETURN OK


PROCEDURE CLASIFICADOR_CODIGO
PARAMETERS pcAno_Eje, pnId_Clasificador, pnNivel, pcUnidad
*--
Private lccodigo, m.tipo_transaccion, m.generica, m.subgenerica, m.subgenerica_det, m.especifica, m.especifica_det,;
	m.origen_clasificador, m.categ_gasto, m.grupo_gasto, m.modalidad_gasto, m.elemento_gasto, m.clase_ingreso, ;
	m.tipo_ingreso, m.sub_tipo_ingreso, m.elemento_ingreso

*--
*-- Origen Clasificador 1 --> Ingreso
*--						2 --> Gasto
*--						3 --> Nuevo Clasificador
*--						4 --> Elemento Complementaria
*--						9 --> Codigo Inexistente en Maestro
lccodigo = ''
m.origen_clasificador = ''
*--
USE maestro_clasificador   	IN 0 AGAIN ALIAS maesclasif
Use especifica_det	 		In 0 Again Alias clasifniv6
Use elemento_ingreso 		In 0 Again Alias clasifing4
Use elemento_gasto	 		In 0 Again Alias clasifgas4
*--

=Seek(pcano_eje+pnid_clasificador,'maesclasif','idclasif')
m.origen_clasificador = maesclasif.origen_clasificador

*--
Do Case
	Case m.origen_clasificador = '1'
		If Seek(pcano_eje+pnid_clasificador,'clasifing4','elementop') Then
			m.clase_ingreso	   = clasifing4.clase_ingreso
			m.tipo_ingreso	   = clasifing4.tipo_ingreso
			m.sub_tipo_ingreso = clasifing4.sub_tipo_ingreso
			m.elemento_ingreso = clasifing4.elemento_ingreso
			Do Case
				Case pnnivel = 1
					lccodigo = IIF(pcUnidad = 'T', m.clase_ingreso, m.clase_ingreso)
				Case pnnivel = 2
					lccodigo = IIF(pcUnidad = 'T', m.clase_ingreso+m.tipo_ingreso, m.tipo_ingreso)
				Case pnnivel = 3
					lccodigo = IIF(pcUnidad = 'T', m.clase_ingreso+m.tipo_ingreso+m.sub_tipo_ingreso, m.sub_tipo_ingreso)
				Case pnnivel = 4
					lccodigo = IIF(pcUnidad = 'T', m.clase_ingreso+m.tipo_ingreso+m.sub_tipo_ingreso+m.elemento_ingreso, m.elemento_ingreso)
			Endcase
		Endif
	Case m.origen_clasificador = '2'
		If Seek(pcano_eje+pnid_clasificador,'clasifgas4','elementop') Then
			m.categ_gasto	  = clasifgas4.categ_gasto
			m.grupo_gasto	  = clasifgas4.grupo_gasto
			m.modalidad_gasto = clasifgas4.modalidad_gasto
			m.elemento_gasto  = clasifgas4.elemento_gasto
			Do Case
				Case pnnivel = 1
					lccodigo = IIF(pcUnidad = 'T', m.categ_gasto, m.categ_gasto)
				Case pnnivel = 2
					lccodigo = IIF(pcUnidad = 'T', m.categ_gasto+m.grupo_gasto, m.grupo_gasto)
				Case pnnivel = 3
					lccodigo = IIF(pcUnidad = 'T', m.categ_gasto+m.grupo_gasto+m.modalidad_gasto, m.modalidad_gasto)
				Case pnnivel = 4
					lccodigo = IIF(pcUnidad = 'T', m.categ_gasto+m.grupo_gasto+m.modalidad_gasto+m.elemento_gasto, m.elemento_gasto)
			Endcase
		Endif
	Case m.origen_clasificador = '3'
		If Seek(pcano_eje+pnid_clasificador,'clasifniv6','idclasif') Then
			m.tipo_transaccion	  = clasifniv6.tipo_transaccion
			m.generica			  = clasifniv6.generica
			m.subgenerica		  = clasifniv6.subgenerica
			m.subgenerica_det	  = clasifniv6.subgenerica_det
			m.especifica		  = clasifniv6.especifica
			m.especifica_det	  = clasifniv6.especifica_det
			Do Case
				Case pnnivel = 1
					lccodigo = IIF(pcUnidad = 'T', m.tipo_transaccion, m.tipo_transaccion)
				Case pnnivel = 2
					lccodigo = IIF(pcUnidad = 'T', m.tipo_transaccion+m.generica, m.generica)
				Case pnnivel = 3
					lccodigo = IIF(pcUnidad = 'T', m.tipo_transaccion+m.generica+m.subgenerica, m.subgenerica)
				Case pnnivel = 4
					lccodigo = IIF(pcUnidad = 'T', m.tipo_transaccion+m.generica+m.subgenerica+m.subgenerica_det, m.subgenerica_det)
				Case pnnivel = 5
					lccodigo = IIF(pcUnidad = 'T', m.tipo_transaccion+m.generica+m.subgenerica+m.subgenerica_det+m.especifica, m.especifica)
				Case pnnivel = 6
					lccodigo = IIF(pcUnidad = 'T', m.tipo_transaccion+m.generica+m.subgenerica+m.subgenerica_det+m.especifica+m.especifica_det, m.especifica_det)
			Endcase
		Endif
Endcase
*--
Use In clasifniv6
Use In clasifing4
Use In clasifgas4
USE IN maesclasif
*--
Return lccodigo

*!*	PROCEDURE EXCELTOCURSOR_NEW 

*!*	PARAMETERS pcNameFile, pcNameHojaExcel, pcNameCursor
*!*	PUBLIC  oCA as CursorAdapter  
*!*	PUBLIC  oRS as ADODB.Recordset
*!*	PUBLIC  oConnDS  as ADODB.CONNECTION
*!*	PRIVATE lcArchivo,lcHojaXls,lcExt     

*!*	lcArchivo   = ALLTRIM(UPPER(pcNameFile))
*!*	lcExt       = UPPER(ALLTRIM(SUBSTR(lcArchivo, RAT(".",lcArchivo)+1)))
*!*	lcHojaXls   = ALLTRIM(UPPER(pcNameHojaExcel))
*!*	lcNameCursor= ALLTRIM(UPPER(pcNameCursor))
*!*	lbSalir     = .F.
*!*	OK          = .T.
*!*	DO CASE 
*!*	      CASE lcExt =="XLS"
*!*	            lcConnString= "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + lcArchivo+ ";Extended Properties=Excel 8.0;"
*!*	      CASE lcExt =="XLSX"
*!*	            lcConnString= "Provider=Microsoft.ACE.OLEDB.12.0;Data Source=" + lcArchivo+ ";Extended Properties=Excel 12.0;"
*!*	      OTHERWISE
*!*	            WAIT WINDOW "Formato no valido ..." NOWAIT 
*!*	            lbSalir = .T.
*!*	            Ok          = .F.
*!*	ENDCASE 
*!*	 
*!*	IF !lbSalir 
*!*	      oConnDS = CREATEOBJECT('ADODB.CONNECTION')
*!*	      TRY
*!*	            oConnDS.Open(lcConnString)
*!*	            csql = "SELECT * FROM ["+lcHojaXls +"$]"
*!*	            oRS = CREATEOBJECT("ADODB.Recordset")
*!*	          oRS.DataSource.CursorLocation = 3   &&adUseClient
*!*	          oRS.DataSource.LockType     = 3   &&adLockOptimistic
*!*	          oRS.CursorType                          = 3 &&adOpenStatic
*!*	          oRS.ActiveConnection = oConnDS
*!*	          oCA = CREATEOBJECT("CursorAdapter")
*!*	          oCA.DataSourceType = "ADO"
*!*	          oCA.DataSource            = oRS
*!*	          oCA.MapBinary             = .T.
*!*	          oCA.MapVarchar            = .T.
*!*	          oCA.Alias = lcNameCursor
*!*	            oCA.SelectCmd = csql
*!*	            oCA.BufferModeOverride= 5
*!*	          IF !oCA.CursorFill()
*!*	                  lcMensaje= 'Línea de código con error: ' + str(cLine) + CHR(13) +;
*!*	                                 'Nombre del Programa: ' + cProg + CHR(13) +;
*!*	                                 'Mensaje de error: ' + MESSAGE( ) + CHR(13) +;
*!*	                                 'Sentencia SQL: ' + CHR(13) + csql
*!*	              MESSAGEBOX("Error al leer Excel",16,"Aviso del Sistema")
*!*	              sqlreterr=.T.
*!*	          ELSE
*!*	            SELECT &lcNameCursor.
*!*	            GO bottom
*!*	              LOCAL laFlds,lcStr,lnFldCount,i
*!*	              DIMENSION laFlds[1]
*!*	              lnFldCount=AFIELDS(laFlds)
*!*	              lcStr=""
*!*	              FOR i = 1 TO lnFldCount
*!*	                lcStr = lcStr + laFlds[m.i,1] +  ","
*!*	              ENDFOR
*!*	              oCA.UpdatableFieldList = lcStr
*!*	          ENDIF
*!*	      CATCH TO oException
*!*	            OK = .F.       
*!*	      ENDTRY
*!*	ENDIF 
*!*	IF !OK THEN
*!*	      WAIT WINDOW "No se puede cargar información del archivo "+lcArchivo NOWAIT 
*!*	ENDIF 
*!*	RETURN OK


FUNCTION MacAddress
Local pGUID,rGUID,lnSize
Declare integer CoCreateGuid in 'OLE32.dll' ;
  string @pguid
Declare integer StringFromGUID2 in 'OLE32.dll' ;
  string rguid, string @lpsz, integer cchMax
pGUID=replicate(chr(0),16)
rGUID=replicate(chr(0),80)

If "5." $ OS() && 2000/XP
  Declare integer UuidCreateSequential in 'RPCRT4.dll'  string @ Uuid
  Return substr( iif( UuidCreateSequential(@pGUID) = 0 ;
    and StringFromGUID2(pGUID,@rGUID,40) # 0, ;
    StrConv(left(rGUID,76),6), "" ), 26,12)
Else
  Return substr( iif( CoCreateGuid(@pGUID) = 0 ;
    and StringFromGUID2(pGUID,@rGUID,40) # 0, ;
    StrConv(left(rGUID,76),6), "" ), 26,12)
ENDIF


FUNCTION rbInputBox
lparameters tcPrompt, tcTitle, txDefaultValue, tnLeft, tnTop, ;
				tcFormat, tcInputMask, tcPasswordChar
private pcReturnValue
pcReturnValue = txDefaultValue
local oInputBox
oInputBox = CreateObject("rbInputBox", tcPrompt, tcTitle, ;
								 txDefaultValue, tnLeft, tnTop, ;
								 tcFormat, tcInputMask, tcPasswordChar)
oInputBox.Show()
RETURN pcReturnValue


**************************************************
*-- Class:        rbinputbox
*-- ParentClass:  form
*-- BaseClass:    form
*-- Time Stamp:   01/29/03 01:03:14 PM
*
DEFINE CLASS rbinputbox AS form


	Height = 113
	Width = 318
	DoCreate = .T.
	AutoCenter = .T.
	Caption = "Input Box"
	ControlBox = .F.
	WindowType = 1
	Name = "frmInputBox"

	*-- empty value to return if Cancel is chosen; data type depends on data type of txValueIn
	xemptyvalue = .F.

	*-- the default value (if any)
	xdefaultvalue = .F.

	*-- the return value
	xreturnvalue = .F.


	ADD OBJECT lblinputbox AS label WITH ;
		FontName = "Arial", ;
		FontSize = 9, ;
		Alignment = 1, ;
		Caption = "Enter the value", ;
		Height = 20, ;
		Left = 6, ;
		Top = 26, ;
		Width = 190, ;
		TabIndex = 1, ;
		Name = "lblInputBox"


	ADD OBJECT txtinputbox AS textbox WITH ;
		FontName = "Arial", ;
		FontSize = 9, ;
		Century = 1, ;
		Height = 24, ;
		Left = 202, ;
		SelectOnEntry = .T., ;
		TabIndex = 2, ;
		Top = 22, ;
		Width = 110, ;
		Name = "txtInputBox"


	ADD OBJECT cmdok AS commandbutton WITH ;
		Top = 72, ;
		Left = 84, ;
		Height = 24, ;
		Width = 72, ;
		Caption = "OK", ;
		Default = .T., ;
		TabIndex = 3, ;
		Name = "cmdOK"


	ADD OBJECT cmdcancel AS commandbutton WITH ;
		Top = 72, ;
		Left = 172, ;
		Height = 24, ;
		Width = 72, ;
		Cancel = .T., ;
		Caption = "Cancel", ;
		TabIndex = 4, ;
		Name = "cmdCancel"


	PROCEDURE Unload
		with thisform
			if type(".xReturnValue") = "C"
				.xReturnValue = RTRIM( .xReturnValue)
			endif
			pcReturnValue = .xReturnValue
		endwith
	ENDPROC


	PROCEDURE Init
		lparameters tcPrompt, tcTitle, txDefaultValue, tnLeft, tnTop, ;
						tcFormat, tcInputMask, tcPasswordChar
		if type("tcPrompt") <> "C"
			tcPrompt = "Enter the value"
		endif
		if type("tcTitle") <> "C"
			tcTitle = "Input Box"
		endif
		if !( type("txDefaultValue") $ "CDNY")
			*	Valid input data types are C, D, N, and Y
			txDefaultValue = ""	&& default to character data type
		endif
		if type("tcFormat") <> "C"
			tcFormat = ""
		endif
		if type("tcInputMask") <> "C"
			tcInputMask = ""
		endif
		if type("tcPasswordChar") <> "C"
			tcPasswordChar = ""
		endif
		if len( alltrim( tcPasswordChar)) > 1
			tcPasswordChar = left( tcPasswordChar, 1)
		endif
		local llAutoCenter
		if pcount() < 5	&& Top and Left parameters were not passed
			tnLeft = 0
			tnTop = 0
		else	&& Top and left parameters were passed but may not be numeric
			if type("tnTop") = "N" and type("tnLeft") = "N"		&& both are numeric
				llAutoCenter = .F.
			else	&& one or both is not numeric, so AutoCenter the form
				tnLeft = 0
				tnTop = 0
				llAutoCenter = .T.
			endif
		endif

		with thisform
			.lblInputBox.caption = ALLTRIM( tcPrompt)
			.caption = ALLTRIM( tcTitle)
			.xDefaultValue = txDefaultValue
			.xReturnValue = .xDefaultValue
			.txtInputBox.value = .xDefaultValue
			.txtInputBox.format = ALLTRIM( tcFormat)
			.txtInputBox.InputMask = ALLTRIM( tcInputMask)
			.txtInputBox.PasswordChar = tcPasswordChar
			.Top = tnTop
			.Left = tnLeft
			.AutoCenter = llAutoCenter		&& Set AutoCenter last so it overrides Top and Left if .T.

			do case
				case type("txDefaultValue") = "D"
					.xEmptyValue = {}
				case type("txDefaultValue") = "N"
					.xEmptyValue = 0
				case type("txDefaultValue") = "Y"
					.xEmptyValue = $0
				otherwise
					.xEmptyValue = ""
			endcase
		endwith
	ENDPROC


	PROCEDURE cmdok.Click
		with thisform
			.xReturnValue = .txtInputBox.value
			.release()
		endwith
	ENDPROC


	PROCEDURE cmdcancel.Click
		*
		*	If Cancel was chosen, return the empty value of the correct data type.
		*
		with thisform
			.xReturnValue = .xEmptyValue
			.release()
		endwith
	ENDPROC


ENDDEFINE
*
*-- EndDefine: rbinputbox
**************************************************

*------------------------
FUNCTION CONSOLIDA_NOTA()
*------------------------
PARAMETER wano,wsec_ejec,expe,cic,fas,secu
PRIVATE Texto
Texto=''
SELE EXPEDIENTE_NOTA
SEEK wano+wsec_ejec+expe+cic+fas+secu
SCAN WHILE ano_eje+sec_ejec+expediente+ciclo+fase+secuencia==wano+wsec_ejec+expe+cic+fas+secu
	texto=texto+expediente_nota.notas
ENDSCAN
RETURN TEXTO

**************************
FUNCTION LETREO()
**************************
PARAMETER m.monto,m.moneda
IF m.monto<.01
	RETURN ''
ENDIF
PRIVATE m.monto,m.moneda,m.centimos,m.importe,m.cientos,m.miles,m.millones,m.enletras,nlet
DECLARE nlet(42)
STORE 'UNO'           TO nlet(01)
STORE 'DOS'           TO nlet(02)
STORE 'TRES'          TO nlet(03)
STORE 'CUATRO'        TO nlet(04)
STORE 'CINCO'         TO nlet(05)
STORE 'SEIS'          TO nlet(06)
STORE 'SIETE'         TO nlet(07)
STORE 'OCHO'          TO nlet(08)
STORE 'NUEVE'         TO nlet(09)
STORE 'DIEZ'          TO nlet(10)
STORE 'ONCE'          TO nlet(11)
STORE 'DOCE'          TO nlet(12)
STORE 'TRECE'         TO nlet(13)
STORE 'CATORCE'       TO nlet(14)
STORE 'QUINCE'        TO nlet(15)
STORE 'DIEZ'          TO nlet(16)
STORE 'VEINTE'        TO nlet(17)
STORE 'TREINTA'       TO nlet(18)
STORE 'CUARENTA'      TO nlet(19)
STORE 'CINCUENTA'     TO nlet(20)
STORE 'SESENTA'       TO nlet(21)
STORE 'SETENTA'       TO nlet(22)
STORE 'OCHENTA'       TO nlet(23)
STORE 'NOVENTA'       TO nlet(24)
STORE 'DIECI'         TO nlet(25)
STORE 'VEINTI'        TO nlet(26)
STORE 'TREINTI'       TO nlet(27)
STORE 'CUARENTI'      TO nlet(28)
STORE 'CINCUENTI'     TO nlet(29)
STORE 'SESENTI'       TO nlet(30)
STORE 'SETENTI'       TO nlet(31)
STORE 'OCHENTI'       TO nlet(32)
STORE 'NOVENTI'       TO nlet(33)
STORE 'CIENTO'        TO nlet(34)
STORE 'DOSCIENTOS'    TO nlet(35)
STORE 'TRESCIENTOS'   TO nlet(36)
STORE 'CUATROCIENTOS' TO nlet(37)
STORE 'QUINIENTOS'    TO nlet(38)
STORE 'SEISCIENTOS'   TO nlet(39)
STORE 'SETECIENTOS'   TO nlet(40)
STORE 'OCHOCIENTOS'   TO nlet(41)
STORE 'NOVECIENTOS'   TO nlet(42)

m.centimos=(m.monto-INT(m.monto))*100
m.centimos=IIF(1<m.monto,'Y ','')+PADL(INT(m.centimos),2,'0')+'/100 '+IIF(m.moneda=1,'NUEVOS SOLES','DOLARES AMERICANOS')
m.importe=PADL(INT(m.monto),12,'0')
m.cientos=RIGHT(m.importe,3)
m.miles=SUBSTR(m.importe,7,3)
m.millones=SUBSTR(m.importe,4,3)
m.milesmillones=LEFT(m.importe,3)
m.enletras=''
IF m.cientos<>'000'
	m.enletras=letras(m.cientos,1)
ENDIF
IF m.miles<>'000'
	m.enletras=letras(m.miles,2)+'MIL '+m.enletras
ENDIF
IF m.millones<>'000'
	m.enletras=letras(m.millones,3)+IIF(VAL(m.millones)>1,'MILLONES ','MILLON ')+m.enletras
ENDIF
IF m.milesmillones<>'000'
	m.enletras=letras(m.milesmillones,3)+IIF(VAL(m.milesmillones)>1,'MIL ','MIL ')+m.enletras
ENDIF
RETURN m.enletras+m.centimos

FUNCTION letras
PARAMETER let,CENT
PRIVATE let,CENT,LETREO
LETREO=''
FOR i=1 TO 3
	x=IIF(i>1,VAL(SUBSTR(let,i-1,1)),0)
	Y=VAL(SUBSTR(let,i,1))
	z=IIF(i<3,VAL(SUBSTR(let,i+1,1)),0)
	DO porcientos
ENDFOR
DO CASE
	CASE 'UNO'$LETREO.AND.CENT>1
		LETREO=STRTRAN(LETREO,'UNO','UN')
	CASE 'CIENTO '=LETREO.AND.RIGHT(let,2)='00'
		LETREO='CIEN '
ENDCASE
RETURN LETREO

FUNCTION porcientos
DO CASE
	CASE i=3.AND.(Y>5.OR.(BETWEEN(Y,1,5).AND.x<>1))
		LETREO=LETREO+nlet(Y)+' '
	CASE i=3.AND.BETWEEN(Y,1,5).AND.x=1
		LETREO=LETREO+nlet(Y+10)+' '
	CASE i=2.AND.Y>0.AND.z=0
		LETREO=LETREO+nlet(Y+15)+' '
	CASE i=2.AND.(Y>0.AND.!(Y=1.AND.BETWEEN(z,1,5)))
		LETREO=LETREO+nlet(Y+24)
	CASE i=1.AND.Y>0
		LETREO=LETREO+nlet(Y+33)+' '
ENDCASE

************************************************
PROCEDURE INICIALIZA_COMPROBANTE
************************************************
PRIVATE i,j,wclasif,wmeta,wcuentad,wcuentah,wmontod,wmontoh,wsubctad,wsubctah
FOR i=1 TO 17
	j=PADL(i,2,'0')
	wclasif='clasificador'+j
	wmonto ='monto'+j
	&wclasif=''
	&wmonto =0

ENDFOR
FOR i=1 TO 20
	j=PADL(i,2,'0')
	wmeta='meta'+j
	&wmeta=''
ENDFOR

FOR i=1 TO 10
	j=PADL(i,2,'0')
	wcuentad='cuentad'+j
	wcuentah='cuentah'+j
	wsubctad='subctad'+j
	wsubctah='subctah'+j
	&wcuentad=''
	&wcuentah=''
	&wsubctad=''
	&wsubctah=''
	wmontod='monto_d'
	wmontoh='monto_h'
	&wmontod=0
	&wmontoh=0
ENDFOR
FOR I=1 TO 10
	FOR J=1 TO 14
		TEMPO_ASIENTO_DEBE=''
		TEMPO_ASIENTO_HABER=''
	ENDFOR
ENDFOR

RETURN

***********************************************
PROCEDURE CONSOLIDA_ASIENTOS
***********************************************
PRIVATE wano,wsec_ejec,expe,cic,fas,secu,cor,wtipo_ctb,wnro_asiento,wtipo_d_h,wmayor,wsubcta,monto_asiento
PRIVATE FLG, tmonto
PUBLIC c_alias
c_alias=ALIAS()
select comprobante_asiento 
set order to tag inx_asto1
GO TOP
SCAN ALL 

	nreg		 =  recno()
	wano		 =	comprobante_asiento.ano_eje
	wsec_ejec	 =	comprobante_asiento.sec_ejec
	expe		 =	comprobante_asiento.expediente
	cic			 =	comprobante_asiento.ciclo
	fas			 =	comprobante_asiento.fase
	secu		 =	comprobante_asiento.secuencia
	cor			 =	comprobante_asiento.correlativo
	wtipo_ctb	 =	comprobante_asiento.tipo_ctb
	wnro_asiento =	comprobante_asiento.nro_asiento
	wtipo_d_h	 =	comprobante_asiento.tipo_d_h
	wmayor		 =	comprobante_asiento.mayor
	wsubcta		 =	comprobante_asiento.sub_cta
	monto_asiento=	comprobante_asiento.monto
	tmonto = 0
	seek wano+wsec_ejec+expe+cic+fas+secu+cor+wtipo_ctb+wnro_asiento+wtipo_d_h+wmayor+wsubcta
	if found() then 
		Scan while ano_eje+sec_ejec+expediente+ciclo+fase+secuencia+correlativo+tipo_ctb+nro_asiento+tipo_d_h+mayor+sub_cta==wano+wsec_ejec+expe+cic+fas+secu+cor+wtipo_ctb+wnro_asiento+wtipo_d_h+wmayor+wsubcta
			tmonto	=	tmonto+comprobante_asiento.monto
			set delete off
			delete
		endscan		
		GO nreg
		recall
		replace monto with tmonto
		set delete on
	endif
ENDSCAN	
SELECT (c_alias)
RETURN  
**************************************************************************************


***********************************************
PROCEDURE LLENA_CURSOR_COMPROBANTE
***********************************************
PARAMETER wano,wsec_ejec,expe,cic,fas,secu,cor,sec_oper, wmes, wanoDoc
PUBLIC numero,totalRetencion,cta_debe,cta_haber
PUBLIC clasificador01,clasificador02,clasificador03,clasificador04,clasificador05,clasificador06,clasificador07,clasificador08,clasificador09,clasificador10
PUBLIC clasificador11,clasificador12,clasificador13,clasificador14,clasificador15,clasificador16,clasificador17,clasificador18,clasificador19,clasificador20
PUBLIC monto01,monto02,monto03,monto04,monto05,monto06,monto07,monto08,monto09,monto10,monto_doc_b
PUBLIC monto11,monto12,monto13,monto14,monto15,monto16,monto17,monto18,monto19,monto20
PUBLIC monto_debe,monto_haber,Tot_monto_clasif
PUBLIC meta01,meta02,meta03,meta04,meta05,meta06,meta07,meta08,meta09,meta10,meta11
PUBLIC c_alias
inicializa_comprobante()
c_alias=ALIAS()
wexp=expe
wcic=cic
wfas=fas
wsec=secu
wcor=cor
mto_debe =0
mto_haber=0
Totclasif=0
totalRetencion=0.0
cta_debe=''
cta_haber=''

		************* calculo de asientos presupuestales ******************************
		IF wcic+wfas='GG' AND gcano_eje<'2009' THEN 
			SELECT comprobante_asiento
			IF VAL(sec_oper)<>0 THEN 
				SCAN ALL
					IF mayor='95' .AND. tipo_d_h='D' and ano_eje+expediente+ciclo+fase+secuencia+correlativo=wano+expe+cic+fas+secu+cor
						mto_debe=mto_debe+monto
						cta_debe=comprobante_asiento.mayor+'.'+comprobante_asiento.sub_cta												
					ENDIF
					IF mayor='97' .AND. tipo_d_h='H' and ano_eje+expediente+ciclo+fase+secuencia+correlativo=wano+expe+cic+fas+secu+cor
						mto_haber=mto_haber+monto
						cta_haber=comprobante_asiento.mayor+'.'+comprobante_asiento.sub_cta							
					ENDIF
				ENDSCAN
				select comprobante
				replace comprobante.monto_haber 	with mto_haber
				replace comprobante.monto_debe  	with mto_debe
				Replace comprobante.cta_pres_debe 	with cta_debe
				Replace comprobante.cta_pres_haber 	with cta_haber
			ENDIF 
		ELSE
*!*				IF wmes >=lcMesIniProcCtb  AND wanoDoc >= lcAnoIniProcCtb 
*!*					SELECT comprobante_asiento
*!*					IF VAL(sec_oper)<>0 THEN 
*!*						SCAN ALL
*!*							IF  tipo_ctb='3' AND tipo_d_h='D' and ano_eje+expediente+ciclo+fase+secuencia+correlativo=wano+expe+cic+fas+secu+cor
*!*								mto_debe=mto_debe+monto
*!*								cta_debe=comprobante_asiento.mayor+'.'+comprobante_asiento.sub_cta
*!*							ENDIF
*!*							IF  tipo_ctb='3' AND tipo_d_h='H' and ano_eje+expediente+ciclo+fase+secuencia+correlativo=wano+expe+cic+fas+secu+cor
*!*								mto_haber=mto_haber+monto
*!*								cta_haber=comprobante_asiento.mayor+'.'+comprobante_asiento.sub_cta
*!*							ENDIF
*!*						ENDSCAN
*!*						**select comprobante
*!*						**replace comprobante.monto_haber with mto_haber
*!*						**replace comprobante.monto_debe  with mto_debe
*!*						**Replace comprobante.cta_pres_debe with cta_debe
*!*						**Replace comprobante.cta_pres_haber with cta_haber

*!*					ENDIF 
*!*				ELSE
				SELECT comprobante_asiento
				if val(sec_oper)<>0 then 
					SCAN ALL
						IF  tipo_ctb='2' AND tipo_d_h='D' and ano_eje+expediente+ciclo+fase+secuencia+correlativo=wano+expe+cic+fas+secu+cor
								mto_debe=mto_debe+monto
								IF gcano_eje<'2009' THEN 
									cta_debe=comprobante_asiento.mayor+comprobante_asiento.sub_cta
								ELSE
									cta_debe=comprobante_asiento.mayor+'.'+comprobante_asiento.sub_cta
								ENDIF 
						ENDIF
						IF  tipo_ctb='2' AND tipo_d_h='H' and ano_eje+expediente+ciclo+fase+secuencia+correlativo=wano+expe+cic+fas+secu+cor
								mto_haber=mto_haber+monto
								IF gcano_eje<'2009' THEN 
									cta_haber=comprobante_asiento.mayor+comprobante_asiento.sub_cta
								ELSE
									cta_haber=comprobante_asiento.mayor+'.'+comprobante_asiento.sub_cta
								ENDIF 
						ENDIF
					ENDSCAN
					select comprobante
					replace comprobante.monto_haber with mto_haber
					replace comprobante.monto_debe  with mto_debe
					Replace comprobante.cta_pres_debe with cta_debe
					Replace comprobante.cta_pres_haber with cta_haber
				ENDIF 
*!*				ENDIF 				
		ENDIF 
		*  insercion de los datos de los asientos en arrays **************
		SELECT comprobante_asiento
*!*			IF wmes>=lcMesIniProcCtb AND wanoDoc >= lcAnoIniProcCtb
*!*				if VAL(sec_oper)<>0 THEN
*!*					COPY TO ARRAY tempo_asiento_debe FOR tipo_ctb='3' AND tipo_d_h='D' and ano_eje+expediente+ciclo+fase+secuencia+correlativo=wano+expe+cic+fas+secu+cor
*!*					COUNT TO nreg ALL FOR tipo_ctb='3' AND tipo_d_h='D' and ano_eje+expediente+ciclo+fase+secuencia+correlativo=wano+expe+cic+fas+secu+cor
*!*					FOR i=1 TO nreg
*!*						T=PADL(i,2,'0')
*!*						wcuentad = 'comprobante.cuentad'+T
*!*						wsubctad = 'comprobante.subctad'+T
*!*						wmonto_d = 'comprobante.monto_d'+T
*!*						select comprobante
*!*						replace &wcuentad with tempo_asiento_debe(i,12)
*!*						replace &wsubctad with tempo_asiento_debe(i,12)+'.'+tempo_asiento_debe(i,13)
*!*						replace &wmonto_d with tempo_asiento_debe(i,14)
*!*					ENDFOR
*!*				endif
*!*				SELECT comprobante_asiento
*!*				if VAL(sec_oper)<>0 then 
*!*					COPY TO ARRAY tempo_asiento_haber FOR tipo_ctb='3' AND tipo_d_h='H'	and ano_eje+expediente+ciclo+fase+secuencia+correlativo=wano+expe+cic+fas+secu+cor
*!*					COUNT TO nreg ALL FOR tipo_ctb='3' AND tipo_d_h='H' and ano_eje+expediente+ciclo+fase+secuencia+correlativo=wano+expe+cic+fas+secu+cor
*!*					FOR i=1 TO nreg
*!*						T=PADL(i,2,'0')
*!*						wcuentah = 'comprobante.cuentah'+T
*!*						wsubctah = 'comprobante.subctah'+T
*!*						wmonto_h = 'comprobante.monto_h'+T
*!*						select comprobante
*!*						replace &wcuentah with tempo_asiento_haber(i,12)
*!*						replace &wsubctah with tempo_asiento_haber(i,12)+'.'+tempo_asiento_haber(i,13)
*!*						replace &wmonto_h with tempo_asiento_haber(i,14)
*!*					ENDFOR
*!*				ENDIF
*!*			ELSE 
			if val(sec_oper)<>0 then 
				COPY TO ARRAY tempo_asiento_debe FOR tipo_ctb='1' AND tipo_d_h='D' and ano_eje+expediente+ciclo+fase+secuencia+correlativo=wano+expe+cic+fas+secu+cor
				COUNT TO nreg ALL FOR tipo_ctb='1' AND tipo_d_h='D' and ano_eje+expediente+ciclo+fase+secuencia+correlativo=wano+expe+cic+fas+secu+cor
				FOR i=1 TO nreg
					T=PADL(i,2,'0')
					wcuentad = 'comprobante.cuentad'+T
					wsubctad = 'comprobante.subctad'+T
					wmonto_d = 'comprobante.monto_d'+T
					select comprobante
					replace &wcuentad with tempo_asiento_debe(i,12)
					IF gcano_eje<'2009' THEN 
						replace &wsubctad with tempo_asiento_debe(i,12)+tempo_asiento_debe(i,13)
					ELSE
						replace &wsubctad with tempo_asiento_debe(i,12)+'.'+tempo_asiento_debe(i,13)
					ENDIF 
					replace &wmonto_d with tempo_asiento_debe(i,14)
				ENDFOR
			endif
			SELECT comprobante_asiento
			if val(sec_oper)<>0 then 
				COPY TO ARRAY tempo_asiento_haber FOR tipo_ctb='1' AND tipo_d_h='H'	and ano_eje+expediente+ciclo+fase+secuencia+correlativo=wano+expe+cic+fas+secu+cor
				COUNT TO nreg ALL FOR tipo_ctb='1' AND tipo_d_h='H' and ano_eje+expediente+ciclo+fase+secuencia+correlativo=wano+expe+cic+fas+secu+cor
				FOR i=1 TO nreg
					T=PADL(i,2,'0')
					wcuentah = 'comprobante.cuentah'+T
					wsubctah = 'comprobante.subctah'+T
					wmonto_h = 'comprobante.monto_h'+T
					select comprobante
					replace &wcuentah with tempo_asiento_haber(i,12)
					IF gcano_eje<'2009' THEN 
						replace &wsubctah with tempo_asiento_haber(i,12)+tempo_asiento_haber(i,13)
					ELSE
						replace &wsubctah with tempo_asiento_haber(i,12)+'.'+tempo_asiento_haber(i,13)
					ENDIF 
					replace &wmonto_h with tempo_asiento_haber(i,14)

				ENDFOR
			endif
*!*			ENDIF 		
		* insercion de datos de los clasificadores en un arreglo *********
		SELECT comprobante_clasif
		COUNT TO nreg FOR ano_eje+sec_ejec+expediente+ciclo+fase+secuencia+correlativo=wano+wsec_ejec+expe+cic+fas+secu+cor
		COPY TO ARRAY tempo_clasif FOR ano_eje+expediente+ciclo+fase+secuencia+correlativo=wano+expe+cic+fas+secu+cor
		IF NREG>17 THEN 
			NREG=17
		ENDIF
		FOR i=1 TO nreg

				T=PADL(i,2,'0')
				sele comprobante
				*-				
				wclasif='comprobante.clasificador'+T
				wmonto ='comprobante.monto'+T
				IF gcano_eje<'2009' THEN 
					replace &wclasif with tempo_clasif(i,08)
				ELSE 
					replace &wclasif with SUBSTR(tempo_clasif(i,08),1,1)+'.'+SUBSTR(tempo_clasif(i,08),2,1)+'.'+SUBSTR(tempo_clasif(i,08),3,2)+' '+SUBSTR(tempo_clasif(i,08),5,2)+'.'+SUBSTR(tempo_clasif(i,08),7,2)+' '+SUBSTR(tempo_clasif(i,08),9,2)
				ENDIF 
				replace &wmonto  with tempo_clasif(i,10)
		ENDFOR
		SELECT comprobante_clasif
			SCAN ALL
				IF ano_eje+expediente+ciclo+fase+secuencia+correlativo = wano+expe+cic+fas+secu+cor THEN
					Totclasif=Totclasif+monto_nacional
				ENDIF
				
			ENDSCAN
		SELECT comprobante
		REPLACE comprobante.Tot_monto_clasif  with Totclasif 
		*  insercion de datos de las metas en un arreglo ************
		SELECT comprobante_meta
		COUNT TO nreg FOR ano_eje+expediente+ciclo+fase+secuencia+correlativo=wano+expe+cic+fas+secu+cor
		COPY TO ARRAY tempo_meta FOR ano_eje+expediente+ciclo+fase+secuencia+correlativo=wano+expe+cic+fas+secu+cor
		IF NREG>20 THEN 
			NREG=20
		ENDIF
		FOR i=1 TO nreg

			T=PADL(i,2,'0')
			wmeta='comprobante.meta'+T
		    selec comprobante
		    DO case
		    	CASE gcano_eje<='2008'
		    		meta=tempo_meta(i,9)+'    '+tempo_meta(i,10)+'   '+tempo_meta(i,11)+' . '+tempo_meta(i,12)+' . '+tempo_meta(i,13)+' . '+tempo_meta(i,14)+' . '+tempo_meta(i,15)+' . '+tempo_meta(i,16)+' '+tempo_meta(i,17)+' '+str(tempo_meta(i,18),19,2)
		   		CASE gcano_eje>='2009' AND gcano_eje<='2011'
		   			meta=tempo_meta(i,9)+'        '+tempo_meta(i,10)+'   '+tempo_meta(i,11)+' . '+tempo_meta(i,12)+' . '+tempo_meta(i,13)+' . '+tempo_meta(i,14)+' . '+tempo_meta(i,15)+' . '+tempo_meta(i,16)+'        '+tempo_meta(i,17)+'  '+tempo_meta(i,18)+' '+str(tempo_meta(i,19),19,2)			    
		   		CASE gcano_eje>='2012'
		   			meta=tempo_meta(i,9)+'   '+tempo_meta(i,10)+'      '+tempo_meta(i,11)+' . '+tempo_meta(i,12)+' . '+tempo_meta(i,13)+' . '+tempo_meta(i,14)+' . '+tempo_meta(i,15)+' . '+tempo_meta(i,16)+' '+tempo_meta(i,17)+'      '+tempo_meta(i,18)+' '+tempo_meta(i,19) +' '+str(tempo_meta(i,20),19,2)			    
		   	ENDCASE 
		    replace &wmeta with meta
		ENDFOR
		**************************************************************************
			
		* Insercion de las retenciones en un arreglo
		SELECT  Comprobante_Retencion
		COUNT TO nreg FOR ano_eje+expediente+ciclo+fase+secuencia+correlativo=wano+expe+cic+fas+secu+cor		
		COPY TO ARRAY Tempo_Retenciones FOR ano_eje+expediente+ciclo+fase+secuencia+correlativo=wano+expe+cic+fas+secu+cor
		IF NREG>19 THEN 
			NREG=19
		ENDIF
		FOR i=1 TO nreg
			if i<36 then 
				T=PADL(i,2,'0')
				wretencion='comprobante.retencion'+T
				wmonto_ret='comprobante.monto_ret'+T
			    selec comprobante
			    retencion=tempo_retenciones(i,10)+tempo_retenciones(i,11)
			    REPLACE &wretencion with LEFT(retencion,4)+'.'+SUBSTR(retencion,5)
			    REPLACE &wmonto_ret with tempo_retenciones(i,12)
			endif			    
		ENDFOR
		SELECT comprobante_retencion
			=Seek(wano+gcsec_ejec+expe+cic+fas+secu+cor,'Comprobante_Retencion','comp_ret')
			SCAN While ano_eje+sec_ejec+expediente+ciclo+fase+secuencia+correlativo = wano+gcsec_ejec+expe+cic+fas+secu+cor
					TotalRetencion=TotalRetencion+monto_nacional
			ENDSCAN
		SELECT comprobante
		REPLACE comprobante.TotRetencion  with TotalRetencion

		*
		SELE (c_alias)
RETURN


*--------------------
FUNCTION SoloNumero()
*--------------------
* Retorna .t. - Verdad si el parametro es solo número
* Retorna .f. - Falso si el parametro tiene una letra o caracter especial
*********************************************************************************
PARAMETERS documento
indicador=.T.
FOR NN = 1 TO LEN(ALLTRIM(documento))
	IF  ASC(SUBSTR(ALLTRIM(documento), NN, 01)) < 48 OR ASC(SUBSTR(ALLTRIM(documento), NN, 01)) > 57 THEN
		indicador = .F.
		EXIT
	ENDIF
ENDFOR
RETURN(indicador)


function VFP2Excel
	lparameters tcDataSource, tcSQL, toRange
	Local loConn As AdoDB.Connection, ;
		loRS As AdoDB.Recordset,;
		ix
	loConn = Createobject("Adodb.connection")
	loConn.ConnectionString = "Provider=VFPOLEDB;Data Source="+m.tcDataSource
	loConn.Open()
	loRS = loConn.Execute(m.tcSQL)

	FOR ix=1 TO loRS.Fields.Count
		toRange.Offset(0,m.ix-1).Value = PROPER(loRs.Fields(m.ix-1).Name)
		toRange.Offset(0,m.ix-1).Font.Bold = .t.
	ENDFOR
	toRange.Offset(1,0).CopyFromRecordSet( loRS )
	loRs.Close
	loConn.Close
RETURN 


*----------------------
FUNCTION Devuelve_fecha
*----------------------
PARAMETERS X_MES, X_FLAG
DO CASE
	CASE GCANO_EJE+X_MES==PADL(YEAR(DATE()),4,'0')+PADL(MONTH(DATE()),2,'0')
		IF X_MES<>'01' THEN
			A=DATE()											&& FECHA ACTUAL
			B=MONTH(A)
			C=YEAR(A)
			D=CTOD('01/'+PADL(B,2,'0')+'/'+PADL(C,4,'0')) - 1   && FECHA ANTERIOR
		ENDIF
		IF X_MES=='01' THEN
			A=DATE()											&& FECHA ACTUAL
			C=YEAR(A)-1
			D=CTOD('31/12/'+PADL(C,4,'0'))						&& FECHA ANTERIOR
		ENDIF
	CASE GCANO_EJE+X_MES <> PADL(YEAR(DATE()),4,'0')+PADL(MONTH(DATE()),2,'0')
		IF !INLIST(X_MES,'01','12') THEN
			B=INT(VAL(X_MES))
			C=INT(VAL(GCANO_EJE))
			D=CTOD('01/'+PADL(B,2,'0')+'/'+PADL(C,4,'0')) - 1	&& FECHA ANTERIOR
			A=CTOD('01/'+PADL(B+1,2,'0')+'/'+PADL(C,4,'0')) - 1	&& FECHA ACTUAL
		ENDIF
		IF X_MES=='01' THEN
			B=INT(VAL(X_MES))
			C=INT(VAL(GCANO_EJE))
			D=CTOD('31/12/'+PADL(C-1,4,'0'))					&& FECHA ANTERIOR
			A=CTOD('31/01/'+PADL(C,4,'0'))						&& FECHA ACTUAL
		ENDIF
		IF X_MES=='12' THEN
			B=INT(VAL(X_MES))
			C=INT(VAL(GCANO_EJE))
			D=CTOD('01/'+PADL(B,2,'0')+'/'+PADL(C,4,'0')) - 1	&& FECHA ANTERIOR
			A=CTOD('31/12/'+PADL(C,4,'0'))						&& FECHA ACTUAL
		ENDIF
ENDCASE
IF X_FLAG = 'A' THEN
	RETURN A
ELSE
	RETURN D
ENDIF


*--------------------------
FUNCTION Devuelve_fecha_rpt
*--------------------------
LPARAMETERS LPano, LPmes, LPmescierre, LPforma
LOCAL X_DIA1, X_DIA2, X_DIA_BIC, X_MES, X_ANO
IF !USED('ejecutora_rpt')
	USE DATA\ejecutora IN 0 SHARED ALIAS ejecutora_rpt AGAIN ORDER TAG sec_ejec
ENDIF
IF VAL(LPano) = VAL(gcano_eje)-2
	LPmescierre = '12'
	LPmes = '13'
	IF SEEK(LPano+gcsec_ejec,'ejecutora_rpt','sec_ejec')
		LPmescierre = IIF(EMPTY(ejecutora_rpt.mes_cierre),'12',ejecutora_rpt.mes_cierre)
	ENDIF
ENDIF
IF VAL(LPano) = VAL(gcano_eje)-1
	LPmescierre = '12'
	LPmes = '13'
	IF SEEK(LPano+gcsec_ejec,'ejecutora_rpt','sec_ejec')
		LPmescierre = IIF(EMPTY(ejecutora_rpt.mes_cierre),'12',ejecutora_rpt.mes_cierre)
	ENDIF
ENDIF
=SEEK(gcano_eje+gcsec_ejec,'ejecutora_rpt','sec_ejec')
X_ANO=IIF(EMPTY(LPano),GCano_eje,LPano)
X_MES=IIF(EMPTY(LPmes),'12',LPmescierre)
FOR X=1980 TO VAL(X_ANO) STEP 4
	X_DIA_BIC = IIF(VAL(X_ANO)=X,'29','28')
ENDFOR
DO CASE
	CASE X_MES $ '01/03/05/07/08/10/12'
		X_DIA1 = '31'
		IF X_MES='01'
			X_DIA2 = X_DIA_BIC
		ELSE
			X_DIA2 = IIF(X_MES $ '07/12','31','30')
		ENDIF
	CASE X_MES $ '04/06/09/11'
		X_DIA1 = '30'
		X_DIA2 = '31'
	OTHER
		X_DIA1 = X_DIA_BIC
		X_DIA2 = '31'
ENDCASE
lddiaini=CTOD(X_DIA1 +'/'+ X_MES +'/'+ X_ANO)
lddiafin=ctod(X_DIA1 +'/'+ X_MES +'/'+ X_ANO)
CIERRE = 'al ' + X_DIA1 + ' de ' + CMONTH(GOMONTH(lddiaini,0)) + ' de ' + PADL(YEAR(GOMONTH(lddiafin,0)),4,'0')
LIQUID = 'al ' + X_DIA2 + ' de ' + CMONTH(GOMONTH(lddiaini,1)) + ' de ' + PADL(YEAR(GOMONTH(lddiafin,1)),4,'0')
IF LPforma = '1'
	CIERRE = 'al ' + X_DIA1 + '.' + CMONTH(GOMONTH(lddiaini,0)) + '.' + PADL(YEAR(GOMONTH(lddiaini,0)),4,'0')
	LIQUID = 'al ' + X_DIA2 + '.' + CMONTH(GOMONTH(lddiafin,1)) + '.' + PADL(YEAR(GOMONTH(lddiafin,1)),4,'0')
ENDIF
IF LPforma = '2'
	X_ANO_ANT=IIF(EMPTY(LPano),PADL(INT(VAL(GCano_eje)-1),4,'0'),PADL(INT(VAL(LPano)-1),4,'0'))
	CIERRE = 'al ' + X_DIA1 + ' de ' + CMONTH(GOMONTH(lddiaini,0)) + ' de ' + X_ANO_ANT + ' y ' + PADL(YEAR(GOMONTH(lddiaini,0)),4,'0')
	LIQUID = 'al ' + X_DIA2 + ' de ' + CMONTH(GOMONTH(lddiafin,1)) + ' de ' + X_ANO_ANT + ' y ' + PADL(YEAR(GOMONTH(lddiafin,1)),4,'0')
ENDIF
RETURN IIF(LPmes='14',LIQUID,CIERRE)






ENDPROC 


FUNCTION BuscaInfo
LPARAMETER lcItem
#DEFINE ERROR_SUCCESS 0
#DEFINE ERROR_NOT_SUPPORTED 50
#DEFINE ERROR_INVALID_PARAMETER 87
#DEFINE ERROR_BUFFER_OVERFLOW 111
#DEFINE ERROR_NO_DATA 232
DECLARE INTEGER GetAdaptersInfo IN iphlpapi;
      STRING @pAdapterInfo, LONG @pOutBufLen
LOCAL lcBuffer, lnBufsize
lnBufsize = 0
lcBuffer = ""
* this call usually returns the ERROR_BUFFER_OVERFLOW
* with lnBufsize set to the required amount of memory
= GetAdaptersInfo(@lcBuffer, @lnBufsize)
lcBuffer = Repli(Chr(0), lnBufsize)
IF GetAdaptersInfo(@lcBuffer, @lnBufsize) <> ERROR_SUCCESS
      * still something is wrong
      RETURN ""
ENDIF
#DEFINE MAX_ADAPTER_NAME_LENGTH 256
#DEFINE MAX_ADAPTER_DESCRIPTION_LENGTH 128
#DEFINE MAX_ADAPTER_ADDRESS_LENGTH 8

DO CASE
      CASE lcItem='IP'
            RETURN STRTRAN(SUBSTR(lcBuffer, 433,15), Chr(0),"")
      CASE lcItem='MAC'
            LOCAL lnAddrlen, lcAddress, ii, ch, lcMacAddr
            lnAddrlen = Asc(SUBSTR(lcBuffer, 401, 1))
            lcAddress = SUBSTR(lcBuffer, 405, lnAddrlen)
            lcMacAddr = ''
            FOR ii=1 TO lnAddrlen
                  lcMacAddr = lcMacAddr + PadL(Int2Hex(Asc(SUBSTR(lcAddress, ii,1))),2,'0')+ ":"
            ENDFOR
            RETURN SUBSTR(lcMacAddr, 1, LEN(ALLTRIM(lcMacAddr))-1)
ENDCASE


PROCEDURE int2hex
PARAMETER zndecimal
*: Maxmimum value at lnfactor of 10: 1,099,511,284,178 ==> FFFFFAC1D2
PRIVATE lnvalue, lchexval, lnfactor, lncol
m.lnvalue = m.zndecimal
m.lchexval = ""
FOR m.lnfactor = 10 TO 1 STEP -1
      m.lncol = INT(m.lnvalue / (16^(m.lnfactor - 1)))
      IF BETWEEN(m.lncol, 1, 15)
            m.lchexval = m.lchexval + SUBSTR("123456789ABCDEF", m.lncol, 1)
            m.lnvalue = m.lnvalue - (16^(m.lnfactor - 1) * m.lncol)
      ELSE
            IF !EMPTY(m.lchexval)
                  m.lchexval = m.lchexval + "0"
            ENDIF
      ENDIF

NEXT m.lnfactor
IF m.lnvalue > 0
      m.lchexval = m.lchexval + STR(m.lnvalue, 1)
ENDIF


function xls_ncol_let
LPARAMETERS nNumero
name=''
d= nNumero
DO WHILE d>0
  m = MOD((d - 1),26)
                name = Chr(65 + m) + name
               d = Int((d - m) / 26)
    ENDDO
RETURN name
ENDFUNC



PROCEDURE generar_tabla_dinamica
PARAMETERS pTabla, pNombreArchivo, pGenTableDin
PRIVATE lnFilas, lnColumnas, lnFiltros, laSubTotal(12)
 
	CREATE CURSOR cur_fila (id_campo			c(3),;
							campo				c(30),;
							orden				c(3))


	SELECT cur_fila 
	INDEX on campo TAG campo 
	INDEX on id_campo TAG id_campo ADDITIVE 
	INDEX on orden TAG orden ADDITIVE 


	CREATE CURSOR cur_columna (	id_campo			c(3),;
								campo				c(30),;
								orden				c(3))


	SELECT cur_columna
	INDEX on campo TAG campo 
	INDEX on id_campo TAG id_campo ADDITIVE 
	INDEX on orden TAG orden ADDITIVE 


	STORE 0 TO lnFilas , lnColumnas
	gnFieldcount = AFIELDS(gaMyArray,pTabla)  && Create array.
	FOR nCount = 1 TO gnFieldcount 
		lcTipoCampo = gaMyArray(nCount,2)
		SCATTER MEMVAR 
		m.id_campo = PADL(ncount,3,'0')
		m.campo = gaMyArray(nCount,1)
		m.orden = m.id_campo
		
		DO CASE
			CASE INLIST(lcTipoCampo,'C','D') 
				lnFilas = lnFilas + 1
				INSERT INTO CUR_FILA FROM MEMVAR 
			CASE lcTipoCampo = 'N'
				lnColumnas = lnColumnas + 1
				INSERT INTO CUR_COLUMNA FROM MEMVAR 
		
		ENDCASE 

	ENDFOR


	SELECT &pTabla
	GO TOP
	
	loExcel = CREATEOBJECT('Excel.Application')
	loLibro = loExcel.Workbooks.Add()

	lcSalida = pNombreArchivo
	SELECT * FROM &pTabla INTO CURSOR curxx READWRITE 
	XFILE = LCSALIDA
	XCAD=DTOC(DATE())
	XFILE=XFILE+'_'+SUBST(XCAD,1,2)+'_'+SUBST(XCAD,4,2)+'_'+SUBST(XCAD,7)
	XCAD=TIME()
	XFILE=XFILE+'_'+SUBST(XCAD,1,2)+'_'+SUBST(XCAD,4,2)+'_'+SUBSTR(XCAD,7,2)
	XFILE = GCRUTA_APP + GCCARPETALISTADO+'\'+XFILE
	XFILE=XFILE+'.XLSX'
	COPYTOEXCEL_OK(XFILE,  "CURXX")
	llOk = .F.
	lnReg = RECCOUNT()
	IF lnFilas > 0 AND lnColumnas > 0 AND lnReg > 0 AND pGenTableDin THEN 
		nAnswer = MESSAGEBOX('Generar Tabla Dinámica?', 4 + 16 + 256, 'SIGPRES')
		DO CASE

		   CASE nAnswer = 6
				llOk = .T.
		     

		   CASE nAnswer = 7

		      	llOk = .F.

		ENDCASE	
	ENDIF
	
	IF llOk THEN 
		
		gnFieldcount = AFIELDS(gaMyArray,'curXX')
		lcRango = "A1:"+xls_ncol_let(gnFieldcount)+ALLTRIM(STR(lnReg+1)) 
		lcArchivo = xfile 


		LOCAL laPagina(1), laFilas(lnFilas), laColumnas(lnColumnas), laSubTotal(12)

		i = 0
		SELECT cur_fila
		SCAN ALL 
			SCATTER MEMVAR 
			i = i + 1
			laFilas(i) = ALLTRIM(m.campo)
		ENDSCAN 

		i = 0
		SELECT cur_columna
		SCAN ALL 
			SCATTER MEMVAR 
			i = i + 1
			laColumnas(i) = ALLTRIM(m.campo)
		ENDSCAN 


		FOR j= 1 TO 12
			laSubTotal(j) = .F.
		ENDFOR 	  
		  
		 WITH loExcel.APPLICATION
		  .VISIBLE = .F. && oculto el trabajo en la aplicacion Excel
		  .workbooks.OPEN(lcArchivo)

		  *--- Formato datos numéricos
		  .Cells.SELECT
		  .SELECTION.COLUMNS.AutoFit
		  .RANGE("A1").SELECT

		  *=== Tabla dinámica ===
		  *--- Llamo al generador de Tablas Dinámicas
		  .ActiveSheet.PivotTableWizard(1,lcRango,"","MiTablaDinamica")
		
		  *--- Armo la Tabla
		  FOR i = 1 TO lnFilas
			  .ActiveSheet.PivotTables("MiTablaDinamica").PivotFields(laFilas(i)).ORIENTATION = 1    &&xlRowField  
				WITH .ActiveSheet.PivotTables("MiTablaDinamica").PivotFields(laFilas(i))
		            .Subtotals(1)  = .F.
		            .Subtotals(2)  = .F.
		            .Subtotals(3)  = .F.
		            .Subtotals(4)  = .F.
		            .Subtotals(5)  = .F.
		            .Subtotals(6)  = .F.
		            .Subtotals(7)  = .F.
		            .Subtotals(8)  = .F.
		            .Subtotals(9)  = .F.
		            .Subtotals(10) = .F.
		            .Subtotals(11) = .F.
		            .Subtotals(12) = .F.
			            	            	            	            	            	            	            	            	            	            	            	            
				 ENDWITH
			  
		  ENDFOR 
		  #define xlSum -4157
		  FOR i = 1 TO lnColumnas
			   lc = [.ActiveSheet.PivotTables("MiTablaDinamica").AddDataField(.ActiveSheet.PivotTables("MiTablaDinamica").PivotFields("]+laColumnas(i)+["), "Suma de ]+laColumnas(i)+[", -4157)]    &&xlSum  -4157
			   &lc
		  ENDFOR 
		   IF lnFilas > 1 THEN 
		     With .ActiveSheet.PivotTables("MiTablaDinamica").DataPivotField
		        .Orientation = 1 &&xlRowField
		        .Position = 3
		    ENDWITH
		   ENDIF 
		    With .ActiveSheet.PivotTables("MiTablaDinamica").DataPivotField
		        .Orientation = 2 &&xlColumnField
		        .Position = 1
		    ENDWITH 


		  *--- Selecciono toda la hoja y ajusto columnas
		  .Cells.SELECT
		  .SELECTION.COLUMNS.AutoFit
		  TRY 
		  .Sheets("Hoja1").NAME = "Datos"
		  CATCH
		  	.Sheets("Sheet1").NAME = "Datos"
		  ENDTRY 
		  *--- Selecciono la celda donde queda el cursor
		  .RANGE("A3").SELECT

		  *--- Grabo planilla y cierro
		  .VISIBLE = .F.
		  .ActiveWorkbook.SAVE
		  .workbooks.CLOSE

		 ENDWITH
	ENDIF 

	RELE loExcel

	USE IN cur_columna
	USE IN cur_fila
	WAIT WINDOW 'Se ha generado el archivo: '+xfile NOWAIT TIMEOUT 2		
ENDPROC 
