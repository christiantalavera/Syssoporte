*SET PROCEDURE TO prgs\rutinas ADDITIVE 
PUBLIC gctable

gcArchivo = 'listado_new.xls'
=ExcelToCursor_NEW(gcArchivo,'HOJA1', "curTmp")
*ExcelToCursor(gcArchivo, "curTmp")
USE h:\certificados\sigpres_new\data\comprobante_pago IN 0 exclusive
*ZAP IN comprobante_pago 
gcano_eje='2014'
gcsec_ejec = '000003'
SELECT LEFT(ano_comprobante,4) ano_comprobante,;
	   LEFT(nro_comprobante,10) nro_comprobante,;
	   LEFT(ano_eje,4) ano_eje,;
	   LEFT(sec_ejec,6) sec_ejec,;
	   left(expediente,10) expediente,;
	   LEFT(ciclo,1) ciclo,;
	   LEFT(fase,1) fase,;
	   LEFT(secuencia,4) secuencia,;
	   LEFT(secuencia_anterior,4) secuencia_anterior,;
	   LEFT(correlativo,4) correlativo,;
	   fecha_comprobante,;
	   LEFT(descripcion_pago,150) descripcion_pago,;
	   LEFT(cod_doc,3) cod_doc,;
	   LEFT(num_doc,3) num_doc,;
	   monto_importe,;
	   monto_retencion,;
	   monto_neto,;
	   LEFT(cod_doc_compromiso,3) cod_doc_compromiso,;
	   LEFT(num_doc_compromiso,30) num_doc_compromiso,;
	   LEFT(nro_carta_orden,20) nro_carta_orden,;
	   LEFT(dni,8) dni,;
	   LEFT(ano_cta_cte,4) ano_cta_cte,;
	   LEFT(banco,3) banco,;
	   LEFT(cta_cte,3) cta_cte,;
	   LEFT(tipo_pago,1) tipo_pago,;
	   LEFT(tipo_operacion,2) tipo_operacion,;
	   IIF(ISNULL(nro_resolucion),SPACE(30),LEFT(nro_resolucion,30)) nro_resolucion,;
	   LEFT(cod_doc_compromiso_descrip,100) cod_doc_compromiso_descrip,;
	   LEFT(firma_1,150) firma_1,;
	   LEFT(firma_2,150) firma_2,;
	   LEFT(cargo_1,150) cargo_1,;
	   LEFT(cargo_2,150) cargo_1;	   
FROM curTmp INTO CURSOR curgen
	   
	   
SELECT curgen
SCAN ALL 
	SCATTER MEMVAR 
	INSERT INTO comprobante_pago FROM memvar

ENDSCAN 


USE IN comprobante_pago

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

