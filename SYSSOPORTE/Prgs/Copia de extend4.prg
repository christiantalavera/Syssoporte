**** AUTOR: MARIO HOLGADO
**** LIBRERIA 

********************
PROCEDURE DeclaraTodo
********************
PUBLIC gcAno_Eje,gcSec_Ejec, gcOldCaption,gcOldClassLib,gcOldDir,gcOldEscape,;
gcOldPath,gcOldTalk,gcMonto,gcFileIni  ,gcSession 
#include ..\include\CERT.h

*-- Instrucciones DECLARE DLL para leer/escribir en archivos INI privados
DECLARE INTEGER GetPrivateProfileString IN Win32API  AS GetPrivStr ;
	String cSection, String cKey, String cDefault, String @cBuffer, ;
	Integer nBufferSize, String cINIFile

DECLARE INTEGER WritePrivateProfileString IN Win32API AS WritePrivStr ;
	String cSection, String cKey, String cValue, String cINIFile
	
gcOldCaption=""
gcOldDir=CURDIR()
gcOldPath=fullpath(CURDIR())
gcOldEscape="ON"
gcOldTalk="ON"
gcFileIni = 'cert.ini'
gcSession = "PARAMETROS DEL CERT"

gcAno_Eje = '2011'
*gcSec_Ejec ='000770'
gcSec_Ejec ='001311'
gcMonto = ''

*** SETEANDO DATOS DE LA PANTALLA
_screen.Icon 		= 'Cert.ICO'
_screen.Picture 	= 'bghome.gif'
_screen.Caption 	= 'Cert - Administrador de Certificados' 
_screen.BorderStyle = 3

**2
_screen.windowstate = 2
_screen.Closable = .F.
_SCREEN.CONTROLBOX=.T.
_screen.AlwaysOnTop = .F. 

**DO SYSTEM.APP

ENDPROC

*/ ********************************************************
*/ Chequeo inicial si ya se configuro la B.D.
*/ ------------------------------------------
Function CheckInitial
Private cIniciado, lOk, cTipoEnti, cCodiEnti, cUniEjec, cRuta_Siaf,cSec_Ejec, ;
		cValor, lSeguir, cInstalac, cTypeInst

If !File( gcFileIni ) then
	=MessageBox( "SE CREARA ARCHIVO DE INICIO " + gcFileIni , 048, "Aviso" )
	=SaveFileIni( gcFileIni , gcSession, "", "", "" )
EndIf
cIniciado = REPLICATE( Chr(0), 8 )
=GetPrivStr( gcSession, "Iniciado", "", @cIniciado, 9, gcFileIni )
cIniciado = QuitaChrCero( cIniciado )
o=CREATEOBJECT('blowfish.blowfish')
If cIniciado <> "S" Then
    cSec_Ejec = oAppEnv.Sec_Ejec
    cRuta_Siaf= oAppEnv.Ruta_Siaf
	=WritePrivStr( gSession, "Iniciado", "S", gcFileIni )
	=WritePrivStr( gSession, "Version", "BETA", gcFileIni )		
	=WritePrivStr( gSession, "RutaSiaf", cRuta_Siaf, gcFileIni )	
	=WritePrivStr( gSession, "SecEjec", cSec_Ejec , gcFileIni )		
ELSE
	IF File( gcFileIni ) Then
		cTipoBD = REPLICATE( Chr(0), 15 )
		cVersion = REPLICATE( Chr(0), 8 )
		cRuta_Siaf = REPLICATE( Chr(0), 100 )
		cSec_Ejec  = REPLICATE( Chr(0), 6 )
		
			
		=GetPrivStr( gSession, "Version", "", @cVersion, 8, gcFileIni )
		=GetPrivStr( gSession, "RutaSiaf", "", @cRuta_Siaf , 15, gcFileIni )
		=GetPrivStr( gSession, "SecEjec", "", @cSec_Ejec, 10, gcFileIni )
		
		
		cVersion    = QuitaChrCero( cVersion    )
		cRuta_Siaf = QuitaChrCero( cRuta_Siaf )
		cSec_Ejec= QuitaChrCero( cSec_Ejec)
		
		
		o.CodificarBlowfish(ALLTRIM(QuitaChrCero( cBD_ACCESS  )),'MFJEKIPU666') 
		
		oAppEnv.Sec_Ejec= ALLTRIM(cSec_Ejec)
		oAppEnv.Ruta_Siaf= ALLTRIM(cRuta_Siaf )
			
	ENDIF 
EndIf

Return .T.


*/ ************************************************************************
*/ =SaveFileIni( "c:\windows\system\sagu.ini", "SAGU", "2", "0001", ;
*/		"c:\sistemas\vfp\sagu\SAGU.MDB )
*/ ------------------------------------------------------------------
Function SaveFileIni
Parameter cFileIni, cSess, cTipEnt, cCodEnt, cNameFileMDB
=WritePrivStr( cSess, "Iniciado", "N", cFileIni )
RETURN .T.



*/ -------------------------------------------------
Function QuitaChrCero
Parameter cCadena
Private nI, cCadReturn
cCadReturn = ""
For nI = 1 to Len( cCadena )
	If Substr( cCadena, nI, 1 ) = Chr(0) Then
		cCadReturn = Substr( cCadena, 1, nI - 1 ) 
		Return Alltrim( cCadReturn )
		nI = Len( cCadena ) + 1
	EndIf
Next nI
Return ALLT( IIf( Empty( cCadReturn ), cCadena, cCadReturn ) )

PROCEDURE CloseTablesRpts

IF USED("RCERTIFICADO_CLASIF")
	SELECT RCERTIFICADO_CLASIF
	USE 
ENDIF

IF USED("RCERTIFICADO_META")
	SELECT RCERTIFICADO_META
	USE 
ENDIF

IF USED("RMAESTRO_DOCUMENTO")
	SELECT RMAESTRO_DOCUMENTO
	USE 
ENDIF

IF USED("RMAESTRO_CLASIFICADOR")
	SELECT RMAESTRO_CLASIFICADOR
	USE 
ENDIF

IF USED("RFUENTE_FINANC")
	SELECT RFUENTE_FINANC
	USE 
ENDIF

IF USED("RMETA")
	SELECT RMETA
	USE 
ENDIF

IF USED("RMAESTRO")
	SELECT RMAESTRO 
	USE 
ENDIF
IF USED("pliego")
	SELECT pliego 
	USE 
ENDIF
IF USED("ejecutora")
	SELECT ejecutora
	USE 
ENDIF
IF USED("sector")
	SELECT sector
	USE 
ENDIF

RETURN

*** REPORTE DE CERTIFICADOS
PROCEDURE RptCertificado
=CloseTablesRpts()

USE ejecutora 					IN 0 SHARED ALIAS ejecutora 		AGAIN ORDER TAG sec_ejec
USE sector 						IN 0 SHARED ALIAS sector 			AGAIN ORDER TAG sector
USE pliego 						IN 0 SHARED ALIAS pliego 			AGAIN ORDER TAG pliego

*!*	USE  CERTIFICADO			AGAIN ORDER TAG CERTI   	 && ANO_EJE+SEC_EJEC+CERTIFICADO
USE  CERTIFICADO_FASE	  		IN 0 SHARED ALIAS RCERTIFICADO_FASE			AGAIN ORDER TAG CERTI_FASE   && ANO_EJE+SEC_EJEC+CERTIFICADO+SECUENCIA 
USE  CERTIFICADO_SECUENCIA 		IN 0 SHARED ALIAS RCERTIFICADO_SECUENCIA	AGAIN ORDER TAG CERTI_SEC    && ANO_EJE+SEC_EJEC+CERTIFICADO+SECUENCIA+CORRELATIVO
USE  CERTIFICADO_CLASIF  		IN 0 SHARED ALIAS RCERTIFICADO_CLASIF		AGAIN ORDER TAG CERTI_CLAS   && ANO_EJE+SEC_EJEC+CERTIFICADO+SECUENCIA+CORRELATIVO+ID_CLASIFICADOR
USE  CERTIFICADO_META	  		IN 0 SHARED ALIAS RCERTIFICADO_META			AGAIN ORDER TAG CERTI_META   && ANO_EJE+SEC_EJEC+CERTIFICADO+SECUENCIA+CORRELATIVO+ID_C
USE  MAESTRO_DOCUMENTO	  		IN 0 SHARED ALIAS RMAESTRO_DOCUMENTO		AGAIN ORDER TAG COD_DOC   	 && ANO_EJE+SEC_EJEC+CERTIFICADO+SECUENCIA+CORRELATIVO+ID_C
USE  MAESTRO_CLASIFICADOR  		IN 0 SHARED ALIAS RMAESTRO_CLASIFICADOR		AGAIN ORDER TAG IDCLASIF   	
USE  FUENTE_FINANC				IN 0 SHARED ALIAS RFUENTE_FINANC			AGAIN ORDER TAG FUENTE_FIN   	&& ANO_EJE+ORIGEN+FUENTE_FINANC
USE  META	  					IN 0 SHARED ALIAS RMETA						AGAIN ORDER TAG META   			&& ANO_EJE+SEC_EJEC+SEC_FUNC
USE  CERT!MAESTRO_DETALLE_CERT  IN 0 SHARED ALIAS RMAESTRO 					AGAIN ORDER TAG MAESTRO   		&& TIPO+CODIGO


=SEEK(gcano_eje+gcsec_ejec,'ejecutora')
&&
SELECT ejecutora
SET RELATION TO ano_eje+sector INTO sector
SET RELATION TO ano_eje+sector+pliego INTO pliego ADDITIVE
*!*	SET RELATION TO departamento INTO departamento ADDITIVE
*!*	SET RELATION TO departamento+provincia INTO provincia ADDITIVE


SELECT A.*, B.tipo_transaccion, B.GENERICA, B.SUBGENERICA, B.SUBGENERICA_DET, B.ESPECIFICA, B.ESPECIFICA_DET,B.DESCRIPCION,B.AMBITO ,;
(ALLTRIM(B.tipo_transaccion) +". "+ALLTRIM(B.GENERICA) + ". "+ ALLTRIM(B.SUBGENERICA)+" "+ALLTRIM(B.SUBGENERICA_DET)+". "+ ALLTRIM(B.ESPECIFICA)+" "+ ALLTRIM(B.ESPECIFICA_DET)) AS CLASIF ;
FROM CERTIFICADO_CLASIF A, ESPECIFICA_DET B ;
WHERE A.ANO_EJE = B.ANO_EJE AND A.ID_CLASIFICADOR = B.ID_CLASIFICADOR AND B.ESTADO = 'A' AND A.ANO_EJE=gcAno_Eje;
into CURSOR tmpClasifRPT 

SELECT tmpClasifRPT 
INDEX on ano_eje+sec_ejec+certificado+secuencia+correlativo+ID_CLASIFICADOR TAG clasif


*===========================================================================
*===========================CREANDO CURSOR==================================
*===========================================================================

CREATE CURSOR cur_prin	(	ANO_EJE C( 4 ),;
								SEC_EJEC C( 6 ),;
								CERTIFICADO C( 10 ),;
								TIPO_CERTIFICADO C( 1 ),;
								COD_ERROR C( 2 ),;
								COD_MENSA C( 4 ),;
								TIPO_OPERACION C( 2 ),;
								GLOSA C(250),;				
								SECUENCIA C( 4),;
								CORRELATIVO C( 4),;
								COD_DOC C( 3),;
								NUM_DOC C( 20),;
								FECHA_DOC D(8) ,;
								ESTADO_REGISTRO C( 1),; 		&&	A=Aprobado	R=Rechazado		''=Estado inicial
								ESTADO_ENVIO C( 1),;			&&	P=Pendiente	T=Transmitido	N=Habilitado para envio		V='Verificado
								ESTADO_ENVIO2 C( 1),; 
								IND_CERTIFICACION C( 1 ),;
								MONTO N(19, 2),;
								MONTO_COMPROMETIDO N(19,2),;
								MONTO_NACIONAL N(19, 2),;
								MONEDA C( 3),;
								TIPO_CAMBIO N( 19 ,15),;
								TIPO_REGISTRO C( 1),;
								DESC_DOC C(100),;
								SELECCIONAR N(1),;
								RECHAZAR N(1),;
								tipo_modificacion C(01),;	&&	I=Inicial	M=Ampliacion	R=Rebaja	A=Anulacion
								ORIGEN C(1),FUENTE_FINANC C(2),ruc c(11),;
								meta c(4), cadfuncional c(50),especifica c(20),;
								saldopiM n(19,2), impSol n(19,2),impAprob n(19,2),;
								etapa c(1),descarea c(100),impTAprobXCert n(19,2))

						  
INDEX on ano_eje+sec_ejec TAG id001
INDEX on ano_eje+sec_ejec+certificado+etapa								  TAG id01						  
INDEX ON ano_eje+sec_ejec+FUENTE_FINANC+certificado+secuencia+correlativo TAG idFte				  
INDEX ON ano_eje+sec_ejec+certificado+secuencia+correlativo TAG idxLstDoc

SET RELA TO ano_eje+origen+fuente_financ INTO RFUENTE_FINANC ADDITIVE
SET RELA TO '01'+meta INTO RMAESTRO ADDITIVE

ZAP IN cur_prin


***** construyendo cursor para saldo pim
SELECT c.id_clasificador, c.certificado, c.sec_func, e.tipo_transaccion, e.generica, e.subgenerica, e.subgenerica_det, e.especifica, e.especifica_det, ;
c.monto_nacional, g.presupuesto + g.modificacion as PIM, g.monto_certificado, g.monto_precertificado, g.monto_comprometido_anual ;
FROM especifica_det e, certificado_meta c, gasto g, certificado_fase f ;
where g.ano_eje = e.ano_eje ;
AND e.ano_eje = f.ano_eje ;
AND f.ano_eje = c.ano_eje ;
and f.certificado = c.certificado ;
AND g.sec_func = c.sec_func ;
AND e.id_clasificador = g.id_clasificador ;
AND g.id_clasificador = c.id_clasificador ;
and g.sec_ejec = c.sec_ejec ;
and g.fuente_financ = f.fuente_financ ;
AND c.ano_eje = gcAno_eje ;
AND c.SECUENCIA ='0001';
GROUP BY c.certificado, c.sec_func, e.tipo_transaccion,;
e.generica, e.subgenerica, e.subgenerica_det, e.especifica, e.especifica_det, ;
c.monto_nacional, g.presupuesto, g.modificacion, g.monto_certificado, g.monto_precertificado,;
g.monto_comprometido_anual,c.id_clasificador  INTO CURSOR rpim

&&AND c.certificado = gcCertificado ; 

&&and c.estado_registro = 'V' ;

INDEX on certificado + sec_func + id_clasificador TAG idxPim
*****

SELECT RCertificado_META
GO TOP
SEEK gcAno_eje+gcSec_ejec &&+gcCertificado
SET RELA TO ano_eje+id_clasificador INTO Rmaestro_clasificador ADDITIVE
SET RELA TO ANO_EJE+SEC_EJEC+CERTIFICADO+SECUENCIA  INTO Rcertificado_fase ADDITIVE
SET RELATION TO ANO_EJE+SEC_EJEC+CERTIFICADO+SECUENCIA+CORRELATIVO INTO Rcertificado_secuencia ADDITIVE
SET RELA TO ANO_EJE+SEC_EJEC+sec_func INTO Rmeta ADDITIVE
SET RELATION TO '01'+sec_func  INTO RMaestro ADDITIVE
SET RELATION TO CERTIFICADO+SEC_FUNC+id_clasificador INTO RPIM ADDITIVE
SET RELA TO ANO_EJE+SEC_EJEC+CERTIFICADO+SECUENCIA+CORRELATIVO+ID_CLASIFICADOR INTO tmpClasifRPT ADDITIVE 

*SCAN FOR !DELETED() AND ((Ano_eje+Sec_ejec+Certificado)=(gcAno_eje+gcSec_ejec+gcCertificado))
SCAN FOR !DELETED() AND ((Ano_eje+Sec_ejec)=(gcAno_eje+gcSec_ejec)) AND  SECUENCIA ='0001'
	SCATTER MEMVAR
	IF Rcertificado_secuencia.ind_certificacion = 'S' AND  Rcertificado_secuencia.ESTADO_REGISTRO ='V'
	INSERT INTO CUR_PRIN FROM MEMVAR
	REPLACE CUR_PRIN.META WITH M.SEC_FUNC
	*REPLACE CUR_PRIN.ESPECIFICA  WITH STRTRAN(RMAESTRO_CLASIFICADOR.CLASIFICADOR,' ','.')
	REPLACE CUR_PRIN.ESPECIFICA  WITH tmpClasifRPT.clasif
	REPLACE CUR_PRIN.GLOSA WITH ALLTRIM(UPPER(RCERTIFICADO_FASE.GLOSA))
	REPLACE CUR_PRIN.FUENTE_FINANC WITH RCERTIFICADO_FASE.FUENTE_FINANC
	REPLACE CUR_PRIN.ORIGEN WITH RCERTIFICADO_FASE.ETAPA
	REPLACE CUR_PRIN.DESCAREA WITH "OFICINA DE ADMINISTRACIÓN"
	REPLACE CUR_PRIN.CADFUNCIONAL WITH RMETA.FUNCION+"."+RMETA.PROGRAMA+"."+RMETA.SUB_PROGRAMA+;
	"."+RMETA.ACT_PROY+"."+RMETA.COMPONENTE+"."+RMETA.FINALIDAD
	REPLACE CUR_PRIN.IMPAPROB WITH CUR_PRIN.MONTO
	REPLACE CUR_PRIN.IMPSOL  WITH CUR_PRIN.MONTO
	REPLACE CUR_PRIN.FECHA_DOC WITH RCERTIFICADO_SECUENCIA.FECHA_DOC
	**REPLACE CUR_PRIN.DESCAREA WITH RMAESTRO.NOMBRE
	*REPLACE CUR_PRIN.saldopiM WITH RPIM.PIM
	REPLACE CUR_PRIN.saldopiM WITH RPIM.PIM-RPIM.monto_certificado
	REPLACE CUR_PRIN.COD_DOC WITH  RCERTIFICADO_SECUENCIA.COD_DOC
	REPLACE CUR_PRIN.NUM_DOC WITH  RCERTIFICADO_SECUENCIA.NUM_DOC
	REPLACE CUR_PRIN.RUC WITH RCERTIFICADO_FASE.RUC
	REPLACE CUR_PRIN.NUM_DOC WITH ALLTRIM(SUBSTR(CUR_PRIN.NUM_DOC,1,IIF(AT('-',CUR_PRIN.NUM_DOC)>0,AT('-',CUR_PRIN.NUM_DOC)-1,LEN(CUR_PRIN.NUM_DOC))))

	ENDIF 
	SELECT RCertificado_META
ENDSCAN
SELECT cur_prin 
GO top

*** CALCULANDO ACUMULADOS DE IMPORTE APROBADO
llave = cur_prin.ano_eje+cur_prin.sec_ejec + cur_prin.certificado
SUM ALL monto_nacional to gcmonto for cur_prin.ano_eje+cur_prin.sec_ejec + cur_prin.certificado=llave
GO TOP 
SCAN 
	IF llave =cur_prin.ano_eje+cur_prin.sec_ejec + cur_prin.certificado then
		replace cur_prin.impTAprobXCert WITH gcmonto
	ELSE
		rNum = RECNO()
		llave = cur_prin.ano_eje+cur_prin.sec_ejec + cur_prin.certificado	
		SUM ALL monto_nacional to gcmonto FOR  cur_prin.ano_eje+cur_prin.sec_ejec + cur_prin.certificado=llave
		GO rNum
		replace cur_prin.impTAprobXCert WITH gcmonto
	ENDIF
	SELECT cur_prin
ENDSCAN 

GO TOP

RETURN 

****************************************************************************************
******************************** PROCEDIMIENTO PARA GUARDAR HISTORICO DE CERTIFICADOS
****************************************************************************************
PROCEDURE SaveCertificado
*** se almacena el contenido de la tabla cur_prin
IF !USED("sys_cert") THEN
	USE CERT!certificados ALIAS sys_cert  ORDER CERT IN 0
ELSE
	SELECT sys_cert
	SET ORDER TO cert
ENDIF
SELECT DISTINCT ANO_EJE, SEC_EJEC, CERTIFICADO FROM RCERTIFICADO_secuencia WHERE estado_registro ='V';
 AND SECUENCIA = '0001' AND CORRELATIVO ='0001' AND ind_certificacion <>'S' INTO CURSOR TMP0
SELECT TMP0 
GO TOP
SCAN
	SELECT sys_cert
	GO TOP
	SEEK TMP0.ano_eje+ TMP0.sec_ejec+TMP0.certificado
	IF FOUND()
		DELETE ALL FOR ano_eje+ sec_ejec+certificado=TMP0.ano_eje+ TMP0.sec_ejec+TMP0.certificado
	ENDIF
	SELECT TMP0 
ENDSCAN
SELECT DISTINCT ANO_EJE, SEC_EJEC, CERTIFICADO FROM  CUR_PRIN INTO CURSOR TMP0
SELECT TMP0 
GO TOP
SCAN
	SELECT sys_cert
	GO TOP
	SEEK TMP0.ano_eje+ TMP0.sec_ejec+TMP0.certificado
	IF FOUND()
		DELETE ALL FOR ano_eje+ sec_ejec+certificado=TMP0.ano_eje+ TMP0.sec_ejec+TMP0.certificado
	ENDIF
	SELECT TMP0 
ENDSCAN
SELECT CUR_PRIN
GO TOP
SCAN
	SCATTER MEMVAR
	INSERT INTO sys_cert FROM MEMVAR
ENDSCAN
IF USED("sys_cert") THEN
	SELECT sys_cert
	USE
ENDIF
SELECT CUR_PRIN
GO TOP
RETURN

****************************************************************************************
****************************** PROCEDIMIENTO PARA GUARDAR HISTORICO DE CERTIFICADO CHECK
****************************************************************************************
PROCEDURE SaveCertificadoSel
PARAMETERS pCertificado
*** se almacena el contenido de la tabla cur_prin
tblAnt = ALIAS()
DO RptCertificadoSel with  pCertificado
IF !USED("sys_cert") THEN
	USE CERT!certificados ALIAS sys_cert  ORDER CERT IN 0
ELSE
	SELECT sys_cert
	SET ORDER TO cert
ENDIF
SELECT sys_cert
DELETE ALL FOR ano_eje+ sec_ejec+certificado=CUR_PRIN.ano_eje+ CUR_PRIN.sec_ejec+pCertificado

SELECT CUR_PRIN
GO TOP
SCAN FOR CUR_PRIN.certificado = pCertificado
	SCATTER MEMVAR
	m.fech_ingsi = DATETIME()
	INSERT INTO sys_cert FROM MEMVAR
	SELECT CUR_PRIN
ENDSCAN
IF USED("sys_cert") THEN
	SELECT sys_cert
	USE
ENDIF
IF USED(tblAnt ) THEN
	SELECT &tblAnt 
ENDIF

RETURN


****************************************************************************************
****************************** DELETEAR CERTIFICADO
****************************************************************************************
PROCEDURE DeleteCertificadoSel
PARAMETERS pCertificado
*** se almacena el contenido de la tabla cur_prin
tblAnt = ALIAS()
IF !USED("sys_cert") THEN
	USE CERT!certificados ALIAS sys_cert  ORDER CERT IN 0
ELSE
	SELECT sys_cert
	SET ORDER TO cert
ENDIF
SELECT sys_cert
DELETE ALL FOR ano_eje+ sec_ejec+certificado=cur_cert.ano_eje+ cur_cert.sec_ejec+pCertificado

IF USED("sys_cert") THEN
	SELECT sys_cert
	USE
ENDIF
IF USED(tblAnt ) THEN
	SELECT &tblAnt 
ENDIF

RETURN


PROCEDURE RptCertificadoAprobado 
WAIT WINDOW "Recuperando información..." NOWAIT
=CloseTablesRpts()

USE ejecutora 		IN 0 SHARED ALIAS ejecutora 		AGAIN ORDER TAG sec_ejec
USE sector 			IN 0 SHARED ALIAS sector 			AGAIN ORDER TAG sector
USE pliego 			IN 0 SHARED ALIAS pliego 			AGAIN ORDER TAG pliego

*!*	USE  CERTIFICADO			AGAIN ORDER TAG CERTI   	 && ANO_EJE+SEC_EJEC+CERTIFICADO
USE  CERTIFICADO_FASE	  	IN 0 SHARED ALIAS RCERTIFICADO_FASE		AGAIN ORDER TAG CERTI_FASE   && ANO_EJE+SEC_EJEC+CERTIFICADO+SECUENCIA 
USE  CERTIFICADO_SECUENCIA 	IN 0 SHARED ALIAS RCERTIFICADO_SECUENCIA	AGAIN ORDER TAG CERTI_SEC    && ANO_EJE+SEC_EJEC+CERTIFICADO+SECUENCIA+CORRELATIVO
USE  CERTIFICADO_CLASIF  	IN 0 SHARED ALIAS RCERTIFICADO_CLASIF	AGAIN ORDER TAG CERTI_CLAS   && ANO_EJE+SEC_EJEC+CERTIFICADO+SECUENCIA+CORRELATIVO+ID_CLASIFICADOR
USE  CERTIFICADO_META	  	IN 0 SHARED ALIAS RCERTIFICADO_META		AGAIN ORDER TAG CERTI_META   && ANO_EJE+SEC_EJEC+CERTIFICADO+SECUENCIA+CORRELATIVO+ID_C
USE  MAESTRO_DOCUMENTO	  	IN 0 SHARED ALIAS RMAESTRO_DOCUMENTO		AGAIN ORDER TAG COD_DOC   	 && ANO_EJE+SEC_EJEC+CERTIFICADO+SECUENCIA+CORRELATIVO+ID_C
USE  MAESTRO_CLASIFICADOR  	IN 0 SHARED ALIAS RMAESTRO_CLASIFICADOR	AGAIN ORDER TAG IDCLASIF   	
USE  FUENTE_FINANC			IN 0 SHARED ALIAS RFUENTE_FINANC			AGAIN ORDER TAG FUENTE_FIN   && ANO_EJE+ORIGEN+FUENTE_FINANC
USE  META	  	IN 0 SHARED ALIAS RMETA		AGAIN ORDER TAG META   && ANO_EJE+SEC_EJEC+SEC_FUNC
USE  CERT!MAESTRO_DETALLE_CERT  	IN 0 SHARED ALIAS RMAESTRO 	AGAIN ORDER TAG MAESTRO   && TIPO+CODIGO


=SEEK(gcano_eje+gcsec_ejec,'ejecutora')
&&
SELECT ejecutora
SET RELATION TO ano_eje+sector INTO sector
SET RELATION TO ano_eje+sector+pliego INTO pliego ADDITIVE
*!*	SET RELATION TO departamento INTO departamento ADDITIVE
*!*	SET RELATION TO departamento+provincia INTO provincia ADDITIVE


SELECT A.*, B.tipo_transaccion, B.GENERICA, B.SUBGENERICA, B.SUBGENERICA_DET, B.ESPECIFICA, B.ESPECIFICA_DET,B.DESCRIPCION,B.AMBITO ,;
(ALLTRIM(B.tipo_transaccion) +". "+ALLTRIM(B.GENERICA) + ". "+ ALLTRIM(B.SUBGENERICA)+" "+ALLTRIM(B.SUBGENERICA_DET)+". "+ ALLTRIM(B.ESPECIFICA)+" "+ ALLTRIM(B.ESPECIFICA_DET)) AS CLASIF ;
FROM CERTIFICADO_CLASIF A, ESPECIFICA_DET B ;
WHERE A.ANO_EJE = B.ANO_EJE AND A.ID_CLASIFICADOR = B.ID_CLASIFICADOR AND B.ESTADO = 'A' AND A.ANO_EJE=gcAno_Eje;
into CURSOR tmpClasifRPT 

SELECT tmpClasifRPT 
INDEX on ano_eje+sec_ejec+certificado+secuencia+correlativo+ID_CLASIFICADOR TAG clasif


*===========================================================================
*===========================CREANDO CURSOR==================================
*===========================================================================

CREATE CURSOR cur_prin	(	ANO_EJE C( 4 ),;
								SEC_EJEC C( 6 ),;
								CERTIFICADO C( 10 ),;
								TIPO_CERTIFICADO C( 1 ),;
								COD_ERROR C( 2 ),;
								COD_MENSA C( 4 ),;
								TIPO_OPERACION C( 2 ),;
								GLOSA C(250),;				
								SECUENCIA C( 4),;
								CORRELATIVO C( 4),;
								COD_DOC C( 3),;
								NUM_DOC C( 20),;
								FECHA_DOC D(8) ,;
								ESTADO_REGISTRO C( 1),; 		&&	A=Aprobado	R=Rechazado		''=Estado inicial
								ESTADO_ENVIO C( 1),;			&&	P=Pendiente	T=Transmitido	N=Habilitado para envio		V='Verificado
								ESTADO_ENVIO2 C( 1),; 
								IND_CERTIFICACION C( 1 ),;
								MONTO N(19, 2),;
								MONTO_COMPROMETIDO N(19,2),;
								MONTO_NACIONAL N(19, 2),;
								MONEDA C( 3),;
								TIPO_CAMBIO N( 19 ,15),;
								TIPO_REGISTRO C( 1),;
								DESC_DOC C(100),;
								SELECCIONAR N(1),;
								RECHAZAR N(1),;
								tipo_modificacion C(01),;	&&	I=Inicial	M=Ampliacion	R=Rebaja	A=Anulacion
								ORIGEN C(1),FUENTE_FINANC C(2),ruc c(11),;
								meta c(4), cadfuncional c(50),especifica c(20),;
								saldopiM n(19,2), impSol n(19,2),impAprob n(19,2),;
								etapa c(1),descarea c(100),impTAprobXCert n(19,2))

						  
INDEX on ano_eje+sec_ejec TAG id001
INDEX on ano_eje+sec_ejec+certificado+etapa								  TAG id01						  
INDEX ON ano_eje+sec_ejec+FUENTE_FINANC+certificado+secuencia+correlativo TAG idFte				  
INDEX ON ano_eje+sec_ejec+certificado+secuencia+correlativo TAG idxLstDoc

SET RELA TO ano_eje+origen+fuente_financ INTO RFUENTE_FINANC ADDITIVE
SET RELA TO '01'+meta INTO RMAESTRO ADDITIVE

ZAP IN cur_prin


IF !USED("sys_cert") THEN
	USE CERT!certificados ALIAS sys_cert  ORDER CERT IN 0
ELSE
	SELECT sys_cert
	SET ORDER TO cert
ENDIF

SELECT sys_cert
SCAN 
	SCATTER MEMVAR
	INSERT INTO  cur_prin FROM MEMVAR
	SELECT sys_cert
ENDSCAN	
	
SELECT cur_prin

GO TOP



RETURN



*** REPORTE DE CERTIFICADOS
PROCEDURE RptCertificadoSel
PARAMETERS pCertificado
=CloseTablesRpts()

USE ejecutora 		IN 0 SHARED ALIAS ejecutora 		AGAIN ORDER TAG sec_ejec
USE sector 			IN 0 SHARED ALIAS sector 			AGAIN ORDER TAG sector
USE pliego 			IN 0 SHARED ALIAS pliego 			AGAIN ORDER TAG pliego

*!*	USE  CERTIFICADO			AGAIN ORDER TAG CERTI   	 && ANO_EJE+SEC_EJEC+CERTIFICADO
USE  CERTIFICADO_FASE	  	IN 0 SHARED ALIAS RCERTIFICADO_FASE		AGAIN ORDER TAG CERTI_FASE   && ANO_EJE+SEC_EJEC+CERTIFICADO+SECUENCIA 
USE  CERTIFICADO_SECUENCIA 	IN 0 SHARED ALIAS RCERTIFICADO_SECUENCIA	AGAIN ORDER TAG CERTI_SEC    && ANO_EJE+SEC_EJEC+CERTIFICADO+SECUENCIA+CORRELATIVO
USE  CERTIFICADO_CLASIF  	IN 0 SHARED ALIAS RCERTIFICADO_CLASIF	AGAIN ORDER TAG CERTI_CLAS   && ANO_EJE+SEC_EJEC+CERTIFICADO+SECUENCIA+CORRELATIVO+ID_CLASIFICADOR
USE  CERTIFICADO_META	  	IN 0 SHARED ALIAS RCERTIFICADO_META		AGAIN ORDER TAG CERTI_META   && ANO_EJE+SEC_EJEC+CERTIFICADO+SECUENCIA+CORRELATIVO+ID_C
USE  MAESTRO_DOCUMENTO	  	IN 0 SHARED ALIAS RMAESTRO_DOCUMENTO		AGAIN ORDER TAG COD_DOC   	 && ANO_EJE+SEC_EJEC+CERTIFICADO+SECUENCIA+CORRELATIVO+ID_C
USE  MAESTRO_CLASIFICADOR  	IN 0 SHARED ALIAS RMAESTRO_CLASIFICADOR	AGAIN ORDER TAG IDCLASIF   	
USE  FUENTE_FINANC			IN 0 SHARED ALIAS RFUENTE_FINANC			AGAIN ORDER TAG FUENTE_FIN   && ANO_EJE+ORIGEN+FUENTE_FINANC
USE  META	  	IN 0 SHARED ALIAS RMETA		AGAIN ORDER TAG META   && ANO_EJE+SEC_EJEC+SEC_FUNC
USE  CERT!MAESTRO_DETALLE_CERT  	IN 0 SHARED ALIAS RMAESTRO 	AGAIN ORDER TAG MAESTRO   && TIPO+CODIGO


=SEEK(gcano_eje+gcsec_ejec,'ejecutora')
&&
SELECT ejecutora
SET RELATION TO ano_eje+sector INTO sector
SET RELATION TO ano_eje+sector+pliego INTO pliego ADDITIVE

SELECT A.*, B.tipo_transaccion, B.GENERICA, B.SUBGENERICA, B.SUBGENERICA_DET, B.ESPECIFICA, B.ESPECIFICA_DET,B.DESCRIPCION,B.AMBITO ,;
(ALLTRIM(B.tipo_transaccion) +". "+ALLTRIM(B.GENERICA) + ". "+ ALLTRIM(B.SUBGENERICA)+" "+ALLTRIM(B.SUBGENERICA_DET)+". "+ ALLTRIM(B.ESPECIFICA)+" "+ ALLTRIM(B.ESPECIFICA_DET)) AS CLASIF ;
FROM CERTIFICADO_CLASIF A, ESPECIFICA_DET B ;
WHERE A.ANO_EJE = B.ANO_EJE AND A.ID_CLASIFICADOR = B.ID_CLASIFICADOR AND B.ESTADO = 'A' AND A.ANO_EJE=gcAno_Eje;
AND A.CERTIFICADO = pCertificado ;
into CURSOR tmpClasifRPT 

SELECT tmpClasifRPT 
INDEX on ano_eje+sec_ejec+certificado+secuencia+correlativo+ID_CLASIFICADOR TAG clasif


*===========================================================================
*===========================CREANDO CURSOR==================================
*===========================================================================

CREATE CURSOR cur_prin	(	ANO_EJE C( 4 ),;
								SEC_EJEC C( 6 ),;
								CERTIFICADO C( 10 ),;
								TIPO_CERTIFICADO C( 1 ),;
								COD_ERROR C( 2 ),;
								COD_MENSA C( 4 ),;
								TIPO_OPERACION C( 2 ),;
								GLOSA C(250),;				
								SECUENCIA C( 4),;
								CORRELATIVO C( 4),;
								COD_DOC C( 3),;
								NUM_DOC C( 20),;
								FECHA_DOC D(8) ,;
								ESTADO_REGISTRO C( 1),; 		&&	A=Aprobado	R=Rechazado		''=Estado inicial
								ESTADO_ENVIO C( 1),;			&&	P=Pendiente	T=Transmitido	N=Habilitado para envio		V='Verificado
								ESTADO_ENVIO2 C( 1),; 
								IND_CERTIFICACION C( 1 ),;
								MONTO N(19, 2),;
								MONTO_COMPROMETIDO N(19,2),;
								MONTO_NACIONAL N(19, 2),;
								MONEDA C( 3),;
								TIPO_CAMBIO N( 19 ,15),;
								TIPO_REGISTRO C( 1),;
								DESC_DOC C(100),;
								SELECCIONAR N(1),;
								RECHAZAR N(1),;
								tipo_modificacion C(01),;	&&	I=Inicial	M=Ampliacion	R=Rebaja	A=Anulacion
								ORIGEN C(1),FUENTE_FINANC C(2),ruc c(11),;
								meta c(4), cadfuncional c(50),especifica c(20),;
								saldopiM n(19,2), impSol n(19,2),impAprob n(19,2),;
								etapa c(1),descarea c(100),impTAprobXCert n(19,2))

						  
INDEX on ano_eje+sec_ejec TAG id001
INDEX on ano_eje+sec_ejec+certificado+etapa								  TAG id01						  
INDEX ON ano_eje+sec_ejec+FUENTE_FINANC+certificado+secuencia+correlativo TAG idFte				  
INDEX ON ano_eje+sec_ejec+certificado+secuencia+correlativo TAG idxLstDoc

SET RELA TO ano_eje+origen+fuente_financ INTO RFUENTE_FINANC ADDITIVE
SET RELA TO '01'+meta INTO RMAESTRO ADDITIVE

ZAP IN cur_prin


***** construyendo cursor para saldo pim
SELECT c.id_clasificador, c.certificado, c.sec_func, e.tipo_transaccion, e.generica, e.subgenerica, e.subgenerica_det, e.especifica, e.especifica_det, ;
c.monto_nacional, g.presupuesto + g.modificacion as PIM, g.monto_certificado, g.monto_precertificado, g.monto_comprometido_anual ;
FROM especifica_det e, certificado_meta c, gasto g, certificado_fase f ;
where g.ano_eje = e.ano_eje ;
AND e.ano_eje = f.ano_eje ;
AND f.ano_eje = c.ano_eje ;
and f.certificado = c.certificado ;
AND g.sec_func = c.sec_func ;
AND e.id_clasificador = g.id_clasificador ;
AND g.id_clasificador = c.id_clasificador ;
and g.sec_ejec = c.sec_ejec ;
and g.fuente_financ = f.fuente_financ ;
AND c.ano_eje = gcAno_eje ;
AND c.SECUENCIA ='0001';
AND c.certificado = pCertificado;
GROUP BY c.certificado, c.sec_func, e.tipo_transaccion,;
e.generica, e.subgenerica, e.subgenerica_det, e.especifica, e.especifica_det, ;
c.monto_nacional, g.presupuesto, g.modificacion, g.monto_certificado, g.monto_precertificado,;
g.monto_comprometido_anual,c.id_clasificador  INTO CURSOR rpim

&&AND c.certificado = gcCertificado ; 

&&and c.estado_registro = 'V' ;

INDEX on certificado + sec_func + id_clasificador TAG idxPim
*****

SELECT RCertificado_META
GO TOP
SEEK gcAno_eje+gcSec_ejec &&+gcCertificado
SET RELA TO ano_eje+id_clasificador INTO Rmaestro_clasificador ADDITIVE
SET RELA TO ANO_EJE+SEC_EJEC+CERTIFICADO+SECUENCIA  INTO Rcertificado_fase ADDITIVE
SET RELATION TO ANO_EJE+SEC_EJEC+CERTIFICADO+SECUENCIA+CORRELATIVO INTO Rcertificado_secuencia ADDITIVE
SET RELA TO ANO_EJE+SEC_EJEC+sec_func INTO Rmeta ADDITIVE
SET RELATION TO '01'+sec_func  INTO RMaestro ADDITIVE
SET RELATION TO CERTIFICADO+SEC_FUNC+id_clasificador INTO RPIM ADDITIVE
SET RELA TO ANO_EJE+SEC_EJEC+CERTIFICADO+SECUENCIA+CORRELATIVO+ID_CLASIFICADOR INTO tmpClasifRPT ADDITIVE 

*SCAN FOR !DELETED() AND ((Ano_eje+Sec_ejec+Certificado)=(gcAno_eje+gcSec_ejec+gcCertificado))
SCAN FOR !DELETED() AND ((Ano_eje+Sec_ejec)=(gcAno_eje+gcSec_ejec)) AND  SECUENCIA ='0001' AND CERTIFICADO = pCertificado
	SCATTER MEMVAR
	INSERT INTO CUR_PRIN FROM MEMVAR
	REPLACE CUR_PRIN.META WITH M.SEC_FUNC
	*REPLACE CUR_PRIN.ESPECIFICA  WITH STRTRAN(RMAESTRO_CLASIFICADOR.CLASIFICADOR,' ','.')
	REPLACE CUR_PRIN.ESPECIFICA  WITH tmpClasifRPT.clasif
	REPLACE CUR_PRIN.GLOSA WITH ALLTRIM(UPPER(RCERTIFICADO_FASE.GLOSA))
	REPLACE CUR_PRIN.FUENTE_FINANC WITH RCERTIFICADO_FASE.FUENTE_FINANC
	REPLACE CUR_PRIN.ORIGEN WITH RCERTIFICADO_FASE.ETAPA
	REPLACE CUR_PRIN.DESCAREA WITH "OFICINA DE ADMINISTRACIÓN"
	REPLACE CUR_PRIN.CADFUNCIONAL WITH RMETA.FUNCION+"."+RMETA.PROGRAMA+"."+RMETA.SUB_PROGRAMA+;
	"."+RMETA.ACT_PROY+"."+RMETA.COMPONENTE+"."+RMETA.FINALIDAD
	REPLACE CUR_PRIN.IMPAPROB WITH CUR_PRIN.MONTO
	REPLACE CUR_PRIN.IMPSOL  WITH CUR_PRIN.MONTO
	REPLACE CUR_PRIN.FECHA_DOC WITH RCERTIFICADO_SECUENCIA.FECHA_DOC
	**REPLACE CUR_PRIN.DESCAREA WITH RMAESTRO.NOMBRE
	*REPLACE CUR_PRIN.saldopiM WITH RPIM.PIM
	REPLACE CUR_PRIN.saldopiM WITH RPIM.PIM-RPIM.monto_certificado
	REPLACE CUR_PRIN.COD_DOC WITH  RCERTIFICADO_SECUENCIA.COD_DOC
	REPLACE CUR_PRIN.NUM_DOC WITH  RCERTIFICADO_SECUENCIA.NUM_DOC
	REPLACE CUR_PRIN.RUC WITH RCERTIFICADO_FASE.RUC
	REPLACE CUR_PRIN.NUM_DOC WITH ALLTRIM(SUBSTR(CUR_PRIN.NUM_DOC,1,IIF(AT('-',CUR_PRIN.NUM_DOC)>0,AT('-',CUR_PRIN.NUM_DOC)-1,LEN(CUR_PRIN.NUM_DOC))))
	SELECT RCertificado_META
ENDSCAN
SELECT cur_prin 
GO top

*** CALCULANDO ACUMULADOS DE IMPORTE APROBADO
llave = cur_prin.ano_eje+cur_prin.sec_ejec + cur_prin.certificado
SUM ALL monto_nacional to gcmonto for cur_prin.ano_eje+cur_prin.sec_ejec + cur_prin.certificado=llave
GO TOP 
SCAN 
	IF llave =cur_prin.ano_eje+cur_prin.sec_ejec + cur_prin.certificado then
		replace cur_prin.impTAprobXCert WITH gcmonto
	ELSE
		rNum = RECNO()
		llave = cur_prin.ano_eje+cur_prin.sec_ejec + cur_prin.certificado	
		SUM ALL monto_nacional to gcmonto FOR  cur_prin.ano_eje+cur_prin.sec_ejec + cur_prin.certificado=llave
		GO rNum
		replace cur_prin.impTAprobXCert WITH gcmonto
	ENDIF
	SELECT cur_prin
ENDSCAN 

GO TOP

RETURN 


PROCEDURE RptSoloPendientes 
*** se almacena el contenido de la tabla cur_prin
SELECT DISTINCT ANO_EJE, SEC_EJEC, CERTIFICADO FROM RCERTIFICADO_secuencia WHERE estado_registro ='V';
 AND SECUENCIA = '0001' AND CORRELATIVO ='0001' AND ind_certificacion ='S' INTO CURSOR TMP0
INDEX on ano_eje+ sec_ejec+certificado TAG tmpt0
SELECT cur_prin
GO top
SCAN
	SELECT TMP0 
	GO TOP
	SEEK cur_prin.ano_eje+ cur_prin.sec_ejec+cur_prin.certificado
	IF !FOUND()
		SELECT cur_prin
		DELETE 
	ENDIF
	SELECT cur_prin
ENDSCAN

SELECT cur_prin
GO TOP 
RETURN 

PROCEDURE RptMesSeleccionado
IF !EMPTY(gcMes) AND gcMes<>'00' then
	SELECT cur_prin
	GO TOP
	DELETE ALL FOR ano_eje = gcAno_eje AND sec_ejec = gcSec_ejec AND MONTH(fecha_doc)<>VAL(gcMes )
ENDIF 
SELECT cur_prin
GO top
RETURN 
