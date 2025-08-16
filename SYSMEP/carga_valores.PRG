PUBLIC xRutaActual
PUBLIC  loJustify
xRutaActual = FULLPATH(CURDIR())
lcRutaSystem = RutaSystem()

SET PATH TO fORMS, PRGS, menus, graficos, include, data , clases, reports, lcRutaSystem, exporta
#INCLUDE include\excel.h
#DEFINE autoColumnFormat 4
#DEFINE autoOneSeriesLabel 1
#DEFINE autoOneCategoryLabel 1
#DEFINE autoHasLegend .T.
*SET CLASSLIB TO  clases\manage 
*SET CLASSLIB TO  clases\blowfish
SET CLASSLIB TO manage   ADDITIVE 
SET CLASSLIB TO ssultimate ADDITIVE 
SET DATE  BRITISH
SET CENTURY ON 
SET SAFETY OFF
SET TALK OFF
PUBLIC ssUltimate
ssUltimate = CREATEOBJECT('EMPTY')
ADDPROPERTY(ssUltimate,'SkinsFolder',ADDBS(GETENV("APPDATA")))
*SET CLASSLIB TO ssultimate ADDITIVE && this should be on your path. If not, include fullpath()

*SET CLASSLIB TO cbsearch ADDITIVE 
SET PROCEDURE TO rutinas ADDITIVE 
SET PROCEDURE TO pdflistener ADDITIVE 
*SET PROCEDURE TO extend4 ADDITIVE 
SET PROCEDURE TO foxypreviewercaller ADDITIVE 
SET PROCEDURE TO appendxlsx ADDITIVE 
SET PROCEDURE TO justify ADDITIVE 
SET MULTILOCKS ON
SET HELP OFF
*SET CLASSLIB TO vfpcalendar ADDITIVE 
SET CLASSLIB TO sfrutina ADDITIVE
SET CLASSLIB TO sfbase	 ADDITIVE
SET CLASSLIB TO sfmanage ADDITIVE
*SET CLASSLIB TO sfrutina ADDITIVE

*SET STATUS BAR OFF
SET ECHO OFF 
SET NOTIFY OFF 
*oSiafRutina	 = CREATEOBJECT( 'SiafRutina' ) && Clase con los metodos de rutina
*!*	oSiafEnv = CREATEOBJECT( 'SiafEnviron' )
*!*	oSiafEditWin = CREATEOBJECT( 'FormManager' )
oSiafRutina	= CREATEOBJECT( 'SiafRutina' ) && Clase con los metodos de rutina	

loJustify = CREATEOBJECT("justify")

#INCLUDE include\cert.h

pArchivo = lcRutaSystem + '\'+"FoxyPreviewer.App"

SET PROCEDURE TO LOCFILE(pArchivo) ADDITIVE 

*!*	pArchivo = lcRutaSystem + '\'+"system.App"

*!*	SET PROCEDURE TO system.app ADDITIVE 

DO system.app
DO DeclaraTodo
DO check_ini
gcSEC_EJEC_pliego = GCSEC_EJEC

SET EXCLUSIVE OFF 
SET DELETED ON 

xRutaActual = FULLPATH(CURDIR())
SET DEFAULT TO &xRutaActual
xRutaCert = gcRutaCert+'SYSMEP'
OPEN DATABASE &XRutaCert SHARED 
xRutaSiaf = gcRutaSiaf+'SIAF'
OPEN DATABASE &xRutaSiaf SHARED 
gcLLave = 'SPIDERMANHYPE'
USE SIAF!TIPO_UNIDAD_X_AMBITO
SELECT TIPO_UNIDAD_X_AMBITO
SCAN ALL
	gcambito = gcambito+ALLTRIM(ambito)
ENDSCAN
USE IN TIPO_UNIDAD_X_AMBITO
gcorganismo = '002'
GCANO_EJE='2023'
gcsiglas = ''
SET PATH TO FORMS, PRGS, MENUS, GRAFICOS, INCLUDE, DATA , CLASES, REPORTS, LCRUTASYSTEM, EXPORTA, xml
********************
PROCEDURE DeclaraTodo
********************
PUBLIC gcAno_Eje,gcSec_Ejec, gcTipo_Unidad,GCTIPO_UE, gcOldCaption,gcOldClassLib,gcOldDir,gcOldEscape,;
gcOldPath,gcOldTalk,gcMonto,gcFileIni  ,gcSession , gcOpcRpt, gcFiltroPca, gcMes, gcRutaSiaf, gcRutaCert, glmuestra_fuente_agregada, gcnom_fuente ,gcnom_fuente_abrev,;
gcCab_mod, gcId_rep, gcVersion ,gcTipo_reporte, gcclaveusr, gcuserid, gcEncriptaDesencripta, gcLlave, gces_supervisor,;
gcSubNivel01, gcSubNivel02, gcAbrevSubNivel01, gcAbrevSubNivel02, gcmes_apertura , gcmes_cierre, gcfuncion_entidad, GCES_SUPERVISOR, gcigv, lcmes,;
gcRutaInterfase, gcSEC_EJEC_pliego, gcversion_office, gccaja, gcversion_office, gctipo_rendicion, gcruta_app, gcorganismo, gcproyecto, GCAMBITO, gccarpetalistado, gcsiglas, gcTipo_excel, gcvalidaMonetizacion
#include include\CERT.h

*-- Instrucciones DECLARE DLL para leer/escribir en archivos INI privados
DECLARE INTEGER GetPrivateProfileString IN Win32API  AS GetPrivStr ;
	String cSection, String cKey, String cDefault, String @cBuffer, ;
	Integer nBufferSize, String cINIFile

DECLARE INTEGER WritePrivateProfileString IN Win32API AS WritePrivStr ;
	String cSection, String cKey, String cValue, String cINIFile
gcMes=""
gcorganismo = '001'
gcversion_office = '11.00'
lcmes = '12'
gcFiltroPca=""	
gcOpcRpt='EST_V'	
gcOldCaption=""
gcOldDir=CURDIR()
gcOldPath=fullpath(CURDIR())
gcruta_app = gcOldPath
gcOldEscape="ON"
gcOldTalk="ON"
gcFileIni = 'SYSMEP.ini'
gcSession = "PARAMETROS DEL CERT"
gcnom_fuente = 'RUBRO'
gcnom_fuente_abrev = 'RB'
glmuestra_fuente_agregada = .T.
gcTipo_Unidad='E'
GCTIPO_UE='01'
gcMonto = ''
gcVersion = 'Versión 1.1'
gcCab_mod = 'SIGPRES' 
gcuserid = 'USUARIO'
gccaja = '01'

gcproyecto = '222'
gcSubNivel01 = 'Centro de Costo'
gcSubNivel02 = 'Tareas / Objetivos'
gcAbrevSubNivel01 = 'CC'
gcAbrevSubNivel02 = 'T/O'
gcmes_apertura = '01'
gcfuncion_entidad = '01'
gcmes_cierre = '13'
gces_supervisor = 1
gcigv = 0.18
gcRutaInterfase = 'h:\siaf\siaf_1438\interfase\'
gcSEC_EJEC_pliego = gcsec_ejec
gcTipo_rendicion = ''
gcTipo_excel = '1'   && 1 - xls  2 - xlsx
*oWord = CreateObject('Word.Application')
*gcVersion_office = oWord.Version

GCAMBITO = ''
gcCarpetalistado = 'LISTADOS'


*** SETEANDO DATOS DE LA PANTALLA
_screen.Icon 		= 'bmpPrincipal.ICO'
_screen.Picture 	= 'bghome.gif'
_screen.Caption 	= 'SYSMEP 1.0' 
_screen.BorderStyle = 3

**2
_screen.windowstate = 2
_screen.Closable = .F.
_SCREEN.CONTROLBOX=.T.
_screen.AlwaysOnTop = .F. 
oSiafRutina	 = CREATEOBJECT( 'SiafRutina' ) && Clase con los metodos de rutina
*!*	? "UE :"+gcsec_ejec
*!*	? "PROYECTO :"+gcProyecto
*!*	? "RUTA SIAF :"+gcRutaSIAF

ENDPROC

PROCEDURE CHECK_INI

lcArchivo = 'SYSMEP.ini'
IF FILE('&lcArchivo.')
	lnFileHandle = FOPEN(lcArchivo,2)
	DO WHILE !FEOF(lnFileHandle)
		lcString = ALLTRIM(FGETS(lnFileHandle, 254))
		lnSize = FSEEK(lnFileHandle,0,1)
		DO CASE
			CASE 'SEC_EJEC' $ UPPER(lcString)
				gcsec_ejec = SUBSTR(lcString,AT('=',lcString)+1)
				gcsec_ejec = decodificar_simple(gcsec_ejec,'SPIDERMANHYPE')
				gcsec_ejec = PADL(gcsec_ejec,6,'0')
			CASE 'RUTASIAF' $ UPPER(lcString)
				gcRutaSiaf = SUBSTR(lcString,AT('=',lcString)+1)
			CASE 'RUTACERT' $ UPPER(lcString)
				gcRutaCert = SUBSTR(lcString,AT('=',lcString)+1)
			CASE 'RUTAINTERFASE' $ UPPER(lcString)
				gcRutaInterfase = SUBSTR(lcString,AT('=',lcString)+1)
			CASE 'TIPO_REPORTE' $ UPPER(lcString)
				gcTipo_reporte  = SUBSTR(lcString,AT('=',lcString)+1)
			CASE 'ANO_EJE' $ UPPER(lcString)
				gcano_eje = SUBSTR(lcString,AT('=',lcString)+1)
			CASE 'TIPO_EXCEL' $ UPPER(lcString)
				gcTipo_excel  = SUBSTR(lcString,AT('=',lcString)+1)				

		ENDCASE
	ENDDO
	=FCLOSE(lnFileHandle)
ELSE
	MESSAGEBOX('No Existe el Archivo de Configuración.',0+64,'Aviso')
ENDIF
ENDPROC 

PROCEDURE CHECK_ESTRUCTURA

lcruta	= SYS(5)+SYS(2003)

DIMENSION ladirectory(13)

STORE lcruta+'\Listados' TO ladirectory(1)

FOR i=1 TO 1
	IF !DIRECTORY(ladirectory(i)) THEN 
		MKDIR &ladirectory(i).
	ENDIF 
ENDFOR 

ENDPROC 

PROCEDURE SalirSistema
	CLEAR EVENTS
	RETURN
ENDPROC 

FUNCTION RutaSystem
	PRIVATE lcRutaSystem
	#DEFINE MAX_PATH 260
	*!* Declare the GetWindowsDirectory function from the WIN32API
	DECLARE INTEGER GetWindowsDirectory IN kernel32.dll ;
	    STRING @WinBuffer, INTEGER WinBuffLen
	lcWinBuffer = SPACE(MAX_PATH)
	*!* Get the path to the windows directory
	=GetWindowsDirectory(@lcWinBuffer, MAX_PATH)
	*!* Parse the null terminator from the returned string
	lcWinBuffer = LEFT(lcWinBuffer, AT(CHR(0), lcWinBuffer) - 1)

	*!* Declare the GetSystemDirectory function from the WIN32API
	DECLARE INTEGER GetSystemDirectory IN kernel32.dll ;
	    STRING @SysBuffer, INTEGER SysBufferLen
	lcSysBuffer = SPACE(MAX_PATH)
	*!* Get the path to the system directory
	=GetSystemDirectory(@lcSysBuffer, MAX_PATH)
	*!* Parse the null terminator from the returned string
	lcSysBuffer = LEFT(lcSysBuffer, AT(CHR(0), lcSysBuffer) - 1)

*	? "Windows Directory = " + lcWinBuffer
*	? "System Directory  = " + lcSysBuffer
	lcRutaSystem = lcSysBuffer
*	CLEAR DLLS
	RETURN lcRutaSystem

ENDFUNC
