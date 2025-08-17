#DEFINE HKEY_CLASSES_ROOT           -2147483648  && BITSET(0,31)
#DEFINE HKEY_CURRENT_USER           -2147483647  && BITSET(0,31)+1
#DEFINE HKEY_LOCAL_MACHINE          -2147483646  && BITSET(0,31)+2
#DEFINE HKEY_USERS                  -2147483645  && BITSET(0,31)+3
#DEFINE LLKEY				"SPIDERMANHYPE"
#DEFINE LLKEY_MENU			"SPIDERMANHOMECOMING"

lcRutaSystem = RutaSystem()
SET PATH TO fORMS, PRGS, menus, graficos, include, data , clases, reports, &lcRutaSystem, xml, sys
#INCLUDE include\excel.h
#INCLUDE include\SYSSOPORTE.h
SET CLASSLIB TO manage   ADDITIVE 
SET CLASSLIB TO blowfish ADDITIVE 
SET CLASSLIB TO registry ADDITIVE 
SET CLASSLIB TO CLASES\zip 	 ADDITIVE 
SET DATE BRITISH 
PUBLIC ssUltimate
ssUltimate = CREATEOBJECT('EMPTY')
ADDPROPERTY(ssUltimate,'SkinsFolder',ADDBS(GETENV("APPDATA")))
*SET CLASSLIB TO ssultimate ADDITIVE && this should be on your path. If not, include fullpath()
SET CLASSLIB TO cbsearch ADDITIVE 
***
SET CLASSLIB TO sfbase	 ADDITIVE
SET CLASSLIB TO sfmanage ADDITIVE
SET CLASSLIB TO sfrutina ADDITIVE
oSiafRutina	 = CREATEOBJECT( 'SiafRutina' ) && Clase con los metodos de rutina

***
SET DATE TO	DMY
SET CLOCK OFF
SET CENTURY ON
SET CENTURY  TO 19 rollover 90

*SET DATE TO BRITISH
*SET CENTURY ON 
SET SAFETY OFF
SET TALK OFF
SET PROCEDURE TO rutinas ADDITIVE 
*SET PROCEDURE TO pdflistener ADDITIVE 
SET PROCEDURE TO foxypreviewercaller ADDITIVE 
SET PROCEDURE TO appendxlsx ADDITIVE 
SET MULTILOCKS ON
SET HELP OFF
SET ECHO OFF 
SET NOTIFY OFF 
SET STATUS BAR OFF 
SET ESCAPE OFF 

DO DeclaraTodo
DO check_ini
pArchivo 		= lcRutaSystem + '\'+"FoxyPreviewer.App"
pArchivoSystem  = lcRutaSystem + '\'+"system.App"

TRY 
	IF FILE(pArchivoSystem) THEN 
		DO &pArchivoSystem 		
	ENDIF 
	TRY 
		DO system.app
	CATCH 
		glSystemApp = .F.
	ENDTRY 

CATCH TO loExc
	=ReporteErrores(loExc, 0)
	EXIT 
	glSystemApp = .F.
ENDTRY 


TRY 
	IF FILE(parchivo) THEN 
		SET PROCEDURE TO LOCFILE(pArchivo) ADDITIVE 
	ENDIF 
CATCH TO loExc
	=ReporteErrores(loExc, 0)
	glFoxy = .F.
ENDTRY 

IF !glSystemApp OR !glFoxy THEN 
	QUIT 
ENDIF 
DO CHECK_ESTRUCTURA
SET EXCLUSIVE OFF 
SET DELETED ON 

***** Lee y actualiza el valor en el Registro de Windows ***********************

cValue = []
oRegistry = CREATEOBJECT("Registry")
lnResult = oRegistry.GetRegKey("Plex",@cValue,"Software\Intel",HKEY_CURRENT_USER)
cvalue = STR(INT(VAL(cvalue))+1)
oRegistry.setregkey("Plex",cvalue,"Software\Intel",HKEY_CURRENT_USER)



xRutaActual = FULLPATH(CURDIR())
SET DEFAULT TO (xRutaActual)
xRutaCert = gcRutaCert+'SYSSOPORTE'
TRY 
	OPEN DATABASE (xRutaCert) SHARED 
CATCH TO loExc
	=ReporteErrores(loExc, 0)
	=MESSAGEBOX('La ruta es incorrecta. Verifique' ,64,'Aviso')
	gcRutaOk = .F.

FINALLY 
	IF !gcRutaOk THEN 
		QUIT 
	ENDIF 
ENDTRY 

TRY 
	xRutaSiaf = gcRutaSiaf+'SIAF'
	OPEN DATABASE (xRutaSiaf) SHARED 
CATCH TO loExc
	=ReporteErrores(loExc, 0)
	=MESSAGEBOX('La ruta es incorrecta. Verifique' ,64,'Aviso')
	gcRutaOk = .F.
FINALLY 
	IF !gcRutaOk THEN 
		QUIT 
	ENDIF 
ENDTRY 


gcRuta_app 				= xRutaActual
gcEncriptaDesencripta 	= CREATEOBJECT("blowfish")
gcrutacli				= xRutaActual
gcSubNivel01 			= ''
gcSubNivel02 			= ''
gcAbrevSubNivel01 		= ''
gcAbrevSubNivel02 		= ''
gcAmbito 				= ''
*!*	USE SIAF!TIPO_UNIDAD_X_AMBITO
*!*	SELECT TIPO_UNIDAD_X_AMBITO
*!*	SCAN ALL
*!*		gcambito = gcambito+ALLTRIM(ambito)
*!*	ENDSCAN
*!*	USE IN TIPO_UNIDAD_X_AMBITO



SET SYSMENU OFF
ON SHUTDOWN DO SalirSistema IN inicio_sincroniza.prg



DO SETEAR_PANTALLA		
limiteAccesos = 20

IF gcRutaOk THEN 
	DO menu_sincroniza.mpr
	READ EVENTS
 
*!*			DO FORM f_userlog TO lIngreso
*!*			gcClaveMenu 			= ALLTRIM(gcEncriptaDesencripta.codificarBlowFish(gcano_eje+gcsec_ejec,gcLlaveMenu))
*!*			DO SETEAR_PANTALLA
*!*			IF lIngreso
*!*			
*!*				oSiafEnv = CREATEOBJECT( 'SiafEnviron' )
*!*				oSiafEditWin = CREATEOBJECT( 'FormManager' )
*!*				oSiafRutina	= CREATEOBJECT( 'SiafRutina' ) && Clase con los metodos de rutina		

*!*				TRY 	
*!*					llOk = .F.
*!*					IF gcActivo = 1  THEN 
*!*						lcCodigoEncriptado = INT(VAL((gcEncriptaDesencripta.DecodificarBlowFish(gcClave,gcllave))))		
*!*						
*!*						DO CASE 
*!*							CASE INLIST(ALLTRIM(gcuserid) , 'ADMINISTRADOR','MASTER')
*!*							
*!*								DO menu_administrador.mpr
*!*								llOk = .T.
*!*							CASE ALLTRIM(gcuserid) = 'MATRIX'						
*!*								DO menu_master.mpr					
*!*								llOk = .T.

*!*								
*!*							OTHERWISE 
*!*								IF VAL(cvalue) > LimiteAccesos AND lcCodigoEncriptado <> INT(val(gcsec_ejec)) THEN 
*!*									=MESSAGEBOX('Ya supero el periodo de prueba del Sistema',64,'Aviso')
*!*									EXIT 
*!*									llOk = .F.
*!*								ENDIF 
*!*						ENDCASE 
*!*						IF !llOk THEN 
*!*							DO Crea_Menu					
*!*						ENDIF 
*!*						DO coloca_barra
*!*					    READ EVENTS
*!*					 ENDIF
*!*				CATCH TO loException
*!*					WAIT WINDOW 'ERROR LLAMAR A SOPORTE' 
*!*					QUIT
*!*				FINALLY
*!*				    ON SHUTDOWN
*!*					RELEASE ALL
*!*					DEACTIVATE WIND ALL
*!*					CLEAR WINDOW
*!*					CLEAR ALL
*!*					CLOSE ALL
*!*					CLOSE DATA
*!*					CLEAR
*!*					SET SYSMENU TO DEFAULT
*!*					SET CLASSLIB TO 
*!*					QUIT 
*!*					RETURN
*!*				ENDTRY				
*!*			ENDIF
ENDIF 

ON SHUTDOWN
RELEASE ALL
DEACTIVATE WIND ALL
CLEAR WINDOW
CLEAR ALL
CLOSE ALL
close data
clear
set sysmenu to default
SET CLASSLIB TO 
RETURN


*!*	PROCEDURE Crea_Menu
*!*		USE systesoreria!menu_niv01                      IN 0 SHARED AGAIN ORDER TAG id_menu
*!*		USE systesoreria!menu_niv02                      IN 0 SHARED AGAIN ORDER TAG menu_niv2
*!*		USE systesoreria!menu_niv03                      IN 0 SHARED AGAIN ORDER TAG id_menu	 
*!*		lcSeek = 'gcano_eje'
*!*		lcBusqueda = 'ano_eje= gcano_eje'		

*!*		SET SYSMENU TO
*!*		SET SYSMENU AUTOMATIC

*!*		p_user = RTRIM(gcuserid) 
*!*		lnContador=0 
*!*		
*!*		 
*!*		SELECT menu_niv01
*!*		SEEK &lcSeek
*!*		SCAN WHILE &lcBusqueda
*!*		  SCATTER MEMVAR 
*!*		  IF m.ano_eje+RTRIM(m.usuario) <> gcano_eje+p_user THEN 
*!*		   LOOP 
*!*		  ENDIF 
*!*		  IF DELETED() THEN 
*!*		   LOOP 
*!*		  ENDIF 
*!*		  IF m.visible  <> 'S' THEN 
*!*		   LOOP 
*!*		  ENDIF  
*!*		    
*!*		    lcPad   = '_Niv01_' + m.id_menuniv01
*!*		    lcPromp = IIF(m.estado='A',"'","'\") + " " + ALLTRIM(m.des_menuniv1) + "'"
*!*		    
*!*		      Define Pad &lcPad Of _MsysMenu Prompt &lcPromp  COLOR SCHEME 3   
*!*		   
*!*		   SELECT menu_niv02
*!*		   SELECT * FROM menu_niv02 ;
*!*		    WHERE ano_eje = gcano_eje;
*!*		    and RTRIM(usuario) == p_user ; 
*!*		    AND visible ="S" ;
*!*		    AND estado = 'A';
*!*		    and id_menuniv01 = m.id_menuniv01 ;
*!*		    INTO CURSOR Cur_menu_niv02 ORDER BY 1,2,3,4 
*!*		    
*!*		    
*!*		    SELECT Cur_menu_niv02
*!*		    COUNT ALL TO lnCountOpc
*!*		    GO TOP            
*!*		    SELECT menu_niv01
*!*		 
*!*		                lnContador=lnContador+1
*!*		                lcNamePopup ='_Niv01_' + m.id_menuniv01 + '_' +PADL(lnContador,2,'0')
*!*		                lcOnPad     ='_Niv01_' + m.id_menuniv01
*!*		                
*!*		              On Pad  &lcOnPad  Of _MsysMenu Activate Popup  &lcNamePopup
*!*		              Define Popup  &lcNamePopup   Margin Relative Shadow Color Scheme 4
*!*		                        i=1
*!*		                        SELECT Cur_menu_niv02
*!*		                        SCAN WHILE i<=lnCountOpc
*!*		                        i=i+1
*!*		                        =SEEK(Cur_menu_niv02.ano_eje+Cur_menu_niv02.sec_ejec+p_user + m.id_menuniv01+Cur_menu_niv02.ID_MENUNIV02,"menu_niv02","MENU_NIV2")
*!*		 
*!*		                         lcNumBar   =PADL(INT(i),2,'0')
*!*		                         
*!*		                         lcPrompBar =IIF(menu_niv02.estado="A","'","'\") + ALLTRIM(menu_niv02.des_menuniv2)  + "'"
*!*		                         Define Bar &lcNumBar  of  &lcNamePopup   Prompt &lcPrompBar 
*!*		                         
*!*		                         IF menu_niv02.tipo_nodoniv2="S" THEN                          
*!*		                             
*!*		                                SELECT * FROM menu_niv03 ;
*!*		                                 WHERE ano_eje = gcano_eje ;
*!*		                                 and RTRIM(usuario) == p_user ; 
*!*		                                 and id_menuniv01 = m.id_menuniv01 ;
*!*		                                 AND id_menuniv02 = menu_niv02.id_menuniv02 ;
*!*		                                 AND visible = "S" ;
*!*		                                 AND estado = 'A';
*!*		                                 INTO CURSOR Cur_menu_niv03 ORDER BY 1,2,3,4,5
*!*		                                 SELECT Cur_menu_niv03
*!*		                                    COUNT ALL TO lnCountOpc3                                  
*!*		                             
*!*		                             lcNamePopup2 = lcNamePopup + "_" + padl(INT(i),2,'0')
*!*									  lcNumBar =PADL(INT(i),2,'0')
*!*		                              ON BAR  &lcNumBar  of   &lcNamePopup  ACTIVATE POPUP  &lcNamePopup2
*!*		                              DEFINE POPUP  &lcNamePopup2   MARGIN RELATIVE SHADOW COLOR SCHEME 4
*!*		                              SELECT Cur_menu_niv03
*!*		                              GO TOP 
*!*		                              i3=1
*!*		                              SCAN WHILE i3<=lnCountOpc3
*!*		                              
*!*		                                    =SEEK(Cur_menu_niv03.ano_eje+Cur_menu_niv03.sec_ejec+p_user + m.id_menuniv01+menu_niv02.id_menuniv02+Cur_menu_niv03.ID_MENUNIV03,"menu_niv03","id_menu")
*!*		                                    i3=i3+1
*!*											lcNumBar = PADL(INT(i3),2,'0')
*!*		                                    lcPromp = IIF(menu_niv03.estado="A","'","'\")  + ALLTRIM(menu_niv03.des_menuniv3)  + "'"
*!*		                                    Define Bar  &lcNumBar   of  &lcNamePopup2   Prompt  &lcPromp 
*!*		                                    lcMenuOpcion = " " + ALLTRIM(menu_niv03.ejecutaniv3) 
*!*		                                     
*!*		                                    ON SELECTION BAR  &lcNumBar   of  &lcNamePopup2 &lcMenuOpcion                                                                                   
*!*		                              ENDSCAN  
*!*		                         ELSE                         
*!*		                             lcNumBar = PADL(INT(i),2,'0')
*!*		                             lcMenuOpcion = ' ' + ALLTRIM(menu_niv02.ejecutaniv2) 
*!*		                              ON SELECTION BAR  &lcNumBar   of  &lcNamePopup &lcMenuOpcion  
*!*		                         ENDIF 
*!*		                        ENDSCAN 
*!*		                        SELECT menu_niv01
*!*		 
*!*			ENDSCAN 
*!*		 
*!*			USE IN menu_niv01 
*!*			USE IN menu_niv02
*!*			USE IN menu_niv03
*!*			 
*!*			SET SYSMENU  SAVE 


*!*	ENDPROC 


********************
PROCEDURE DeclaraTodo
********************
	PUBLIC gcAno_Eje,gcSec_Ejec, gcTipo_Unidad,GCTIPO_UE, gcOldCaption,gcOldClassLib,gcOldDir,gcOldEscape,;
	gcOldPath,gcOldTalk,gcMonto,gcFileIni  ,gcSession , gcOpcRpt, gcFiltroPca, gcMes, gcRutaSiaf, gcRutaCert, glmuestra_fuente_agregada, gcnom_fuente ,gcnom_fuente_abrev,;
	gcCab_mod, gcId_rep, gcVersion ,gcTipo_reporte, gcclaveusr, gcuserid, gcEncriptaDesencripta, gcLlave, gces_supervisor, gcClave, gcActivo,;
	gcSubNivel01, gcSubNivel02, gcAbrevSubNivel01, gcAbrevSubNivel02, gchoja_calculo,P_USER, gcRutaOk, gcRutaSystem, gcfuncion_entidad, gcmes_apertura, gcmes_cierre, gcIgv,;
	gcRutaInterfase, gcNombre_ejecutora, gcSector, gcPliego, gcSec_ejec_pliego,lcArchivoFrame, gcVersion_office, gcCaja, glParamNET, glSystemApp, glFoxy, gcTipo_rendicion, gcProyecto,;
	gcOrganismo, gcRuta_app, gcCarpetaListado, gcClaveMenu, gcLlaveMenu, gcLlave,gcAmbito,gcFondoPantalla, gcfase, gcUe_envio,gcrutacli
	

	

	#INCLUDE include\excel.h
	gcLLave		 = LLKEY
	gcllaveMenu  = ''

	*-- Instrucciones DECLARE DLL para leer/escribir en archivos INI privados-
	DECLARE INTEGER GetPrivateProfileString IN Win32API  AS GetPrivStr ;
		String cSection, String cKey, String cDefault, String @cBuffer, ;
		Integer nBufferSize, String cINIFile

	DECLARE INTEGER WritePrivateProfileString IN Win32API AS WritePrivStr ;
		String cSection, String cKey, String cValue, String cINIFile
	gcMes=""
	glSystemApp = .T.
	glFoxy = .T.
	gcOpcRpt='EST_V'	
	gcOldCaption=""
	gcOldDir=CURDIR()
	gcOldPath=fullpath(CURDIR())
	gcOldEscape="ON"
	gcOldTalk="ON"
	gcFileIni = 'SYSSOPORTE.INI'
	gcSession = "PARAMETROS DEL SINCRONIZADOR"
	gcnom_fuente = 'RUBRO'
	gcnom_fuente_abrev = 'RB'
	glmuestra_fuente_agregada = .T.
	gcTipo_Unidad='E'
	GCTIPO_UE='01'
	gcMonto = ''
	gcVersion = '1.0'
	gcCab_mod = 'SINCRONIZADOR'
	gcuserid = ''
	gcClave = ''
	gcActivo= 0
	gchoja_calculo = 'xls'
	gcRutaOk = .T.
	gcfuncion_entidad = '01'
	gcRutaInterfase = ''
	gcProyecto = '000'
	gcOrganismo = '000'
	gcCarpetalistado = 'LISTADOS'
	gcfase = ''
	gcUe_envio = 'P'
ENDPROC


PROCEDURE SETEAR_PANTALLA
	*** SETEANDO DATOS DE LA PANTALLA
	gcNombre_ejecutora = ''
	USE siaf!ejecutora	IN 0 
	IF SEEK(gcano_eje+gcsec_ejec,'ejecutora','sec_ejec') THEN 
		gcNombre_ejecutora  = 	ejecutora.nombre 
		gcSector			= 	ejecutora.sector
		gcPliego			= 	ejecutora.pliego
		gcsec_ejec_pliego   = 	ejecutora.sec_ejec
		
	ENDIF 
	USE IN ejecutora
	_screen.Caption 	= 'SYSSOPORTE ' +gcversion+space(2)+CHR(91)+gcsec_ejec+CHR(93)+SPACE(2)+RTRIM(gcNombre_ejecutora)+SPACE(2)+' '+'['+gcano_eje+']'
	_screen.BorderStyle = 3
	_screen.Closable = .T.
	_SCREEN.MaxButton =.T.
	_SCREEN.MinButton = .T.
	_SCREEN.CONTROLBOX=.T.
	_screen.AlwaysOnTop = .F. 
	_screen.WindowState = 2
	_screen.AutoCenter =.T.
		
	CLEAR
	IF GCSEC_EJEC='001736' THEN 
		gcFondoPantalla = "FONDO.JPG"
	ENDIF 
	@0,0 SAY gcFondoPantalla BITMAP CENTER





PROCEDURE CHECK_INI
	lcArchivo = 'SYSSOPORTE.INI'
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
					gcsec_ejec_pliego = gcsec_ejec
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
				CASE 'CLAVE' $ UPPER(lcString)
					gcClave = SUBSTR(lcString,AT('=',lcString)+1)
				CASE 'FONDO' $ UPPER(lcString)
					gcFondoPantalla = SUBSTR(lcString,AT('=',lcString)+1)						
			ENDCASE
		ENDDO
		=FCLOSE(lnFileHandle)
	ELSE
		MESSAGEBOX('No Existe el Archivo de Configuración.',0+64,'Aviso')
	ENDIF
ENDPROC 

PROCEDURE CHECK_ESTRUCTURA

lcruta	= SYS(5)+SYS(2003)

DIMENSION ladirectory(3)

STORE lcruta+'\Listados' 	TO ladirectory(1)
STORE lcruta+'\XML' 	 	TO ladirectory(2)
STORE lcruta+'\INTERFASE'  TO ladirectory(3)


FOR i=1 TO 3
IF !DIRECTORY(ladirectory(i)) THEN 
		MKDIR &ladirectory(i).
	ENDIF 
ENDFOR 

ENDPROC 

PROCEDURE SalirSistema
	CLEAR EVENTS
	RELEASE ALL 
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


FUNCTION Save_Log
PARAMETERS pErrorInfo
cFileLog = "syssoporte.log"
IF FILE(cFileLog ) THEN 
	XMLTOCURSOR(cFileLog,"syssoporte_log",512)
ELSE
	CREATE CURSOR syssoporte_log(dFecha datetime, cErrorInfo m)
ENDIF
INSERT INTO syssoporte_log(dFecha , cErrorInfo) VALUES (DATETIME(),  pErrorInfo)
CURSORTOXML("syssoporte_log",cFileLog ,1,512,0,"1" )
RETURN .T.

FUNCTION ReporteErrores(toExc AS EXCEPTION, tnLevel)
	LOCAL lcErrorInfo, lcPadding
	lcPadding = REPLICATE(CHR(9), tnLevel)
	lcErrorInfo = lcPadding + [Exception Object ]  + TRANSFORM(tnLevel) + [:]+ CHR(13) + ;
		lcPadding + [  ErrorNo: ] + TRANSFORM(toExc.ERRORNO)  + CHR(13) + ;
		lcPadding + [  LineNo: ] + TRANSFORM(toExc.LINENO)   + CHR(13) + ;
		lcPadding + [  Message: ] + toExc.MESSAGE   + CHR(13) + ;
		lcPadding + [  Procedure: ] + toExc.PROCEDURE   + CHR(13) + ;
		lcPadding + [  Detalles: ] + toExc.DETAILS   + CHR(13) + ;
		lcPadding + [  StackLevel: ] + TRANSFORM(toExc.STACKLEVEL)   + CHR(13) + ;
		lcPadding + [  LineContents: ] + toExc.LINECONTENTS   + CHR(13) + ;
		lcPadding + [  Comments: ] + toExc.COMMENT   + CHR(13) + CHR(13)
	IF TYPE("toExc.UserValue") = "O"
		lcErrorInfo = lcErrorInfo + ReporteErrores(toExc.USERVALUE, tnLevel + 1)
	ENDIF
	=Save_Log(lcErrorInfo)
	RETURN lcErrorInfo
ENDPROC && ReporteErrores


FUNCTION ReporteErrores(toExc AS EXCEPTION, tnLevel)
	LOCAL lcErrorInfo, lcPadding
	lcPadding = REPLICATE(CHR(9), tnLevel)
	lcErrorInfo = lcPadding + [Exception Object ]  + TRANSFORM(tnLevel) + [:]+ CHR(13) + ;
		lcPadding + [  ErrorNo: ] + TRANSFORM(toExc.ERRORNO)  + CHR(13) + ;
		lcPadding + [  LineNo: ] + TRANSFORM(toExc.LINENO)   + CHR(13) + ;
		lcPadding + [  Message: ] + toExc.MESSAGE   + CHR(13) + ;
		lcPadding + [  Procedure: ] + toExc.PROCEDURE   + CHR(13) + ;
		lcPadding + [  Detalles: ] + toExc.DETAILS   + CHR(13) + ;
		lcPadding + [  StackLevel: ] + TRANSFORM(toExc.STACKLEVEL)   + CHR(13) + ;
		lcPadding + [  LineContents: ] + toExc.LINECONTENTS   + CHR(13) + ;
		lcPadding + [  Comments: ] + toExc.COMMENT   + CHR(13) + CHR(13)
	IF TYPE("toExc.UserValue") = "O"
		lcErrorInfo = lcErrorInfo + ReporteErrores(toExc.USERVALUE, tnLevel + 1)
	ENDIF
	=Save_Log(lcErrorInfo)
	RETURN lcErrorInfo
ENDPROC && ReporteErrores
