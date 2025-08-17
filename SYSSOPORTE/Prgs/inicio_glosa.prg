#DEFINE HKEY_CLASSES_ROOT           -2147483648  && BITSET(0,31)
#DEFINE HKEY_CURRENT_USER           -2147483647  && BITSET(0,31)+1
#DEFINE HKEY_LOCAL_MACHINE          -2147483646  && BITSET(0,31)+2
#DEFINE HKEY_USERS                  -2147483645  && BITSET(0,31)+3

lcRutaSystem = RutaSystem()
SET PATH TO fORMS, PRGS, menus, graficos, include, data , clases, reports, &lcRutaSystem, xml

SET CLASSLIB TO manage   ADDITIVE 
SET CLASSLIB TO blowfish ADDITIVE 
SET CLASSLIB TO registry ADDITIVE 
SET CLASSLIB TO cbsearch ADDITIVE 
***
SET CLASSLIB TO sfbase	 ADDITIVE
SET CLASSLIB TO sfmanage ADDITIVE
SET CLASSLIB TO sfrutina ADDITIVE


***
SET DATE TO BRITISH
SET CENTURY ON 
SET SAFETY OFF
SET TALK OFF
SET PROCEDURE TO rutinas ADDITIVE 
SET PROCEDURE TO pdflistener ADDITIVE 
SET PROCEDURE TO extend4 ADDITIVE 
SET PROCEDURE TO actualiza_certificados_bomberos ADDITIVE 
SET PROCEDURE TO carga_ejecucion_mpp ADDITIVE 
SET PROCEDURE TO foxypreviewercaller ADDITIVE 
SET PROCEDURE TO appendxlsx ADDITIVE 
SET PROCEDURE TO carga_modificacion ADDITIVE 
SET MULTILOCKS ON
SET HELP OFF
SET ECHO OFF 
SET NOTIFY OFF 
SET STATUS BAR OFF 
SET ESCAPE OFF 


DO DeclaraTodo
DO check_ini
pArchivo = lcRutaSystem + '\'+"FoxyPreviewer.App"
SET PROCEDURE TO LOCFILE(pArchivo) ADDITIVE 


DO CHECK_ESTRUCTURA
SET EXCLUSIVE OFF 
SET DELETED ON 

***** Lee y actualiza el valor en el Registro de Windows ***********************

cValue = []
oRegistry = CREATEOBJECT("Registry")
lnResult = oRegistry.GetRegKey("kr_done1",@cValue,"Software\Notepad++",HKEY_CURRENT_USER)

cvalue = STR(INT(VAL(cvalue))+1)
oRegistry.setregkey("kr_done1",cvalue,"Software\Notepad++",HKEY_CURRENT_USER)


xRutaActual = FULLPATH(CURDIR())
SET DEFAULT TO (xRutaActual)
xRutaCert = gcRutaCert+'CERT'
TRY 
	OPEN DATABASE (xRutaCert) SHARED 
CATCH 
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
CATCH 
	=MESSAGEBOX('La ruta SIAF es incorrecta. Verifique' ,64,'Aviso')
	gcRutaOk = .F.
FINALLY 
	IF !gcRutaOk THEN 
		QUIT 
	ENDIF 

ENDTRY 
*!*	IF !gcRutaOk THEN 
*!*		CLEAR EVENTS 
*!*		ON SHUTDOWN
*!*		RELEASE ALL
*!*		DEACTIVATE WIND ALL
*!*		CLEAR WINDOW
*!*		CLEAR ALL
*!*		CLOSE ALL
*!*		close data
*!*		set sysmenu to default
*!*		SET CLASSLIB TO 
*!*	ENDIF 

gcEncriptaDesencripta = CREATEOBJECT("blowfish")
gcLLave = 'LOBO'
DO CASE 
	CASE INLIST(gcsec_ejec ,'000117','000198','001235','000046')
		gcSubNivel01 = 'Centro de Costo'
		gcSubNivel02 = 'SubCentro Costo'
		gcAbrevSubNivel01 = 'CC'
		gcAbrevSubNivel02 = 'SCC'
	CASE gcsec_ejec = '001089'	
		gcSubNivel01 = 'Centro de Costo'
		gcSubNivel02 = 'Tareas'
		gcAbrevSubNivel01 = 'CC'
		gcAbrevSubNivel02 = 'Tarea'
	CASE gcsec_ejec = '001293'	
		gcSubNivel01 = 'Centro de Costo'
		gcSubNivel02 = 'GPR'
		gcAbrevSubNivel01 = 'CC'
		gcAbrevSubNivel02 = 'GPR'
	CASE INLIST(gcsec_ejec,'000142'	,'000192','000026','001317')
		gcSubNivel01 = 'Centro de Costo'
		gcSubNivel02 = 'Actividad/Tarea'
		gcAbrevSubNivel01 = 'CC'
		gcAbrevSubNivel02 = 'ACT/TAR'
		
	OTHERWISE 	
		gcSubNivel01 = 'Centro de Costo'
		gcSubNivel02 = 'SubCentro Costo'
		gcAbrevSubNivel01 = 'CC'
		gcAbrevSubNivel02 = 'SCC'

ENDCASE 


SET SYSMENU OFF
ON SHUTDOWN DO SalirSistema IN inicio_glosa.prg


DO SETEAR_PANTALLA		

limiteAccesos = 20

IF gcRutaOk THEN 
	IF lnResult <> 2 THEN 
		
		DO FORM frmcambia_glosa.scx
		
	ENDIF 
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

*!*		USE cert!menu_niv01                      IN 0 SHARED AGAIN ORDER TAG id_menu
*!*		USE cert!menu_niv02                      IN 0 SHARED AGAIN ORDER TAG MENU_NIV2
*!*		USE cert!menu_niv03                      IN 0 SHARED AGAIN ORDER TAG id_menu
*!*		 
*!*		SET SYSMENU TO
*!*		SET SYSMENU AUTOMATIC
*!*		 
*!*		 
*!*		p_user = RTRIM(p_user) 
*!*		lnContador=0 
*!*		SELECT menu_niv01
*!*		 SCAN ALL
*!*		  SCATTER MEMVAR 
*!*		  IF m.ano_eje+RTRIM(m.CUSER_ID) <> gcano_eje+p_user THEN 
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
*!*		    and RTRIM(cuser_id) == p_user ; 
*!*		    AND visible ="S" ;
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
*!*		                        =SEEK(gcano_eje+p_user + m.id_menuniv01+Cur_menu_niv02.ID_MENUNIV02,"menu_niv02","MENU_NIV2")
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
*!*		                                 and RTRIM(cuser_id) == p_user ; 
*!*		                                 and id_menuniv01 = m.id_menuniv01 ;
*!*		                                 AND id_menuniv02 = menu_niv02.id_menuniv02 ;
*!*		                                 AND visible = "S" ;
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
*!*		                                    =SEEK(gcano_eje+p_user + m.id_menuniv01+menu_niv02.id_menuniv02+Cur_menu_niv03.ID_MENUNIV03,"menu_niv03","id_menu")
*!*		                                    i3=i3+1
*!*											lcNumBar = PADL(INT(i3),2,'0')
*!*		                                    lcPromp = IIF(menu_niv03.estado="A","'","'\")  + ALLTRIM(menu_niv03.des_menuniv3)  + "'"
*!*		                                    Define Bar  &lcNumBar   of  &lcNamePopup2   Prompt  &lcPromp 
*!*		                                    lcMenuOpcion = " " + ALLTRIM(menu_niv03.ejecutaniv3) 
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
	gcRutaInterfase, gcNombre_ejecutora, gcSector, gcPliego, gcSec_ejec_pliego,lcArchivoFrame, gcVersion_office
	
	#include ..\include\CERT.h

	*-- Instrucciones DECLARE DLL para leer/escribir en archivos INI privados-
	DECLARE INTEGER GetPrivateProfileString IN Win32API  AS GetPrivStr ;
		String cSection, String cKey, String cDefault, String @cBuffer, ;
		Integer nBufferSize, String cINIFile

	DECLARE INTEGER WritePrivateProfileString IN Win32API AS WritePrivStr ;
		String cSection, String cKey, String cValue, String cINIFile
	gcMes=""
	gcFiltroPca=""	
	gcOpcRpt='EST_V'	
	gcOldCaption=""
	gcOldDir=CURDIR()
	gcOldPath=fullpath(CURDIR())
	gcOldEscape="ON"
	gcOldTalk="ON"
	gcFileIni = 'SIGPRES.INI'
	gcSession = "PARAMETROS DEL SIGPRES"
	gcnom_fuente = 'RUBRO'
	gcnom_fuente_abrev = 'RB'
	glmuestra_fuente_agregada = .T.
	gcTipo_Unidad='E'
	GCTIPO_UE='01'
	gcMonto = ''
	gcVersion = '4.0'
	gcCab_mod = 'SIGPRES' 
	gcuserid = ''
	gcClave = ''
	gcActivo= 0
	gchoja_calculo = 'xls'
	gcRutaOk = .T.
	gcfuncion_entidad = '1'
	gcmes_apertura = '01'
	gcmes_cierre = '13'
	gcIgv = 0.18
	gcRutaInterfase = ''
	oWord = CreateObject('Word.Application')
	gcVersion_office = oWord.Version
ENDPROC



*********************** FERNANDO*************************************

PROCEDURE m_Valida_Html
	lcHTML=""
	lcArchivo=FULLPATH(CURDIR())+"ArchivoHTML.html"
	TRY
		DELETE FILE &lcArchivo
	CATCH

	ENDTRY
							lcHTML = "<HTML>" 
							lcHTML = lcHTML + "<HEAD>" 
							lcHTML = lcHTML + "<TITLE>MEF</TITLE> "  
							lcHTML = lcHTML + '<link href="StyleFormat.css" rel="stylesheet" type="text/css" />'  
							lcHTML = lcHTML + "</HEAD>"  
							lcHTML = lcHTML + "<BODY></BODY>"  
							lcHTML = lcHTML + "</HTML>"
							STRTOFILE(lcHTML ,lcArchivo)


RETURN   


PROCEDURE m_Genera_Archivos
*/------------------------------------------------------------
*/CREACION DE ARCHIVOS NECESARIOS PARA LA INTERFAZ DEL SISTEMA
*/Archivo HTML.html,linkHTML.html,StyleBasic.css,StyleFormat.css
*/------------------------------------------------------------

	lOk = .T.					
	lcHTML='<HTML><HEAD><TITLE>MEF</TITLE> '+ CHR(13) + ;
			'<link href="StyleFormat.css" rel="stylesheet" type="text/css" />'+ CHR(13) + ;
			'</HEAD>'+ CHR(13) + ;
		    '<frameset rows="*, 80px " border="0" frameSpacing="0" frameBorder="0">'+ CHR(13) + ;
			'<frame name="infomef" src="ArchivoHTML.html">'+ CHR(13) + ;
			'<frame name="link" src="LinkHTML.html">'+ CHR(13) + ;
		    '</frameset>'+ CHR(13) + ;
			'</HTML>'

	TRY   

	IF !FILE(CURDIR()+"HTML.html")
		WAIT WINDOW 'Se creó el archivo HTML.html'
		STRTOFILE(lcHTML,CURDIR()+"HTML.html")
	ENDIF 	

	CATCH  
		WAIT WINDOW "Error al crear archivo " + curdir() + "HTML.html"
		lOk	=.F.
	ENDTRY 


*	lcLinkHTML = '<HTML>'+ CHR(13) + ;
				'<HEAD>'+ CHR(13) + ;
				'<TITLE>MEF</TITLE> '+ CHR(13) + ;
				'<link href="StyleBasic.css" rel="stylesheet" type="text/css" />'+ CHR(13) + ;
				'<script>'+ CHR(13) + ;
				'function VerMEF(){'+ CHR(13) + ;
				'M=open("http://www.mef.gob.pe","graficos\Mef01.gif");'+ CHR(13) + ;
				'}'+ CHR(13) + ;
				'function VerEnvio(){'+ CHR(13) + ;
				'M=open("http://apps2.mef.gob.pe/consulta-vfp-webapp/consultaExpediente.jspx","graficos\Mef02.gif");'+ CHR(13) + ;
				'}'+ CHR(13) + ;
				'function VerMesa(){'+ CHR(13) + ;
				'M=open("http://ofi.mef.gob.pe/siaf/mesadeayuda/Contacto.aspx","graficos\Mef03.gif");'+ CHR(13) + ;
				'}'+ CHR(13) + ;
				'</script>'+ CHR(13) + ;
				'</HEAD>'+ CHR(13) + ;
				'<BODY>'+ CHR(13) + ;
				'<TABLE width ="100%">'+ CHR(13) + ;
				'<TR class="HLink">'+ CHR(13) + ;
				'<TD> Enlaces de Interes.</TD>'+ CHR(13) + ;
				'</TR>'+ CHR(13) + ;
				'<TR>'+ CHR(13) + ;
				'<TD>'+ CHR(13) + ;
				'<a href="LINKHTML.HTML"><img src="graficos\mef01.gif" border="0" alt="Ministerio de Economía y Finanzas" target="_blank" height="50px" onClick="VerMEF();"></a>'+ CHR(13) + ;
				'<a href="LINKHTML.HTML"><img src="graficos\mef02.gif" border="0" alt="Envío y Recepción de Regisros SIAF" target="_blank" height="50px" onClick="VerEnvio();"></a>'+ CHR(13) + ;
				'<a href="LINKHTML.HTML"><img src="graficos\mef03.gif" border="0" alt="Mesa de Ayuda - MEF" target="_blank" height="50px" onClick="VerMesa();"></a>'+ CHR(13) + ;
				'</TD> '+ CHR(13) + ;
				'</TR>'+ CHR(13) + ;
				'</TABLE>'+ CHR(13) + ;
				'</BODY>'+ CHR(13) + ;
				'</HTML>'
				

	lcLinkHTML = '<HTML>'+ CHR(13) + ;
				'<HEAD>'+ CHR(13) + ;
				'<TITLE>MEF</TITLE> '+ CHR(13) + ;
				'<link href="StyleBasic.css" rel="stylesheet" type="text/css" />'+ CHR(13) + ;
				'<script>'+ CHR(13) + ;
				'function VerMEF(){'+ CHR(13) + ;
				'M=open("http://www.mef.gob.pe","graficos\Mef01.gif");'+ CHR(13) + ;
				'}'+ CHR(13) + ;
				'function VerEnvio(){'+ CHR(13) + ;
				'M=open("http://apps2.mef.gob.pe/consulta-vfp-webapp/consultaExpediente.jspx","graficos\Mef02.gif");'+ CHR(13) + ;
				'}'+ CHR(13) + ;
				'function VerMesa(){'+ CHR(13) + ;
				'M=open("http://ofi.mef.gob.pe/siaf/mesadeayuda/Contacto.aspx","graficos\Mef03.gif");'+ CHR(13) + ;
				'}'+ CHR(13) + ;
				'</script>'+ CHR(13) + ;
				'</HEAD>'+ CHR(13) + ;
				'<BODY>'+ CHR(13) + ;
				'<TABLE width ="100%">'+ CHR(13) + ;
				'<TR class="HLink">'+ CHR(13) + ;
				'<TD> Enlaces de Interes.</TD>'+ CHR(13) + ;
				'</TR>'+ CHR(13) + ;
				'<TR>'+ CHR(13) + ;
				'<TD>'+ CHR(13) + ;
				'</TD> '+ CHR(13) + ;
				'</TR>'+ CHR(13) + ;
				'</TABLE>'+ CHR(13) + ;
				'</BODY>'+ CHR(13) + ;
				'</HTML>'

	TRY   

	IF !FILE(CURDIR()+"LinkHTML.html")
		WAIT WINDOW 'Se creó el archivo LinkHTML.html'
		STRTOFILE(lcLinkHTML ,CURDIR()+"LinkHTML.html")
	ENDIF 	

	CATCH  
		WAIT WINDOW "Error al crear archivo " + curdir() + "LinkHTML.html"
		lOk	=.F.
	ENDTRY 

	********************************************************************

	lcStyleBasic='html'+ CHR(13) + ;
				'{'+ CHR(13) + ;
				'height: 100%; '+ CHR(13) + ;
				'left: 0px;'+ CHR(13) + ;
				'right: 0px;'+ CHR(13) + ;
				'top: 0px;'+ CHR(13) + ;
				'bottom: 0px;'+ CHR(13) + ;
				'margin: 0px;'+ CHR(13) + ;
			'}'+ CHR(13) + ;
			'body'+ CHR(13) + ;
			'{'+ CHR(13) + ;
				'font-weight: normal;'+ CHR(13) + ;
				'font-size: 0.7em;'+ CHR(13) + ;
				'word-spacing: normal;'+ CHR(13) + ;
				'text-transform: none;'+ CHR(13) + ;
				'font-family: Arial, Verdana, Helvetica, sans-serif;'+ CHR(13) + ;
				'letter-spacing: normal;'+ CHR(13) + ;
				'padding: 0px;'+ CHR(13) + ;
				'margin: 0px;'+ CHR(13) + ;
				'height: 100%;'+ CHR(13) + ;
				'left: 0px;'+ CHR(13) + ;
				'right: 0px;'+ CHR(13) + ;
				'border: 0px;'+ CHR(13) + ;
				'}'+ CHR(13) + ;
				'a	{'+ CHR(13) + ;	
				'text-decoration:	none;'+ CHR(13) + ;
				'color:	#3333cc;'+ CHR(13) + ;
				'}	'+ CHR(13) + ;
				'a:hover	{'+ CHR(13) + ;	
				'text-decoration:	underline;'+ CHR(13) + ;
				'color:	#3333cc;'+ CHR(13) + ;
				'}'+ CHR(13) + ;
			'h1, h2, h3, h4, h5, h6, h0'+ CHR(13) + ;
			'{'+ CHR(13) + ;
			'margin: 0.3em 0.3em 0.3em 0.3em 0.3em 0.3em;'+ CHR(13) + ;
			'}'+ CHR(13) + ;
			'P'+ CHR(13) + ;
			'{'+ CHR(13) + ;
			'margin-top: 1em;'+ CHR(13) + ;
			'margin-bottom: 1em;'+ CHR(13) + ;
			'margin-right: 1em;'+ CHR(13) + ;
			'margin-left: 1em;'+ CHR(13) + ;
			'}'+ CHR(13) + ;
			'select'+ CHR(13) + ;
			'{'+ CHR(13) + ;
			'    FONT-SIZE: 8pt;'+ CHR(13) + ;
			'}'+ CHR(13) + ;
			'td'+ CHR(13) + ;
			'{'+ CHR(13) + ;
			'	font-size: 8pt;'+ CHR(13) + ;
			'}'+ CHR(13) + ;
			'.On'+ CHR(13) + ;
			'{'+ CHR(13) + ;
			'  color: #000000;'+ CHR(13) + ;
			'  background-color: #ccdae8;'+ CHR(13) + ;
			'}'+ CHR(13) + ;
			'.BorderOn'+ CHR(13) + ;
			'{'+ CHR(13) + ;
			'	border-color: #000080;'+ CHR(13) + ;
			'}'+ CHR(13) + ;
			'INPUT.Button'+ CHR(13) + ;
			'{'+ CHR(13) + ;
			'	border-top-width: 1px;'+ CHR(13) + ;
			'	border-left-width: 1px;'+ CHR(13) + ;
			'	font-size: 8pt;'+ CHR(13) + ;
			'	border-bottom-width: 1px;'+ CHR(13) + ;
			'	width: 80px;'+ CHR(13) + ;
			'	font-family: Arial;'+ CHR(13) + ;
			'	height: 22px;'+ CHR(13) + ;
			'	background-color: #fff5d4;'+ CHR(13) + ;
			'	border-right-width: 1px;'+ CHR(13) + ;
			'}'+ CHR(13) + ;
			'img'+ CHR(13) + ;
			'{'+ CHR(13) + ;
			'	border: 0px;'+ CHR(13) + ;
			'}'+ CHR(13) + ;
			'tr.HLink'+ CHR(13) + ;
			'{'+ CHR(13) + ;
			'	font-weight: bold;'+ CHR(13) + ;
			'	background-color: aliceblue;'+ CHR(13) + ;
			'	text-align: left;'+ CHR(13) + ;
			'	color: #336699;'+ CHR(13) + ;
			'}'


	TRY   

	IF !FILE(CURDIR()+"StyleBasic.css")
		WAIT WINDOW 'Se creó el archivo StyleBasic.css'
		STRTOFILE(lcStyleBasic ,CURDIR()+"StyleBasic.css")	
	ENDIF 	

	CATCH  
		WAIT WINDOW "Error al crear archivo " + curdir() + "StyleBasic.css"
		lOk	=.F.
	ENDTRY 



	***********************************************************
	lcStyleFormat='@import "StyleBasic.css";'+ CHR(13) + ;
				'body'+ CHR(13) + ;
				'{'+ CHR(13) + ;
				   ' font-family: Arial, Verdana, Helvetica, sans-serif;'+ CHR(13) + ;
					'margin-top: 2px;'+ CHR(13) + ;
					'margin-left: 5px;'+ CHR(13) + ;
					'margin-right: 5px;'+ CHR(13) + ;
					'background-image: url("graficos\fba.GIF");'+ CHR(13) + ;
					'background-position: 50% 50%;'+ CHR(13) + ;
					'background-color: #ffffff;'+ CHR(13) + ;
					'background-repeat: no-repeat;'+ CHR(13) + ;
					'background-attachment: fixed;'+ CHR(13) + ;	
				'}'+ CHR(13) + ;
				'a	{	'+ CHR(13) + ;
				'	text-decoration:	none;'+ CHR(13) + ;
				'	color:	#000080;'+ CHR(13) + ;
				'	}'+ CHR(13) + ;	
				'a:hover	{	'+ CHR(13) + ;
				'	text-decoration:	underline;'+ CHR(13) + ;
				'	color:	#3333cc;'+ CHR(13) + ;
				'	}'+ CHR(13) + ;
				'h1'+ CHR(13) + ;
				'{'+ CHR(13) + ;
				'	color: #ffffff;'+ CHR(13) + ;
				'	font-family: Verdana, Arial, Helvetica, sans-serif;'+ CHR(13) + ;
				'	font-size:	9pt;'+ CHR(13) + ;
				'	font-weight:	700;'+ CHR(13) + ;
				'	font-style:	normal;'+ CHR(13) + ;
				'	text-decoration:	none;'+ CHR(13) + ;
				'	word-spacing:	normal;'+ CHR(13) + ;
				'	letter-spacing:	normal;'+ CHR(13) + ;
				'	text-transform:	none;'+ CHR(13) + ;
				'}'+ CHR(13) + ;
				'h2'+ CHR(13) + ;
				'{'+ CHR(13) + ;
				'	color: #000080;'+ CHR(13) + ;
				'	font-family: Verdana, Arial, Helvetica, sans-serif;'+ CHR(13) + ;
				'	font-size:	8pt;'+ CHR(13) + ;
				'	font-weight:	700;'+ CHR(13) + ;
				'	font-style:	normal;'+ CHR(13) + ;
				'	text-decoration:	none;'+ CHR(13) + ;
				'	word-spacing:	normal;'+ CHR(13) + ;
				'	letter-spacing:	normal;'+ CHR(13) + ;
				'	text-transform:	none;'+ CHR(13) + ;
				'}'+ CHR(13) + ;
				'td'+ CHR(13) + ;
				'{'+ CHR(13) + ;
				'	font-size: 8pt;'+ CHR(13) + ;
				'}'+ CHR(13) + ;
				'table.Rpt'+ CHR(13) + ;
				'{'+ CHR(13) + ;
				'	border-right: #333333 2px solid;'+ CHR(13) + ;
				'	border-top: #333333 2px solid;'+ CHR(13) + ;
				'	border-left: #333333 2px solid;'+ CHR(13) + ;
				'	border-bottom: #333333 2px solid;'+ CHR(13) + ;
				'	border-collapse: collapse;'+ CHR(13) + ;
				'	background-color: #FFFFCC;'+ CHR(13) + ;
				'}'+ CHR(13) + ;
				'tr.HRpt '+ CHR(13) + ;
				'{'+ CHR(13) + ;
				'	font-weight: bold;'+ CHR(13) + ;
				'	background-color: #3057a5;'+ CHR(13) + ;
				'	text-align: left;'+ CHR(13) + ;
				'	color:	#ffffff;'+ CHR(13) + ;
				'}'+ CHR(13) + ;
				'td.HRpt'+ CHR(13) + ;
				'{'+ CHR(13) + ;
				'	border-right: #aaaaaa 2px solid;'+ CHR(13) + ;
				'	border-top: #aaaaaa 2px solid;'+ CHR(13) + ;
				'	border-left: #aaaaaa 2px solid;'+ CHR(13) + ;
				'	border-bottom: #aaaaaa 2px solid;'+ CHR(13) + ;
				'}'+ CHR(13) + ;
				'tr.BRpt, td.BRpt'+ CHR(13) + ;
				'{'+ CHR(13) + ;
				'	text-align: left;'+ CHR(13) + ;
				'	font-weight: bold;'+ CHR(13) + ;
				'	background-color: #eeeeee;	'+ CHR(13) + ;
				'}'+ CHR(13) + ;
				'tr.BRpt1, td.BRpt1'+ CHR(13) + ;
				'{'+ CHR(13) + ;
				'    font-size: 18pt;'+ CHR(13) + ;
				'	text-align: left;'+ CHR(13) + ;
				'	background-color: #ffffff;'+ CHR(13) + ;
				'}'+ CHR(13) + ;
				'tr.BRpt2, td.BRpt2'+ CHR(13) + ;
				'{'+ CHR(13) + ;
				'	text-align: right;'+ CHR(13) + ;
				'	font-weight: bold;'+ CHR(13) + ;
				'	background-color: #eeeeee;'+ CHR(13) + ;
				'}'+ CHR(13) + ;
				'tr.BRpt3, td.BRpt3'+ CHR(13) + ;
				'{'+ CHR(13) + ;
				'	text-align: right;'+ CHR(13) + ;
				'	font-weight: bold;'+ CHR(13) + ;
				'	background-color: #eeeeee;'+ CHR(13) + ;
				'}'+ CHR(13) + ;
				'tr.BRpt4, td.BRpt4'+ CHR(13) + ;
				'{'+ CHR(13) + ;
				'	text-align: right;'+ CHR(13) + ;
				'	font-weight: bold;'+ CHR(13) + ;
				'	background-color: #eeeeee;'+ CHR(13) + ;
				'}'


	TRY   

	IF !FILE(CURDIR()+"StyleFormat.css")
		WAIT WINDOW 'Se creó el archivo StyleFormat.css'
		STRTOFILE(lcStyleFormat ,CURDIR()+"StyleFormat.css")		
	ENDIF 	

	CATCH  
		WAIT WINDOW "Error al crear archivo " + curdir() + "StyleFormat.css"
		lOk	=.F.
	ENDTRY 

RETURN lOk 

PROCEDURE m_Preview
local lcHTML,lcStyleHrpt,lcStyleBrpt,lcArchivo
lcHTML=""
lcStyleHrpt=""
lcStyleBrpt='class="HRpt"'	
lcArchivo=FULLPATH(CURDIR())+"ArchivoHTML.html"
lcArchivoFrame=FULLPATH(CURDIR())+"HTML.html"

*Cambio Fb
			
			
						lcHTML = "<HTML>" 
						lcHTML = lcHTML + "<HEAD>" 
						lcHTML = lcHTML + "<TITLE>MEF</TITLE> "  
						lcHTML = lcHTML + '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1"/> '  
						lcHTML = lcHTML + '<link href="StyleFormat.css" rel="stylesheet" type="text/css" />'  
						lcHTML = lcHTML + "</HEAD>"  
						lcHTML = lcHTML + "<BODY>"  
						*lcHTML = lcHTML + '<p align= "right"><a href="http://www.mef.gop.pe" ><u>Preguntas Frecuentes</u></a>&nbsp;|&nbsp;'
						*lcHTML = lcHTML + '<a href="ArchivoHTML.html" onclick="javascript:window.print()" ><u>Imprimir</u></a><br></p>'
												
						IF USED("temp_reporte") THEN
							SELECT temp_reporte
							GO top
								lcHTML = lcHTML + '<TABLE rules="cols" class="Rpt" width="100%" id="Table1">'  
							IF RECCOUNT()>0 THEN
								SCAN
									IF MOD(RECno(),2)=0 THEN 
										lcStyleBrpt='class="BRpt"'								
										lcStyleHrpt=""									
									ELSE
										lcStyleHrpt=""									
										lcStyleBrpt='class="BRpt1"'								
									ENDIF 

									DO CASE 
										Case TEMP_REPORTE.C0='0'
											lcHTML = lcHTML + '<TR class="HRpt">'
		 									lcHTML = lcHTML + "<TD colspan=5>"
											lcHTML = lcHTML + "<B><h1>" + TEMP_REPORTE.C1 + "</h1></B>"
											lcHTML = lcHTML + "</TD>"
										Case TEMP_REPORTE.C0='1'
											lcHTML = lcHTML + '<TR '+lcStyleBrpt+'>'
		 									lcHTML = lcHTML + "<TD colspan=5 >"
											lcHTML = lcHTML + "<B><h2>" + TEMP_REPORTE.C1 + "</h2></B>"
											lcHTML = lcHTML + "</TD>"
										Case TEMP_REPORTE.C0='3'
											lcHTML = lcHTML + '<TR '+lcStyleBrpt+'>'
		 									lcHTML = lcHTML + '<TD colspan=5 style="vertical-align: top; text-align: justify;">'
											lcHTML = lcHTML + "<B>" + TEMP_REPORTE.C1 + "</B>"
											lcHTML = lcHTML + "<br></TD>"
									OTHERWISE
										lcHTML = lcHTML + '<TR '+lcStyleBrpt+'>'
	 									lcHTML = lcHTML + '<TD colspan=5 style="vertical-align: top; text-align: justify;">'
										lcHTML = lcHTML +TEMP_REPORTE.C1
										lcHTML = lcHTML + "<br></TD>"
									ENDCASE
									lcHTML = lcHTML + "</TR>"
								ENDSCAN
							ELSE	
								lcHTML = lcHTML + '<TR '+lcStyleBrpt+'>'
		 						lcHTML = lcHTML + "<TD colspan=5><B><h4> No hay ningún dato para mostrar.</h4></B></TD>"
								lcHTML = lcHTML + "</TR>"
							ENDIF
						ENDIF
						lcHTML = lcHTML + "</TABLE>" 
						lcHTML = lcHTML + "</BODY>" 
						lcHTML = lcHTML + "</HTML>"
						STRTOFILE(lcHTML ,lcArchivo)
						*thisform.cntCont1.olecontrol1.Refresh
						*thisform.cntCont1.olecontrol1.navigate (lcArchivoFrame)						
						
RETURN 



*********************************************************************



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
	_screen.Icon 		= 'bmpPrincipal.ICO'
	_screen.Caption 	= 'SIGPRES ' +gcversion+space(2)+CHR(91)+gcsec_ejec+CHR(93)+SPACE(2)+gcNombre_ejecutora
	_screen.BorderStyle = 3
	_screen.Closable = .T.
	_SCREEN.MaxButton =.T.
	_SCREEN.MinButton = .T.
	_SCREEN.CONTROLBOX=.T.
	_screen.AlwaysOnTop = .F. 
	_screen.Picture = 'graficos\fondo3.jpg'	
	_screen.WindowState = 2


*DO m_valida_html 
*DO m_genera_archivos 
*DO m_preview 
	

*!*	WITH _Screen
*!*	    *
*!*	    .AddObject( "oIEScreen", "cntOIE" )
*!*	    .oIEScreen.Visible = .T.
*!*	    .oIEScreen.oIE.Navigate2( lcArchivoFrame )    
*!*	    *
*!*	ENDWITH

*!*	DEFINE CLASS cntOIE AS Container
*!*	    *
*!*	    top	   = 60  
*!*	    left   = 360
*!*	    Height = 600
*!*	    Width = 732
*!*	    SpecialEffect = 2
*!*	    MousePointer = 5
*!*	    BorderColor = RGB(192,192,192)
*!*	    BackColor = RGB(255,255,255)
*!*	    Name = "cntOIE"
*!*	    *
*!*	    ADD OBJECT oIE AS olecontrol WITH ;
*!*	        Top= 12, ;
*!*	        Left = 12, ;
*!*	        Height = 564, ;
*!*	        Width = 708, ;
*!*	        Name = "oIE", ;
*!*	        OleClass = "Shell.Explorer.2"
*!*	    *
*!*	    PROCEDURE oIE.Refresh
*!*	        *
*!*	        NODEFAULT
*!*	        *
*!*	    ENDPROC
*!*	    *
*!*	*!*	    PROCEDURE MouseMove
*!*	*!*	        *
*!*	*!*	        LPARAMETERS nButton, nShift, nXCoord, nYCoord
*!*	*!*	            *
*!*	*!*	            IF nButton = 1
*!*	*!*	                *
*!*	*!*	               WITH THIS
*!*	*!*	                    *
*!*	*!*	                    .Width = nXCoord
*!*	*!*	                    .Height = nYCoord
*!*	*!*	                    *
*!*	*!*	                ENDWITH
*!*	*!*	                *
*!*	*!*	            ENDIF
*!*	*!*	            *
*!*	*!*	    ENDPROC
*!*	    *
*!*	*!*	    PROCEDURE Resize
*!*	*!*	        *
*!*	*!*	        WITH THIS
*!*	*!*	            *
*!*	*!*	            .oIE.Width = .Width- 2
*!*	*!*	            .oIE.Height = .Height - 2
*!*	*!*	            *
*!*	*!*	        ENDWITH
*!*	*!*	        *
*!*	*!*	    ENDPROC
*!*	    *
*!*	ENDDEFINE	
***


PROCEDURE CHECK_INI
lcArchivo = 'SIGPRES.INI'
IF FILE('&lcArchivo.')
	lnFileHandle = FOPEN(lcArchivo,2)
	DO WHILE !FEOF(lnFileHandle)
		lcString = ALLTRIM(FGETS(lnFileHandle, 254))
		lnSize = FSEEK(lnFileHandle,0,1)
		DO CASE
			CASE 'SEC_EJEC' $ UPPER(lcString)
				gcsec_ejec = SUBSTR(lcString,AT('=',lcString)+1)
				gcsec_ejec = decodificar_simple(gcsec_ejec,'LOBO')
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
			CASE 'CLAVE' $ UPPER(lcString)
				gcClave = SUBSTR(lcString,AT('=',lcString)+1)
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
STORE lcruta+'\XML' 	 TO ladirectory(2)

FOR i=1 TO 2	
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
