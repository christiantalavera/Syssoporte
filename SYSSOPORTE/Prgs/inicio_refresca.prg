#DEFINE HKEY_CLASSES_ROOT           -2147483648  && BITSET(0,31)
#DEFINE HKEY_CURRENT_USER           -2147483647  && BITSET(0,31)+1
#DEFINE HKEY_LOCAL_MACHINE          -2147483646  && BITSET(0,31)+2
#DEFINE HKEY_USERS                  -2147483645  && BITSET(0,31)+3

lcRutaSystem = RutaSystem()
SET PATH TO fORMS, PRGS, menus, graficos, include, data , clases, reports, &lcRutaSystem, xml


SET CLASSLIB TO blowfish ADDITIVE 
SET CLASSLIB TO registry ADDITIVE 
SET DATE TO BRITISH
SET CENTURY ON 
SET SAFETY OFF
SET TALK OFF
SET PROCEDURE TO rutinas ADDITIVE 
SET PROCEDURE TO refresca_expediente ADDITIVE 
SET MULTILOCKS ON
SET HELP OFF
SET ECHO OFF 
SET NOTIFY OFF 
SET STATUS BAR OFF 
SET ESCAPE OFF 

DO DeclaraTodo
DO check_ini

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
xRutaCert = gcRutaCert+'syssoporte'
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


gcEncriptaDesencripta	= CREATEOBJECT("blowfish")
gcClaveMenu 			= ALLTRIM(gcEncriptaDesencripta.codificarBlowFish(gcano_eje+gcsec_ejec,gcLlaveMenu))
gcSubNivel01 			= ''
gcSubNivel02 			= ''
gcAbrevSubNivel01 		= ''
gcAbrevSubNivel02 		= ''




SET SYSMENU OFF
ON SHUTDOWN DO SalirSistema IN inicio_refresca.prg
gcFondoPantalla = IIF(EMPTY(gcFondoPantalla),'graficos\fondo.jpg',gcFondoPantalla)



*DO SETEAR_PANTALLA		
limiteAccesos = 20

IF gcRutaOk THEN 
	IF lnResult <> 2 THEN 
			DO SysRefresca
*		    READ EVENTS
	ELSE
		=MESSAGEBOX('Copia no valida del Sistema',64,'Aviso')
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



********************
PROCEDURE DeclaraTodo
********************
	PUBLIC gcAno_Eje,gcSec_Ejec, gcTipo_Unidad,GCTIPO_UE, gcOldCaption,gcOldClassLib,gcOldDir,gcOldEscape,;
	gcOldPath,gcOldTalk,gcMonto,gcFileIni  ,gcSession , gcOpcRpt, gcFiltroPca, gcMes, gcRutaSiaf, gcRutaCert, glmuestra_fuente_agregada, gcnom_fuente ,gcnom_fuente_abrev,;
	gcCab_mod, gcId_rep, gcVersion ,gcTipo_reporte, gcclaveusr, gcuserid, gcEncriptaDesencripta, gcLlave, gces_supervisor, gcClave, gcActivo,;
	gcSubNivel01, gcSubNivel02, gcAbrevSubNivel01, gcAbrevSubNivel02, gchoja_calculo,P_USER, gcRutaOk, gcRutaSystem, gcfuncion_entidad, gcmes_apertura, gcmes_cierre, gcIgv,;
	gcRutaInterfase, gcNombre_ejecutora, gcSector, gcPliego, gcSec_ejec_pliego,lcArchivoFrame, gcVersion_office, gcCaja, glParamNET, glSystemApp, glFoxy, gcTipo_rendicion, gcClaveMenu, gcLlaveMenu, gcLlave,gcFondoPantalla

	#INCLUDE include\syssoporte.h
	gcLLave		 = LLKEY
	gcllaveMenu  = LLKEY_MENU
	*-- Instrucciones DECLARE DLL para leer/escribir en archivos INI privados-
	DECLARE INTEGER GetPrivateProfileString IN Win32API  AS GetPrivStr ;
		String cSection, String cKey, String cDefault, String @cBuffer, ;
		Integer nBufferSize, String cINIFile

	DECLARE INTEGER WritePrivateProfileString IN Win32API AS WritePrivStr ;
		String cSection, String cKey, String cValue, String cINIFile
	gcMes=""
	glSystemApp = .T.
	glFoxy = .T.
	gcFiltroPca=""	
	gcOpcRpt='EST_V'	
	gcOldCaption=""
	gcOldDir=CURDIR()
	gcOldPath=fullpath(CURDIR())
	gcOldEscape="ON"
	gcOldTalk="ON"
	gcFileIni = 'SYSSOPORTE.INI'
	gcSession = "PARAMETROS DEL SYSSOPORTE"
	gcnom_fuente = 'RUBRO'
	gcnom_fuente_abrev = 'RB'
	glmuestra_fuente_agregada = .T.
	gcTipo_Unidad=''
	GCTIPO_UE='01'
	gcMonto = ''
	gcVersion = '2.0'
	gcCab_mod = 'SYSSOPORTE'
	gcuserid = ''
	gcClave = ''
	gcActivo= 0
	gchoja_calculo = 'xls'
	gcRutaOk = .T.
	gcfuncion_entidad = '01'
	gcmes_apertura = '01'
	gcmes_cierre = '13'
	gcIgv = 0.18
	gcRutaInterfase = ''
	TRY
		oWord = CreateObject('Word.Application')
		gcVersion_office = oWord.Version
	CATCH
		gcversion_office = ''
	ENDTRY 
	gcCaja = ''
	gcTipo_rendicion = ''
ENDPROC



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

DIMENSION ladirectory(13)

STORE lcruta+'\Listados' TO ladirectory(1)
STORE lcruta+'\XML' 	 TO ladirectory(2)
STORE lcruta+'\Graficos' TO ladirectory(3)

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



PROCEDURE sysRefresca
	** RM  : REFRESCAR MARCO PRESUPUESTAL Y PCA
	** RC  : REFRESCAR CERTIFICACION
	** RE  : REFRESCAR EXPEDIENTE

	CREATE CURSOR curGenerica (ano_eje			c(4),;
							   sec_ejec			c(6),;
							   origen			c(1),;
							   fuente_financ	c(2),;
							   categoria_gasto	c(1),;
							   tipo_transaccion	c(1),;
							   generica			c(1))

	SELECT curGenerica
	INDEX on ano_eje+sec_ejec+origen+fuente_financ+categoria_gasto+tipo_transaccion+generica TAG inxgen

	CREATE CURSOR curCertificado (ano_eje		c(4),;
								  sec_ejec		c(6),;
								  certificado	c(10),;
								  secuencia		c(4),;
								  seleccionado	n(1) DEFAULT 1)

	SELECT curCertificado
	INDEX on ano_eje+sec_ejec+certificado+secuencia TAG inx1 
								
	CREATE CURSOR CurExpediente  (ano_eje		c(4),;
								  sec_ejec		c(6),;
								  expediente	c(10),;
								  ciclo			c(1),;
								  fase			c(1),;
								  seleccionado	N(1))		

	SELECT curExpediente							  					  
	INDEX ON ano_eje+sec_ejec+expediente+ciclo+fase TAG inx1 
							   	
	USE syssoporte!log_proceso IN 0 ORDER tag ueestado
	
	SELECT log_proceso
	DO WHILE SEEK('NRM','log_proceso','ueestado')	
		SCATTER MEMVAR 
		IF !SEEK(m.ano_eje+m.sec_ejec+m.origen+m.fuente_financ+m.categoria_gasto+m.tipo_transaccion+m.generica,'curGenerica') THEN 
			INSERT INTO curGenerica (ano_eje,sec_ejec, origen, fuente_financ, categoria_gasto, tipo_transaccion, generica) ;
							VALUES (m.ano_eje, m.sec_ejec, m.origen, m.fuente_financ, m.categoria_gasto, m.tipo_transaccion, m.generica)
		ENDIF 
		REPLACE log_proceso.ue_estado WITH 'A'
	ENDDO 
	
	DO REFRESCA_PCA_PIM
	SELECT log_proceso
	DO WHILE SEEK('NRC','log_proceso','ueestado')	
		SCATTER MEMVAR 
		IF !SEEK(m.ano_eje+m.sec_ejec+m.certificado+m.certificado_secuencia,'curCertificado','inx1') THEN 
			INSERT INTO curCertificado (ano_eje,sec_ejec, certificado, secuencia,seleccionado) ;
							VALUES (m.ano_eje, m.sec_ejec, m.certificado,m.certificado_secuencia,1)
		ENDIF 
		REPLACE log_proceso.ue_estado WITH 'A'	
	ENDDO 
	DO REFRESCA_CERTIFICADO		


	SELECT log_proceso
	SEEK 'NRE'
	SCAN WHILE ue_estado+tipo_operacion = "NRE" 
		SCATTER MEMVAR 
		IF !SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase,'curExpediente','inx1') THEN 
				INSERT INTO curExpediente (ano_eje,sec_ejec, expediente, ciclo, fase, seleccionado) ;
							VALUES (m.ano_eje, m.sec_ejec, m.expediente,m.ciclo, m.fase,1)
		ENDIF 
		REPLACE log_proceso.ue_estado WITH 'A'	
	ENDSCAN  
	DO Refresca_Expediente


ENDPROC 

PROCEDURE REFRESCA_PCA_PIM
	*************
	*ABRIR TABLAS
	*************
	PUBLIC llcheck 
	PUBLIC llErrorFatal 
	llErrorFatal = .F.
	llCheck = .T.
	TRY 	
		USE siaf!mpp_pca_x_generica				IN 0 AGAIN ORDER tag pca_ge
		USE siaf!mpp_pca_x_especifica			IN 0 AGAIN ORDER tag pcadet
		USE siaf!mpp_distribucion_pca_det		IN 0 again ORDER tag notapcadet
		USE siaf!mpp_distribucion_pca_cab		IN 0 AGAIN ORDER tag notapca
		USE siaf!gasto							IN 0 AGAIN order tag mp_gastop
		USE siaf!fuente_financ					IN 0 AGAIN ORDER tag fuente_fin			NOUPDATE
		USE siaf!especifica_det					IN 0 AGAIN ORDER tag idclasif			NOUPDATE
		USE siaf!generica						IN 0 AGAIN ORDER tag generica			NOUPDATE
		USE siaf!subgenerica					IN 0 AGAIN ORDER tag sgenerica			NOUPDATE	
		USE siaf!subgenerica_det				IN 0 AGAIN ORDER tag sgenericad			NOUPDATE
		USE siaf!especifica						IN 0 AGAIN ORDER tag especif			NOUPDATE
		USE siaf!especifica_det					IN 0 AGAIN ORDER tag especifdet			NOUPDATE
		USE siaf!meta							IN 0 AGAIN ORDER tag meta				NOUPDATE
		USE siaf!maestro_clasificador			IN 0 AGAIN ORDER tag idclasif 			NOUPDATE
		USE siaf!nota_modificatoria_cab			IN 0 AGAIN ORDER tag mp_notacab			NOUPDATE
		USE siaf!nota_modificatoria_sec			IN 0 AGAIN ORDER tag mp_notasec			NOUPDATE
		USE siaf!nota_modificatoria_det			IN 0 AGAIN ORDER tag mpnotadetp			NOUPDATE	
		USE siaf!certificado_meta				IN 0 AGAIN ORDER tag certi_meta			NOUPDATE
		USE siaf!certificado_fase				IN 0 AGAIN ORDER tag certi_fase			NOUPDATE
		USE siaf!maestro_clasificador			IN 0 AGAIN ORDER tag idclasif			NOUPDATE
		USE siaf!expediente_fase				IN 0 AGAIN ORDER tag EXP_FASE			NOUPDATE
		USE siaf!expediente_meta				IN 0 AGAIN ORDER tag EXP_METAP			NOUPDATE
			
	CATCH   
	   	llerrorfatal = .T.
		=MESSAGEBOX('NO TIENE ACCESO AL SIAF',64,'AVISO')  
	FINALLY 
	ENDTRY 	

	IF llErrorFatal THEN 
		RETURN .F.
	ENDIF 


	CREATE CURSOR cur_mpp_pca_x_generica (ano_eje				c(4),;
										  sec_ejec				c(6),;
										  fuente_financ			c(2),;
										  categoria_gasto		c(1),;
										  tipo_transaccion		c(1),;
										  generica				c(1),;
										  descripcion			c(150),;
										  monto_asignado		N(19,2),;
										  monto_precertificado	N(19,2),;
										  monto_distribuido		N(19,2),;
										  seleccionado			N(1))
										  
	SELECT cur_mpp_pca_x_generica
	INDEX on ano_eje+sec_ejec+fuente_financ+categoria_gasto+tipo_transaccion+generica TAG inx1


	CREATE CURSOR cur_mpp_pca_x_especifica (ano_eje				c(4),;
										   sec_ejec				c(6),;
										   fuente_financ		c(2),;
										   categoria_gasto		c(1),;
										   tipo_transaccion		c(1),;
										   generica				c(1),;
										   id_clasificador		c(7),;
										   clasificador			c(20),;
										   descripcion			c(150),;
										   monto_pim			N(19,2),;
										   monto_asignado		N(19,2),;
										   monto_a_solicitado	N(19,2),;
										   monto_de_solicitado	N(19,2),;
										   seleccionado			N(1))									  
										  
	SELECT cur_mpp_pca_x_especifica
	INDEX on ano_eje+sec_ejec+fuente_financ+categoria_gasto+tipo_transaccion+generica+id_clasificador 	TAG inx1
	INDEX on ano_eje+sec_ejec+fuente_financ+categoria_gasto+tipo_transaccion+generica+clasificador 		TAG inx2 ADDITIVE 


										  
	CREATE CURSOR cur_gasto				  (ano_eje				c(4),;
										   sec_ejec				c(6),;
										   fuente_financ		c(2),;
										   categoria_gasto		c(1),;
										   tipo_transaccion		c(1),;
										   generica				c(1),;
										   id_clasificador		c(7),;
										   clasificador			c(20),;
										   sec_func				c(4),;
										   tipo_recurso			c(2),;
										   descripcion			c(150),;
										   monto_presupuesto	N(19,2),;
										   monto_modificacion	N(19,2),;
										   monto_pim			N(19,2),;
										   monto_a_solicitado	N(19,2),;
										   monto_de_solicitado	N(19,2),;
										   monto_precertificado	N(19,2),;
										   monto_certificado	N(19,2),;
										   monto_comprometido_anual N(19,2),;
										   seleccionado			N(1))								  
										   
	INDEX on ano_eje+sec_ejec+fuente_financ+tipo_recurso+id_clasificador+sec_func TAG inx1	
	INDEX on ano_eje+sec_ejec+fuente_financ+id_clasificador+sec_func TAG inx2 ADDITIVE 	
	INDEX on ano_eje+sec_ejec+fuente_financ+clasificador+sec_func TAG inx3 ADDITIVE 	
	INDEX on ano_eje+sec_ejec+fuente_financ+categoria_gasto+tipo_transaccion+generica TAG inx4 ADDITIVE 
	SELECT cur_gasto
	SET ORDER TO inx3




	******************
	***	CARGAR DATOS
	******************
	SELECT cur_mpp_pca_x_especifica
	ZAP
	SELECT cur_mpp_pca_x_generica
	ZAP 
	SELECT cur_gasto
	ZAP 

	SELECT mpp_pca_x_generica
	SEEK gcano_eje+gcsec_ejec
	SCAN WHILE ano_eje+sec_ejec = gcano_eje+gcsec_ejec
		SCATTER MEMVAR 
		m.seleccionado  = 0
		m.descripcion = IIF(SEEK(m.ano_eje+m.tipo_transaccion+m.generica,'generica'),generica.descripcion,'')
		
		INSERT INTO cur_mpp_pca_x_generica FROM MEMVAR 
	ENDSCAN 

	SELECT mpp_pca_x_especifica
	SEEK gcano_eje+gcsec_ejec
	SCAN WHILE ano_eje+sec_ejec = gcano_eje+gcsec_ejec
		SCATTER MEMVAR 
		m.seleccionado  = 0
		m.descripcion = IIF(SEEK(m.ano_eje+m.id_clasificador,'especifica_det'),especifica_det.descripcion,'')
		m.clasificador= IIF(SEEK(m.ano_eje+m.id_clasificador,'especifica_det'),m.categoria_gasto+' '+especifica_det.tipo_transaccion+'.'+;
																			   especifica_det.generica+'.'+especifica_det.subgenerica+'.'+;
																			   especifica_det.subgenerica_det+'.'+especifica_det.especifica+'.'+;
																			   especifica_det.especifica_det,'')
		INSERT INTO cur_mpp_pca_x_especifica FROM MEMVAR 
	ENDSCAN 


	SELECT gasto
	SEEK gcano_eje+gcsec_ejec
	SCAN WHILE ano_eje+sec_ejec = gcano_eje+gcsec_ejec
		SCATTER MEMVAR 
		m.monto_presupuesto = m.presupuesto
		m.monto_modificacion = m.modificacion
		m.monto_pim = m.presupuesto + m.modificacion
		SELECT maestro_clasificador
		IF SEEK(m.ano_eje+m.id_clasificador,'maestro_clasificador','IDCLASIF') THEN 
			m.tipo_transaccion	= SUBSTR(maestro_clasificador.clasificador,1,1)
			m.generica			= SUBSTR(maestro_clasificador.clasificador,2,1)
			m.subgenerica		= SUBSTR(maestro_clasificador.clasificador,3,2)
			m.subgenerica_det	= SUBSTR(maestro_clasificador.clasificador,5,2)
			m.especifica		= SUBSTR(maestro_clasificador.clasificador,7,2)
			m.especifica_det	= SUBSTR(maestro_clasificador.clasificador,9,2)
			llave1=m.ano_eje+m.tipo_transaccion+m.generica+m.subgenerica+m.subgenerica_det
			IF SEEK(llave1,'subgenerica_det') THEN
				m.categoria_gasto = subgenerica_det.categoria_gasto
			ENDIF
		ENDIF 
		
		m.descripcion 	= IIF(SEEK(m.ano_eje+m.id_clasificador,'especifica_det','IDCLASIF'),especifica_det.descripcion,'')
		m.clasificador	= m.categoria_gasto+' '+m.tipo_transaccion+'.'+m.generica+'.'+m.subgenerica+'.'+m.subgenerica_det+'.'+m.especifica+'.'+m.especifica_det

		IF !SEEK(m.ano_eje+m.sec_ejec+m.fuente_financ+m.tipo_recurso+m.id_clasificador+m.sec_func,'cur_gasto','inx1') THEN 
			INSERT INTO cur_gasto FROM MEMVAR 
		ELSE
			REPLACE cur_gasto.monto_presupuesto 	WITH cur_gasto.monto_presupuesto + m.monto_presupuesto
			REPLACE cur_gasto.monto_modificacion 	WITH cur_gasto.monto_modificacion + m.monto_modificacion 
			REPLACE cur_gasto.monto_pim				WITH cur_gasto.monto_pim + m.monto_pim
			
		ENDIF 
		IF SEEK(m.ano_eje+m.sec_ejec+m.fuente_financ+m.categoria_gasto+m.tipo_transaccion+m.generica+m.id_clasificador,'cur_mpp_pca_x_especifica','inx1') THEN 
			REPLACE cur_mpp_pca_x_especifica.monto_pim WITH cur_mpp_pca_x_especifica.monto_pim + m.monto_pim
		ENDIF 
	ENDSCAN 




	SELECT curGenerica
	SCAN ALL 
		SCATTER MEMVAR 
		IF SEEK(m.ano_eje+m.sec_ejec+m.fuente_financ+m.categoria_gasto+m.tipo_transaccion+m.generica,'cur_mpp_pca_x_generica','inx1') THEN 
			REPLACE cur_mpp_pca_x_generica.seleccionado WITH 1
			SELECT cur_mpp_pca_x_especifica
			REPLACE ALL cur_mpp_pca_x_especifica.seleccionado WITH 1 FOR ano_eje = m.ano_eje AND sec_ejec = m.sec_ejec AND ;
																				  fuente_financ = m.fuente_financ AND ;
																				  categoria_gasto = m.categoria_gasto AND ;
																				  tipo_transaccion = m.tipo_transaccion AND ;
																				  generica = m.generica

			SELECT cur_gasto
			REPLACE ALL cur_gasto.seleccionado WITH 1 FOR ano_eje = m.ano_eje AND sec_ejec = m.sec_ejec AND ;
																				  fuente_financ = m.fuente_financ AND ;
																				  tipo_transaccion = m.tipo_transaccion AND ;
																				  generica = m.generica
		ENDIF 
		SELECT curGenerica	
	ENDSCAN 

	*****************
	*REFRESCAR SALDOS
	*****************
	WAIT WINDOW 'Actualizando Marco Presupuestal ' NOWAIT 
	SELECT cur_gasto
	SCAN ALL FOR seleccionado = 1
		SCATTER MEMVAR 

		m.origen = '1'
		REPLACE monto_precertificado WITH 0, monto_certificado WITH 0, monto_comprometido_anual WITH 0 IN cur_gasto 
		
		IF SEEK(m.ano_eje+m.sec_ejec+m.origen+m.fuente_financ+m.tipo_recurso+m.sec_func+m.id_clasificador,'gasto','MP_GASTOP') THEN 
			REPLACE modificacion WITH 0, monto_a_solicitado  WITH 0, monto_de_solicitado WITH 0 IN gasto
			REPLACE monto_precertificado WITH 0, monto_certificado WITH 0, monto_comprometido_anual WITH 0, compromiso WITH 0, devengado WITH 0, girado WITH 0, pagado WITH 0 IN gasto 
		ENDIF 
		IF SEEK(m.ano_eje+m.sec_ejec+m.fuente_financ+m.categoria_gasto+m.tipo_transaccion+m.generica+m.id_clasificador,'mpp_pca_x_especifica','pcadet') THEN 
			REPLACE mpp_pca_x_especifica.monto_asignado WITH 0, monto_a_solicitado WITH 0, monto_de_solicitado WITH 0, monto_comprometido WITH 0 IN mpp_pca_x_especifica 
		ENDIF 
		IF SEEK(m.ano_eje+m.sec_ejec+m.fuente_financ+m.categoria_gasto+m.tipo_transaccion+m.generica,'mpp_pca_x_generica','pca_ge') THEN 
			REPLACE mpp_pca_x_generica.monto_distribuido WITH 0, monto_precertificado WITH 0, monto_certificado WITH 0 IN mpp_pca_x_generica 
		ENDIF 
		
		SELECT nota_modificatoria_det
		SEEK gcano_eje + gcsec_ejec
		SCAN REST WHILE ano_eje+sec_ejec = gcano_eje+gcsec_ejec
			IF nota_modificatoria_det.id_clasificador = m.id_clasificador AND nota_modificatoria_det.sec_func = m.sec_func AND nota_modificatoria_det.fuente_financ = m.fuente_financ THEN 
					=SEEK(gcano_eje+id_clasificador, 'maestro_clasificador', 'idclasif')
					=SEEK(gcano_eje+LEFT(maestro_clasificador.clasificador,6), 'subgenerica_det', 'sgenericad')
					lccateg_gasto = subgenerica_det.categoria_gasto
					lctipo_transaccion = SUBSTR(maestro_clasificador.clasificador, 1, 1)
					lcgenerica = SUBSTR(maestro_clasificador.clasificador, 2, 1)
					*--
					lcllavegasto = ano_eje + sec_ejec + origen + fuente_financ + tipo_recurso + sec_func + id_clasificador
					lcllavepresu = ano_eje + sec_ejec + origen + fuente_financ + lccateg_gasto + LEFT(maestro_clasificador.clasificador,2)
					lcllaveplieg = ano_eje + sec_ejec + fuente_financ + lccateg_gasto + LEFT(maestro_clasificador.clasificador,2)
					lcllaveespec = ano_eje + sec_ejec + fuente_financ + lccateg_gasto + LEFT(maestro_clasificador.clasificador,2) + id_clasificador
					lcllavexenti = ano_eje + sec_ejec + sec_ejec + fuente_financ + lccateg_gasto + LEFT(maestro_clasificador.clasificador,2)
					*--
					lcllavesec = ano_eje + sec_ejec + sec_ejec2 + sec_nota
					SELECT nota_modificatoria_sec
					=SEEK(lcllavesec)
					SCAN REST WHILE ano_eje + sec_ejec + sec_ejec2 + sec_nota = lcllavesec
						nmonto_a  = nota_modificatoria_det.monto_a  * IIF(secuencia='0001',1,-1)
						nmonto_de = nota_modificatoria_det.monto_de * IIF(secuencia='0001',1,-1)
						*
						nmonto_a  = IIF(nota_modificatoria_sec.estado='R' AND nota_modificatoria_sec.estado_envio='X', 0, nmonto_a)
						nmonto_de = IIF(nota_modificatoria_sec.estado='R' AND nota_modificatoria_sec.estado_envio='X', 0, nmonto_de)
						IF nota_modificatoria_sec.estado_envio = 'A' THEN
							REPLACE modificacion WITH modificacion + nmonto_a - nmonto_de IN gasto
						ELSE
							IF secuencia = '0001' THEN
								REPLACE monto_a_solicitado  WITH monto_a_solicitado  + nmonto_a  ,;
									monto_de_solicitado WITH monto_de_solicitado + nmonto_de  ;
									IN gasto
									
							ENDIF
						ENDIF
					ENDSCAN
			ENDIF 
		ENDSCAN
	ENDSCAN 
	WAIT WINDOW 'Actualizado PCA ' NOWAIT 
	SELECT cur_mpp_pca_x_especifica
	SCAN ALL FOR seleccionado = 1
		SCATTER MEMVAR 
		REPLACE cur_mpp_pca_x_generica.seleccionado WITH 1
		
		SELECT mpp_distribucion_pca_det
		SEEK gcano_eje+gcsec_ejec
		SCAN REST WHILE ano_eje+sec_ejec = gcano_eje+gcsec_ejec FOR fuente_financ + categoria_gasto + tipo_transaccion + generica  = m.fuente_financ + m.categoria_gasto + m.tipo_transaccion + m.generica 
		 IF m.id_clasificador <> id_clasificador THEN 
			 LOOP 
		 ENDIF 
		
			=SEEK(ano_eje + sec_ejec+ fuente_financ + categoria_gasto + tipo_transaccion + generica + id_clasificador, 'mpp_pca_x_especifica', 'pcadet')
			IF mpp_distribucion_pca_cab.estado_registro = 'A' THEN
				REPLACE mpp_pca_x_especifica.monto_asignado WITH mpp_pca_x_especifica.monto_asignado + mpp_distribucion_pca_det.monto IN mpp_pca_x_especifica
			ELSE
				wmonto_a  = IIF(mpp_distribucion_pca_det.monto> 0, mpp_distribucion_pca_det.monto, 0)
				wmonto_de = IIF(mpp_distribucion_pca_det.monto> 0, 0, ABS(mpp_distribucion_pca_det.monto))
				REPLACE mpp_pca_x_especifica.monto_a_solicitado WITH mpp_pca_x_especifica.monto_a_solicitado + wmonto_a, ;
					mpp_pca_x_especifica.monto_de_solicitado WITH mpp_pca_x_especifica.monto_de_solicitado + wmonto_de IN mpp_pca_x_especifica
			ENDIF
		ENDSCAN



	ENDSCAN 
	WAIT WINDOW 'Actualizando Monto Ejecutado ' NOWAIT 
	SELECT certificado_meta
	SEEK gcano_eje + gcsec_ejec
	SCAN REST WHILE ano_eje = gcano_eje AND sec_ejec = gcsec_ejec
		STORE 0 TO wmonto_precertificado, wmonto_certificado, wmonto_comprometido_anual
		SCATTER MEMVAR 
		m.fuente_financ = certificado_fase.fuente_financ
		IF SEEK(m.ano_eje+m.sec_ejec+m.fuente_financ+m.id_clasificador+m.sec_func,'cur_gasto','inx2') THEN 
			IF cur_gasto.seleccionado = 1 THEN 
				IF certificado_fase.etapa = '1'
					wmonto_precertificado = monto_nacional + monto_nacional_ajuste
					IF estado_registro = 'A'
						wmonto_certificado = monto_nacional + monto_nacional_ajuste
					ENDIF
				ELSE
					IF certificado_fase.etapa = '2' AND estado_registro = 'A'
						wmonto_comprometido_anual = monto_nacional + monto_nacional_ajuste
					ENDIF
				ENDIF
				
				*--
				=SEEK(gcano_eje+id_clasificador, 'maestro_clasificador', 'idclasif')
				=SEEK(gcano_eje+LEFT(maestro_clasificador.clasificador,6), 'subgenerica_det', 'sgenericad')
				lccateg_gasto = subgenerica_det.categoria_gasto
				lcllavegasto = gcano_eje + gcsec_ejec + '1' + certificado_fase.fuente_financ + ;
					'0 ' + certificado_meta.sec_func + certificado_meta.id_clasificador
				IF SEEK(lcllavegasto, 'gasto', 'mp_gastop')
					REPLACE gasto.monto_precertificado 	WITH gasto.monto_precertificado + wmonto_precertificado, ;
						gasto.monto_certificado 		WITH gasto.monto_certificado + wmonto_certificado, ;
						gasto.monto_comprometido_anual	WITH gasto.monto_comprometido_anual + wmonto_comprometido_anual IN gasto
				ENDIF
			ENDIF 
		ENDIF 
	ENDSCAN

	SELECT expediente_meta
	SEEK gcano_eje + gcsec_ejec
	SCAN REST WHILE ano_eje = gcano_eje AND sec_ejec = gcsec_ejec
		STORE 0 TO wmonto_compromiso, wmonto_devengado, wmonto_girado, wmonto_pagado
		SCATTER MEMVAR 
		m.fuente_financ = expediente_fase.fuente_financ
		IF m.estado_envio <> 'A' THEN 
			LOOP 
		ENDIF 
		IF SEEK(m.ano_eje+m.sec_ejec+m.fuente_financ+m.id_clasificador+m.sec_func,'cur_gasto','inx2') THEN 
			IF cur_gasto.seleccionado = 1 THEN 
				DO CASE
					CASE expediente_fase.ciclo + expediente_fase.fase = 'GC' 
						wmonto_compromiso = expediente_meta.monto_nacional 							
					CASE expediente_fase.ciclo + expediente_fase.fase = 'GD'				
						wmonto_devengado = expediente_meta.monto_nacional
					CASE expediente_fase.ciclo + expediente_fase.fase = 'GG'				
						wmonto_girado = expediente_meta.monto_nacional
					CASE expediente_fase.ciclo + expediente_fase.fase = 'GP'				
						wmonto_pagado = expediente_meta.monto_nacional
				ENDCASE 
				
				=SEEK(gcano_eje+id_clasificador, 'maestro_clasificador', 'idclasif')
				=SEEK(gcano_eje+LEFT(maestro_clasificador.clasificador,6), 'subgenerica_det', 'sgenericad')
				lccateg_gasto = subgenerica_det.categoria_gasto

				lcllavegasto = gcano_eje + gcsec_ejec + '1' + expediente_fase.fuente_financ + ;
					'0 ' + expediente_meta.sec_func + expediente_meta.id_clasificador
				IF SEEK(lcllavegasto, 'gasto', 'mp_gastop')
					REPLACE gasto.compromiso 			WITH gasto.compromiso + wmonto_compromiso, ;
							gasto.devengado		 		WITH gasto.devengado  + wmonto_devengado ;
							gasto.girado				WITH gasto.girado + wmonto_girado ;
							gasto.pagado				WITH gasto.pagado + wmonto_pagado IN gasto
				ENDIF
			ENDIF 
		ENDIF 
	ENDSCAN

	SELECT cur_gasto
	SET ORDER TO inx4

	SELECT cur_mpp_pca_x_generica
	SCAN ALL FOR seleccionado = 1
		SCATTER MEMVAR 
		=SEEK(m.ano_eje+m.sec_ejec+m.fuente_financ+m.categoria_gasto+m.tipo_transaccion+m.generica,'mpp_pca_x_generica','pca_ge') 
		SELECT mpp_distribucion_pca_det
		SEEK m.ano_eje+m.sec_ejec
		SCAN REST WHILE ano_eje+sec_ejec = m.ano_eje+m.sec_ejec FOR fuente_financ+categoria_gasto+tipo_transaccion+generica = m.fuente_financ+m.categoria_gasto+m.tipo_transaccion+m.generica
			IF mpp_distribucion_pca_cab.estado_registro = 'A' THEN
				REPLACE mpp_pca_x_generica.monto_distribuido WITH mpp_pca_x_generica.monto_distribuido + mpp_distribucion_pca_det.monto IN mpp_pca_x_generica
			ENDIF
		ENDSCAN
		SELECT cur_gasto
		SEEK m.ano_eje+m.sec_ejec+m.fuente_financ+m.categoria_gasto+m.tipo_transaccion+m.generica
		SCAN WHILE ano_eje+sec_ejec+fuente_financ+categoria_gasto+tipo_transaccion+generica = m.ano_eje+m.sec_ejec+m.fuente_financ+m.categoria_gasto+m.tipo_transaccion+m.generica
			=SEEK(m.ano_eje+m.sec_ejec+'1'+m.fuente_financ+'0 '+cur_gasto.sec_func+cur_gasto.id_clasificador,'gasto','mp_gastop') 
			REPLACE mpp_pca_x_generica.monto_precertificado WITH mpp_pca_x_generica.monto_precertificado + gasto.monto_precertificado
			REPLACE mpp_pca_x_generica.monto_certificado 	WITH mpp_pca_x_generica.monto_certificado + gasto.monto_certificado
		ENDSCAN 
	ENDSCAN 

	SELECT gasto
	SEEK GCano_eje+GCsec_ejec
	SCAN WHILE ano_eje+sec_ejec=GCano_eje+GCsec_ejec FOR tipo_recurso='0 '
		SCATTER MEMVAR 
		IF SEEK(m.ano_eje+m.sec_ejec+m.fuente_financ+m.id_clasificador+m.sec_func,'cur_gasto','inx2') THEN 
			IF cur_gasto.seleccionado = 1 THEN 
				IF SEEK(gasto.ano_eje+gasto.id_clasificador,'especifica_det','idclasif')
					IF SEEK(especifica_det.ano_eje+especifica_det.tipo_transaccion+especifica_det.generica+especifica_det.subgenerica+especifica_det.subgenerica_det,'subgenerica_det','sgenericad')
						m.key1 = gasto.ano_eje+gasto.sec_ejec+gasto.fuente_financ+subgenerica_det.categoria_gasto+especifica_det.tipo_transaccion+especifica_det.generica+gasto.id_clasificador
						IF !SEEK(m.key1,'mpp_pca_x_especifica','pcadet')
							INSERT INTO mpp_pca_x_especifica (ano_eje,sec_ejec,fuente_financ,categoria_gasto,tipo_transaccion,generica,id_clasificador) ;
								VALUES (gasto.ano_eje,gasto.sec_ejec,gasto.fuente_financ,subgenerica_det.categoria_gasto,especifica_det.tipo_transaccion,especifica_det.generica,gasto.id_clasificador)
						ENDIF
						REPLACE monto_comprometido WITH monto_comprometido + gasto.monto_precertificado IN mpp_pca_x_especifica
					ENDIF
				ENDIF		
			ENDIF 
		ENDIF 
	ENDSCAN




	SELECT cur_gasto
	SET ORDER TO inx3



	USE IN mpp_pca_x_generica				
	USE IN mpp_pca_x_especifica			
	USE IN mpp_distribucion_pca_det		
	USE IN mpp_distribucion_pca_cab		
	USE IN gasto							
	USE IN fuente_financ					
	USE IN especifica_det					
	USE IN generica						
	USE IN subgenerica					
	USE IN subgenerica_det				
	USE IN especifica						
	USE IN meta							
	USE IN maestro_clasificador			
	USE IN nota_modificatoria_cab			
	USE IN nota_modificatoria_sec			
	USE IN nota_modificatoria_det			
	USE IN certificado_meta				
	USE IN certificado_fase				
	USE IN expediente_fase				
	USE IN expediente_meta				



ENDPROC 



PROCEDURE REFRESCA_CERTIFICADO
		**-----------*
		* ABRE TABLAS
		**-----------*
		USE SIAF!ejecutora						IN 0 SHARED AGAIN ORDER tag sec_ejec
		USE siaf!certificado					IN 0 SHARED AGAIN order tag certi			ALIAS c_certificado
		USE siaf!certificado_fase				IN 0 SHARED AGAIN ORDER tag certi_fase		ALIAS c_certificado_fase
		USE siaf!certificado_secuencia			IN 0 SHARED AGAIN ORDER tag certi_sec		ALIAS c_certificado_secuencia
		USE siaf!certificado_clasif				IN 0 SHARED AGAIN ORDER tag certi_clas		ALIAS c_certificado_clasif
		USE siaf!certificado_meta				IN 0 SHARED AGAIN ORDER tag certi_meta		ALIAS c_certificado_meta
		USE siaf!certificado_fase 				IN 0 ALIAS c_fase 		AGAIN ORDER TAG CERTI_FASE
		USE siaf!certificado_secuencia			IN 0 ALIAS c_secuencia	AGAIN ORDER TAG CERTI_SEC
		USE siaf!certificado_clasif				IN 0 ALIAS c_clasif		AGAIN ORDER TAG CERTI_CLAS
		USE siaf!certificado_meta				IN 0 ALIAS c_meta		AGAIN ORDER TAG CERTI_META
		USE syssoporte!log_soporte				IN 0 SHARED AGAIN ORDER tag operacion
		USE siaf!expediente_fase				IN 0 SHARED AGAIN ORDER tag CER_FASE
		USE siaf!expediente_secuencia			IN 0 SHARED AGAIN ORDER tag exp_sec
		USE siaf!expediente_clasif				IN 0 SHARED AGAIN ORDER TAG EXPCLASIFP
		USE siaf!expediente_meta				IN 0 SHARED AGAIN ORDER tag EXP_METAP



		CREATE CURSOR cur_certi   (ano_eje					C(04)    ,;
								   sec_ejec					C(06)    ,;
								   certificado				c(10)	 ,;
								   secuencia				c(4)	 )
							   

		SELECT cur_certi
		INDEX on ano_eje+sec_ejec+certificado+secuencia TAG inx1

		CREATE CURSOR cur_ca  (ano_eje					C(04)    ,;
							   sec_ejec					C(06)    ,;
							   certificado				c(10)	 ,;
							   secuencia				c(4)	 )
							   

		SELECT cur_ca
		INDEX on ano_eje+sec_ejec+certificado+secuencia TAG inx1



		SELECT c_certificado_secuencia
		SET RELATION TO ano_eje+sec_ejec+certificado INTO c_certificado
		SET RELATION TO ano_eje+sec_ejec+certificado+secuencia INTO c_certificado_fase ADDITIVE 



	***----------------
	* REFRESCAR SALDOS
	***----------------
		ZAP IN cur_ca 
		ZAP IN cur_certi


		*====================================================================================================================================================
		*  Coloca los Montos en Cero de la Certificacion
		*----------------------------------------------------------------------------------------------------------------------------------------------------

		SELECT curCertificado
		SCAN ALL FOR seleccionado = 1
			lcCertificado = curCertificado.certificado
			lcSecuencia   = curCertificado.secuencia
			*
			SELECT c_secuencia 
			SEEK gcano_eje+gcsec_ejec+lcCertificado+lcsecuencia
			SCAN WHILE ano_eje+sec_ejec+certificado+secuencia=gcano_eje+gcsec_ejec+lcCertificado+lcSecuencia
			     SCATTER MEMVAR 
			     *	     
			     REPLACE monto WITH 0, monto_nacional WITH 0 IN c_secuencia
			     *
			     IF c_secuencia.correlativo<>'0001'
			        LOOP 
			     Endif
			
				 SELECT c_fase
				 SEEK gcano_eje+gcsec_ejec+m.Certificado+m.Secuencia
				 SCAN WHILE ano_eje+sec_ejec+certificado+secuencia = gcano_eje+gcsec_ejec+m.Certificado+m.Secuencia
				 	 REPLACE monto WITH 0, monto_nacional WITH 0, saldo_nacional WITH 0, monto_comprometido WITH 0
		 		 ENDSCAN 
		 		 *
				 SELECT c_clasif
				 SEEK gcano_eje+gcsec_ejec+m.Certificado+m.Secuencia
				 SCAN WHILE ano_eje+sec_ejec+certificado+Secuencia = gcano_eje+gcsec_ejec+m.Certificado+m.Secuencia
				 	  REPLACE monto WITH 0, monto_nacional WITH 0
				 ENDSCAN 
				 
				INSERT INTO cur_certi (ano_eje, sec_ejec, certificado, secuencia) VALUES (m.ano_eje, m.sec_ejec, m.Certificado, m.secuencia)
							
				STORE 0 TO wmonto_nacional, wmonto
			ENDSCAN 		
		ENDSCAN 

		*====================================================================================================================================================
		* Inserta Compromisos Anuales de la Certificacion
		*----------------------------------------------------------------------------------------------------------------------------------------------------
		SELECT curCertificado
		SCAN ALL
			SELECT c_fase
			SEEK curCertificado.ano_eje+curCertificado.sec_ejec+curCertificado.certificado
			SCAN WHILE ano_eje+sec_ejec+certificado = curCertificado.ano_eje+curCertificado.sec_ejec+curCertificado.certificado
				SCATTER MEMVAR 
				IF m.secuencia_padre <> curCertificado.secuencia THEN 
					LOOP 
				ENDIF 
				INSERT INTO cur_ca (ano_eje, sec_ejec, certificado, secuencia) VALUES (m.ano_eje, m.sec_ejec, m.Certificado, m.secuencia)
			ENDSCAN 
		ENDSCAN 


		*======================================================================================================================================================
		* Actualiza Saldos de la Certificacion
		*------------------------------------------------------------------------------------------------------------------------------------------------------
		SELECT cur_certi
		SCAN ALL 
			lcCertificado = cur_certi.certificado
			lcSecuencia   = cur_certi.secuencia
			SELECT c_meta
			=SEEK(gcano_eje + gcsec_ejec + lcCertificado + lcSecuencia)
			SCAN REST WHILE ano_eje = gcano_eje AND sec_ejec = gcsec_ejec AND certificado = lcCertificado AND secuencia = lcSecuencia
				wmonto				= c_meta.monto
				wmonto_nacional 	= c_meta.monto_nacional + c_meta.monto_nacional_ajuste
				wmonto_comprometido = c_meta.monto_comprometido
				*-tabla certificado_clasif
				SELECT c_clasif
				IF SEEK(c_meta.ano_eje+c_meta.sec_ejec+c_meta.certificado+c_meta.secuencia+;
						c_meta.correlativo+c_meta.id_clasificador,'c_clasif','certi_clas') THEN
					REPLACE c_clasif.monto			WITH c_clasif.monto + wmonto ,;
							c_clasif.monto_nacional	WITH c_clasif.monto_nacional + wmonto_nacional IN c_clasif
				ENDIF
				
				*-tabla certificado_fase
				IF SEEK(c_meta.ano_eje+c_meta.sec_ejec+c_meta.certificado+c_meta.secuencia,'c_fase') AND ;
						SEEK(c_meta.ano_eje+c_meta.sec_ejec+c_meta.certificado+c_meta.secuencia+c_meta.correlativo,'c_secuencia') THEN
						
					IF INLIST(c_secuencia.tipo_registro, 'N', 'T',' ') THEN
						REPLACE monto			WITH monto 			+ wmonto,;
								monto_nacional	WITH monto_nacional + wmonto_nacional,;
								saldo_nacional	WITH saldo_nacional + wmonto_nacional - wmonto_comprometido IN c_fase
					ELSE
						REPLACE saldo_nacional WITH saldo_nacional + wmonto_nacional IN c_fase
					ENDIF

					IF c_fase.etapa ='2' THEN
						lcSecuenciaPadre = c_fase.secuencia_padre
						IF SEEK(c_meta.ano_eje+c_meta.sec_ejec+c_meta.certificado+lcSecuenciaPadre,'c_fase')
							REPLACE saldo_nacional WITH saldo_nacional - wmonto_nacional IN c_fase
						ENDIF
					ENDIF
				ENDIF
				*-tabla certificado_secuencia
				SELECT c_secuencia
				IF SEEK(c_meta.ano_eje+c_meta.sec_ejec+c_meta.certificado+c_meta.secuencia+;
						c_meta.correlativo,'c_secuencia') THEN
					REPLACE c_secuencia.monto			WITH  c_secuencia.monto + wmonto,;
							c_secuencia.monto_nacional	WITH  c_secuencia.monto_nacional + wmonto_nacional IN c_secuencia
				ENDIF
			ENDSCAN
		ENDSCAN 


		*======================================================================================================================================================
		* INICIALIZA MONTO COMPROMETIDO ***
		*------------------------------------------------------------------------------------------------------------------------------------------------------
		SELECT cur_ca
		SCAN ALL 
			lcCertificado = cur_ca.certificado
			lcSecuencia   = cur_ca.secuencia

			SELECT c_fase
			SEEK gcano_eje+gcsec_ejec+lcCertificado+lcSecuencia
			SCAN WHILE ano_eje+sec_ejec+certificado+secuencia = gcano_eje+gcsec_ejec+lcCertificado+lcSecuencia
				REPLACE monto_comprometido WITH 0
			ENDSCAN 
			SELECT c_secuencia
			SEEK gcano_eje+gcsec_ejec+lcCertificado+lcSecuencia
			SCAN WHILE ano_eje+sec_ejec+certificado+Secuencia = gcano_eje+gcsec_ejec+lcCertificado+lcSecuencia
				REPLACE monto_comprometido WITH 0
			ENDSCAN 
			SELECT c_clasif
			SEEK gcano_eje+gcsec_ejec+lcCertificado+lcSecuencia
			SCAN WHILE ano_eje+sec_ejec+certificado+lcSecuencia = gcano_eje+gcsec_ejec+lcCertificado+lcSecuencia
				REPLACE monto_comprometido WITH 0
			ENDSCAN 
			SELECT c_meta
			SEEK gcano_eje+gcsec_ejec+lcCertificado+lcSecuencia
			SCAN WHILE ano_eje+sec_ejec+certificado+lcSecuencia = gcano_eje+gcsec_ejec+lcCertificado+lcSecuencia
				REPLACE monto_comprometido WITH 0
			ENDSCAN 

		ENDSCAN 


		*======================================================================================================================================================
		* ACTUALIZA MONTO COMPROMETIDO
		*------------------------------------------------------------------------------------------------------------------------------------------------------
		SELECT cur_ca
		SCAN ALL 
			SELECT expediente_fase
			SEEK gcano_eje+gcsec_ejec+cur_ca.Certificado+cur_ca.secuencia
			SCAN WHILE ano_eje+sec_ejec+certificado+certificado_secuencia = gcano_eje+gcsec_ejec+cur_ca.Certificado+cur_ca.secuencia 
				SELECT expediente_meta
				lcClave_sec = gcano_eje+gcsec_ejec+expediente_fase.expediente+expediente_fase.ciclo+expediente_fase.fase+expediente_fase.secuencia
				SEEK lcClave_sec
				SCAN WHILE ano_eje+sec_ejec+expediente+ciclo+fase+secuencia = lcClave_sec
					IF expediente_meta.ciclo + expediente_meta.fase <> 'GC' THEN 
						LOOP 
					ENDIF 
					lnMonto_nacional = expediente_meta.monto_nacional
					IF SEEK(gcano_eje + gcsec_ejec + expediente_fase.certificado + expediente_fase.certificado_secuencia, ;
							'c_fase')
						REPLACE c_fase.monto_comprometido WITH c_fase.monto_comprometido + lnmonto_nacional IN c_fase
					ENDIF
					IF SEEK(gcano_eje + gcsec_ejec + expediente_fase.certificado + expediente_fase.certificado_secuencia ;
							+ '0001', 'c_secuencia')
						REPLACE c_secuencia.monto_comprometido WITH c_secuencia.monto_comprometido + lnmonto_nacional IN c_secuencia
					ENDIF
					lcCorrelativoCert="0001"
					IF SEEK(gcano_eje + gcsec_ejec + expediente_fase.certificado + expediente_fase.certificado_secuencia + lcCorrelativoCert + ;
							expediente_meta.id_clasificador + expediente_meta.sec_func, 'c_meta')
						REPLACE c_meta.monto_comprometido WITH c_meta.monto_comprometido + lnmonto_nacional IN c_meta
					ELSE
						IF SEEK(gcano_eje + gcsec_ejec + expediente_fase.certificado + expediente_fase.certificado_secuencia +  ;
								expediente_meta.id_clasificador + expediente_meta.sec_func, 'c_meta','CERTI_CLF')
							REPLACE c_meta.monto_comprometido WITH c_meta.monto_comprometido + lnmonto_nacional IN c_meta
							lcCorrelativoCert=c_meta.correlativo
						ENDIF
					ENDIF
					IF SEEK(gcano_eje + gcsec_ejec + expediente_fase.certificado + expediente_fase.certificado_secuencia + lcCorrelativoCert + ;
							expediente_meta.id_clasificador, 'c_clasif')
						REPLACE c_clasif.monto_comprometido WITH c_clasif.monto_comprometido + lnmonto_nacional IN c_clasif
					ENDIF
				ENDSCAN 
			ENDSCAN 
			
		ENDSCAN 

		USE IN EJECUTORA
		USE IN C_CERTIFICADO
		USE IN C_CERTIFICADO_FASE
		USE IN C_CERTIFICADO_SECUENCIA
		USE IN C_CERTIFICADO_CLASIF
		USE IN C_CERTIFICADO_META
		USE IN C_FASE
		USE IN C_SECUENCIA
		USE IN C_CLASIF
		USE IN C_META
		USE IN LOG_SOPORTE
		USE IN EXPEDIENTE_FASE
		USE IN EXPEDIENTE_SECUENCIA
		USE IN EXPEDIENTE_CLASIF
		USE IN EXPEDIENTE_META				




ENDPROC 


PROCEDURE REFRESCA_EXPEDIENTE

	USE SIAF!EJECUTORA						IN 0 SHARED AGAIN ORDER tag sec_ejec
	USE siaf!expediente						IN 0 SHARED AGAIN ORDER tag expediente
	USE siaf!expediente_documento			IN 0 SHARED AGAIN ORDER tag exp_doc
	USE siaf!expediente_fase				IN 0 SHARED AGAIN ORDER tag exp_fase
	USE siaf!expediente_secuencia			IN 0 SHARED AGAIN ORDER tag exp_sec
	USE siaf!expediente_ingreso				IN 0 SHARED AGAIN ORDER tag exp_ing
	USE siaf!expediente_meta				IN 0 SHARED AGAIN ORDER tag exp_metap
	USE siaf!expediente_encargo				IN 0 SHARED AGAIN ORDER TAG expencargp
	USE siaf!expediente_clasif				IN 0 SHARED AGAIN ORDER TAG EXPCLASIFP
	USE siaf!expediente_enlace				IN 0 SHARED AGAIN ORDER tag exp_sec
	USE siaf!expediente_fase				IN 0 SHARED AGAIN ORDER tag exp_fase 	ALIAS e_fase
	USE siaf!expediente_clasif				IN 0 SHARED AGAIN ORDER TAG EXPCLASIFP 	ALIAS e_clasif
	USE siaf!expediente_secuencia			IN 0 SHARED AGAIN ORDER tag exp_sec 	ALIAS e_secuencia
	USE siaf!expediente_ingreso				IN 0 SHARED AGAIN ORDER tag exp_ingp	ALIAS e_ingreso
	USE siaf!expediente_meta				IN 0 SHARED AGAIN ORDER tag exp_metap	ALIAS e_meta
	USE siaf!meta							IN 0 SHARED AGAIN ORDER tag meta
	USE siaf!meta_encargo					IN 0 SHARED AGAIN ORDER tag meta
	USE siaf!gasto							IN 0 SHARED AGAIN ORDER tag mp_gastop
	USE siaf!gasto_acumulado				IN 0 SHARED AGAIN ORDER tag mpgtoacump
	USE siaf!maestro_clasificador			IN 0 SHARED AGAIN ORDER tag idclasif
	USE siaf!generica						IN 0 SHARED AGAIN ORDER tag generica
	USE siaf!tipo_recurso					IN 0 SHARED AGAIN ORDER tag tipo
	USE siaf!tipo_recurso_ejec_x_mes		IN 0 SHARED AGAIN ORDER tag rec_ejec
	USE siaf!subgenerica_det				IN 0 SHARED AGAIN ORDER tag sgenericad
	USE siaf!calendario						IN 0 SHARED AGAIN ORDER tag calendario
	USE siaf!calendario_distribuido_cab		IN 0 SHARED AGAIN ORDER tag dmp_cab
	USE siaf!calendario_distribuido			IN 0 SHARED AGAIN ORDER tag dmp_dst
	USE siaf!calendario_x_distribuir		IN 0 SHARED AGAIN ORDER tag dmp_ori
	USE siaf!acumulado						IN 0 SHARED AGAIN ORDER tag acumulado
	USE siaf!acumulado_2009					IN 0 SHARED AGAIN ORDER tag acumulado
	USE siaf!acumulado_presupuestal			IN 0 SHARED AGAIN ORDER tag acupto
	USE siaf!acumulado_mensual_presupuesto	IN 0 SHARED AGAIN ORDER tag trimestre
	USE siaf!acumulado_presupuestal_mensual	IN 0 SHARED AGAIN ORDER tag acuptomes
	USE siaf!certificado_fase 				IN 0 SHARED	AGAIN ORDER TAG CERTI_FASE



	CREATE CURSOR cur_reg (ano_eje					C(04)    ,;
						   sec_ejec					C(06)    ,;
						   expediente				c(10)	 ,;
						   secuencia				c(4)	 ,;
						   correlativo				c(4)	 ,;
						   ciclo					c(1)	 ,;
						   fase						c(1)	 ,;
						   num_doc					c(20)	 ,;
						   fecha_doc				date	 ,;
						   monto_nacional			n(19,2)	 ,;
						   estado					c(1)	 ,;
						   estado_envio				c(1)	 ,;
						   seleccionado				N(1))
						   

	SELECT cur_reg
	INDEX on ano_eje+sec_ejec+expediente+secuencia+correlativo	 TAG inx1




	CREATE CURSOR cur_ca  (ano_eje					C(04)    ,;
						   sec_ejec					C(06)    ,;
						   certificado				c(10)	 ,;
						   secuencia				c(4)	 )
						   

	SELECT cur_ca
	INDEX on ano_eje+sec_ejec+certificado+secuencia TAG inx1


	SELECT curExpediente
	SCAN ALL FOR seleccionado = 1
		 xExpediente = curExpediente.expediente 
	     DO CASE 
	        CASE ciclo+fase='GC'
				*****************************************************************************
				*-- Proceso de Expediente_meta que actualiza administrativo y otros módulos *
				*****************************************************************************
				SELECT e_meta
				SET RELATION OFF INTO e_fase
				SET RELATION TO ano_eje+sec_ejec+expediente+ciclo+fase+secuencia INTO e_fase
				SELECT expediente_meta
				SET ORDER TO exp_metap
				SET RELATION OFF INTO expediente
				SET RELATION OFF INTO expediente_fase
				SET RELATION OFF INTO expediente_secuencia
				SET RELATION OFF INTO meta
				SET RELATION OFF INTO meta_encargo
				*--
				SET RELATION TO ano_eje+sec_ejec+expediente INTO expediente
				SET RELATION TO ano_eje+sec_ejec+expediente+ciclo+fase+secuencia INTO expediente_fase ADDITIVE
				SET RELATION TO ano_eje+sec_ejec+expediente+ciclo+fase+secuencia+correlativo INTO expediente_secuencia	ADDITIVE
				SET RELATION TO ano_eje+sec_ejec+expediente+ciclo+fase+secuencia+correlativo+id_clasificador INTO expediente_clasif	ADDITIVE
				SET RELATION TO ano_eje+sec_ejec+sec_func INTO meta ADDITIVE
				SET RELATION TO ano_eje+expediente.sec_ejec2+sec_func INTO meta_encargo ADDITIVE
				*--
				STORE 'Nada' TO lcfaseclave, lcclave, lcclasif
				STORE 0 TO lncount
				STORE 0 TO lnmonto_sec, lnmonto_clas, lnmonto_nacional_sec, lnmonto_nacional_clas, lnmonto_saldo
				*--
				SELECT expediente_meta
				=SEEK(gcano_eje + gcsec_ejec + xExpediente)
				SCAN REST WHILE ano_eje = gcano_eje AND sec_ejec = gcsec_ejec AND expediente = xExpediente
					IF INLIST(estado_envio, 'P', 'R', 'N') AND ;
							( EOF('expediente_secuencia') OR EOF('expediente_fase') OR EOF('expediente_clasif') )
						DELETE
						LOOP
					ENDIF
					*--
					IF expediente_secuencia.estado_envio = 'A' AND expediente_meta.estado_envio <> 'A'
						REPLACE expediente_meta.estado_envio WITH 'A' IN expediente_meta
					ENDIF
					*--
					IF expediente_meta.sec_func='0000' AND expediente.tipo_operacion <> 'SU'
						LOOP
					ENDIF
					*--
					gcmes_ejec = PADL(MONTH(expediente_secuencia.fecha_doc),2,'0')
					IF EMPTY(monto_nacional)
						lnmonto_nacional=IIF(expediente_secuencia.moneda = 'S/.  ', monto, ;
							ROUND(VAL(STR(monto*expediente_secuencia.tipo_cambio,31,15)), 2))
						REPLACE monto_nacional WITH lnmonto_nacional
					ELSE
						lnmonto_nacional=monto_nacional
					ENDIF
					IF ano_eje+sec_ejec+expediente+ciclo+fase+secuencia+correlativo <> lcclave OR ;
							id_clasificador <> lcclasif
						lncount=lncount+1
						IF lncount > 1
							DO actualiza_gasto
						ENDIF
						SELECT expediente_meta
						lcclave=ano_eje+sec_ejec+expediente+ciclo+fase+secuencia+correlativo
						lcclasif=id_clasificador
						lcfaseclave=ano_eje+sec_ejec+expediente+ciclo+fase+secuencia
					ENDIF
					lnmonto_clas = lnmonto_clas + monto
					lnmonto_sec  = lnmonto_sec  + monto
					lnmonto_nacional_clas = lnmonto_nacional_clas + monto_nacional
					lnmonto_nacional_sec  = lnmonto_nacional_sec  + monto_nacional
					
					*-- Suma para termómetro
				ENDSCAN
				DO actualiza_gasto

	     	CASE ciclo+fase='ID'	
					SELECT e_meta
					SET RELATION OFF INTO e_fase
					SET RELATION OFF INTO e_secuencia
					SELECT expediente_meta
					SET RELATION OFF INTO expediente
					SET RELATION OFF INTO expediente_fase
					SET RELATION OFF INTO expediente_secuencia
					SELECT expediente_ingreso
					SET RELATION OFF INTO expediente
					SET RELATION OFF INTO expediente_fase
					SET RELATION OFF INTO expediente_secuencia
					*--
					SELECT e_ingreso
					SET RELATION TO ano_eje+sec_ejec+expediente+ciclo+fase+secuencia INTO e_fase
					SELECT expediente_ingreso
					SET RELATION TO ano_eje+sec_ejec+expediente INTO expediente
					SET RELATION TO ano_eje+sec_ejec+expediente+ciclo+fase+secuencia INTO expediente_fase ADDITIVE
					SET RELATION TO ano_eje+sec_ejec+expediente+ciclo+fase+secuencia+correlativo INTO expediente_secuencia ADDITIVE
					*--

					STORE 0 TO lnmonto_sec, lnmonto_clas, lnmonto_nacional_sec, lnmonto_nacional_clas, lnmonto_saldo
					STORE 'Nada' TO lcfaseclave, lcclave, lcclasif
					STORE 0 TO lncount, lnmonto_nacional_sec, lnmonto_saldo


					=SEEK(gcano_eje + gcsec_ejec + xExpediente)
					*--
					SCAN REST WHILE ano_eje = gcano_eje AND sec_ejec = gcsec_ejec AND expediente=xExpediente
						IF INLIST(estado_envio, 'P', 'R', 'N') AND ;
								( EOF('expediente_secuencia') OR EOF('expediente_fase') )
							DELETE
							LOOP
						ENDIF
						gcmes_ejec = PADL(MONTH(expediente_secuencia.fecha_doc),2,'0')
						lnmonto_nacional=IIF(expediente_secuencia.moneda = 'S/.  ',monto, ;
							ROUND(VAL(STR(monto*expediente_secuencia.tipo_cambio,31,15)), 2))
						IF EMPTY(monto_nacional)
							REPLACE monto_nacional WITH lnmonto_nacional IN expediente_ingreso
						ENDIF
						lncount=lncount+1
						IF ano_eje+sec_ejec+expediente+ciclo+fase+secuencia+correlativo <> lcclave
							IF lncount > 1
								DO actualiza_ingreso
							ENDIF
							SELECT expediente_ingreso
							lcclave=ano_eje+sec_ejec+expediente+ciclo+fase+secuencia+correlativo
							lcfaseclave=ano_eje+sec_ejec+expediente+ciclo+fase+secuencia
						ENDIF
						lnmonto_sec = lnmonto_sec + monto
						lnmonto_nacional_sec = lnmonto_nacional_sec + monto_nacional
					ENDSCAN
					DO actualiza_ingreso



	     ENDCASE 		
	ENDSCAN 
ENDPROC 

PROCEDURE Actualiza_Gasto
	IF SEEK(lcclave+lcclasif, 'e_clasif','expclasifp')
		DO CASE
			CASE e_clasif.monto <> lnMonto_clas OR e_clasif.monto_nacional <> lnMonto_nacional_clas
				REPLACE monto WITH lnMonto_clas, monto_nacional WITH lnMonto_nacional_clas IN e_clasif
			CASE EMPTY(e_clasif.monto_nacional)
				REPLACE e_clasif.monto_nacional WITH lnMonto_nacional_clas IN e_clasif
		ENDCASE
	ENDIF
	STORE 0.00 TO lnMonto_clas, lnMonto_nacional_clas
	IF ano_eje+sec_ejec+expediente+ciclo+fase+secuencia+correlativo <> lcclave
		IF SEEK(lcclave,'e_secuencia','exp_sec')
			DO CASE
				CASE e_secuencia.monto <> lnMonto_sec OR e_secuencia.monto_nacional <> lnMonto_nacional_sec
					REPLACE e_secuencia.monto WITH lnMonto_sec, monto_nacional WITH lnMonto_nacional_sec IN e_secuencia
				CASE EMPTY(e_secuencia.monto_nacional)
					REPLACE e_secuencia.monto_nacional WITH lnMonto_nacional_sec IN e_secuencia
			ENDCASE
		ENDIF
		STORE 0 TO lnMonto_sec, lnMonto_nacional_sec
	ENDIF
	IF ano_eje+sec_ejec+expediente+ciclo+fase+secuencia <> lcfaseclave
		=SEEK(lcfaseclave,'e_fase')
		lcAno_eje=e_fase.ano_eje
		lcSec_ejec=e_fase.sec_ejec
		lcExpediente=e_fase.expediente
		lcCiclo=e_fase.ciclo
		lcFase=e_fase.fase
		lcSecuencia=e_fase.secuencia
		DO CASE
		CASE lcCiclo+lcFase='GC'
			lcFaseSiguiente='D'
		CASE lcCiclo+lcFase='GD'
			lcFaseSiguiente='G'
		CASE lcCiclo+lcFase='GG'
			lcFaseSiguiente='R'
		OTHER
			lcFaseSiguiente='#'
		ENDCASE
		lnSaldo=0.00
		SELECT e_meta
		IF SEEK(lcAno_eje+lcSec_ejec+lcExpediente+lcCiclo+lcFase+lcSecuencia)
			SCAN REST WHILE ;
					(ano_eje+sec_ejec+expediente+ciclo+fase+secuencia=lcAno_eje+lcSec_ejec+lcExpediente+lcCiclo+lcFase+lcSecuencia)
				lnSaldo=lnSaldo + e_meta.monto_nacional
			ENDSCAN
		ENDIF
		IF SEEK(lcAno_eje+lcSec_ejec+lcExpediente+lcCiclo+lcFaseSiguiente)
			SCAN REST WHILE ;
					(ano_eje+sec_ejec+expediente+ciclo+fase=lcAno_eje+lcSec_ejec+lcExpediente+lcCiclo+lcFaseSiguiente) FOR ;
					e_fase.secuencia_anterior=lcSecuencia
				lnSaldo=lnSaldo - e_meta.monto_nacional
			ENDSCAN
		ENDIF
		=SEEK(lcfaseclave,'e_fase')
		IF e_fase.monto_saldo <> lnSaldo
			REPLACE e_fase.monto_saldo WITH lnSaldo IN e_fase
		ENDIF
	ENDIF


ENDPROC 

PROCEDURE actualiza_ingreso
	IF ano_eje+sec_ejec+expediente+ciclo+fase+secuencia+correlativo <> lcclave
		IF SEEK(lcclave,'e_secuencia','exp_sec')
			DO CASE
				CASE e_secuencia.monto <> lnMonto_sec OR e_secuencia.monto_nacional <> lnMonto_nacional_sec
					REPLACE monto WITH lnMonto_sec, monto_nacional WITH lnMonto_nacional_sec IN e_secuencia
				CASE EMPTY(e_secuencia.monto_nacional)
					REPLACE monto_nacional WITH lnMonto_nacional_sec IN e_secuencia
			ENDCASE
		ENDIF
		STORE 0 TO lnMonto_sec, lnMonto_nacional_sec
	ENDIF
	IF ano_eje+sec_ejec+expediente+ciclo+fase+secuencia <> lcfaseclave
		=SEEK(lcfaseclave,'e_fase')
		lcAno_eje=e_fase.ano_eje
		lcSec_ejec=e_fase.sec_ejec
		lcExpediente=e_fase.expediente
		lcCiclo=e_fase.ciclo
		lcFase=e_fase.fase
		lcSecuencia=e_fase.secuencia
		IF lcCiclo+lcFase='ID'
			lcFaseSiguiente='R'
		ELSE
			lcFaseSiguiente='#'
		ENDIF
		lnSaldo=0
		SELECT e_ingreso
		IF SEEK(lcAno_eje+lcSec_ejec+lcexpediente+lcciclo)
			SCAN REST WHILE ;
					(ano_eje+sec_ejec+expediente+ciclo=lcAno_eje+lcSec_ejec+lcexpediente+lcciclo) FOR ;
					INLIST(fase,lcFase,lcFaseSiguiente)
				IF secuencia=lcSecuencia
					lnSaldo=lnSaldo + monto_nacional
				ELSE
					IF e_fase.secuencia_anterior=lcSecuencia
						lnSaldo=lnSaldo - monto_nacional
					ENDIF
				ENDIF
			ENDSCAN
		ENDIF
		=SEEK(lcfaseclave,'e_fase')
		IF e_fase.monto_saldo <> lnSaldo
			REPLACE monto_saldo WITH lnSaldo IN e_fase
		ENDIF
	ENDIF
ENDPROC 