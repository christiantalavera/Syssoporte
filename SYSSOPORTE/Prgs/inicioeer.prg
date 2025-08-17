SET DELE ON
set libra to
clos all
SET STAT BAR OFF
SET EXAC ON
SET CENTURY ON
SET TALK OFF
SET SYSM OFF
SET BELL ON
SET SAFETY OFF
SET ESCAPE OFF
SET CONFIRM OFF
SET CURSOR ON
SET CONSOLE OFF
SET EXCLUSI OFF
SET DATE TO BRIT
clear macros

PUBLIC m.nhandle, controlador, strConSQL, strSQL, TmpCursor, mDataSource
PUBLIC vUsuario, vRutaDBF, cnxQry
PUBLIC vViaTrans, vCdEmprEER, vCdDepoERR, vEmpVinExt, vNombCasil, vPassCasil, vCodPrvSof, vRutaTMP, vRutaDbfEn, vRutaGnEnv

WAIT WINDOW 'Conectandose a la BD. ADUANA ....' NOWAIT

vUsuario='repusr'
vpasword='repusr'
mDataSource='aduana'

STORE SQLConnect(mDataSource,vUsuario,vpasword ) To m.nhandle
IF m.nhandle <= 0
	=messagebox("Falló la conexión inicial",64,"Mensaje del Sistema")
	sTerminar = .t.
	CLOSE ALL  
ELSE
	controlador = m.nhandle
ENDIF
*Controlador = 1
SET PROCEDURE TO libreria.prg

IF controlador > 0	
	** Configuramos las Variables para el Envio:
	*strConSQL = "SELECT * FROM ctrlenvioeer "
	*SQLEXEC(Controlador, strConSQL, 'qConfig')
	*IF RECCOUNT()!=0
	*    vViaTrans  = "5"
	*    vCdEmprEER = '4173' &&qConfig.CodiEmprEER
	*    vCdDepoERR = '3272' &&qConfig.CodiDepoEER
	*    vEmpVinExt = '4173' &&qConfig.CodiEmpExtVinc
   	*	vNombCasil = 'tdae3272' &&qConfig.casillaenvio
	*	vPassCasil = '0811' &&qConfig.passcasienv
	*	vCodPrvSof = '1234' &&qConfig.codiprovsoft
	*	vRutaTMP   = 'C:\TEMP\' &&ALLTRIM(qConfig.RutaTmp)
	*	vRutaDbfEn = 'C:\TD\AEREA\EER\ESTENV\' &&ALLTRIM(qConfig.RutaDbfsEnvio)
	*	vRutaGnEnv = 'C:\TD\AEREA\EER\' &&ALLTRIM(qConfig.rutagenzip)
	*ENDIF
	*USE IN 'qConfig'	

	** Configuramos las Variables para el Envio:
	strConSQL = "SELECT * FROM ctrlenvioeer "
	SQLEXEC(Controlador, strConSQL, 'qConfig')
	IF RECCOUNT()!=0
	    vViaTrans  = "5"
	    vCdEmprEER = qConfig.CodiEmprEER
	    vCdDepoERR = qConfig.CodiDepoEER
	    vEmpVinExt = qConfig.CodiEmpExtVinc
   		vNombCasil = qConfig.casillaenvio
		vPassCasil = qConfig.passcasienv
		vCodPrvSof = qConfig.codiprovsoft
		vRutaTMP   = ALLTRIM(qConfig.RutaTmp)
		vRutaDbfEn = ALLTRIM(qConfig.RutaDbfsEnvio)
		vRutaGnEnv = ALLTRIM(qConfig.rutagenzip)
	ENDIF
	USE IN 'qConfig'	

	
	*DO FORM frmProcesoseer_00
	DO FORM frmProceManiEER
ENDIF
*USE CMNF0000 SHARED IN 0
*USE PMNF0000 SHARED IN 0