gcRutaActual = FULLPATH(CURDIR())

IF FILE('sigpres.ini') THEN 
	lcArchivoIni = 'SIGPRES.INI'
ELSE 
	lcArchivoIni = 'CERT.INI'
ENDIF 


gcFileIni 	= gcrutaActual+lcArchivoINI
gcClave 	= gcEncriptaDesencripta.codificarBlowFish(gcsec_ejec,gcllave)

	
IF !FILE(gcFileIni) THEN 
	= WriteFileIni(gcFileIni,'Parametros del Sistema','Ano_eje',gcano_eje)	
ELSE
	= WriteFileIni(gcFileIni,'Parametros del Sistema','Ano_eje',gcano_eje)	
ENDIF 
