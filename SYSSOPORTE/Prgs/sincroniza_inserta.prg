PARAMETERS parmAnoEje,parmSecEjec
lcRutaImporta = gcRutaImporta

DO carga_funcion
DO carga_programa
DO carga_programa_nombre
DO carga_sub_programa
DO carga_sub_programa_nombre
DO carga_act_proy_nombre
DO carga_componente_nombre
DO carga_finalidad
DO carga_meta

DO inserta_funcion
DO inserta_programa
DO inserta_programa_nombre
DO inserta_sub_programa
DO inserta_sub_programa_nombre
DO inserta_act_proy_nombre
DO inserta_componente_nombre
DO inserta_finalidad
DO inserta_meta


DO carga_certificado
DO carga_certificado_fase
DO carga_certificado_secuencia
DO carga_certificado_clasif
DO carga_certificado_meta
DO inserta_certificado
DO inserta_certificado_fase
DO inserta_certificado_secuencia
DO inserta_certificado_clasif
DO inserta_certificado_meta



DO carga_expediente
DO carga_expediente_fase
DO carga_expediente_ingreso	
DO carga_expediente_secuencia	
DO CARGA_EXPEDIENTE_META
DO CARGA_EXPEDIENTE_DOCUMENTO
DO CARGA_EXPEDIENTE_NOTA
DO carga_expediente_secuencia_ingreso
DO carga_expediente_clasif_ingreso
DO carga_expediente_documento_ingreso	
DO carga_expediente_fase_pagado
DO carga_expediente_secuencia_pagado
DO carga_expediente_meta_pagado

DO inserta_expediente
DO inserta_expediente_fase
DO inserta_expediente_secuencia
DO inserta_expediente_clasif
DO inserta_expediente_meta
DO inserta_expediente_documento
DO inserta_expediente_nota
DO inserta_expediente_ingreso
DO inserta_expediente_secuencia_ingreso
DO inserta_expediente_clasif_ingreso
DO inserta_expediente_documento_ingreso
DO inserta_expediente_fase_pagado
DO inserta_expediente_secuencia_pagado
DO inserta_expediente_clasif_pagado	
DO inserta_expediente_meta_pagado


DO carga_nota_modificatoria_cab
DO carga_nota_modificatoria_sec
DO carga_nota_modificatoria_det
DO carga_nota_modificatoria_doc
DO carga_nota_modificatoria_doc_sec
DO carga_nota_modificatoria_fte
DO carga_nota_modificatoria_ing

DO inserta_nota_modificatoria_cab
DO inserta_nota_modificatoria_sec
DO inserta_nota_modificatoria_det
DO inserta_nota_modificatoria_doc
DO inserta_nota_modificatoria_doc_sec
DO inserta_nota_modificatoria_fte
DO inserta_nota_modificatoria_ing

DO carga_mpp_pca_x_generica
DO carga_mpp_pca_x_especifica

DO inserta_mpp_pca_x_generica
DO inserta_mpp_pca_x_especifica



PROCEDURE carga_funcion
	lcArchivoXML = lcRutaImporta+'funcion.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_funcion")
	ENDIF 
ENDPROC 

PROCEDURE carga_programa
	lcArchivoXML = lcRutaImporta+'programa.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_programa")
	ENDIF 
ENDPROC 

PROCEDURE carga_programa_nombre
	lcArchivoXML = lcRutaImporta+'programa_nombre.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_programa_nombre")
	ENDIF 
ENDPROC 

PROCEDURE carga_sub_programa
	lcArchivoXML = lcRutaImporta+'sub_programa.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_sub_programa")
	ENDIF 
ENDPROC 

PROCEDURE carga_sub_programa_nombre
	lcArchivoXML = lcRutaImporta+'sub_programa_nombre.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_sub_programa_nombre")
	ENDIF 
ENDPROC
 
PROCEDURE carga_act_proy_nombre
	lcArchivoXML = lcRutaImporta+'act_proy_nombre.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_act_proy_nombre")
	ENDIF 
ENDPROC


PROCEDURE carga_componente_nombre
	lcArchivoXML = lcRutaImporta+'componente_nombre.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_componente_nombre")
	ENDIF 
ENDPROC

PROCEDURE carga_programa_ppto
	lcArchivoXML = lcRutaImporta+'programa_ppto.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_programa_ppto")
	ENDIF 
ENDPROC

PROCEDURE carga_programa_ppto_nombre
	lcArchivoXML = lcRutaImporta+'programa_ppto_nombre.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_programa_ppto_nombre")
	ENDIF 
ENDPROC

PROCEDURE carga_finalidad
	lcArchivoXML = lcRutaImporta+'finalidad.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_finalidad")
	ENDIF 
ENDPROC 

PROCEDURE carga_meta
	lcArchivoXML = lcRutaImporta+'meta.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_meta")
	ENDIF 
ENDPROC 

PROCEDURE carga_certificado
	lcArchivoXML = lcRutaImporta+'certificado.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_certificado")
	ENDIF 
	IF SEEK(parmSecEjec+parmAnoEje+'01','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorCarga WITH 1
	ENDIF 
ENDPROC 

PROCEDURE carga_certificado_fase

	lcArchivoXML = lcRutaImporta+'certificado_fase.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_certificado_fase")
	ENDIF 
	IF SEEK(parmSecEjec+parmAnoEje+'02','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorCarga WITH 1
	ENDIF 
ENDPROC 


PROCEDURE carga_certificado_secuencia
	lcArchivoXML = lcRutaImporta+'certificado_secuencia.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_certificado_secuencia")
	ENDIF 

	IF SEEK(parmSecEjec+parmAnoEje+'03','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorCarga WITH 1
	ENDIF 
ENDPROC 


PROCEDURE carga_certificado_clasif

	lcArchivoXML = lcRutaImporta+'certificado_clasif.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_certificado_clasif")
	ENDIF 

	IF SEEK(parmSecEjec+parmAnoEje+'04','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorCarga WITH 1
	ENDIF 
ENDPROC 


PROCEDURE carga_certificado_meta

	lcArchivoXML = lcRutaImporta+'certificado_meta.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_certificado_meta")
	ENDIF 

	IF SEEK(parmSecEjec+parmAnoEje+'05','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorCarga WITH 1
	ENDIF 
ENDPROC 



PROCEDURE carga_expediente
	lcArchivoXML = lcRutaImporta+'expediente.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_expediente")
	ENDIF 
	
	IF SEEK(parmSecEjec+parmAnoEje+'06','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorCarga WITH 1
	ENDIF 	
							  
ENDPROC 

PROCEDURE carga_expediente_fase

	lcArchivoXML = lcRutaImporta+'expediente_fase.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_expediente_fase")		
	ENDIF 

	IF SEEK(parmSecEjec+parmAnoEje+'07','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorCarga WITH 1
	ENDIF 	
							  
ENDPROC 

 
PROCEDURE carga_expediente_secuencia

	lcArchivoXML = lcRutaImporta+'expediente_secuencia.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_expediente_secuencia")
		SELECT cur_expediente_secuencia
		INDEX On ano_eje+sec_ejec+expediente+ciclo+fase+secuencia+correlativo TAG inx1				
	ENDIF 

	SELECT curProceso
	IF SEEK(parmSecEjec+parmAnoEje+'09','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorCarga WITH 1
	ENDIF 	
	
ENDPROC 



PROCEDURE carga_expediente_meta
	lcArchivoXML = lcRutaImporta+'expediente_meta.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_expediente_meta")			
	ENDIF 
	SELECT curProceso	
	IF SEEK(parmSecEjec+parmAnoEje+'10','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorCarga WITH 1
	ENDIF 	
	
ENDPROC


PROCEDURE carga_expediente_documento
	lcArchivoXML = lcRutaImporta+'expediente_documento.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_expediente_documento")		
	ENDIF 
	SELECT curProceso
	IF SEEK(parmSecEjec+parmAnoEje+'11','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorCarga WITH 1
	ENDIF 	
		
		
ENDPROC


PROCEDURE carga_expediente_nota

	lcArchivoXML = lcRutaImporta+'expediente_nota.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_expediente_nota")		
	ENDIF 
	SELECT curProceso
	IF SEEK(parmSecEjec+parmAnoEje+'12','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorCarga WITH 1
	ENDIF 	
	
ENDPROC


PROCEDURE carga_expediente_ingreso

	lcArchivoXML = lcRutaImporta+'expediente_ingreso.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_expediente_ingreso")			
	ENDIF 

	SELECT curProceso
	IF SEEK(parmSecEjec+parmAnoEje+'08','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorCarga WITH 1
	ENDIF 	
		
ENDPROC



PROCEDURE carga_expediente_secuencia_ingreso

	lcArchivoXML = lcRutaImporta+'expediente_secuencia_ingreso.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_expediente_secuencia_ingreso")			
	ENDIF 
	SELECT curProceso
	IF SEEK(parmSecEjec+parmAnoEje+'13','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorCarga WITH 1
	ENDIF 	
			
ENDPROC



PROCEDURE carga_expediente_clasif_ingreso

	lcArchivoXML = lcRutaImporta+'expediente_clasif_ingreso.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_expediente_clasif_ingreso")		
	ENDIF 
	SELECT curProceso
	IF SEEK(parmSecEjec+parmAnoEje+'14','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorCarga WITH 1
	ENDIF 	
		
ENDPROC

PROCEDURE carga_expediente_documento_ingreso

	lcArchivoXML = lcRutaImporta+'expediente_documento_ingreso.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_expediente_documento_ingreso")			
	ENDIF 
	SELECT curProceso	
	IF SEEK(parmSecEjec+parmAnoEje+'15','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorCarga WITH 1
	ENDIF 	
	
ENDPROC 


PROCEDURE carga_expediente_fase_pagado

	lcArchivoXML = lcRutaImporta+'expediente_fase_pagado.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_expediente_fase_pagado")		
	ENDIF 

	IF SEEK(parmSecEjec+parmAnoEje+'16','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorCarga WITH 1
	ENDIF 	
		
ENDPROC


PROCEDURE carga_expediente_secuencia_pagado
	lcArchivoXML = lcRutaImporta+'expediente_secuencia_pagado.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_expediente_secuencia_pagado")	
		SELECT cur_expediente_secuencia_pagado
		INDEX on ano_eje+sec_ejec+expediente+ciclo+fase+secuencia+correlativo TAG inx1		
	ENDIF 

	SELECT curProceso
	IF SEEK(parmSecEjec+parmAnoEje+'17','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorCarga WITH 1
	ENDIF 	
ENDPROC


PROCEDURE carga_expediente_meta_pagado

	lcArchivoXML = lcRutaImporta+'expediente_meta_pagado.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_expediente_meta_pagado")		
	ENDIF 
	SELECT curProceso	
	IF SEEK(parmSecEjec+parmAnoEje+'18','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorCarga WITH 1
	ENDIF 	
ENDPROC


PROCEDURE carga_nota_modificatoria_cab
	lcArchivoXML = lcRutaImporta+'nota_modificatoria_cab.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_nota_modificatoria_cab")
	ENDIF 
ENDPROC 

PROCEDURE carga_nota_modificatoria_sec
	lcArchivoXML = lcRutaImporta+'nota_modificatoria_sec.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_nota_modificatoria_sec")
	ENDIF 
ENDPROC 

PROCEDURE carga_nota_modificatoria_det
	lcArchivoXML = lcRutaImporta+'nota_modificatoria_det.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_nota_modificatoria_det")
	ENDIF 
ENDPROC 

PROCEDURE carga_nota_modificatoria_doc
	lcArchivoXML = lcRutaImporta+'nota_modificatoria_doc.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_nota_modificatoria_doc")
	ENDIF 
ENDPROC 

PROCEDURE carga_nota_modificatoria_doc_sec
	lcArchivoXML = lcRutaImporta+'nota_modificatoria_doc_sec.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_nota_modificatoria_doc_sec")
	ENDIF 
ENDPROC 


PROCEDURE carga_nota_modificatoria_fte
	lcArchivoXML = lcRutaImporta+'nota_modificatoria_fte.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_nota_modificatoria_fte")
	ENDIF 
ENDPROC 

PROCEDURE carga_nota_modificatoria_ing
	lcArchivoXML = lcRutaImporta+'nota_modificatoria_ing.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_nota_modificatoria_ing")
	ENDIF 
ENDPROC 

PROCEDURE carga_mpp_pca_x_generica
	lcArchivoXML = lcRutaImporta+'mpp_pca_x_generica.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_mpp_pca_x_generica")
	ENDIF 
ENDPROC 

PROCEDURE carga_mpp_pca_x_especifica
	lcArchivoXML = lcRutaImporta+'mpp_pca_x_especifica.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_mpp_pca_x_especifica")
	ENDIF 
ENDPROC 

PROCEDURE carga_mpp_distribucion_pca_cab
	lcArchivoXML = lcRutaImporta+'mpp_distribucion_pca_cab.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_mpp_distribucion_pca_cab")
	ENDIF 
ENDPROC 

PROCEDURE carga_mpp_distribucion_pca_det
	lcArchivoXML = lcRutaImporta+'mpp_distribucion_pca_det.xml'
	IF FILE(lcArchivoXML) THEN 
		importarxml_adapter(lcArchivoXML, "cur_mpp_distribucion_pca_det")
	ENDIF 
ENDPROC 







PROCEDURE inserta_certificado
	USE siaf!certificado IN 0 ORDER tag CERTI AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'certificado.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 
	SET DELETED OFF 
	SELECT cur_certificado
	GO TOP 
	lnContador = 0
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje = PADL(m.ano_eje,4,'0')
		m.sec_ejec = PADL(m.sec_ejec,6,'0')
		m.certificado = PADL(m.certificado,10,'0')
		m.tipo_certificado  = padl(m.tipo_certificado,1,'0')
		m.estado_envio = 'T'		
		m.estado_registro = 'A'
		m.tipo_operacion = IIF(ISNULL(m.tipo_operacion),'',m.tipo_operacion)
		IF !SEEK(m.ano_eje+m.sec_ejec+m.certificado,'certificado','CERTI') THEN 
			lnContador = lnContador + 1
			WAIT WINDOW 'Insertando en Tabla Certificado -->>'+m.ano_eje+"-"+m.certificado NOWAIT 
			INSERT INTO certificado FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 

	USE IN certificado
	USE IN cur_certificado
	SET DELETED ON 
	SELECT curProceso
	IF SEEK(parmSecEjec+parmAnoEje+'01','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorInserta WITH 1
	ENDIF 	
	
ENDPROC 


PROCEDURE inserta_certificado_fase
	USE siaf!certificado_fase IN 0 ORDER tag certi_fase AGAIN  
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'certificado_fase.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 	
	
	SET DELETED OFF 
	SELECT cur_certificado_fase
	GO TOP 
	lnContador = 0
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje = PADL(m.ano_eje,4,'0')
		m.sec_ejec = PADL(m.sec_ejec,6,'0')
		m.certificado = PADL(m.certificado,10,'0')
		m.secuencia = padl(m.secuencia,4,'0')
		m.secuencia_padre = PADL(IIF(ISNULL(m.secuencia_padre),'0000',m.secuencia_padre),4,'0')
		m.secuencia_padre = PADL(m.secuencia_padre,4,'0')
		m.fuente_financ = PADL(m.fuente_financ,2,'0')
		m.etapa = PADL(m.etapa,1,'0')
		m.tipo_id = IIF(ISNULL(m.tipo_id),'9',PADL(m.tipo_id,1,'0'))
		m.ruc = IIF(ISNULL(m.ruc),'',m.ruc)
		m.es_compromiso = ALLTRIM(m.es_compromiso)
		m.glosa = RTRIM(m.glosa)
		m.tipo_financiamiento=IIF(ISNULL(m.tipo_financiamiento),'',m.tipo_financiamiento)
		m.tipo_financiamiento = RTRIM(m.tipo_financiamiento)
		m.tipo_operacion = IIF(ISNULL(m.tipo_operacion),'',m.tipo_operacion)
		m.tipo_operacion = ALLTRIM(m.tipo_operacion)
		m.transferencia_financiera_id=IIF(ISNULL(m.transferencia_financiera_id),'0000000000',PADL(m.transferencia_financiera_id,10,'0'))
		m.sec_ejec_destino=IIF(ISNULL(m.sec_ejec_destino),'',PADL(m.sec_ejec_destino,6,'0'))
		m.ceam_oce_id=IIF(ISNULL(m.ceam_oce_id),'',PADL(m.ceam_oce_id,10,'0'))
		m.es_compromiso = IIF(ISNULL(m.es_compromiso),'',m.es_compromiso)
		m.estado_envio = 'T'		
		m.estado_registro = 'A'

		IF !SEEK(m.ano_eje+m.sec_ejec+m.certificado+m.secuencia,'certificado_fase','certi_fase') THEN 
			lnContador = lnContador + 1
			WAIT WINDOW 'Insertando en Tabla Certificado_Fase -->>'+m.ano_eje+"-"+m.certificado+"-"+m.secuencia NOWAIT 
			INSERT INTO certificado_fase FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 
	USE IN certificado_fase
	USE IN cur_certificado_fase
	SET DELETED ON 
	SELECT curProceso
	IF SEEK(parmSecEjec+parmAnoEje+'02','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorInserta WITH 1
	ENDIF 		
ENDPROC 


PROCEDURE inserta_certificado_secuencia
	USE siaf!certificado_secuencia IN 0 ORDER tag certi_sec AGAIN  
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'certificado_secuencia.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 	
	SET DELETED OFF 
	SELECT cur_certificado_secuencia
	GO TOP 
	lnContador = 0
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje = PADL(m.ano_eje,4,'0')
		m.sec_ejec = PADL(m.sec_ejec,6,'0')
		m.certificado = PADL(m.certificado,10,'0')
		m.secuencia = padl(m.secuencia,4,'0')
		m.correlativo = PADL(m.correlativo,4,'0')
		m.cod_doc = ALLTRIM(m.cod_doc)
		m.num_doc = ALLTRIM(m.num_doc)
		m.fecha_doc = ctod(m.fecha_doc)
		m.fecha_bd_oracle = ctod(m.fecha_bd_oracle)
		m.moneda = ALLTRIM(m.moneda)
		m.secuencia_solicitud = PADL(IIF(ISNULL(m.secuencia_solicitud),'',m.secuencia_solicitud),10,'0')
		m.usuario_creacion_clt=ALLTRIM(m.usuario_creacion_clt)
		m.fecha_creacion_clt = IIF(ISNULL(m.fecha_creacion_clt), {//}, CTOD(m.fecha_creacion_clt))
*		m.fecha_creacion_clt = CTOD(m.fecha_creacion_clt)
*		m.fecha_creacion_clt = CTOD(IIF(ISNULL(m.fecha_creacion_clt), {//},m.fecha_creacion_clt))
		m.usuario_modificacion_clt=IIF(ISNULL(m.usuario_modificacion_clt),'',m.usuario_modificacion_clt)
		m.fecha_modificacion_clt = IIF(ISNULL(m.fecha_modificacion_clt ), {//}, CTOD(m.fecha_modificacion_clt) )		
*		m.fecha_modificacion_clt = CTOD(m.fecha_modificacion_clt)
		
		
		m.estado_envio = 'T'		
		m.estado_registro = 'A'

		IF !SEEK(m.ano_eje+m.sec_ejec+m.certificado+m.secuencia+m.correlativo,'certificado_secuencia','certi_sec') THEN 
			lnContador = lnContador + 1
			WAIT WINDOW 'Insertando en Tabla Certificado_Secuencia -->>'+m.ano_eje+"-"+m.certificado+"-"+m.secuencia+"-"+m.correlativo NOWAIT 
			INSERT INTO certificado_secuencia FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 

	USE IN certificado_secuencia
	USE IN cur_certificado_secuencia
	SET DELETED ON 
	SELECT curProceso
	IF SEEK(parmSecEjec+parmAnoEje+'03','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorInserta WITH 1
	ENDIF 		
ENDPROC 


PROCEDURE inserta_certificado_clasif
	USE siaf!certificado_clasif IN 0 ORDER tag CERTI_CLAS AGAIN  
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'certificado_clasif.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 	
	SET DELETED OFF 
	SELECT cur_certificado_clasif
	GO TOP 
	lnContador = 0
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje = PADL(m.ano_eje,4,'0')
		m.sec_ejec = PADL(m.sec_ejec,6,'0')
		m.certificado = PADL(m.certificado,10,'0')
		m.secuencia = padl(m.secuencia,4,'0')
		m.correlativo = PADL(m.correlativo,4,'0')
		m.id_clasificador = ALLTRIM(m.id_clasificador)
		m.estado_ctb = IIF(ISNULL(m.estado_ctb),'',m.estado_ctb)
		m.estado_ctb = ALLTRIM(m.estado_ctb)
		m.estado_envio = 'T'		
		m.estado_registro = 'A'

		IF !SEEK(m.ano_eje+m.sec_ejec+m.certificado+m.secuencia+m.correlativo+m.id_clasificador,'certificado_clasif','CERTI_CLAS') THEN 
			lnContador = lnContador + 1
			WAIT WINDOW 'Insertando en Tabla Certificado_Clasif -->>'+m.ano_eje+"-"+m.certificado+"-"+m.secuencia+"-"+m.correlativo+"-"+m.id_clasificador NOWAIT 
			INSERT INTO certificado_clasif FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 

	USE IN certificado_clasif 
	USE IN cur_certificado_clasif
	SET DELETED ON 
	SELECT curProceso
	IF SEEK(parmSecEjec+parmAnoEje+'04','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorInserta WITH 1
	ENDIF 	
ENDPROC 

PROCEDURE inserta_certificado_meta
	USE siaf!certificado_meta IN 0 ORDER tag certi_meta AGAIN  
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'certificado_meta.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 	
	SET DELETED OFF 
	SELECT cur_certificado_meta
	GO TOP 
	lnContador = 0
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje = PADL(m.ano_eje,4,'0')
		m.sec_ejec = PADL(m.sec_ejec,6,'0')
		m.certificado = PADL(m.certificado,10,'0')
		m.secuencia = padl(m.secuencia,4,'0')
		m.correlativo = PADL(m.correlativo,4,'0')
		m.id_clasificador = ALLTRIM(m.id_clasificador)
		m.sec_func = padl(m.sec_func,4,'0')
		m.estado_envio = 'T'		
		m.estado_registro = 'A'

		IF !SEEK(m.ano_eje+m.sec_ejec+m.certificado+m.secuencia+m.correlativo+m.id_clasificador+m.sec_func,'certificado_meta','certi_meta') THEN 
			lnContador = lnContador + 1
			WAIT WINDOW 'Insertando en Tabla Certificado_Meta -->>'+m.ano_eje+"-"+m.certificado+"-"+m.secuencia+"-"+m.correlativo+"-"+m.id_clasificador+"-"+m.sec_func NOWAIT 
			INSERT INTO certificado_meta FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 
	USE IN certificado_meta
	USE IN cur_certificado_meta
	SET DELETED ON 
	SELECT curProceso
	IF SEEK(parmSecEjec+parmAnoEje+'05','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorInserta WITH 1
	ENDIF 	
ENDPROC

PROCEDURE inserta_expediente
	USE siaf!expediente IN 0 ORDER tag expediente AGAIN  
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'expediente.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 	
	
	SET DELETED OFF 
	SELECT cur_expediente
	GO TOP 
	lnContador = 0
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje = PADL(m.ano_eje,4,'0')
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.expediente = ALLTRIM(m.expediente)
		m.sec_ejec2 = ALLTRIM(m.sec_ejec2)
		m.expediente_encargante  = IIF(ISNULL(m.expediente_encargante),'', PADL(m.expediente_encargante,10,'0'))
		m.expediente_financiamiento  = IIF(ISNULL(m.expediente_financiamiento),'', PADL(m.expediente_financiamiento,10,'0'))	
		m.estado = IIF(ISNULL(m.estado),'',m.estado)
		m.tipo_proceso = IIF(ISNULL(m.tipo_proceso) or VAL(m.tipo_proceso)=0,'',m.tipo_proceso)
		m.id_proceso = IIF(ISNULL(m.id_proceso),'',m.id_proceso)
		m.id_contrato = IIF(ISNULL(m.id_contrato),'',m.id_contrato)
		m.sec_ejec_contrato = IIF(ISNULL(m.sec_ejec_contrato),'',PADL(m.sec_ejec_contrato,6,'0'))
		m.fase_contractual = IIF(ISNULL(m.fase_contractual),'',m.fase_contractual)
		m.estado_envio = 'A'
		IF !SEEK(m.ano_eje+m.sec_ejec+m.expediente,'expediente','expediente') THEN 
			lnContador = lnContador + 1
			WAIT WINDOW 'Insertando en Tabla Expediente -->>'+m.ano_eje+"-"+ m.expediente NOWAIT 
			INSERT INTO expediente FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 
	USE IN expediente
	SET DELETED ON 
	SELECT curProceso
	IF SEEK(parmSecEjec+parmAnoEje+'06','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorInserta WITH 1
	ENDIF 	
ENDPROC 


PROCEDURE inserta_expediente_fase
	USE siaf!expediente_fase IN 0 ORDER tag exp_fase
	USE siaf!expediente IN 0 ORDER tag expediente AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'expediente_fase.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		USE IN expediente_fase
		USE IN expediente
		RETURN 
	ENDIF 	
	SET DELETED OFF 	
	lnContador = 0	
	SELECT cur_expediente_fase
	SCAN ALL 
	SCATTER MEMVAR 
		m.ano_eje = PADL(m.ano_eje,4,'0')
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.expediente = ALLTRIM(m.expediente)
		m.secuencia = ALLTRIM(m.secuencia)	
		m.secuencia_padre = ALLTRIM(m.secuencia_padre)
		m.secuencia_anterior = ALLTRIM(m.secuencia_anterior)
		m.monto_saldo = 0
		m.ruc = iif(ISNULL(m.ruc),'',m.ruc)
		m.proyecto = IIF(ISNULL(m.proyecto),'000',m.proyecto)
		m.tipo_giro = IIF(ISNULL(m.tipo_giro),'',m.tipo_giro)
		m.tipo_financiamiento = IIF(ISNULL(m.tipo_financiamiento),'', m.tipo_financiamiento)
		m.certificado = RTRIM(IIF(ISNULL(m.certificado),'',m.certificado))
		m.certificado_secuencia = RTRIM(IIF(ISNULL(m.certificado_secuencia),'',m.certificado_secuencia))
		m.sec_ejec_reciproca = IIF(ISNULL(m.sec_ejec_reciproca) ,'',PADL(m.sec_ejec_reciproca,6,'0'))
		m.ceam_oce_det_id = IIF(ISNULL(m.ceam_oce_det_id),'',PADL(m.ceam_oce_det_id,10,'0'))	
		m.estado_envio = 'A'
		IF !SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia,'expediente_fase','exp_fase') THEN 
			lnContador = lnContador + 1
			IF SEEK(m.ano_eje+m.sec_ejec+m.expediente,'expediente','expediente') THEN 
				WAIT WINDOW 'Insertando en Tabla Expediente_fase -->>'+m.ano_eje+"-"+m.expediente+"-"+m.ciclo+m.fase+"-"+m.secuencia NOWAIT 		
				INSERT INTO expediente_fase FROM MEMVAR 
			ENDIF 
		ENDIF 
	
	ENDSCAN 
	USE IN expediente_fase
	USE IN expediente 
	SET DELETED ON 
	SELECT curProceso
	IF SEEK(parmSecEjec+parmAnoEje+'07','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorInserta WITH 1
	ENDIF 	
ENDPROC 



PROCEDURE inserta_expediente_secuencia
	USE siaf!expediente_secuencia IN 0 ORDER tag exp_sec
	USE siaf!expediente_fase IN 0 ORDER tag exp_fase AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'expediente_secuencia.xml' 
	IF !FILE(lcArchivoXML) THEN 
		USE IN expediente_secuencia
		USE IN expediente_fase
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 	
	
	SET DELETED OFF 
	lnContador = 0	
	SELECT cur_expediente_secuencia
	SCAN ALL 
	SCATTER MEMVAR 
		m.ano_eje = PADL(m.ano_eje,4,'0')
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.expediente = ALLTRIM(m.expediente)
		m.secuencia = ALLTRIM(m.secuencia)	
		m.correlativo = ALLTRIM(m.correlativo)
		m.num_doc = IIF(ISNULL(m.num_doc),'',m.num_doc)
		m.fecha_doc = ctod(m.fecha_doc)
		m.moneda = TRIM(m.moneda)
		m.monto_extranjero = IIF(ISNULL(m.monto_extranjero),0,m.monto_extranjero)
		m.fecha_ing = ctod(m.fecha_ing)
		m.usuario_ing = IIF(ISNULL(m.usuario_ing),'',m.usuario_ing)
		m.num_record  = IIF(ISNULL(m.num_record),0,m.num_record)
		m.serie_doc = IIF(ISNULL(m.serie_doc),'',m.serie_doc)
		m.ano_proceso = PADL(m.ano_proceso,4,'0')
		m.mes_proceso = PADL(m.mes_proceso,2,'0')
		m.dia_proceso = PADL(m.dia_proceso,2,'0')
		m.grupo = ''
		m.edicion = IIF(ISNULL(m.edicion),'',m.edicion)
		m.ano_cta_cte = ALLTRIM(m.ano_cta_cte)
		m.banco = ALLTRIM(m.banco)
		m.ano_cta_cte = IIF(ISNULL(m.ano_cta_cte),'',PADL(m.ano_cta_cte,4,'0'))
		m.banco = IIF(ISNULL(m.banco) ,'',PADL(m.banco,3,'0'))		
		m.cta_cte = IIF(ISNULL(m.cta_cte) ,'',PADL(ALLTRIM(m.cta_cte),3,'0'))				
		m.fecha_autorizacion = ctod(m.fecha_autorizacion)
		m.fecha_bd_oracle = ctod(m.fecha_bd_oracle)
		m.usuario_creacion_clt = IIF(ISNULL(m.usuario_creacion_clt),'',ALLTRIM(m.usuario_creacion_clt))
		m.usuario_modificacion_clt = IIF(ISNULL(m.usuario_modificacion_clt),'',ALLTRIM(m.usuario_modificacion_clt))
		m.secuencia_solicitud = IIF(ISNULL(m.secuencia_solicitud),'',PADL(m.secuencia_solicitud,10,'0'))
		m.cod_doc_b = RTRIM(IIF(ISNULL(m.cod_doc_b),'',m.cod_doc_b))
		m.num_doc_b = RTRIM(IIF(ISNULL(m.num_doc_b),'',m.num_doc_b))
		m.fecha_doc_b = IIF(ISNULL(m.fecha_doc_b ),  {//} ,ctod(m.fecha_doc_b))
		m.reg_multiple = padl(m.reg_multiple,10,'0')
		m.fecha_modificacion_clt = IIF(ISNULL(m.fecha_modificacion_clt),  {//} ,CTOD(m.fecha_modificacion_clt))
		m.fecha_creacion_clt = IIF(ISNULL(m.fecha_creacion_clt),  {//} ,CTOD(m.fecha_creacion_clt))		
		m.monto_saldo = 0 
		m.estado_envio = 'A'

		IF !SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia+m.correlativo,'expediente_secuencia','exp_sec') THEN 
			lnContador = lnContador + 1
			IF SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia,'expediente_fase','exp_fase') THEN 			
				WAIT WINDOW 'Insertando en Tabla Expediente_secuencia -->>'+m.ano_eje+"-"+m.expediente+"-"+m.ciclo+m.fase+"-"+m.secuencia NOWAIT 		
				INSERT INTO expediente_secuencia FROM MEMVAR 
			ENDIF 
		ENDIF 
	
	ENDSCAN 
	USE IN expediente_secuencia
	USE IN expediente_fase
	SET DELETED ON 
	SELECT curProceso
	IF SEEK(parmSecEjec+parmAnoEje+'09','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorInserta WITH 1
	ENDIF 	
ENDPROC


PROCEDURE inserta_expediente_clasif
	USE siaf!expediente_clasif IN 0 ORDER tag expclasifp
	USE siaf!expediente_secuencia IN 0 ORDER tag exp_sec AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'expediente_meta.xml' 
	IF !FILE(lcArchivoXML) THEN 
		USE IN expediente_clasif
		USE IN expediente_secuencia
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 		
	SET DELETED OFF 
	lnContador = 0	
	SELECT ano_eje, sec_ejec, expediente, ciclo, fase, secuencia, correlativo, id_clasificador,;
	SUM(monto) as monto, SUM(monto_nacional) as monto_nacional FROM cur_expediente_meta ;
	GROUP BY ano_eje, sec_ejec, expediente, ciclo, fase, secuencia, correlativo, id_clasificador ;
	INTO CURSOR cur_expediente_clasif READWRITE 
	
	SELECT cur_expediente_clasif
	SCAN ALL 
	SCATTER MEMVAR 
		m.ano_eje = PADL(m.ano_eje,4,'0')
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.expediente = ALLTRIM(m.expediente)
		m.secuencia = ALLTRIM(m.secuencia)	
		m.correlativo = ALLTRIM(m.correlativo)
		m.id_clasificador = ALLTRIM(m.id_clasificador)
		m.categ_gasto = '0'
		m.grupo_gasto = '0'
		m.modalidad_gasto = '00'
		m.elemento_gasto = '00'
		m.secuencia_to = '000'
		m.monto_saldo = 0 
		m.estado_envio = 'A'
		m.estado = IIF(SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia+m.correlativo,'cur_expediente_secuencia','inx1'),cur_expediente_secuencia.estado,'')
		IF !SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia+m.correlativo+m.id_clasificador,'expediente_clasif','expclasifp') THEN 
			lnContador = lnContador + 1
			IF SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia+m.correlativo,'expediente_secuencia','exp_sec') THEN 			
				WAIT WINDOW 'Insertando en Tabla Expediente_Clasif -->>'+m.ano_eje+"-"+m.expediente+m.ciclo+m.fase+"-"+m.secuencia+"-"+m.correlativo+"-"+m.id_clasificador NOWAIT 		
				INSERT INTO expediente_clasif FROM MEMVAR 
			ENDIF 
		ENDIF 
	
	ENDSCAN 

	USE IN expediente_clasif
	USE IN cur_expediente_clasif
	USE IN expediente_secuencia
	SET DELETED ON 
	SELECT curProceso
	IF SEEK(parmSecEjec+parmAnoEje+'10','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorInserta WITH 1
	ENDIF 	
ENDPROC  



PROCEDURE inserta_expediente_meta
	USE siaf!expediente_meta IN 0 ORDER tag exp_metap
	USE siaf!expediente_clasif IN 0 ORDER tag expclasifp AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'expediente_meta.xml' 
	IF !FILE(lcArchivoXML) THEN 
		USE IN expediente_meta
		USE IN expediente_clasif
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 		
	SET DELETED OFF 
	lnContador = 0	
	
	SELECT cur_expediente_meta
	SCAN ALL 
	SCATTER MEMVAR 
		m.ano_eje = PADL(m.ano_eje,4,'0')
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.expediente = ALLTRIM(m.expediente)
		m.secuencia = ALLTRIM(m.secuencia)	
		m.correlativo = ALLTRIM(m.correlativo)
		m.id_clasificador = ALLTRIM(m.id_clasificador)
		m.sec_func = PADL(m.sec_func,4,'0')
		m.categ_gasto = '0'
		m.grupo_gasto = '0'
		m.modalidad_gasto = '00'
		m.elemento_gasto = '00'
		m.secuencia_to = '000'
		m.estado = IIF(m.estado='N','',m.estado)
		m.monto_saldo = 0 
		m.estado_envio = 'A'

		IF !SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia+m.correlativo+m.id_clasificador+m.sec_func,'expediente_meta','exp_metap') THEN 
			lnContador = lnContador + 1
			IF SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia+m.correlativo+m.id_clasificador,'expediente_clasif','expclasifp') THEN 			
				WAIT WINDOW 'Insertando en Tabla Expediente_Meta -->>'+m.ano_eje+"-"+m.expediente+m.ciclo+m.fase+"-"+m.secuencia+"-"+m.correlativo+"-"+m.id_clasificador+"-"+m.sec_func NOWAIT 		
				INSERT INTO expediente_meta FROM MEMVAR 
			ENDIF 
		ENDIF 
	
	ENDSCAN 

	USE IN expediente_meta
	USE IN expediente_clasif
	USE IN cur_expediente_meta
	SET DELETED ON 
	SELECT curProceso	
	IF SEEK(parmSecEjec+parmAnoEje+'10','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorInserta WITH 1
	ENDIF 	
	
ENDPROC  

PROCEDURE inserta_expediente_nota
	USE siaf!expediente_nota IN 0 ORDER tag exp_nota
	USE siaf!expediente_fase IN 0 ORDER tag exp_fase AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'expediente_nota.xml' 
	IF !FILE(lcArchivoXML) THEN 
		USE IN expediente_nota
		USE IN expediente_fase
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 		
	SET DELETED OFF 
	lnContador = 0	
	SELECT cur_expediente_nota
	SCAN ALL 
	SCATTER MEMVAR 
		m.ano_eje = PADL(m.ano_eje,4,'0')
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.expediente = ALLTRIM(m.expediente)
		m.secuencia = ALLTRIM(m.secuencia)	
		m.secuencia_nota = ALLTRIM(m.secuencia_nota)
		m.estado_envio = 'A'
		IF !SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia+m.secuencia_nota,'expediente_nota','exp_nota') THEN 
			lnContador = lnContador + 1
			WAIT WINDOW 'Insertando en Tabla Expediente_Nota -->>'+PADL(lnContador,10,'0') NOWAIT 	
			IF SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia,'expediente_fase')	THEN 
				WAIT WINDOW 'Insertando en Tabla Expediente_Nota -->>'+m.ano_eje+"-"+m.expediente+m.ciclo+m.fase+"-"+m.secuencia+"-"+m.secuencia_nota NOWAIT 					
				INSERT INTO expediente_nota FROM MEMVAR 
			ENDIF 
		ENDIF 
	
	ENDSCAN 
	USE IN expediente_nota
	USE IN expediente_fase
	USE IN cur_expediente_nota
	SET DELETED ON 
	SELECT curProceso	
	IF SEEK(parmSecEjec+parmAnoEje+'12','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorInserta WITH 1
	ENDIF 	
	
ENDPROC 



PROCEDURE inserta_expediente_documento
	USE siaf!expediente_documento IN 0 ORDER tag exp_doc
	USE siaf!expediente_secuencia IN 0 ORDER tag exp_sec AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'expediente_documento.xml' 
	IF !FILE(lcArchivoXML) THEN 
		USE IN expediente_documento
		USE IN expediente_secuencia
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 		
	SET DELETED OFF 
	lnContador = 0	
	SELECT cur_expediente_documento
	SCAN ALL 
	SCATTER MEMVAR 
		m.ano_eje = PADL(m.ano_eje,4,'0')
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.expediente = ALLTRIM(m.expediente)
		m.secuencia = ALLTRIM(m.secuencia)	
		m.correlativo = ALLTRIM(m.correlativo)
		m.conciliado = ALLTRIM(m.conciliado)
		m.fecha_doc = cTOD(m.fecha_doc)		
		m.cod_doc = IIF(ISNULL(m.cod_doc),'',m.cod_doc)		
		m.num_doc = IIF(ISNULL(m.num_doc),'',m.num_doc)		
		m.nombre = RTRIM(m.nombre)
		m.fecha_entrega = IIF(ISNULL(m.fecha_entrega),{//},cTOD(m.fecha_entrega))
		m.estado = ''
		m.concepto_deposito = IIF(ISNULL(m.concepto_deposito),'',m.concepto_deposito)
		m.clase_planilla_mcpp = IIF(ISNULL(m.clase_planilla_mcpp),'',m.clase_planilla_mcpp)
		m.tipo_documento = IIF(ISNULL(m.tipo_documento),'',m.tipo_documento)
		m.numero_documento = IIF(ISNULL(m.numero_documento),'',m.numero_documento)
		m.forma_pago_mcpp = IIF(ISNULL(m.forma_pago_mcpp),'',m.forma_pago_mcpp)
		m.detalle_mcpp = IIF(ISNULL(m.detalle_mcpp),'',m.detalle_mcpp)
		m.estado_envio = 'A'

		IF !SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia+m.correlativo+m.cod_doc+m.num_doc,'expediente_documento','exp_doc') THEN 
			lnContador = lnContador + 1
			IF SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia+m.correlativo,'expediente_secuencia','exp_sec') THEN 			
				WAIT WINDOW 'Insertando en Tabla Expediente Documento -->>'+m.ano_eje+"-"+m.expediente+"-"+m.ciclo+m.fase+"-"+m.secuencia+"-"+m.correlativo NOWAIT 		
				INSERT INTO expediente_documento FROM MEMVAR 
			ENDIF 
		ENDIF 
	
	ENDSCAN 
	USE IN expediente_documento
	USE IN expediente_secuencia
	SET DELETED ON 
	SELECT curProceso	
	IF SEEK(parmSecEjec+parmAnoEje+'11','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorInserta WITH 1
	ENDIF 	
	
ENDPROC


PROCEDURE inserta_expediente_ingreso
	USE siaf!expediente_fase IN 0 ORDER tag exp_fase
	USE siaf!expediente IN 0 ORDER tag expediente AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'expediente_ingreso.xml' 
	IF !FILE(lcArchivoXML) THEN 
		USE IN expediente_fase
		USE IN expediente
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 		
	SET DELETED OFF 
	lnContador = 0	
	SELECT cur_expediente_ingreso
	SCAN ALL 
	SCATTER MEMVAR 
		m.ano_eje = PADL(m.ano_eje,4,'0')
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.expediente = ALLTRIM(m.expediente)
		m.secuencia = ALLTRIM(m.secuencia)	
		m.secuencia_padre = ALLTRIM(m.secuencia_padre)
		m.secuencia_anterior = ALLTRIM(m.secuencia_anterior)
		m.monto_saldo = 0
		m.tipo_pago = 'E'
		m.tipo_recurso = '0'
		m.tipo_compromiso = '11'
		m.proyecto = '000'
		m.estado_envio = 'A'
		IF !SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia,'expediente_fase','exp_fase') THEN 
			lnContador = lnContador + 1
			IF SEEK(m.ano_eje+m.sec_ejec+m.expediente,'expediente','expediente') THEN 			
				WAIT WINDOW 'Insertando en Tabla Expediente_fase Ingreso-->>'+m.ano_eje+"-"+m.expediente+"-"+m.ciclo+m.fase+"-"+m.secuencia NOWAIT 		
				INSERT INTO expediente_fase FROM MEMVAR 
			ENDIF 
		ENDIF 
	
	ENDSCAN 
	USE IN expediente_fase
	USE in expediente
	SET DELETED ON 
	SELECT curProceso	
	IF SEEK(parmSecEjec+parmAnoEje+'08','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorInserta WITH 1
	ENDIF 	
	
ENDPROC 


PROCEDURE inserta_expediente_secuencia_ingreso
	USE siaf!expediente_secuencia IN 0 ORDER tag exp_sec
	USE siaf!expediente_fase IN 0 ORDER tag exp_fase AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'expediente_secuencia_ingreso.xml' 
	IF !FILE(lcArchivoXML) THEN 
		USE IN expediente_secuencia
		USE IN expediente_fase
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 		
	SET DELETED OFF 
	lnContador = 0	
	SELECT cur_expediente_secuencia_ingreso
	SCAN ALL 
	SCATTER MEMVAR 
		m.ano_eje = PADL(m.ano_eje,4,'0')
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.expediente = ALLTRIM(m.expediente)
		m.secuencia = ALLTRIM(m.secuencia)	
		m.correlativo = ALLTRIM(m.correlativo)
		m.num_doc = IIF(ISNULL(m.num_doc),'',m.num_doc)
		m.fecha_doc = ctod(m.fecha_doc)
		m.moneda = TRIM(m.moneda)
		m.monto_extranjero = IIF(ISNULL(m.monto_extranjero),0,m.monto_extranjero)
		m.fecha_ing = CTOD(m.fecha_ing)
		m.usuario_ing = IIF(ISNULL(m.usuario_ing),'',m.usuario_ing)
		m.fecha_mod = CTOD(m.fecha_mod)
*		m.num_record  = IIF(ISNULL(m.num_record),0,m.num_record)
*		m.serie_doc = IIF(ISNULL(m.serie_doc),'',m.serie_doc)
		m.ano_proceso = PADL(m.ano_proceso,4,'0')
		m.mes_proceso = PADL(m.mes_proceso,2,'0')
		m.dia_proceso = PADL(m.dia_proceso,2,'0')
		m.grupo = ''
		m.edicion = IIF(ISNULL(m.edicion),'',m.edicion)
*		m.ano_cta_cte = STR(INT(m.ano_cta_cte),4)
		m.ano_cta_cte = IIF(ISNULL(m.ano_cta_cte),'',PADL(m.ano_cta_cte,4,'0'))
		m.banco = IIF(ISNULL(m.banco) ,'',PADL(m.banco,3,'0'))		
		m.cta_cte = IIF(ISNULL(m.cta_cte) ,'',PADL(ALLTRIM(m.cta_cte),3,'0'))				
*		m.fecha_autorizacion = TTOD(m.fecha_autorizacion)
		m.fecha_bd_oracle = CTOD(m.fecha_bd_oracle)
		m.usuario_creacion_clt = IIF(ISNULL(m.usuario_creacion_clt),'',ALLTRIM(m.usuario_creacion_clt))
		m.usuario_modificacion_clt = IIF(ISNULL(m.usuario_modificacion_clt),'',ALLTRIM(m.usuario_modificacion_clt))
*		m.secuencia_solicitud = IIF(ISNULL(m.secuencia_solicitud),'',PADL(m.secuencia_solicitud,10,'0'))
		m.cod_doc_b = RTRIM(IIF(ISNULL(m.cod_doc_b),'',m.cod_doc_b))
		m.num_doc_b = RTRIM(IIF(ISNULL(m.num_doc_b),'',m.num_doc_b))
		m.fecha_doc_b = IIF(ISNULL(m.fecha_doc_b ),  {//} ,cTOD(m.fecha_doc_b))
*		m.reg_multiple = STR(m.reg_multiple,10,0)
		m.fecha_modificacion_clt = IIF(ISNULL(m.fecha_modificacion_clt),  {//} ,CTOD(m.fecha_modificacion_clt))
		m.fecha_creacion_clt = IIF(ISNULL(m.fecha_creacion_clt),  {//} ,CTOD(m.fecha_creacion_clt))		
		m.monto_saldo = 0 
		m.estado_envio = 'A'
	
		IF !SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia+m.correlativo,'expediente_secuencia','exp_sec') THEN 
			lnContador = lnContador + 1
			IF !SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia,'expediente_fase','exp_fase') THEN 			
				WAIT WINDOW 'Insertando en Tabla Expediente_secuencia -->>'+m.ano_eje+"-"+m.expediente+"-"+m.ciclo+m.fase+"-"+m.secuencia+"-"+m.correlativo NOWAIT 		
				INSERT INTO expediente_secuencia FROM MEMVAR 
			ENDIF 
		ENDIF 
	
	ENDSCAN 
	USE IN expediente_secuencia
	USE IN expediente_fase
	SET DELETED ON 
	SELECT curProceso
	IF SEEK(parmSecEjec+parmAnoEje+'13','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorInserta WITH 1
	ENDIF 	
	
ENDPROC


PROCEDURE inserta_expediente_clasif_ingreso
	USE siaf!expediente_ingreso IN 0 ORDER tag exp_ingp
	USE siaf!expediente_secuencia IN 0 ORDER tag exp_sec AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'expediente_clasif_ingreso.xml' 
	IF !FILE(lcArchivoXML) THEN 
		USE IN expediente_ingreso
		USE IN expediente_secuencia
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 		
	SET DELETED OFF 
	lnContador = 0	
	
	SELECT cur_expediente_clasif_ingreso
	SCAN ALL 
	SCATTER MEMVAR 
		m.ano_eje = PADL(m.ano_eje,4,'0')
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.expediente = ALLTRIM(m.expediente)
		m.secuencia = ALLTRIM(m.secuencia)	
		m.correlativo = ALLTRIM(m.correlativo)
		m.id_clasificador = ALLTRIM(m.id_clasificador)
		m.clase_ingreso = '0'
		m.tipo_ingreso = '0'
		m.sub_tipo_ingreso = '0'
		m.elemento_ingreso = '000'
		m.secuencia_to = PADL(m.secuencia_to,3,'0')
		m.estado = ''
		m.monto_saldo = 0 
		m.estado_envio = 'A'

		IF !SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia+m.correlativo+m.id_clasificador,'expediente_ingreso','exp_ingp') THEN 
			lnContador = lnContador + 1
			IF SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia+m.correlativo,'expediente_secuencia','exp_sec') THEN 			
				WAIT WINDOW 'Insertando en Tabla Expediente_Ingreso Clasificador-->>'+m.ano_eje+"-"+m.expediente+"-"+m.ciclo+m.fase+"-"+m.secuencia+"-"+m.correlativo+"-"+m.id_clasificador NOWAIT 		
				INSERT INTO expediente_ingreso FROM MEMVAR 
			ENDIF 
		ENDIF 
	
	ENDSCAN 

	USE IN expediente_ingreso
	USE IN expediente_secuencia
	USE IN cur_expediente_clasif_ingreso
	SET DELETED ON 
	SELECT curProceso	
	IF SEEK(parmSecEjec+parmAnoEje+'14','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorInserta WITH 1
	ENDIF 	
ENDPROC  



PROCEDURE inserta_expediente_documento_ingreso
	USE siaf!expediente_documento IN 0 ORDER tag exp_doc
	USE siaf!expediente_secuencia IN 0 ORDER tag exp_sec AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'expediente_documento_ingreso.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		USE IN expediente_documento
		USE IN expediente_secuencia
		RETURN 
	ENDIF 		
	SET DELETED OFF 
	lnContador = 0	
	SELECT cur_expediente_documento_ingreso
	SCAN ALL 
	SCATTER MEMVAR 
		m.ano_eje = PADL(m.ano_eje,4,'0')
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.expediente = ALLTRIM(m.expediente)
		m.secuencia = ALLTRIM(m.secuencia)	
		m.correlativo = ALLTRIM(m.correlativo)
		m.fecha_doc = CTOD(m.fecha_doc)		
		m.cod_doc = IIF(ISNULL(m.cod_doc),'',m.cod_doc)		
		m.num_doc = IIF(ISNULL(m.num_doc),'',m.num_doc)		
		m.nombre = RTRIM(m.nombre)
		m.estado = ''
		m.estado_envio = 'A'

		IF !SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia+m.correlativo+m.cod_doc+m.num_doc,'expediente_documento','exp_doc') THEN 
			lnContador = lnContador + 1
			IF SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia+m.correlativo,'expediente_secuencia','exp_sec') THEN 			
				WAIT WINDOW 'Insertando en Tabla Expediente Documento -->>'+m.ano_eje+"-"+m.expediente+"-"+m.ciclo+m.fase+"-"+m.secuencia+"-"+m.correlativo+"-"+m.cod_doc+"-"+m.num_doc NOWAIT 		
				INSERT INTO expediente_documento FROM MEMVAR 
			ENDIF 
		ENDIF 
	
	ENDSCAN 

	USE IN expediente_documento
	USE IN expediente_secuencia
	SET DELETED ON 
	SELECT curProceso	
	IF SEEK(parmSecEjec+parmAnoEje+'15','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorInserta WITH 1
	ENDIF 	
ENDPROC



PROCEDURE inserta_expediente_fase_pagado
	USE siaf!expediente_fase IN 0 ORDER tag exp_fase
	USE siaf!expediente IN 0 ORDER tag expediente AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'expediente_fase_pagado.xml' 
	IF !FILE(lcArchivoXML) THEN
		USE IN expediente_fase
		USE IN expediente 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 		
	SET DELETED OFF 
	lnContador = 0	
	SELECT cur_expediente_fase_pagado
	SCAN ALL 
	SCATTER MEMVAR 
		m.ano_eje = PADL(m.ano_eje,4,'0')
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.expediente = ALLTRIM(m.expediente)
		m.secuencia = ALLTRIM(m.secuencia)	
		m.secuencia_padre = ALLTRIM(m.secuencia_padre)
		m.secuencia_anterior = ALLTRIM(m.secuencia_anterior)
		m.monto_saldo = 0
		m.proyecto = '000'
		m.certificado = RTRIM(IIF(ISNULL(m.certificado),'',m.certificado))
		m.certificado_secuencia = RTRIM(IIF(ISNULL(m.certificado_secuencia),'',m.certificado_secuencia))
		m.sec_ejec_reciproca = IIF(ISNULL(m.sec_ejec_reciproca),'',PADL(m.sec_ejec_reciproca,6,'0'))
		m.ceam_oce_det_id = IIF(ISNULL(m.ceam_oce_det_id) ,'',PADL(m.ceam_oce_det_id,10,'0'))	
		m.estado_envio = 'A'
		IF !SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia,'expediente_fase','exp_fase') THEN 
			lnContador = lnContador + 1
			IF SEEK(m.ano_eje+m.sec_ejec+m.expediente,'expediente','expediente') THEN 
				WAIT WINDOW 'Insertando en Tabla Expediente_fase Pagado -->>'+m.ano_eje+"-"+m.expediente+"-"+m.ciclo+m.fase+"-"+m.secuencia NOWAIT 		
				INSERT INTO expediente_fase FROM MEMVAR 
			ENDIF 
		ENDIF 
	
	ENDSCAN 

	USE IN expediente_fase
	USE IN expediente
	SET DELETED ON 
	SELECT curProceso	
	IF SEEK(parmSecEjec+parmAnoEje+'16','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorInserta WITH 1
	ENDIF 	
ENDPROC 

PROCEDURE inserta_expediente_secuencia_pagado
	USE siaf!expediente_secuencia IN 0 ORDER tag exp_sec
	USE siaf!expediente_fase IN 0 ORDER tag exp_fase again 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'expediente_secuencia_pagado.xml' 
	IF !FILE(lcArchivoXML) THEN 
		USE IN expediente_secuencia
		USE IN expediente_fase
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 		
	SET DELETED OFF 
	lnContador = 0	
	SELECT cur_expediente_secuencia_pagado
	SCAN ALL 
	SCATTER MEMVAR 
		m.ano_eje = PADL(m.ano_eje,4,'0')
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.expediente = ALLTRIM(m.expediente)
		m.secuencia = ALLTRIM(m.secuencia)	
		m.correlativo = ALLTRIM(m.correlativo)
		m.num_doc = IIF(ISNULL(m.num_doc),'',m.num_doc)
		m.fecha_doc = CTOD(m.fecha_doc)
		m.moneda = TRIM(m.moneda)
		m.monto_extranjero = IIF(ISNULL(m.monto_extranjero),0,m.monto_extranjero)
		m.fecha_ing = CTOD(m.fecha_ing)
		m.usuario_ing = IIF(ISNULL(m.usuario_ing),'',m.usuario_ing)
		m.num_record  = IIF(ISNULL(m.num_record),0,m.num_record)
		m.serie_doc = IIF(ISNULL(m.serie_doc),'',m.serie_doc)
		m.ano_proceso = PADL(m.ano_proceso,4,'0')
		m.mes_proceso = PADL(m.mes_proceso,2,'0')
		m.dia_proceso = PADL(m.dia_proceso,2,'0')
		m.grupo = ''
		m.edicion = IIF(ISNULL(m.edicion),'',m.edicion)
		m.ano_cta_cte = ALLTRIM(m.ano_cta_cte)
		m.banco = ALLTRIM(m.banco)
		m.ano_cta_cte = IIF(ISNULL(m.ano_cta_cte),'',PADL(m.ano_cta_cte,4,'0'))
		m.banco = IIF(ISNULL(m.banco) ,'',PADL(m.banco,3,'0'))		
		m.cta_cte = IIF(ISNULL(m.cta_cte) ,'',PADL(ALLTRIM(m.cta_cte),3,'0'))				
		m.fecha_autorizacion = CTOD(m.fecha_autorizacion)
		m.fecha_bd_oracle = CTOD(m.fecha_bd_oracle)
		m.usuario_creacion_clt = IIF(ISNULL(m.usuario_creacion_clt),'',ALLTRIM(m.usuario_creacion_clt))
		m.usuario_modificacion_clt = IIF(ISNULL(m.usuario_modificacion_clt),'',ALLTRIM(m.usuario_modificacion_clt))
		m.secuencia_solicitud = IIF(ISNULL(m.secuencia_solicitud),'',PADL(m.secuencia_solicitud,10,'0'))
		m.cod_doc_b = RTRIM(IIF(ISNULL(m.cod_doc_b),'',m.cod_doc_b))
		m.num_doc_b = RTRIM(IIF(ISNULL(m.num_doc_b),'',m.num_doc_b))
		m.fecha_doc_b = IIF(ISNULL(m.fecha_doc_b ),  {//} ,CTOD(m.fecha_doc_b))
		m.reg_multiple = PADL(m.reg_multiple,10,'0')
		m.fecha_modificacion_clt = IIF(ISNULL(m.fecha_modificacion_clt),  {//} ,CTOD(m.fecha_modificacion_clt))
		m.fecha_creacion_clt = IIF(ISNULL(m.fecha_creacion_clt),  {//} ,CTOD(m.fecha_creacion_clt))		
		m.monto_saldo = 0 
		m.estado_envio = 'A'

		IF !SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia+m.correlativo,'expediente_secuencia','exp_sec') THEN 
			lnContador = lnContador + 1
			IF SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia,'expediente_fase','exp_fase') THEN 			
				WAIT WINDOW 'Insertando en Tabla Expediente_secuencia Pagado -->>'+m.ano_eje+"-"+m.expediente+"-"+m.ciclo+m.fase+"-"+m.secuencia+"-"+m.correlativo NOWAIT 		
				INSERT INTO expediente_secuencia FROM MEMVAR 
			ENDIF 
		ENDIF 
	
	ENDSCAN 

	USE IN expediente_secuencia
	USE IN expediente_fase
	SET DELETED ON 
	SELECT curProceso	
	IF SEEK(parmSecEjec+parmAnoEje+'17','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorInserta WITH 1
	ENDIF 	
ENDPROC




PROCEDURE inserta_expediente_clasif_pagado
	USE siaf!expediente_clasif IN 0 ORDER tag expclasifp
	USE siaf!expediente_secuencia IN 0 ORDER tag exp_sec AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'expediente_clasif_pagado.xml' 
	IF !FILE(lcArchivoXML) THEN 
		USE IN expediente_clasif
		USE IN expediente_secuencia
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 		
	SET DELETED OFF 
	lnContador = 0	
	SELECT ano_eje, sec_ejec, expediente, ciclo, fase, secuencia, correlativo, id_clasificador,;
	SUM(monto) as monto, SUM(monto_nacional) as monto_nacional FROM cur_expediente_meta_pagado ;
	GROUP BY ano_eje, sec_ejec, expediente, ciclo, fase, secuencia, correlativo, id_clasificador;
	INTO CURSOR cur_expediente_clasif READWRITE 
	
	SELECT cur_expediente_clasif
	SCAN ALL 
	SCATTER MEMVAR 
		m.ano_eje = PADL(m.ano_eje,4,'0')
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.expediente = ALLTRIM(m.expediente)
		m.secuencia = ALLTRIM(m.secuencia)	
		m.correlativo = ALLTRIM(m.correlativo)
		m.id_clasificador = ALLTRIM(m.id_clasificador)
		m.categ_gasto = '0'
		m.grupo_gasto = '0'
		m.modalidad_gasto = '00'
		m.elemento_gasto = '00'
		m.secuencia_to = '000'
		m.estado = IIF(SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia+m.correlativo,'cur_expediente_secuencia','inx1'),cur_expediente_secuencia.estado,'')
		m.monto_saldo = 0 
		m.estado_envio = 'A'

		IF !SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia+m.correlativo+m.id_clasificador,'expediente_clasif','expclasifp') THEN 
			lnContador = lnContador + 1
			IF SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia+m.correlativo,'expediente_secuencia','exp_sec') THEN 			
				WAIT WINDOW 'Insertando en Tabla Expediente_Clasif Pagado -->>'+m.ano_eje+"-"+m.expediente+"-"+m.ciclo+m.fase+"-"+m.secuencia+"-"+m.correlativo+"-"+m.id_clasificador NOWAIT 		
				INSERT INTO expediente_clasif FROM MEMVAR 
			ENDIF 
		ENDIF 
	
	ENDSCAN 

	USE IN expediente_clasif
	USE IN expediente_secuencia
	USE IN cur_expediente_clasif
	SET DELETED ON 
ENDPROC  


PROCEDURE inserta_expediente_meta_pagado
	USE siaf!expediente_meta IN 0 ORDER tag exp_metap
	USE siaf!expediente_clasif IN 0 ORDER tag expclasifp AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'expediente_meta_pagado.xml' 
	IF !FILE(lcArchivoXML) THEN 
		USE IN expediente_meta
		USE IN expediente_clasif
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 		
	SET DELETED OFF 
	lnContador = 0	
	
	SELECT cur_expediente_meta_pagado
	SCAN ALL 
	SCATTER MEMVAR 
		m.ano_eje = PADL(m.ano_eje,4,'0')
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.expediente = ALLTRIM(m.expediente)
		m.secuencia = ALLTRIM(m.secuencia)	
		m.correlativo = ALLTRIM(m.correlativo)
		m.id_clasificador = ALLTRIM(m.id_clasificador)
		m.sec_func = PADL(m.sec_func,4,'0')
		m.categ_gasto = '0'
		m.grupo_gasto = '0'
		m.modalidad_gasto = '00'
		m.elemento_gasto = '00'
		m.secuencia_to = '000'
		m.estado = IIF(m.estado='N','',m.estado)
		m.monto_saldo = 0 
		m.estado_envio = 'A'

		IF !SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia+m.correlativo+m.id_clasificador+m.sec_func,'expediente_meta','exp_metap') THEN 
			lnContador = lnContador + 1
			IF SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia+m.correlativo+m.id_clasificador,'expediente_clasif','expclasifp') THEN 			
				WAIT WINDOW 'Insertando en Tabla Expediente_Meta Pagado -->>'+m.ano_eje+"-"+m.expediente+"-"+m.ciclo+m.fase+"-"+m.secuencia+"-"+m.correlativo+"-"+m.id_clasificador+"-"+m.sec_func NOWAIT 		
				INSERT INTO expediente_meta FROM MEMVAR 
			ENDIF 
		ENDIF 
	
	ENDSCAN 

	USE IN expediente_meta
	USE IN expediente_clasif
	SET DELETED ON 
	SELECT curProceso	
	IF SEEK(parmSecEjec+parmAnoEje+'18','curProceso','inx1') THEN 
		REPLACE curProceso.IndicadorInserta WITH 1
	ENDIF 	
ENDPROC  




PROCEDURE inserta_funcion
	USE siaf!funcion IN 0 ORDER tag funcion AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'funcion.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 
	SET DELETED OFF 
	SELECT cur_funcion
	GO TOP 
	lnContador = 0
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje= ALLTRIM(m.ano_eje)
		m.funcion = ALLTRIM(m.funcion)
		IF !SEEK(m.ano_eje+m.funcion,'funcion','funcion') THEN 
			lnContador = lnContador + 1
			WAIT WINDOW 'Insertando en Tabla Función -->>'+m.ano_eje+"-"+m.funcion NOWAIT 
			INSERT INTO funcion FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 

	USE IN funcion
	USE IN cur_funcion
	SET DELETED ON 

ENDPROC 

PROCEDURE inserta_programa
	USE siaf!programa IN 0 ORDER tag programa AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'programa.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 
	SET DELETED OFF 
	SELECT cur_programa
	GO TOP 
	lnContador = 0
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje= ALLTRIM(m.ano_eje)
		m.funcion = ALLTRIM(m.funcion)
		m.programa =ALLTRIM(m.programa)		
		IF !SEEK(m.ano_eje+m.funcion+m.programa,'programa','programa') THEN 
			lnContador = lnContador + 1
			WAIT WINDOW 'Insertando en Tabla Programa -->>'+m.ano_eje+"-"+m.funcion+"-"+m.programa NOWAIT 
			INSERT INTO programa FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 

	USE IN programa
	USE IN cur_programa
	SET DELETED ON 

ENDPROC 

PROCEDURE inserta_programa_nombre
	USE siaf!programa_nombre IN 0 ORDER tag programa  AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'programa_nombre.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 
	SET DELETED OFF 
	SELECT cur_programa_nombre
	GO TOP 
	lnContador = 0
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje= ALLTRIM(m.ano_eje)
		m.programa =ALLTRIM(m.programa)		
		IF !SEEK(m.ano_eje+m.programa,'programa_nombre','programa') THEN 
			lnContador = lnContador + 1
			WAIT WINDOW 'Insertando en Tabla Programa_nombre -->>'+m.ano_eje+"-"+m.programa NOWAIT 
			INSERT INTO programa_nombre FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 

	USE IN programa_nombre
	USE IN cur_programa_nombre
	SET DELETED ON 

ENDPROC 




PROCEDURE inserta_sub_programa
	USE siaf!sub_programa IN 0 ORDER tag sub_prog AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'sub_programa.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 
	SET DELETED OFF 
	SELECT cur_sub_programa
	GO TOP 
	lnContador = 0
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje= ALLTRIM(m.ano_eje)
		m.funcion = ALLTRIM(m.funcion)
		m.programa =ALLTRIM(m.programa)
		m.sub_programa =ALLTRIM(m.sub_programa)
		IF !SEEK(m.ano_eje+m.funcion+m.programa+m.sub_programa,'sub_programa','sub_prog') THEN 
			lnContador = lnContador + 1
			WAIT WINDOW 'Insertando en Tabla Sub_Programa -->>'+m.ano_eje+"-"+m.funcion+"-"+m.programa+"-"+m.sub_programa NOWAIT 
			INSERT INTO sub_programa FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 

	USE IN sub_programa
	USE IN cur_sub_programa
	SET DELETED ON 

ENDPROC 


PROCEDURE inserta_sub_programa_nombre
	USE siaf!sub_programa_nombre IN 0 ORDER tag sub_prog AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'sub_programa_nombre.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 
	SET DELETED OFF 
	SELECT cur_sub_programa_nombre
	GO TOP 
	lnContador = 0
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje= ALLTRIM(m.ano_eje)
		m.sub_programa =ALLTRIM(m.sub_programa)
		IF !SEEK(m.ano_eje+m.sub_programa,'sub_programa_nombre','sub_prog') THEN 
			lnContador = lnContador + 1
			WAIT WINDOW 'Insertando en Tabla Sub_Programa Nombre -->>'+m.ano_eje+"-"+m.sub_programa NOWAIT 
			INSERT INTO sub_programa_nombre FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 

	USE IN sub_programa_nombre
	USE IN cur_sub_programa_nombre
	SET DELETED ON 

ENDPROC


PROCEDURE inserta_act_proy_nombre
	USE siaf!act_proy_nombre IN 0 ORDER tag act_proy AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'act_proy_nombre.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 
	SET DELETED OFF 
	SELECT cur_act_proy_nombre
	GO TOP 
	lnContador = 0
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje= ALLTRIM(m.ano_eje)
		m.act_proy = ALLTRIM(m.act_proy)
		m.costo_actual = IIF(ISNULL(m.costo_actual),0,m.costo_actual)
		m.costo_actualizado_pip= IIF(ISNULL(m.costo_actualizado_pip),0,m.costo_actualizado_pip)		
		m.costo_expediente = IIF(ISNULL(m.costo_expediente),0,m.costo_expediente)
		m.ind_viabilidad = IIF(ISNULL(m.ind_viabilidad),'',m.ind_viabilidad)
		IF !SEEK(m.ano_eje+m.act_proy,'act_proy_nombre','act_proy') THEN 
			lnContador = lnContador + 1
			WAIT WINDOW 'Insertando en Tabla Act_proy Nombre -->>'+m.ano_eje+"-"+m.act_proy NOWAIT 
			INSERT INTO act_proy_nombre FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 

	USE IN act_proy_nombre
	USE IN cur_act_proy_nombre
	SET DELETED ON 

ENDPROC


PROCEDURE inserta_componente_nombre
	USE siaf!componente_nombre IN 0 ORDER tag COMPONENTE AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'componente_nombre.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 
	SET DELETED OFF 
	SELECT cur_componente_nombre
	GO TOP 
	lnContador = 0
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje = ALLTRIM(m.ano_eje)
		m.componente = ALLTRIM(m.componente)
		IF !SEEK(m.ano_eje+m.componente,'componente_nombre','COMPONENTE') THEN 
			lnContador = lnContador + 1
			WAIT WINDOW 'Insertando en Tabla Componente_Nombre -->>'+m.ano_eje+"-"+m.componente NOWAIT 
			INSERT INTO componente_nombre FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 

	USE IN componente_nombre
	USE IN cur_componente_nombre
	SET DELETED ON 

ENDPROC


PROCEDURE inserta_programa_ppto
	USE siaf!programa_ppto IN 0 ORDER tag progppto AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'programa_ppto.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 
	SET DELETED OFF 
	SELECT cur_programa_ppto
	GO TOP 
	lnContador = 0
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje = ALLTRIM(m.ano_eje)
		m.funcion = ALLTRIM(m.funcion)
		m.programa = ALLTRIM(m.programa)
		m.sub_programa = ALLTRIM(m.sub_programa)
		m.act_proy = ALLTRIM(m.act_proy)
		m.componente = ALLTRIM(m.componente)
		m.programa_ppto = ALLTRIM(m.programa_ppto)		
		IF !SEEK(m.ano_eje+m.funcion+m.programa+m.sub_programa+m.programa_ppto+m.act_proy+m.componente,'programa_ppto','progppto') THEN 
			lnContador = lnContador + 1
			WAIT WINDOW 'Insertando en Tabla Programa_ppto -->>'+m.ano_eje+"-"+m.programa_ppto NOWAIT 
			INSERT INTO programa_ppto FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 

	USE IN programa_ppto
	USE IN cur_programa_ppto
	SET DELETED ON 

ENDPROC



PROCEDURE inserta_programa_ppto_nombre
	USE siaf!programa_ppto_nombre IN 0 ORDER tag PROGPPTO_N AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'programa_ppto_nombre.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 
	SET DELETED OFF 
	SELECT cur_programa_ppto_nombre
	GO TOP 
	lnContador = 0
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje = ALLTRIM(m.ano_eje)
		m.programa_ppto = ALLTRIM(m.programa_ppto)		
		
		IF !SEEK(m.ano_eje+m.programa_ppto,'programa_ppto_nombre','PROGPPTO_N') THEN 
			lnContador = lnContador + 1
			WAIT WINDOW 'Insertando en Tabla Programa_ppto -->>'+m.ano_eje+"-"+m.programa_ppto NOWAIT 
			INSERT INTO programa_ppto_nombre FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 

	USE IN programa_ppto_nombre
	USE IN cur_programa_ppto_nombre
	SET DELETED ON 

ENDPROC



PROCEDURE inserta_finalidad
	USE siaf!finalidad IN 0 ORDER tag finalidad AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'finalidad.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 
	SET DELETED OFF 
	SELECT cur_finalidad
	GO TOP 
	lnContador = 0
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje = ALLTRIM(m.ano_eje)
		m.finalidad = ALLTRIM(m.finalidad)
		
		IF !SEEK(m.ano_eje+m.finalidad,'finalidad','finalidad') THEN 
			lnContador = lnContador + 1
			WAIT WINDOW 'Insertando en Tabla Finalidad -->>'+m.ano_eje+"-"+m.finalidad NOWAIT 
			INSERT INTO finalidad FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 

	USE IN finalidad
	USE IN cur_finalidad
	SET DELETED ON 

ENDPROC


PROCEDURE inserta_meta
	USE siaf!meta IN 0 ORDER tag meta AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'meta.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 
	SET DELETED OFF 
	SELECT cur_meta
	GO TOP 
	lnContador = 0
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje = ALLTRIM(m.ano_eje)
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.sec_func = ALLTRIM(m.sec_func)
		m.funcion = ALLTRIM(m.funcion)
		m.programa = ALLTRIM(m.programa)
		m.sub_programa = ALLTRIM(m.sub_programa)
		m.act_proy = ALLTRIM(m.act_proy)
		m.componente = ALLTRIM(m.componente)
		m.programa_ppto = ALLTRIM(m.programa_ppto)
		m.finalidad = ALLTRIM(m.finalidad)
		m.fecha_ing = CTOD(m.fecha_ing)
		IF !SEEK(m.ano_eje+m.sec_ejec+m.sec_func,'meta','meta') THEN 
			lnContador = lnContador + 1
			WAIT WINDOW 'Insertando en Tabla Meta -->>'+m.ano_eje+"-"+m.sec_ejec+"-"+m.sec_func NOWAIT 
			INSERT INTO meta FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 

	USE IN meta
	USE IN cur_meta
	SET DELETED ON 

ENDPROC




PROCEDURE inserta_nota_modificatoria_cab
	USE siaf!nota_modificatoria_cab IN 0 ORDER tag mp_notacab AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'nota_modificatoria_cab.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 
	SET DELETED OFF 
	SELECT cur_nota_modificatoria_cab
	GO TOP 
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje = ALLTRIM(m.ano_eje)
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.sec_ejec2 = ALLTRIM(m.sec_ejec2)
		m.sec_nota = ALLTRIM(m.sec_nota)
		m.dispositivo_legal = IIF(ISNULL(m.dispositivo_legal),'',m.dispositivo_legal)
		IF !SEEK(m.ano_eje+m.sec_ejec+m.sec_ejec2+m.sec_nota,'nota_modificatoria_cab','mp_notacab') THEN 
			WAIT WINDOW 'Insertando en Tabla Nota_modificatoria_cab -->>'+m.ano_eje+"-"+m.sec_nota NOWAIT 
			INSERT INTO nota_modificatoria_cab FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 

	USE IN nota_modificatoria_cab
	USE IN cur_nota_modificatoria_cab
	SET DELETED ON 
*!*		SELECT curProceso
*!*		IF SEEK(parmSecEjec+parmAnoEje+'01','curProceso','inx1') THEN 
*!*			REPLACE curProceso.IndicadorInserta WITH 1
*!*		ENDIF 	
	
ENDPROC 


PROCEDURE inserta_nota_modificatoria_sec
	USE siaf!nota_modificatoria_sec IN 0 ORDER tag mp_notasec AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'nota_modificatoria_sec.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 
	SET DELETED OFF 
	SELECT cur_nota_modificatoria_sec
	GO TOP 
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje = ALLTRIM(m.ano_eje)
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.sec_ejec2 = ALLTRIM(m.sec_ejec2)
		m.sec_nota = ALLTRIM(m.sec_nota)
		m.secuencia = ALLTRIM(m.secuencia)	
		m.fecha = CTOD(m.fecha)
		m.fecha_doc = CTOD(m.fecha_doc)
		m.fecha_doc2 = CTOD(m.fecha_doc2)
			
		IF !SEEK(m.ano_eje+m.sec_ejec+m.sec_ejec2+m.sec_nota+m.secuencia,'nota_modificatoria_sec','mp_notasec') THEN 
			WAIT WINDOW 'Insertando en Tabla Nota_modificatoria_sec -->>'+m.ano_eje+"-"+m.sec_nota+"-"+m.secuencia NOWAIT 
			INSERT INTO nota_modificatoria_sec FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 

	USE IN nota_modificatoria_sec
	USE IN cur_nota_modificatoria_sec
	SET DELETED ON 
*!*		SELECT curProceso
*!*		IF SEEK(parmSecEjec+parmAnoEje+'01','curProceso','inx1') THEN 
*!*			REPLACE curProceso.IndicadorInserta WITH 1
*!*		ENDIF 	
	
ENDPROC 



PROCEDURE inserta_nota_modificatoria_det
	USE siaf!nota_modificatoria_det IN 0 ORDER tag mpnotadetp AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'nota_modificatoria_det.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 
	SET DELETED OFF 
	SELECT cur_nota_modificatoria_det
	GO TOP 
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje = ALLTRIM(m.ano_eje)
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.sec_ejec2 = ALLTRIM(m.sec_ejec2)
		m.sec_nota = ALLTRIM(m.sec_nota)
		m.origen = ALLTRIM(m.origen)
		m.fuente_financ = ALLTRIM(m.fuente_financ)
*		m.tipo_recurso = alltrim(m.tipo_recurso)
		m.sec_func = ALLTRIM(m.sec_func)	
		m.id_clasificador = ALLTRIM(m.id_clasificador)	
		SELECT nota_modificatoria_det 
		IF !SEEK(m.ano_eje+m.sec_ejec+m.sec_ejec2+m.sec_nota+m.origen+m.fuente_financ+m.tipo_recurso+m.sec_func+m.id_clasificador,'nota_modificatoria_det','mpnotadetp') THEN 
			WAIT WINDOW 'Insertando en Tabla Nota_modificatoria_det -->>'+m.ano_eje+"-"+m.sec_nota+"-"+m.origen+"-"+m.fuente_financ+"-"+m.tipo_recurso+"-"+m.sec_func+"-"+m.id_clasificador NOWAIT 
			INSERT INTO nota_modificatoria_det FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 

	USE IN nota_modificatoria_det
	USE IN cur_nota_modificatoria_det
	SET DELETED ON 
*!*		SELECT curProceso
*!*		IF SEEK(parmSecEjec+parmAnoEje+'01','curProceso','inx1') THEN 
*!*			REPLACE curProceso.IndicadorInserta WITH 1
*!*		ENDIF 	
	
ENDPROC 



PROCEDURE inserta_nota_modificatoria_doc
	USE siaf!nota_modificatoria_doc IN 0 ORDER tag mp_notadoc AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'nota_modificatoria_doc.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 
	SET DELETED OFF 
	SELECT cur_nota_modificatoria_doc
	GO TOP 
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje = ALLTRIM(m.ano_eje)
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.sec_doc = ALLTRIM(m.sec_doc)
		
		IF !SEEK(m.ano_eje+m.sec_ejec+m.sec_doc,'nota_modificatoria_doc','mp_notadoc') THEN 
			WAIT WINDOW 'Insertando en Tabla Nota_modificatoria_doc -->>'+m.ano_eje+"-"+m.sec_doc NOWAIT 
			INSERT INTO nota_modificatoria_doc FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 

	USE IN nota_modificatoria_doc
	USE IN cur_nota_modificatoria_doc
	SET DELETED ON 
*!*		SELECT curProceso
*!*		IF SEEK(parmSecEjec+parmAnoEje+'01','curProceso','inx1') THEN 
*!*			REPLACE curProceso.IndicadorInserta WITH 1
*!*		ENDIF 	
	
ENDPROC


PROCEDURE inserta_nota_modificatoria_doc_sec
	USE siaf!nota_modificatoria_doc_sec IN 0 ORDER tag mp_notados AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'nota_modificatoria_doc_sec.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 
	SET DELETED OFF 
	SELECT cur_nota_modificatoria_doc_sec
	GO TOP 
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje = ALLTRIM(m.ano_eje)
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.sec_ejec2 = ALLTRIM(m.sec_ejec2)
		m.sec_doc = ALLTRIM(m.sec_doc)
		m.sec_nota = ALLTRIM(m.sec_nota)		
		m.secuencia = ALLTRIM(m.secuencia)
		IF !SEEK(m.ano_eje+m.sec_ejec+m.sec_doc+m.sec_ejec2+m.sec_nota+m.secuencia,'nota_modificatoria_doc_sec','mp_notados') THEN 
			WAIT WINDOW 'Insertando en Tabla Nota_modificatoria_doc_sec -->>'+m.ano_eje+m.sec_ejec+m.sec_doc+m.sec_ejec2+m.sec_nota+m.secuencia NOWAIT 
			INSERT INTO nota_modificatoria_doc_sec FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 

	USE IN nota_modificatoria_doc_sec
	USE IN cur_nota_modificatoria_doc_sec
	SET DELETED ON 
*!*		SELECT curProceso
*!*		IF SEEK(parmSecEjec+parmAnoEje+'01','curProceso','inx1') THEN 
*!*			REPLACE curProceso.IndicadorInserta WITH 1
*!*		ENDIF 	
	
ENDPROC

PROCEDURE inserta_nota_modificatoria_fte
	USE siaf!nota_modificatoria_fte IN 0 ORDER tag MP_NOTAFTE AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'nota_modificatoria_fte.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 
	SET DELETED OFF 
	SELECT cur_nota_modificatoria_fte
	GO TOP 
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje = ALLTRIM(m.ano_eje)
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.sec_ejec2 = ALLTRIM(m.sec_ejec2)
		m.sec_nota = ALLTRIM(m.sec_nota)
		m.origen = ALLTRIM(m.origen)
		m.fuente_financ = ALLTRIM(m.fuente_financ)
				
		IF !SEEK(m.ano_eje+m.sec_ejec+m.sec_ejec2+m.sec_nota+m.origen+m.fuente_financ,'nota_modificatoria_fte','MP_NOTAFTE') THEN 
			WAIT WINDOW 'Insertando en Tabla Nota_modificatoria_fte -->>'+m.ano_eje+m.sec_ejec+m.sec_ejec2+m.sec_nota+m.origen+m.fuente_financ NOWAIT 
			INSERT INTO nota_modificatoria_fte FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 

	USE IN nota_modificatoria_fte
	USE IN cur_nota_modificatoria_fte
	SET DELETED ON 
*!*		SELECT curProceso
*!*		IF SEEK(parmSecEjec+parmAnoEje+'01','curProceso','inx1') THEN 
*!*			REPLACE curProceso.IndicadorInserta WITH 1
*!*		ENDIF 	
	
ENDPROC

PROCEDURE inserta_nota_modificatoria_ing
	USE siaf!nota_modificatoria_ing IN 0 ORDER tag mpnotaingp AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'nota_modificatoria_ing.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 
	SET DELETED OFF 
	SELECT cur_nota_modificatoria_ing
	GO TOP 
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje = ALLTRIM(m.ano_eje)
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.sec_ejec2 = ALLTRIM(m.sec_ejec2)
		m.sec_nota = ALLTRIM(m.sec_nota)
		m.origen = ALLTRIM(m.origen)
		m.fuente_financ = ALLTRIM(m.fuente_financ)
*		m.tipo_recurso = RTRIM(m.tipo_recurso)
		m.id_clasificador = ALLTRIM(m.id_clasificador)		
		IF !SEEK(m.ano_eje+m.sec_ejec+m.sec_ejec2+m.sec_nota+m.origen+m.fuente_financ+m.tipo_recurso+m.id_clasificador,'nota_modificatoria_ing','mpnotaingp') THEN 
			WAIT WINDOW 'Insertando en Tabla Nota_modificatoria_ing -->>'+m.ano_eje+m.sec_ejec+m.sec_ejec2+m.sec_nota+m.origen+m.fuente_financ+m.tipo_recurso+m.id_clasificador NOWAIT 
			INSERT INTO nota_modificatoria_ing FROM MEMVAR 
		ENDIF 
	
	ENDSCAN 

	USE IN nota_modificatoria_ing
	USE IN cur_nota_modificatoria_ing
	SET DELETED ON 
*!*		SELECT curProceso
*!*		IF SEEK(parmSecEjec+parmAnoEje+'01','curProceso','inx1') THEN 
*!*			REPLACE curProceso.IndicadorInserta WITH 1
*!*		ENDIF 	
	
ENDPROC


PROCEDURE inserta_mpp_pca_x_generica
	USE siaf!mpp_pca_x_generica IN 0 ORDER tag pca_ge AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'mpp_pca_x_generica.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 
	SET DELETED OFF 
	SELECT cur_mpp_pca_x_generica
	GO TOP 
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje = ALLTRIM(m.ano_eje)
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.fuente_financ = ALLTRIM(m.fuente_financ)
		m.categoria_gasto = ALLTRIM(m.categoria_gasto)
		m.tipo_transaccion = ALLTRIM(m.tipo_transaccion)
		m.generica = ALLTRIM(m.generica)
		IF !SEEK(m.ano_eje+m.sec_ejec+m.fuente_financ+m.categoria_gasto+m.tipo_transaccion+m.generica,'mpp_pca_x_generica','pca_ge') THEN 
			WAIT WINDOW 'Insertando en Tabla Mpp_pca_x_generica -->>'+m.ano_eje+m.sec_ejec+m.fuente_financ+m.categoria_gasto+m.tipo_transaccion+m.generica NOWAIT 
			INSERT INTO mpp_pca_x_generica FROM MEMVAR 
		ENDIF 
	ENDSCAN 

	USE IN mpp_pca_x_generica
	USE IN cur_mpp_pca_x_generica
	SET DELETED ON 
*!*		SELECT curProceso
*!*		IF SEEK(parmSecEjec+parmAnoEje+'01','curProceso','inx1') THEN 
*!*			REPLACE curProceso.IndicadorInserta WITH 1
*!*		ENDIF 	
	
ENDPROC


PROCEDURE inserta_mpp_pca_x_especifica
	USE siaf!mpp_pca_x_especifica IN 0 ORDER tag pcadet AGAIN 
	llOk = .T.
	lcArchivoXML = lcRutaImporta+'mpp_pca_x_especifica.xml' 
	IF !FILE(lcArchivoXML) THEN 
		llOk = .F.
	ENDIF 
	IF !lloK THEN 
		RETURN 
	ENDIF 
	SET DELETED OFF 
	SELECT cur_mpp_pca_x_especifica
	GO TOP 
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje = ALLTRIM(m.ano_eje)
		m.sec_ejec = ALLTRIM(m.sec_ejec)
		m.fuente_financ = ALLTRIM(m.fuente_financ)
		m.categoria_gasto = ALLTRIM(m.categoria_gasto)
		m.tipo_transaccion = ALLTRIM(m.tipo_transaccion)
		m.generica = ALLTRIM(m.generica)
		m.id_clasificador = ALLTRIM(m.id_clasificador)		
		IF !SEEK(m.ano_eje+m.sec_ejec+m.fuente_financ+m.categoria_gasto+m.tipo_transaccion+m.generica+m.id_clasificador,'mpp_pca_x_especifica','pcadet') THEN 
			WAIT WINDOW 'Insertando en Tabla Mpp_pca_x_especifica -->>'+m.ano_eje+m.sec_ejec+m.fuente_financ+m.categoria_gasto+m.tipo_transaccion+m.generica+m.id_clasificador NOWAIT 
			INSERT INTO mpp_pca_x_especifica FROM MEMVAR 
		ENDIF 
	ENDSCAN 

	USE IN mpp_pca_x_especifica
	USE IN cur_mpp_pca_x_especifica
	SET DELETED ON 
*!*		SELECT curProceso
*!*		IF SEEK(parmSecEjec+parmAnoEje+'01','curProceso','inx1') THEN 
*!*			REPLACE curProceso.IndicadorInserta WITH 1
*!*		ENDIF 	
	
ENDPROC