DO Abrir_tablas
DO cargar_datos


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
DO REFRESCAR_SALDOS
DO CERRAR_TABLAS


PROCEDURE ABRIR_TABLAS
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


ENDPROC 

PROCEDURE cargar_datos
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


ENDPROC 

PROCEDURE refrescar_saldos

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



ENDPROC 


PROCEDURE CERRAR_TABLAS
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