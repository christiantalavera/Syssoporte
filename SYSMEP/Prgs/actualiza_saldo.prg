USE siaf!certificado			IN 0 SHARED ALIAS r_certificado AGAIN ORDER tag certi
USE siaf!certificado_fase		IN 0 SHARED ALIAS r_certificado_fase again ORDER tag certi_fase
USE siaf!certificado_secuencia	IN 0 SHARED ALIAS r_certificado_secuencia AGAIN ORDER tag certi_sec
USE siaf!certificado_clasif		IN 0 SHARED ALIAS r_certificado_clasif AGAIN ORDER tag certi_clas
USE siaf!certificado_meta		IN 0 SHARED ALIAS r_certificado_meta	again ORDER tag certi_meta
USE cert!ejecucion_detalle		IN 0 SHARED AGAIN ORDER tag ejecdet
USE cert!certificado_detalle	IN 0 SHARED again ORDER tag cert_det

SELECT r_certificado
SEEK gcano_eje+gcsec_ejec
SCAN WHILE ano_eje+sec_ejec = gcano_eje+gcsec_ejec
	clave_cert = gcano_eje+gcsec_ejec+r_certificado.certificado
	SELECT r_certificado_fase
	SEEK clave_cert
	SCAN WHILE ano_eje+sec_ejec+certificado = clave_cert FOR etapa = '2'
		lcFuente_financ = r_certificado_fase.fuente_financ
		clave_cert_fase = clave_cert + r_certificado_fase.secuencia
		SELECT r_certificado_secuencia
		SEEK clave_cert_fase
		SCAN WHILE ano_eje+sec_ejec+certificado+secuencia = clave_cert_fase
			clave_cert_sec = clave_cert_fase+r_certificado_secuencia.correlativo 
			IF r_certificado_secuencia.estado_registro <> 'A' THEN 
				LOOP 
			ENDIF 
			SELECT r_certificado_clasif
			SEEK clave_cert_sec
			SCAN WHILE ano_eje+sec_ejec+certificado+secuencia+correlativo = clave_cert_sec
				clave_cert_clasif = clave_cert_sec + r_certificado_clasif.id_clasificador
				SELECT r_certificado_meta
				SEEK clave_cert_clasif
				SCAN WHILE ano_eje+sec_ejec+certificado+secuencia+correlativo+id_clasificador = clave_cert_clasif
					SCATTER MEMVAR 
					m.origen = '1'
					m.fuente_financ = lcFuente_financ
					xclave = gcano_eje+gcsec_ejec+m.certificado+m.secuencia+m.correlativo+m.id_clasificador+m.sec_func
					
					*ano_eje+sec_ejec+certificado+secuencia+correlativo+id_clasificador+sec_func+id_centro_costo+id_gpr
					SELECT certificado_detalle
					SEEK xclave
					SCAN WHILE ano_eje+sec_ejec+certificado+secuencia+correlativo+id_clasificador+sec_func = xclave
						SCATTER MEMVAR 
						IF EMPTY(m.id_centro_costo) OR EMPTY(m.id_gpr) THEN 
							LOOP 
						ENDIF 	
						m.comprometido_anual = certificado_detalle.monto_nacional
						IF !SEEK(gcano_eje+gcsec_ejec+m.certificado+m.secuencia+m.origen+m.fuente_financ+m.id_clasificador+m.sec_func+m.id_centro_costo+m.id_gpr,'ejecucion_detalle') THEN 
							INSERT INTO ejecucion_detalle FROM MEMVAR 
						ELSE
							REPLACE ejecucion_detalle.comprometido_anual WITH ejecucion_detalle.comprometido_anual + m.comprometido_anual
						ENDIF 
					ENDSCAN 
				ENDSCAN 
			ENDSCAN 
		ENDSCAN 
	ENDSCAN 

ENDSCAN 

USE IN r_certificado_meta
USE IN r_certificado_clasif
USE IN r_certificado_secuencia
USE IN r_certificado_fase
USE IN r_certificado
USE IN certificado_detalle