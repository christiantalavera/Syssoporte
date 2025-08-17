USE siaf!acum_x_ubigeo_disp_legal 	IN 0 AGAIN ORDER tag idcadena
USE siaf!certificado_meta			IN 0 AGAIN ORDER TAG CERTI_META
USE SIAF!certificado_fase			IN 0 AGAIN ORDER tag certi_fase
USE siaf!maestro_clasificador		IN 0 AGAIN ORDER tag idclasif
USE siaf!subgenerica_det			IN 0 AGAIN ORDER tag sgenericad
USE siaf!especifica_det				IN 0 AGAIN order tag IdClasif
USE siaf!meta						IN 0 AGAIN ORDER tag meta



SELECT certificado_meta
SET RELATION TO ano_eje+sec_ejec+certificado+secuencia INTO certificado_fase
=SEEK(gcano_eje + gcsec_ejec)
SCAN REST WHILE ano_eje = gcano_eje AND sec_ejec = gcsec_ejec
	STORE 0 TO wmonto_precertificado, wmonto_certificado, wmonto_comprometido_anual
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
	SCATTER MEMVAR 
	=SEEK(gcano_eje+id_clasificador, 'maestro_clasificador', 'idclasif')
	=SEEK(gcano_eje+LEFT(maestro_clasificador.clasificador,6), 'subgenerica_det', 'sgenericad')
*!*		lccateg_gasto = subgenerica_det.categoria_gasto
*!*		IF SEEK(gcano_eje + gcsec_ejec+ certificado_fase.fuente_financ + lccateg_gasto + LEFT(maestro_clasificador.clasificador,2), 'mpp_pca_x_generica', 'pca_ge')
*!*			REPLACE mpp_pca_x_generica.monto_precertificado WITH mpp_pca_x_generica.monto_precertificado + wmonto_precertificado, ;
*!*				mpp_pca_x_generica.monto_certificado WITH mpp_pca_x_generica.monto_certificado + wmonto_certificado, ;
*!*				mpp_pca_x_generica.monto_comprometido_anual WITH mpp_pca_x_generica.monto_comprometido_anual + wmonto_comprometido_anual IN mpp_pca_x_generica
*!*		ENDIF
	*--
*!*		lcllavegasto = gcano_eje + gcsec_ejec + '1' + certificado_fase.fuente_financ + ;
*!*			'0 ' + certificado_meta.sec_func + certificado_meta.id_clasificador
*!*		IF SEEK(lcllavegasto, 'gasto', 'mp_gastop')
*!*			REPLACE gasto.monto_precertificado WITH gasto.monto_precertificado + wmonto_precertificado, ;
*!*				gasto.monto_certificado WITH gasto.monto_certificado + wmonto_certificado, ;
*!*				gasto.monto_comprometido_anual WITH gasto.monto_comprometido_anual + wmonto_comprometido_anual IN gasto
*!*		ENDIF
	*-- Actualizamos acumulado de Dispositivos Legales
	IF VAL(certificado_fase.dispositivo_legal) > 0 AND certificado_fase.etapa = '1'
		=SEEK(certificado_meta.ano_eje+certificado_meta.id_clasificador, 'Especifica_det', 'IdClasif')
		=SEEK(certificado_meta.ano_eje+certificado_meta.sec_ejec+certificado_meta.sec_func, 'Meta')
		m.tipo_transaccion = Especifica_det.tipo_transaccion
		m.generica = Especifica_det.generica
		m.subgenerica = Especifica_det.subgenerica
		m.subgenerica_det = Especifica_det.subgenerica_det
		m.especifica = Especifica_det.especifica
		m.especifica_det = Especifica_det.especifica_det
		m.clasificador = m.tipo_transaccion+m.generica+m.subgenerica+;
			m.subgenerica_det+m.especifica+m.especifica_det
		=SEEK(m.ano_eje+m.tipo_transaccion+m.generica+m.subgenerica+m.subgenerica_det, 'SubGenerica_det')
		m.categoria_gasto = SubGenerica_det.categoria_gasto
		IF SEEK( ;
			certificado_meta.ano_eje + certificado_meta.sec_ejec + certificado_fase.dispositivo_legal + ;
			meta.programa_ppto + meta.act_proy + meta.componente + meta.funcion + ;
			meta.departamento + meta.provincia + meta.distrito + ;
			certificado_fase.fuente_financ + ;
			m.categoria_gasto + m.tipo_transaccion + m.generica, 'acum_x_ubigeo_disp_legal', 'idcadena') AND ;
			(estado_registro = 'A' OR (certificado_meta.monto_nacional + certificado_meta.monto_nacional_ajuste) > 0)
			REPLACE monto_certificado WITH acum_x_ubigeo_disp_legal.monto_certificado + ;
				(certificado_meta.monto_nacional + certificado_meta.monto_nacional_ajuste) IN acum_x_ubigeo_disp_legal
		ELSE
*			WAIT 'Dispositivo Legal ' + certificado_fase.dispositivo_legal + 'no encontrado - Avisar a Soporte SIAF' + WINDOW
		ENDIF
	ENDIF
ENDSCAN


USE IN acum_x_ubigeo_disp_legal 
USE IN certificado_meta		
USE IN certificado_fase	
USE IN maestro_clasificador	
USE IN subgenerica_det		
USE IN especifica_det			
USE IN meta					