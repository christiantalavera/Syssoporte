LOCAL lnMonto,lnAdelantos,lnPagos,lnLiquidacion,lnTotal,lnoldalias, ;
	lcTipo_id,lcRuc,lcCod_doc,lcNum_doc,lcSec_ejec_contrato, lcId_proceso, lcId_contrato, lcTipoCambio,lcMoneda_contrato


lnoldalias=SELECT()
USE SIAF!contrato_ps IN 0 again ALIAS a_contrato_ps ORDER contrato
USE SIAF!expediente IN 0 AGAIN ALIAS a_expediente ORDER contrato
USE SIAF!expediente_fase IN 0 AGAIN ALIAS a_fase ORDER exp_fase
USE SIAF!expediente_secuencia IN 0 AGAIN ALIAS a_secuencia ORDER exp_sec
USE SIAF!expediente_meta IN 0 AGAIN ALIAS a_meta ORDER exp_meta
USE SIAF!expediente_ingreso IN 0 AGAIN ALIAS a_ingreso ORDER exp_ing

SELECT a_secuencia
SET RELATION TO ano_eje+sec_ejec+expediente+ciclo+fase+secuencia INTO a_fase

STORE 0 TO lnMonto,lnAdelantos,lnPagos,lnLiquidacion

SELECT a_contrato_Ps
SCAN ALL
	SELECT a_contrato_ps
	SCATTER memvar
	lcTipo_id			=	m.tipo_id
	lcRuc				=	m.ruc
	lcsec_ejec_contrato = 	m.sec_ejec
	lcId_proceso 		= 	m.Id_proceso
	lcId_contrato		= 	m.Id_contrato
	lcMoneda_contrato   =   ALLTRIM(m.moneda)
	*--
	SELECT a_expediente
	IF SEEK(lcsec_ejec_contrato+lcId_proceso+lcId_contrato)
		lnAdelantos		= 0
		lnPagos			= 0
		lnLiquidacion	= 0
		lnDevengado		= 0
		lnGirado		= 0
		lnPagado		= 0
		SCAN WHILE sec_ejec_contrato+id_proceso+id_contrato = lcsec_ejec_contrato +lcId_proceso+lcId_contrato
			SELECT a_secuencia
			IF SEEK(a_expediente.ano_eje+a_expediente.sec_ejec+a_expediente.expediente)
				SCAN WHILE ;
						ano_eje+sec_ejec+expediente=a_expediente.ano_eje+a_expediente.sec_ejec+ ;
						a_expediente.expediente FOR a_fase.tipo_id=lcTipo_id AND a_fase.ruc=lcRuc
					IF lcMoneda_contrato = ALLTRIM(a_secuencia.moneda) then
						IF ALLTRIM(a_secuencia.moneda) = 'S/.'
*!*								IF a_secuencia.tipo_cambio_ps = 0 then
*!*									LOOP
*!*								ENDIF
							lcTipoCambio = 1
							DO CASE
								CASE a_expediente.fase_contractual = 'A' AND ciclo='G' AND fase='C'
									lnAdelantos = lnAdelantos + (a_secuencia.monto_nacional / lcTipoCambio)
								CASE a_expediente.fase_contractual = 'P' AND ciclo='G' AND fase='C'
									lnPagos = lnPagos + (a_secuencia.monto_nacional / lcTipoCambio)
								CASE a_expediente.fase_contractual = 'L' AND ciclo='G' AND fase='C'
									lnLiquidacion = lnLiquidacion + (a_secuencia.monto_nacional / lcTipoCambio)
							ENDCASE
							DO CASE
								CASE ciclo='G' AND fase = 'D'
									lnDevengado = lnDevengado + (a_secuencia.monto_nacional / lcTipoCambio)
								CASE ciclo='G' AND fase = 'G'
									lnGirado = lnGirado + (a_secuencia.monto_nacional / lcTipoCambio)
								CASE ciclo='G' AND fase = 'P'
									lnPagado = lnPagado + (a_secuencia.monto_nacional / lcTipoCambio)
							ENDCASE
						ELSE
							IF a_secuencia.tipo_cambio_ps = 0 then
								LOOP
							ENDIF
							lcTipoCambio = IIF(a_secuencia.tipo_cambio=0,1,a_secuencia.tipo_cambio)
							DO CASE
								CASE a_expediente.fase_contractual = 'A' AND ciclo='G' AND fase='C'
									lnAdelantos = lnAdelantos + (a_secuencia.monto)
								CASE a_expediente.fase_contractual = 'P' AND ciclo='G' AND fase='C'
									lnPagos = lnPagos + (a_secuencia.monto)
								CASE a_expediente.fase_contractual = 'L' AND ciclo='G' AND fase='C'
									lnLiquidacion = lnLiquidacion + (a_secuencia.monto)
							ENDCASE
							DO CASE
								CASE ciclo='G' AND fase = 'D'
									lnDevengado = lnDevengado + a_secuencia.monto
								CASE ciclo='G' AND fase = 'G'
									lnGirado = lnGirado + a_secuencia.monto
								CASE ciclo='G' AND fase = 'P'
									lnPagado = lnPagado + a_secuencia.monto
							ENDCASE
						ENDIF
					ELSE
						IF ALLTRIM(a_secuencia.moneda) <> 'S/.'
							IF a_secuencia.tipo_cambio_ps = 0 then
								LOOP
							ENDIF
							lcTipoCambio = 1
							DO CASE
								CASE a_expediente.fase_contractual = 'A' AND ciclo='G' AND fase='C'
									lnAdelantos = lnAdelantos + (a_secuencia.monto_nacional / lcTipoCambio)
								CASE a_expediente.fase_contractual = 'P' AND ciclo='G' AND fase='C'
									lnPagos = lnPagos + (a_secuencia.monto_nacional / lcTipoCambio)
								CASE a_expediente.fase_contractual = 'L' AND ciclo='G' AND fase='C'
									lnLiquidacion = lnLiquidacion + (a_secuencia.monto_nacional / lcTipoCambio)
							ENDCASE
							DO CASE
								CASE ciclo='G' AND fase = 'D'
									lnDevengado = lnDevengado + (a_secuencia.monto_nacional / lcTipoCambio)
								CASE ciclo='G' AND fase = 'G'
									lnGirado = lnGirado + (a_secuencia.monto_nacional / lcTipoCambio)
								CASE ciclo='G' AND fase = 'P'
									lnPagado = lnPagado + (a_secuencia.monto_nacional / lcTipoCambio)
							ENDCASE
						ELSE
*!*								IF a_secuencia.tipo_cambio_ps = 0 then
*!*									LOOP
*!*								ENDIF
							lcTipoCambio = IIF(a_secuencia.tipo_cambio_ps=0,1,a_secuencia.tipo_cambio_ps)
							DO CASE
								CASE a_expediente.fase_contractual = 'A' AND ciclo='G' AND fase='C'
									lnAdelantos = lnAdelantos + (a_secuencia.monto_nacional / lcTipoCambio)
								CASE a_expediente.fase_contractual = 'P' AND ciclo='G' AND fase='C'
									lnPagos = lnPagos + (a_secuencia.monto_nacional / lcTipoCambio)
								CASE a_expediente.fase_contractual = 'L' AND ciclo='G' AND fase='C'
									lnLiquidacion = lnLiquidacion + (a_secuencia.monto_nacional / lcTipoCambio)
							ENDCASE
							DO CASE
								CASE ciclo='G' AND fase = 'D'
									lnDevengado = lnDevengado + (a_secuencia.monto_nacional / lcTipoCambio)
								CASE ciclo='G' AND fase = 'G'
									lnGirado = lnGirado + (a_secuencia.monto_nacional / lcTipoCambio)
								CASE ciclo='G' AND fase = 'P'
									lnPagado = lnPagado + (a_secuencia.monto_nacional / lcTipoCambio)
							ENDCASE
						ENDIF
					ENDIF
				ENDSCAN
			ENDIF
		ENDSCAN
		
		tempo = lnAdelantos + lnPagos + lnLiquidacion
		SELECT a_contrato_ps
		REPLACE a_contrato_ps.monto_comprometido WITH tempo, ; 
			a_contrato_ps.monto_devengado WITH lndevengado, ;
			a_contrato_ps.monto_girado WITH lngirado, ;
			a_contrato_ps.monto_pagado WITH lnpagado
	ENDIF

ENDSCAN

SELECT a_expediente
USE
SELECT a_fase
USE
SELECT a_meta
USE
SELECT a_ingreso
USE
SELECT a_secuencia
USE
SELECT a_contrato_ps
USE

SELECT (lnoldalias)

