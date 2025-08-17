*-----------------------------*
* S A L D O S  D E  B A N C O *
* S A L D O S  D E  B A N C O *
*-----------------------------*
LPARAMETERS pcejec_rel
*
USE SIAF!saldo_banco			IN 0 AGAIN SHARED 		 			ORDER TAG saldo_bco
USE SIAF!saldo_banco_fuente		IN 0 AGAIN SHARED 		 			ORDER TAG saldo_fte
USE SIAF!expediente				IN 0 AGAIN SHARED ALIAS EXP 			ORDER TAG expediente
USE SIAF!expediente_fase		IN 0 AGAIN SHARED ALIAS exp_fase 		ORDER TAG expediente
USE SIAF!expediente_clasif      IN 0 AGAIN SHARED ALIAS exp_clasif 	ORDER TAG exp_clasif
USE SIAF!expediente_secuencia	IN 0 AGAIN SHARED ALIAS exp_sec 		ORDER TAG exp_sec
USE SIAF!expediente_documento	IN 0 AGAIN SHARED ALIAS exp_doc 		ORDER TAG exp_doc
USE SIAF!expediente_ingreso     IN 0 AGAIN SHARED ALIAS exp_ing 		ORDER TAG exp_ing
USE SIAF!expediente_enlace      IN 0 AGAIN SHARED ALIAS exp_enlace	ORDER TAG exp_enlace
USE SIAF!maestro_documento		IN 0 AGAIN SHARED 					ORDER TAG cod_doc
USE SIAF!maestro_documento_ref	IN 0 AGAIN SHARED 					ORDER TAG cod_doc
***

SELECT exp_fase
SET RELATION TO ano_eje+sec_ejec+expediente INTO EXP
***
CREATE CURSOR cur_bco1 ( mes_eje	c(00002), expediente c(00010), ;
	ciclo      c(00001), cod_doc    c(00003), ;
	num_doc	c(00020), nombre     c(00060), ;
	estado		c(00001), debe		 c(00019), ;
	haber		c(00019), fecha_doc	 c(00008) )
INDEX ON mes_eje+expediente+ciclo+cod_doc+num_doc+nombre+estado+debe+haber+fecha_doc TAG ord01 UNIQUE
***
*** Create Cursor CUR_ACUM ( debe  n(19,02), haber n(19,02), debe_nac n(19,02), haber_nac n(19,02), tipo  c(00001) )
***
SELECT tmp_cur_cta
SET ORDER TO clave
lsec_ejec   = pcejec_rel
***
IF LEFT(lcClave,1) = '*' THEN
	SELECT tmp_cur_cta
	SCAN ALL
		lcano_cta	 = tmp_cur_cta.ano_eje
		lcta_cte 	 = tmp_cur_cta.cta_cte
		gs_banco   	 = tmp_cur_cta.banco
		gs_cta_cte 	 = tmp_cur_cta.cta_cte
		IF SEEK(lcano_cta+lsec_ejec+gs_banco+gs_cta_cte,'cta_cte') THEN
			lmoneda  	 = CTA_CTE.moneda
			gs_sec_ejec	 = CTA_CTE.sec_ejec
			gs_tipo    	 = CTA_CTE.tipo_cta_cte
		ENDIF
		DO proceso_general
	ENDSCAN
ELSE
	SELECT tmp_cur_cta
	SEEK lcClave
	SCAN WHILE clave = lcClave
		lcano_cta	 = tmp_cur_cta.ano_eje
		lcta_cte 	 = tmp_cur_cta.cta_cte
		gs_banco   	 = tmp_cur_cta.banco
		gs_cta_cte 	 = tmp_cur_cta.cta_cte
		IF SEEK(lcano_cta+lsec_ejec+gs_banco+gs_cta_cte,'cta_cte') THEN
			lmoneda  	 = CTA_CTE.moneda
			gs_sec_ejec	 = CTA_CTE.sec_ejec
			gs_tipo    	 = CTA_CTE.tipo_cta_cte
		ENDIF
		DO proceso_general
	ENDSCAN
ENDIF





*
SELECT EXP
USE
SELECT exp_fase
USE
SELECT exp_sec
USE
SELECT exp_clasif
USE
SELECT exp_ing
USE
SELECT exp_doc
USE
SELECT exp_enlace
USE
SELE saldo_banco
USE
SELE saldo_banco_fuente
USE
SELE maestro_documento
USE
SELE maestro_documento_ref
USE
WAIT WINDOW 'Fin del proceso.....'


PROCEDURE proceso_general

	SELE saldo_banco
	SEEK lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje
	SCAN WHILE ano_cta_cte+banco+cta_cte+sec_ejec+ano_eje == lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje
		REPLACE debe_mes WITH 0, haber_mes WITH 0, saldo WITH 0, debe_nac WITH 0, haber_nac WITH 0, saldo_nac WITH 0
	ENDSCAN
	SELE saldo_banco_fuente
	SEEK lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje
	SCAN WHILE ano_cta_cte+banco+cta_cte+sec_ejec+ano_eje == lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje
		REPLACE debe_mes WITH 0, haber_mes WITH 0, saldo WITH 0, debe_nac WITH 0, haber_nac WITH 0, saldo_nac WITH 0
	ENDSCAN
	***
	SELE cta_cte
	SET RELATION TO banco INTO banco
	*
	SELECT exp_sec
	SET ORDER TO TAG cuenta
	SEEK lsec_ejec+lcano_cta+gs_banco+gs_cta_cte
	SCAN WHILE sec_ejec+ano_cta_cte+banco+cta_cte = lsec_ejec+lcano_cta+gs_banco+gs_cta_cte
		*	consistencia entre dos año
		SCATTER MEMVAR
		*TODO: C.RUBDIO para el tratamiento de la cta tesoro igual a las otras
		DO CASE
			CASE gcano_eje < '2003'
				lcif = "exp_sec.BANCO+exp_sec.CTA_CTE <> '001001' and "
				lcif = lcif + "Year(exp_sec.fecha_doc) > Val(lcano_cta) and lcano_cta = gcano_eje"
			OTHER
				lcif = "Year(exp_sec.fecha_doc) > Val(lcano_cta) and lcano_cta = gcano_eje"
		ENDCASE
		IF &lcif. THEN
			wcllave_sec  = 	exp_sec.ano_eje+exp_sec.sec_ejec+;
				exp_sec.expediente+exp_sec.ciclo+;
				exp_sec.fase+exp_sec.secuencia+;
				exp_sec.correlativo
			IF SEEK(wcllave_sec,'exp_doc','exp_doc') THEN
				IF YEAR(exp_doc.fecha_doc) > VAL(lcano_cta) THEN
					SELE exp_sec
					LOOP
				ENDIF
			ELSE
				LOOP
			ENDIF
		ENDIF
		lcexpdoc = exp_sec.ano_eje+exp_sec.sec_ejec+;
			exp_sec.expediente+exp_sec.ciclo+;
			exp_sec.fase+exp_sec.secuencia+;
			exp_sec.correlativo
		&&OB.(22/06/2010) Si no Existe en Expediente_documento, no tomarlo en cuenta
		*!*		IF !SEEK(lcexpdoc,'exp_doc','exp_doc')
		*!*			 LOOP
		*!*		ENDIF
		&&PC/EM(05/01/2011) Se agrega para igualar con lo de libro banco. No considerar si es ingreso y sea Anulacion de Devolucion o Devolucion sin documento B
		*!*		IF m.ciclo='I' AND m.fase='R' AND INLIST(m.estado,'Y','D') AND !SEEK(lcexpdoc,'exp_doc','exp_doc')
		*!*			LOOP
		*!*		ENDIF
		&& Cambio realizado por JM el 14-09-2012
		IF m.ciclo='I' AND m.fase='R' AND m.estado = 'D' AND !SEEK(lcexpdoc,'exp_doc','exp_doc')
			LOOP
		ENDIF
		IF m.ciclo='I' AND m.fase='R' AND m.estado = 'Y' AND !SEEK(lcexpdoc,'exp_doc','exp_doc')
			llExisteEnlace = .f.
			lcLlaveEnlace = m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase+m.secuencia+m.correlativo
			SELECT Exp_enlace
			SEEK lcLlaveEnlace
			SCAN WHILE ano_eje+sec_ejec+expediente+ciclo+fase+secuencia+correlativo==lcLlaveEnlace ;
					FOR estado=='Y' AND !DELETED()
				lcexpdoc = Exp_enlace.ano_eje + ;
					Exp_enlace.sec_ejec + ;
					Exp_enlace.expediente + ;
					Exp_enlace.ciclo_gen + ;
					Exp_enlace.fase_gen + ;
					Exp_enlace.secuencia_gen + ;
					Exp_enlace.correlativo_gen
				llExisteEnlace = .t.
			ENDSCAN
			IF !llExisteEnlace
				LOOP
			ENDIF
		ENDIF
		* no operaciones de ano y cuenta anterior
		IF YEAR(exp_sec.fecha_doc) < VAL(gcano_eje) AND exp_sec.ano_eje == lcano_cta THEN
			LOOP
		ENDIF
		* no anulaciones por error *
		IF INLIST(exp_sec.estado, "X" ) THEN
			LOOP
		ENDIF
		* solo GG, GP o IR *
		IF !INLIST(exp_sec.ciclo+exp_sec.fase, "GG", "GP", "IR")
			LOOP
		ENDIF
		* no modificados *
		IF exp_sec.edicion == "R" THEN    && "R" = Reasignado
			LOOP
		ENDIF
		=SEEK(exp_sec.ano_eje+exp_sec.sec_ejec+exp_sec.expediente+;
			exp_sec.ciclo+exp_sec.fase+exp_sec.secuencia,;
			'exp_fase','EXP_FASE')
		***
		lano_eje       = exp_sec.ano_eje
		ls_ciclo       = exp_sec.ciclo
		ls_fase        = exp_sec.fase
		ls_expediente  = exp_sec.expediente
		ls_secuencia   = exp_sec.secuencia
		***
		ls_correlativo = exp_sec.correlativo
		ls_moneda      = exp_sec.moneda
		ls_tipo_cambio = exp_sec.tipo_cambio
		ls_estado      = exp_sec.estado
		***
		ls_origen	= exp_fase.origen
		ls_fuente	= exp_fase.fuente_financ
		DO CASE
			CASE ls_ciclo+ls_fase == "IR"
				IF INLIST(exp_sec.estado,"D") THEN  && D = Devolución , Y = Anulación de Devolución
					=procesa_ir_d()
				ELSE
					IF INLIST(exp_fase.origen+exp_fase.fuente_financ,'101','103','104',;
							'107','108','109','111','112','113') THEN && EJERCICIOS ANTERIORES "421001"
						=procesa_ir_ej_ant()
					ENDIF
					*
					IF EXP.tipo_operacion <> "YT" THEN
						SELECT exp_sec
						IF EMPTY(exp_sec.serie_doc) THEN
							ls_num_doc = exp_sec.num_doc
						ELSE
							ls_num_doc = LEFT(exp_sec.serie_doc+"-"+exp_sec.num_doc, 20 )
						ENDIF
						ls_cod_doc = exp_sec.cod_doc
						ls_fec_doc = exp_sec.fecha_doc
						STORE SPACE(60) TO ls_nombre
						*
						IF EXP.tipo_operacion == "YV" THEN
							=procesa_ir_yv() && por las operaciones YV ( REBAJA DE IGV )
						ELSE
							IF exp_sec.estado = 'R' THEN && por las rebajas
								=procesa_ir_r()
							ELSE
								=procesa_ir_an() && por las anulaciones
							ENDIF
						ENDIF
					ELSE
						SELECT exp_sec
						IF exp_sec.monto > 0 AND INLIST(exp_sec.estado, "A" ) THEN
							=procesa_ir_tr_an() && por anulaciones de transfrencias
						ELSE
							=procesa_ir_tr() && por las transferencias
						ENDIF
					ENDIF
				ENDIF
			CASE ls_ciclo+ls_fase == "GG"
				control_neg = 'N'

				IF exp_sec.monto < 00 AND INLIST(exp_sec.estado, "A" ) THEN
					ls_correlativo = "0001"
					control_neg = 'S'
				ENDIF
				IF exp_sec.edicion == "E" THEN
					ls_correlativo = "0001"
					control_neg = 'S'
				ENDIF
				IF SEEK(lano_eje+lsec_ejec+ls_expediente+ls_ciclo+ls_fase+ls_secuencia+ls_correlativo,'EXP_DOC','exp_doc') THEN
					IF 		 ( YEAR(exp_doc.fecha_doc) = VAL(gcano_eje) AND control_neg = 'N' ) ;
							.OR. ( YEAR(exp_sec.fecha_doc) = VAL(gcano_eje) AND control_neg = 'S' ) ;
							.OR. ( YEAR(exp_sec.fecha_doc) = VAL(gcano_eje) AND exp_sec.estado == 'D' ) THEN
						DO CASE
							CASE exp_sec.estado == "D" AND ;
								!(exp_fase.origen+exp_fase.fuente_financ $ '100/114/116') AND ;
								!(exp_fase.origen+exp_fase.fuente_financ+exp_fase.tipo_recurso $ '113S ') AND ;
								!(INLIST(exp_fase.fuente_financ+exp_fase.tipo_recurso,'18D ','18E ','18H ','18I ','18J ','18K ','18L ','18M ','18N ','18Ñ ')) and ;
								!(INLIST(exp_fase.fuente_financ+exp_fase.tipo_recurso,'18O ','18P ','18Q ','18R ','18S ','18T ','18U ','18V ','18W ','18Y ')) AND ; &&Cambio solicitado por EM 10/02/2011. realizado por PC
								!(INLIST(exp_fase.fuente_financ+exp_fase.tipo_recurso,'1810','1811','1812')) and ; && Solicitado por Maguiña
								!(INLIST(exp_fase.fuente_financ+exp_fase.tipo_recurso,'13O ','0010','1310','1818','087 ','083 ','1911','1912','1913','006 ','1915','0014')) && LGM 23052014 (RB 00,13,18 y TR 10 y 18)
								IF YEAR(exp_sec.fecha_doc) = VAL(gcano_eje) THEN
									=procesa_gg() 	 && Devoluciones FF <> 00, 14, 16, 13-S
								ENDIF
							CASE 	exp_sec.estado <> "D"
								=procesa_gg_otros() && OPeraciones diferentes de Devolucion y Fuente 09
						ENDCASE
					ENDIF
				ELSE
					IF ( YEAR(exp_sec.fecha_doc) = VAL(gcano_eje) AND exp_sec.estado == 'D' ) THEN
						DO CASE
							CASE exp_sec.estado == "D" AND ;
									!(exp_fase.origen+exp_fase.fuente_financ $ '100/114/116') AND;
									!(exp_fase.origen+exp_fase.fuente_financ+exp_fase.tipo_recurso $ '113S ') AND;
									!(INLIST(exp_fase.fuente_financ+exp_fase.tipo_recurso,'18D ','18E ','18H ','18I ','18J ','18K ','18L ','18M ','18N ','18Ñ ')) and ;
									!(INLIST(exp_fase.fuente_financ+exp_fase.tipo_recurso,'18O ','18P ','18Q ','18R ','18S ','18T ','18U ','18V ','18W ','18Y ')) AND ; && * cambio solicitado por EM 10/02/2011. realizado por PC
								!(INLIST(exp_fase.fuente_financ+exp_fase.tipo_recurso,'1810','1811','1812')) AND ;
								!(INLIST(exp_fase.fuente_financ+exp_fase.tipo_recurso,'13O ','0010','1310','1818','087 ','083 ','1911','1912','1913','006 ','1915','0014')) && LGM 23052014 (RB 00,13,18 y TR 10 y 18) && Solicitado por Maguiña
								=procesa_gg() 	 	&& Devoluciones FF <> 00, 14, 16, 13-S
							OTHERWISE
								=procesa_gg_otros() && OPeraciones diferentes de Devolucion y Fuente 09
						ENDCASE
					ENDIF
				ENDIF
			CASE ls_ciclo+ls_fase == "GP"
				lorden_ciclo = "2"
				* Pagado FF 00 **
				lcondicion01 = "(EXP_FASE.origen+EXP_FASE.fuente_financ $ '100/114/116' .and. EXP_FASE.sec_ejec <> '000047' )"
				lcondicion02 = "EXP_FASE.sec_ejec = '000047' "
				lcondicion03 = "INLIST(EXP_FASE.origen+EXP_FASE.fuente_financ+EXP_FASE.tipo_recurso,'1121 ','1127 ','1128 ')"
				lcondicion04 = "INLIST(EXP_FASE.origen+EXP_FASE.fuente_financ+EXP_FASE.tipo_recurso,'113S ','1131 ','1132 ','1133 ','113N ')"
				lcondicion05 = "gctipo_unidad+EXP_FASE.origen+EXP_FASE.fuente_financ+EXP_FASE.tipo_recurso = 'M113J '"
				lcondicion06 = "EXP_FASE.origen+EXP_FASE.fuente_financ+EXP_FASE.tipo_recurso = '1151 '"
				lcondicion07 = "(EXP_FASE.origen+EXP_FASE.fuente_financ = '117' AND EXP_FASE.tipo_recurso != '0 ')"
				* cambio solicitado por EM 10/02/2011. realizado por PC
				lcondicion08 = "INLIST(exp_fase.fuente_financ+exp_fase.tipo_recurso,'18D ','18E ','18H ','18I ','18J ','18K ','18L ','18M ','18N ','18Ñ ')"
				lcondicion09 = "INLIST(exp_fase.fuente_financ+exp_fase.tipo_recurso,'18O ','18P ','18Q ','18R ','18S ','18T ','18U ','18V ','18W ','18Y ','00D ','00I ')"
				lcondicion10 = "inlist(EXP_FASE.fuente_financ+EXP_FASE.tipo_recurso,'07A ','19A ','1311','1315','1316','1314','19H ','19G ','041 ')"
				lcondicion11 = "inlist(EXP_FASE.fuente_financ+EXP_FASE.tipo_recurso,'1810','1811','1812','1813','1814','1815','1816', '1817','1819','1820','1821','19F ','19G ')" && Solicitado por Maguiña
				lcondicion12 = "inlist(EXP_FASE.fuente_financ+EXP_FASE.tipo_recurso,'13O ','13P ','0010','1310','1818','198','087 ','083 ','1911','1912','1913','1915','0014','1824','18L ')"  && LGM 23052014 (RB 00,13,18 y TR 10 y 18)
				lcondicion13 = "inlist(EXP_FASE.fuente_financ+EXP_FASE.tipo_recurso,'097 ','1318', '1317','1323','1318')"   &&& Por sugerencia de Karina Diaz en acuerdo con Mario Holgado hecho por talavera
				lcondicion14 = "inlist(EXP_FASE.fuente_financ+EXP_FASE.tipo_recurso,'073 ','083 ')"   &&& Por sugerencia de Karina Dia - 21012016				&&& Se agrego 1820,1821,19F Agricultura 08012016
				IF &lcondicion01. OR ;
						&lcondicion02. OR ;
						&lcondicion03. OR ;
						&lcondicion04. OR ;
						&lcondicion05. OR ;
						&lcondicion06. OR ;
						&lcondicion07. OR ;
						&lcondicion08. OR ;
						&lcondicion09. OR ;
						&lcondicion10. OR ;
						&lcondicion11. OR ;
						&lcondicion12. OR ;
						&lcondicion13. OR ;
						&lcondicion14. THEN
					SELECT exp_doc
					SEEK lano_eje+lsec_ejec+ls_expediente+ls_ciclo+ls_fase+ls_secuencia+ls_correlativo
					IF FOUND() THEN
						=procesa_gp_n()		&& pagados normales
					ELSE
						IF exp_sec.estado = 'O' THEN
							=procesa_gp_extorno()	&& Extorno de Pagados
						ELSE
							=procesa_gp_reas()	&& pagados reasignados
						ENDIF
					ENDIF
				ENDIF
		ENDCASE
	ENDSCAN


ENDPROC


*---------------------
FUNCTION procesa_ir_d
	*---------------------
	SELECT exp_doc
	SEEK lano_eje+lsec_ejec+ls_expediente+ls_ciclo+ls_fase+ls_secuencia+ls_correlativo
	SCAN WHILE ano_eje+sec_ejec+expediente+ciclo+fase+secuencia+correlativo == ;
			lano_eje+lsec_ejec+ls_expediente+ls_ciclo+ls_fase+ls_secuencia+ls_correlativo
		* no fecha vacia *
		IF EMPTY(exp_doc.fecha_doc) THEN
			LOOP
		ENDIF
		* documentos permitidos *
		IF !INLIST(exp_doc.cod_doc, '020', '025', '026', '033', '044', '049', ;
				'061', '064', '065', '066', '068', '089', '095') THEN
			LOOP
		ENDIF
		****
		ls_mes_eje  = PADL(MONTH(exp_doc.fecha_doc),2,'0')
		ls_nombre 	= exp_doc.nombre
		ls_cod_doc	= exp_doc.cod_doc
		ls_num_doc	= exp_doc.num_doc
		ls_fec_doc	= exp_doc.fecha_doc
		ls_nombre	= exp_doc.nombre
		* LLENA CUR_BCO1
		IF !SEEK(ls_mes_eje+ls_expediente+'I'+exp_doc.cod_doc+exp_doc.num_doc+exp_doc.nombre+'N'+;
				STR(0,19,02)+STR(exp_doc.monto,19,02)+DTOS(exp_doc.fecha_doc),'Cur_bco1','Ord01') THEN
			INSERT INTO cur_bco1 VALUES (ls_mes_eje,				;
				ls_expediente,				;
				'I',						;
				exp_doc.cod_doc,			;
				exp_doc.num_doc,			;
				exp_doc.nombre,			;
				'N',						;
				STR(0,19,02),				;
				STR(exp_doc.monto,19,02),	;
				DTOS(exp_doc.fecha_doc) 	)
			* Ingreso pero se pone G para ubicar documento *
			IF YEAR(exp_doc.fecha_doc) = VAL(gcano_eje) THEN
				* Saldo_Banco
				IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje,'saldo_banco','saldo_bco') THEN
					REPLACE saldo_banco.haber_mes WITH saldo_banco.haber_mes + exp_doc.monto, ;
						saldo_banco.haber_nac WITH saldo_banco.haber_nac + exp_doc.monto_nacional
					REPLACE	saldo_banco.saldo     WITH saldo_banco.debe_mes  - saldo_banco.haber_mes, ;
						saldo_banco.saldo_nac WITH saldo_banco.debe_nac  - saldo_banco.haber_nac
				ELSE
					INSERT INTO saldo_banco VALUES ( lcano_cta,   gs_banco,               gs_cta_cte, ;
						gcano_eje, lsec_ejec,              ls_mes_eje, ;
						0,         exp_doc.monto,          exp_doc.monto*-1, ;
						0,         exp_doc.monto_nacional, exp_doc.monto_nacional*-1 )
				ENDIF
				* Saldo_Banco_Fuente 29/08/2006
				IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje+ls_origen+ls_fuente,'saldo_banco_fuente','saldo_fte') THEN
					REPLACE saldo_banco_fuente.haber_mes WITH saldo_banco_fuente.haber_mes + exp_doc.monto, ;
						saldo_banco_fuente.haber_nac WITH saldo_banco_fuente.haber_nac + exp_doc.monto_nacional
					REPLACE	saldo_banco_fuente.saldo     WITH saldo_banco_fuente.debe_mes  - saldo_banco_fuente.haber_mes, ;
						saldo_banco_fuente.saldo_nac WITH saldo_banco_fuente.debe_nac  - saldo_banco_fuente.haber_nac
				ELSE
					INSERT INTO saldo_banco_fuente VALUES (  lcano_cta,   gs_banco,               gs_cta_cte, ;
						gcano_eje, lsec_ejec,              ls_mes_eje, ;
						ls_origen, ls_fuente, ;
						0,         exp_doc.monto,          exp_doc.monto*-1, ;
						0,         exp_doc.monto_nacional, exp_doc.monto_nacional*-1 )
				ENDIF
			ENDIF
		ENDIF
	ENDSCAN
	RETURN

	*--------------------------
FUNCTION procesa_ir_ej_ant
	*--------------------------
	* Saldo de Ejercicios Anteriores *
	IF EXP.tipo_operacion == "Y " THEN
		IF SEEK(lano_eje+lsec_ejec+exp_fase.expediente+exp_fase.ciclo+ ;
				exp_fase.fase+exp_fase.secuencia+						 ;
				exp_sec.correlativo, "exp_ing") THEN
			IF  exp_ing.clase_ingreso+exp_ing.tipo_ingreso+ ;
					exp_ing.sub_tipo_ingreso+exp_ing.elemento_ingreso=="421001" THEN
				STORE SPACE(60) TO ls_nombre
				ls_mes_eje = PADL(MONTH(exp_sec.fecha_doc),2,'0')
				*	LLENA CUR_BCO1
				IF !SEEK(ls_mes_eje+ls_expediente+ls_ciclo+exp_sec.cod_doc+;
						exp_sec.num_doc+ls_nombre+;
						'N'+STR(0,19,02)+STR(exp_sec.monto,19,02)+;
						DTOS(exp_sec.fecha_doc),'Cur_bco1','Ord01') THEN
					INSERT INTO cur_bco1 VALUES (ls_mes_eje,;
						ls_expediente,;
						ls_ciclo,;
						exp_sec.cod_doc,;
						exp_sec.num_doc,;
						ls_nombre,;
						'N',;
						STR(0,19,02),;
						STR(exp_sec.monto,19,02),;
						DTOS(exp_sec.fecha_doc) )
					IF YEAR(exp_sec.fecha_doc) = VAL(gcano_eje) THEN
						* Saldo_Banco
						IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje,'saldo_banco','saldo_bco') THEN
							REPLACE saldo_banco.debe_mes  WITH saldo_banco.debe_mes + exp_sec.monto, ;
								saldo_banco.debe_nac  WITH saldo_banco.debe_nac + exp_sec.monto_nacional
							REPLACE	saldo_banco.saldo     WITH saldo_banco.debe_mes - saldo_banco.haber_mes, ;
								saldo_banco.saldo_nac WITH saldo_banco.debe_nac - saldo_banco.haber_nac
						ELSE
							INSERT INTO saldo_banco VALUES ( lcano_cta,                gs_banco,  gs_cta_cte, ;
								gcano_eje,              lsec_ejec, ls_mes_eje, ;
								exp_sec.monto,          0,         exp_sec.monto, ;
								exp_sec.monto_nacional, 0,         exp_sec.monto_nacional )
						ENDIF
						* Saldo_Banco_Fuente 29/08/2006
						IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje+ls_origen+ls_fuente,'saldo_banco_fuente','saldo_fte') THEN
							REPLACE saldo_banco_fuente.debe_mes  WITH saldo_banco_fuente.debe_mes + exp_sec.monto, ;
								saldo_banco_fuente.debe_nac  WITH saldo_banco_fuente.debe_nac + exp_sec.monto_nacional
							REPLACE	saldo_banco_fuente.saldo     WITH saldo_banco_fuente.debe_mes - saldo_banco_fuente.haber_mes, ;
								saldo_banco_fuente.saldo_nac WITH saldo_banco_fuente.debe_nac - saldo_banco_fuente.haber_nac
						ELSE
							INSERT INTO saldo_banco_fuente VALUES (  lcano_cta,                gs_banco,  gs_cta_cte, ;
								gcano_eje,              lsec_ejec, ls_mes_eje, ;
								ls_origen,				 ls_fuente, ;
								exp_sec.monto,          0,         exp_sec.monto, ;
								exp_sec.monto_nacional, 0,         exp_sec.monto_nacional )
						ENDIF
					ENDIF
				ENDIF
			ENDIF
		ENDIF
	ENDIF
	RETURN


	*---------------------
FUNCTION procesa_ir_yv
	*---------------------
	* anulaciones *
	IF exp_sec.monto > 00 .AND. exp_sec.estado == "A" THEN
		ls_estado="A"
	ENDIF
	*******
	SELECT exp_doc
	SEEK lano_eje+lsec_ejec+ls_expediente+ls_ciclo+ls_fase+ls_secuencia+"0001"
	SCAN WHILE ano_eje+sec_ejec+expediente+ciclo+fase+secuencia+correlativo == ;
			lano_eje+lsec_ejec+ls_expediente+ls_ciclo+ls_fase+ls_secuencia+"0001"
		* Documentos Permitidos *
		IF INLIST(exp_doc.cod_doc, '000', '020', '025', '026', '033', '044', '049', ;
				'061', '064', '065', '066', '068', '081', '087', '088', '089', '095') THEN
			IF exp_sec.monto > 00 .AND. exp_sec.estado == "A"
				ls_mes_eje = PADL(MONTH(exp_sec.fecha_doc),2,'0')
			ELSE
				ls_mes_eje = PADL(MONTH(exp_doc.fecha_doc),2,'0')
			ENDIF
			ls_num_doc = exp_doc.num_doc
			ls_nombre  = exp_doc.nombre
			*	LLENA CUR_BCO1
			IF !SEEK(ls_mes_eje+ls_expediente+ls_ciclo+exp_doc.cod_doc+;
					ls_num_doc+ls_nombre+ls_estado+STR(0,19,02)+;
					STR(exp_doc.monto,19,02)+;
					DTOS(exp_doc.fecha_doc),'Cur_bco1','Ord01') THEN
				INSERT INTO cur_bco1 VALUES (ls_mes_eje,;
					ls_expediente,			ls_ciclo,					;
					exp_doc.cod_doc,		ls_num_doc,					;
					ls_nombre,				ls_estado,					;
					STR(0,19,02),			STR(exp_doc.monto,19,02),	;
					DTOS(exp_doc.fecha_doc) )
				*
				IF ls_estado == "A" THEN
					IF YEAR(exp_sec.fecha_doc) == VAL(gcano_eje) THEN && agregado
						SELECT exp_sec
						x = RECNO()
						=SEEK(lano_eje+lsec_ejec+ls_expediente+ls_ciclo+ls_fase+ls_secuencia,'exp_sec','exp_sec')
						IF PADL(YEAR(exp_sec.fecha_doc),4,'0')+PADL(MONTH(exp_sec.fecha_doc),2,'0') <> PADL(YEAR(ls_fec_doc),4,'0')+PADL(MONTH(ls_fec_doc),2,'0') THEN
							GO x
							* Saldo_Banco
							IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje,'saldo_banco','saldo_bco') THEN
								REPLACE saldo_banco.debe_mes  WITH saldo_banco.debe_mes + exp_doc.monto, ;
									saldo_banco.debe_nac  WITH saldo_banco.debe_nac + exp_doc.monto_nacional
								REPLACE	saldo_banco.saldo     WITH saldo_banco.debe_mes  - saldo_banco.haber_mes, ;
									saldo_banco.saldo_nac WITH saldo_banco.debe_nac  - saldo_banco.haber_nac
							ELSE
								INSERT INTO saldo_banco VALUES ( lcano_cta,   			gs_banco,   gs_cta_cte, 	;
									gcano_eje, 			lsec_ejec,  ls_mes_eje, 	;
									exp_doc.monto, 		0,          exp_doc.monto,  ;
									exp_doc.monto_nacional,0,          exp_doc.monto_nacional )
							ENDIF
							* Saldo_Banco_Fuente 29/08/2006
							IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje+ls_origen+ls_fuente,'saldo_banco_fuente','saldo_fte') THEN
								REPLACE saldo_banco_fuente.debe_mes  WITH saldo_banco_fuente.debe_mes + exp_doc.monto, ;
									saldo_banco_fuente.debe_nac  WITH saldo_banco_fuente.debe_nac + exp_doc.monto_nacional
								REPLACE	saldo_banco_fuente.saldo     WITH saldo_banco_fuente.debe_mes  - saldo_banco_fuente.haber_mes, ;
									saldo_banco_fuente.saldo_nac WITH saldo_banco_fuente.debe_nac  - saldo_banco_fuente.haber_nac
							ELSE
								INSERT INTO saldo_banco_fuente VALUES (  lcano_cta,   			gs_banco,   gs_cta_cte, 	;
									gcano_eje, 			lsec_ejec,  ls_mes_eje, 	;
									ls_origen,				ls_fuente, ;
									exp_doc.monto, 		0,          exp_doc.monto,  ;
									exp_doc.monto_nacional,0,          exp_doc.monto_nacional )
							ENDIF
						ELSE
							GO x
							* Saldo_Banco
							IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje,'saldo_banco','saldo_bco') THEN
								REPLACE saldo_banco.haber_mes  WITH saldo_banco.haber_mes + exp_doc.monto*-1, ;
									saldo_banco.haber_nac  WITH saldo_banco.haber_nac + exp_doc.monto_nacional*-1
								REPLACE	saldo_banco.saldo     WITH saldo_banco.debe_mes  - saldo_banco.haber_mes, ;
									saldo_banco.saldo_nac WITH saldo_banco.debe_nac  - saldo_banco.haber_nac
							ELSE
								INSERT INTO saldo_banco VALUES ( lcano_cta,   gs_banco,   				gs_cta_cte, 	;
									gcano_eje, lsec_ejec,  				ls_mes_eje, 	;
									0,         exp_doc.monto*-1,			exp_doc.monto, 	;
									0,			exp_doc.monto_nacional*-1, 	exp_doc.monto_nacional )
							ENDIF
							* Saldo_Banco_Fuente 29/08/2006
							IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje+ls_origen+ls_fuente,'saldo_banco_fuente','saldo_fte') THEN
								REPLACE saldo_banco_fuente.haber_mes  WITH saldo_banco_fuente.haber_mes + exp_doc.monto*-1, ;
									saldo_banco_fuente.haber_nac  WITH saldo_banco_fuente.haber_nac + exp_doc.monto_nacional*-1
								REPLACE	saldo_banco_fuente.saldo     WITH saldo_banco_fuente.debe_mes  - saldo_banco_fuente.haber_mes, ;
									saldo_banco_fuente.saldo_nac WITH saldo_banco_fuente.debe_nac  - saldo_banco_fuente.haber_nac
							ELSE
								INSERT INTO saldo_banco_fuente VALUES (  lcano_cta,   gs_banco,   				gs_cta_cte, 	;
									gcano_eje, lsec_ejec,  				ls_mes_eje, 	;
									ls_origen,	ls_fuente, ;
									0,         exp_doc.monto*-1,			exp_doc.monto, 	;
									0,			exp_doc.monto_nacional*-1, 	exp_doc.monto_nacional )
							ENDIF
						ENDIF
						GO x
					ENDIF
				ELSE
					IF YEAR(exp_doc.fecha_doc) == VAL(gcano_eje) THEN && agregado
						* Saldo_Banco
						IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje,'saldo_banco','saldo_bco') THEN
							REPLACE saldo_banco.haber_mes  WITH saldo_banco.haber_mes + exp_doc.monto, ;
								saldo_banco.haber_nac  WITH saldo_banco.haber_nac + exp_doc.monto_nacional
							REPLACE	saldo_banco.saldo     WITH saldo_banco.debe_mes  - saldo_banco.haber_mes, ;
								saldo_banco.saldo_nac WITH saldo_banco.debe_nac  - saldo_banco.haber_nac
						ELSE
							INSERT INTO saldo_banco VALUES ( lcano_cta,   gs_banco,   			gs_cta_cte, 	;
								gcano_eje, lsec_ejec,  			ls_mes_eje, 	;
								0,         exp_doc.monto,			exp_doc.monto*-1, 	;
								0,			exp_doc.monto_nacional, exp_doc.monto_nacional*-1 )
						ENDIF
						* Saldo_Banco_Fuente 29/08/2006
						IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje+ls_origen+ls_fuente,'saldo_banco_fuente','saldo_fte') THEN
							REPLACE saldo_banco_fuente.haber_mes  WITH saldo_banco_fuente.haber_mes + exp_doc.monto, ;
								saldo_banco_fuente.haber_nac  WITH saldo_banco_fuente.haber_nac + exp_doc.monto_nacional
							REPLACE	saldo_banco_fuente.saldo     WITH saldo_banco_fuente.debe_mes  - saldo_banco_fuente.haber_mes, ;
								saldo_banco_fuente.saldo_nac WITH saldo_banco_fuente.debe_nac  - saldo_banco_fuente.haber_nac
						ELSE
							INSERT INTO saldo_banco_fuente VALUES (  lcano_cta,   gs_banco,   			gs_cta_cte, 	;
								gcano_eje, lsec_ejec,  			ls_mes_eje, 	;
								ls_origen, ls_fuente, ;
								0,         exp_doc.monto,			exp_doc.monto*-1, 	;
								0,			exp_doc.monto_nacional, exp_doc.monto_nacional*-1 )
						ENDIF
					ENDIF
				ENDIF
			ENDIF
		ENDIF
	ENDSCAN
	RETURN


	*--------------------
FUNCTION procesa_ir_r
	*--------------------
	* Ingreso Normal Rebaja *
	ls_nombre = 'NOTA DE CARGO '+SPACE(44)
	IF SEEK(lano_eje+lsec_ejec+exp_fase.expediente+exp_fase.ciclo+exp_fase.fase+;
			exp_fase.secuencia+exp_sec.correlativo, "exp_ing") THEN
		IF exp_ing.clase_ingreso+exp_ing.tipo_ingreso+ ;
				exp_ing.sub_tipo_ingreso+exp_ing.elemento_ingreso<>"421001" THEN
			ls_mes_eje = PADL(MONTH(exp_sec.fecha_doc),2,'0')
			*	LLENA CUR_BCO1
			IF !SEEK(ls_mes_eje+ls_expediente+ls_ciclo+exp_sec.cod_doc+;
					ls_num_doc+ls_nombre+ls_estado+STR(0,19,02)+;
					STR(exp_sec.monto,19,02)+;
					DTOS(exp_sec.fecha_doc),'Cur_bco1','Ord01') THEN
				INSERT INTO cur_bco1 VALUES (ls_mes_eje,				;
					ls_expediente,				;
					ls_ciclo,					;
					exp_sec.cod_doc,			;
					ls_num_doc,				;
					ls_nombre,					;
					ls_estado,					;
					STR(0,19,02),				;
					STR(exp_sec.monto,19,02),	;
					DTOS(exp_sec.fecha_doc) )
				DO CASE
					CASE YEAR(exp_sec.fecha_doc)=VAL(gcano_eje) &&and exp.tipo_operacion<>'YV'
						* Saldo_Banco
						IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje,'saldo_banco','saldo_bco') THEN
							REPLACE saldo_banco.haber_mes WITH saldo_banco.haber_mes + exp_sec.monto*-1, ;
								saldo_banco.haber_nac WITH saldo_banco.haber_nac + exp_sec.monto_nacional*-1
							REPLACE	saldo_banco.saldo     WITH saldo_banco.debe_mes  - saldo_banco.haber_mes, ;
								saldo_banco.saldo_nac WITH saldo_banco.debe_nac  - saldo_banco.haber_nac
						ELSE
							INSERT INTO saldo_banco VALUES ( lcano_cta,   gs_banco,                  gs_cta_cte, ;
								gcano_eje, lsec_ejec,                 ls_mes_eje, ;
								0,         exp_sec.monto*-1,          exp_sec.monto, ;
								0,         exp_sec.monto_nacional*-1, exp_sec.monto_nacional )
						ENDIF
						* Saldo_Banco_Fuente 29/08/2006
						IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje+ls_origen+ls_fuente,'saldo_banco_fuente','saldo_fte') THEN
							REPLACE saldo_banco_fuente.haber_mes WITH saldo_banco_fuente.haber_mes + exp_sec.monto*-1, ;
								saldo_banco_fuente.haber_nac WITH saldo_banco_fuente.haber_nac + exp_sec.monto_nacional*-1
							REPLACE	saldo_banco_fuente.saldo     WITH saldo_banco_fuente.debe_mes  - saldo_banco_fuente.haber_mes, ;
								saldo_banco_fuente.saldo_nac WITH saldo_banco_fuente.debe_nac  - saldo_banco_fuente.haber_nac
						ELSE
							INSERT INTO saldo_banco_fuente VALUES (  lcano_cta,   gs_banco,                  gs_cta_cte, ;
								gcano_eje, lsec_ejec,                 ls_mes_eje, ;
								ls_origen, ls_fuente, ;
								0,         exp_sec.monto*-1,          exp_sec.monto, ;
								0,         exp_sec.monto_nacional*-1, exp_sec.monto_nacional )
						ENDIF
					OTHER
						***
				ENDCASE
			ENDIF
		ENDIF
	ENDIF
	RETURN

	*---------------------
FUNCTION procesa_ir_an
	*---------------------
	* anulaciones *
	IF exp_sec.monto < 00 .AND. INLIST(exp_sec.estado, "A" ) THEN
		ls_estado="A"
	ENDIF
	*******
	IF SEEK(lano_eje+lsec_ejec+exp_fase.expediente+exp_fase.ciclo+exp_fase.fase+;
			exp_fase.secuencia+exp_sec.correlativo,"exp_ing") THEN
		IF exp_ing.clase_ingreso+exp_ing.tipo_ingreso+ ;
				exp_ing.sub_tipo_ingreso+exp_ing.elemento_ingreso<>"421001" THEN
			ls_mes_eje = PADL(MONTH(exp_sec.fecha_doc),2,'0')
			*	LLENA CUR_BCO1
			IF !SEEK(ls_mes_eje+ls_expediente+ls_ciclo+exp_sec.cod_doc+ls_num_doc+ls_nombre+;
					ls_estado+STR(0,19,02)+STR(exp_sec.monto,19,02)+;
					DTOS(exp_sec.fecha_doc),'Cur_bco1','Ord01') THEN
				INSERT INTO cur_bco1 VALUES (ls_mes_eje,				;
					ls_expediente,				;
					ls_ciclo,					;
					exp_sec.cod_doc,			;
					ls_num_doc,				;
					ls_nombre,					;
					ls_estado,					;
					STR(0,19,02),				;
					STR(exp_sec.monto,19,02),	;
					DTOS(exp_sec.fecha_doc) )
				DO CASE
					CASE YEAR(exp_sec.fecha_doc)=VAL(gcano_eje) &&and exp.tipo_operacion<>'YV'
						IF exp_sec.estado == "Y"
							* Saldo_Banco
							IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje,'saldo_banco','saldo_bco') THEN
								REPLACE saldo_banco.debe_mes  WITH saldo_banco.debe_mes + exp_sec.monto, ;
									saldo_banco.debe_nac  WITH saldo_banco.debe_nac + exp_sec.monto_nacional
								REPLACE	saldo_banco.saldo     WITH saldo_banco.debe_mes - saldo_banco.haber_mes, ;
									saldo_banco.saldo_nac WITH saldo_banco.debe_nac - saldo_banco.haber_nac
							ELSE
								INSERT INTO saldo_banco VALUES ( lcano_cta,                gs_banco,  gs_cta_cte, ;
									gcano_eje,              lsec_ejec, ls_mes_eje, ;
									exp_sec.monto,          0,         exp_sec.monto, ;
									exp_sec.monto_nacional, 0,         exp_sec.monto_nacional )
							ENDIF
							* Saldo_Banco_Fuente 29/08/2006
							IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje+ls_origen+ls_fuente,'saldo_banco_fuente','saldo_fte') THEN
								REPLACE saldo_banco_fuente.debe_mes  WITH saldo_banco_fuente.debe_mes + exp_sec.monto, ;
									saldo_banco_fuente.debe_nac  WITH saldo_banco_fuente.debe_nac + exp_sec.monto_nacional
								REPLACE	saldo_banco_fuente.saldo     WITH saldo_banco_fuente.debe_mes - saldo_banco_fuente.haber_mes, ;
									saldo_banco_fuente.saldo_nac WITH saldo_banco_fuente.debe_nac - saldo_banco_fuente.haber_nac
							ELSE
								INSERT INTO saldo_banco_fuente VALUES (  lcano_cta,                gs_banco,  gs_cta_cte, ;
									gcano_eje,              lsec_ejec, ls_mes_eje, ;
									ls_origen,				 ls_fuente, ;
									exp_sec.monto,          0,         exp_sec.monto, ;
									exp_sec.monto_nacional, 0,         exp_sec.monto_nacional )
							ENDIF
						ELSE
							* Saldo_Banco
							IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje,'saldo_banco','saldo_bco') THEN
								REPLACE saldo_banco.debe_mes  WITH saldo_banco.debe_mes + exp_sec.monto, ;
									saldo_banco.debe_nac  WITH saldo_banco.debe_nac + exp_sec.monto_nacional
								REPLACE	saldo_banco.saldo     WITH saldo_banco.debe_mes - saldo_banco.haber_mes, ;
									saldo_banco.saldo_nac WITH saldo_banco.debe_nac - saldo_banco.haber_nac
							ELSE
								INSERT INTO saldo_banco VALUES ( lcano_cta,                gs_banco,  gs_cta_cte, ;
									gcano_eje,              lsec_ejec, ls_mes_eje, ;
									exp_sec.monto,          0,         exp_sec.monto, ;
									exp_sec.monto_nacional, 0,         exp_sec.monto_nacional )
							ENDIF
							* Saldo_Banco_Fuente 29/08/2006
							IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje+ls_origen+ls_fuente,'saldo_banco_fuente','saldo_fte') THEN
								REPLACE saldo_banco_fuente.debe_mes  WITH saldo_banco_fuente.debe_mes + exp_sec.monto, ;
									saldo_banco_fuente.debe_nac  WITH saldo_banco_fuente.debe_nac + exp_sec.monto_nacional
								REPLACE	saldo_banco_fuente.saldo     WITH saldo_banco_fuente.debe_mes - saldo_banco_fuente.haber_mes, ;
									saldo_banco_fuente.saldo_nac WITH saldo_banco_fuente.debe_nac - saldo_banco_fuente.haber_nac
							ELSE
								INSERT INTO saldo_banco_fuente VALUES (  lcano_cta,                gs_banco,  gs_cta_cte, ;
									gcano_eje,              lsec_ejec, ls_mes_eje, ;
									ls_origen,				 ls_fuente, ;
									exp_sec.monto,          0,         exp_sec.monto, ;
									exp_sec.monto_nacional, 0,         exp_sec.monto_nacional )
							ENDIF
						ENDIF
					OTHER
						***
				ENDCASE
			ENDIF
		ENDIF
	ENDIF
	RETURN

	*------------------------
FUNCTION procesa_ir_tr_an
	*------------------------
	ls_estado="A"
	SELECT exp_doc
	SEEK lano_eje+lsec_ejec+ls_expediente+ls_ciclo+ls_fase+ls_secuencia+"0001"
	SCAN WHILE ano_eje+sec_ejec+expediente+ciclo+fase+secuencia+correlativo == ;
			lano_eje+lsec_ejec+ls_expediente+ls_ciclo+ls_fase+ls_secuencia+"0001"
		* Documentos Permitidos *
		IF INLIST(exp_doc.cod_doc, '000', '020', '025', '026', '033', '044', '049', ;
				'061', '064', '065', '066', '068', '089', '095') THEN
			ls_num_doc = exp_doc.num_doc
			ls_nombre = exp_doc.nombre
			IF SEEK(lano_eje+			;
					lsec_ejec+			;
					exp_fase.expediente+;
					exp_fase.ciclo+		;
					exp_fase.fase+		;
					exp_fase.secuencia+	;
					exp_sec.correlativo , "exp_ing") THEN
				IF YEAR(exp_sec.fecha_doc)=VAL(gcano_eje) THEN
					IF PADL(YEAR(exp_sec.fecha_doc),4,'0')+PADL(MONTH(exp_sec.fecha_doc),2,'0') > ;
							PADL(YEAR(exp_doc.fecha_doc),4,'0')+PADL(MONTH(exp_doc.fecha_doc),2,'0') THEN
						SELECT exp_ing
						ls_mes_eje = PADL(MONTH(exp_sec.fecha_doc),2,'0')
						*	LLENA CUR_BCO1
						IF !SEEK(ls_mes_eje+ls_expediente+ls_ciclo+exp_doc.cod_doc+;
								exp_doc.num_doc+exp_doc.nombre+;
								'A'+STR(0,19,02)+STR(exp_doc.monto,19,02)+;
								DTOS(exp_sec.fecha_doc),'Cur_bco1','Ord01') THEN
							INSERT INTO cur_bco1 VALUES (ls_mes_eje,				;
								ls_expediente,				;
								ls_ciclo,					;
								exp_doc.cod_doc,			;
								exp_doc.num_doc,			;
								exp_doc.nombre,			;
								'A',						;
								STR(0,19,02),				;
								STR(exp_doc.monto,19,02),	;
								DTOS(exp_sec.fecha_doc) )
							* Saldo_Banco
							IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje,'saldo_banco','saldo_bco') THEN
								REPLACE saldo_banco.debe_mes  WITH saldo_banco.debe_mes + exp_doc.monto, ;
									saldo_banco.debe_nac  WITH saldo_banco.debe_nac + exp_doc.monto_nacional
								REPLACE	saldo_banco.saldo     WITH saldo_banco.debe_mes - saldo_banco.haber_mes, ;
									saldo_banco.saldo_nac WITH saldo_banco.debe_nac - saldo_banco.haber_nac
							ELSE
								INSERT INTO saldo_banco VALUES ( lcano_cta,                gs_banco,  gs_cta_cte, ;
									gcano_eje,              lsec_ejec, ls_mes_eje, ;
									exp_doc.monto,          0,         exp_doc.monto, ;
									exp_doc.monto_nacional, 0,         exp_doc.monto_nacional )
							ENDIF
							* Saldo_Banco_Fuente 29/08/2006
							IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje+ls_origen+ls_fuente,'saldo_banco_fuente','saldo_fte') THEN
								REPLACE saldo_banco_fuente.debe_mes  WITH saldo_banco_fuente.debe_mes + exp_doc.monto, ;
									saldo_banco_fuente.debe_nac  WITH saldo_banco_fuente.debe_nac + exp_doc.monto_nacional
								REPLACE	saldo_banco_fuente.saldo     WITH saldo_banco_fuente.debe_mes - saldo_banco_fuente.haber_mes, ;
									saldo_banco_fuente.saldo_nac WITH saldo_banco_fuente.debe_nac - saldo_banco_fuente.haber_nac
							ELSE
								INSERT INTO saldo_banco_fuente VALUES (  lcano_cta,                gs_banco,  gs_cta_cte, ;
									gcano_eje,              lsec_ejec, ls_mes_eje, ;
									ls_origen,				 ls_fuente, ;
									exp_doc.monto,          0,         exp_doc.monto, ;
									exp_doc.monto_nacional, 0,         exp_doc.monto_nacional )
							ENDIF
						ENDIF
					ELSE
						SELECT exp_ing
						ls_mes_eje = PADL(MONTH(exp_sec.fecha_doc),2,'0')
						*	LLENA CUR_BCO1
						IF !SEEK(ls_mes_eje+ls_expediente+ls_ciclo+exp_doc.cod_doc+;
								exp_doc.num_doc+exp_doc.nombre+;
								'A'+STR(0,19,02)+STR(exp_doc.monto*-1,19,02)+;
								DTOS(exp_sec.fecha_doc),'Cur_bco1','Ord01') THEN
							INSERT INTO cur_bco1 VALUES (ls_mes_eje,				 ;
								ls_expediente,				 ;
								ls_ciclo,					 ;
								exp_doc.cod_doc,			 ;
								exp_doc.num_doc,			 ;
								exp_doc.nombre,			 ;
								'A',						 ;
								STR(0,19,02),				 ;
								STR(exp_doc.monto*-1,19,02),;
								DTOS(exp_sec.fecha_doc) )
							* Saldo_Banco
							IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje,'saldo_banco','saldo_bco') THEN
								REPLACE saldo_banco.haber_mes WITH saldo_banco.haber_mes + exp_doc.monto*-1, ;
									saldo_banco.haber_nac WITH saldo_banco.haber_nac + exp_doc.monto_nacional*-1
								REPLACE	saldo_banco.saldo     WITH saldo_banco.debe_mes  - saldo_banco.haber_mes, ;
									saldo_banco.saldo_nac WITH saldo_banco.debe_nac  - saldo_banco.haber_nac
							ELSE
								INSERT INTO saldo_banco VALUES ( lcano_cta,   gs_banco,                  gs_cta_cte, ;
									gcano_eje, lsec_ejec,                 ls_mes_eje, ;
									0,         exp_doc.monto*-1,          exp_doc.monto, ;
									0,         exp_doc.monto_nacional*-1, exp_doc.monto_nacional )
							ENDIF
							* Saldo_Banco_Fuente 29/08/2006
							IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje+ls_origen+ls_fuente,'saldo_banco_fuente','saldo_fte') THEN
								REPLACE saldo_banco_fuente.haber_mes WITH saldo_banco_fuente.haber_mes + exp_doc.monto*-1, ;
									saldo_banco_fuente.haber_nac WITH saldo_banco_fuente.haber_nac + exp_doc.monto_nacional*-1
								REPLACE	saldo_banco_fuente.saldo     WITH saldo_banco_fuente.debe_mes  - saldo_banco_fuente.haber_mes, ;
									saldo_banco_fuente.saldo_nac WITH saldo_banco_fuente.debe_nac  - saldo_banco_fuente.haber_nac
							ELSE
								INSERT INTO saldo_banco_fuente VALUES (  lcano_cta,   gs_banco,                  gs_cta_cte, ;
									gcano_eje, lsec_ejec,                 ls_mes_eje, ;
									ls_origen, ls_fuente, ;
									0,         exp_doc.monto*-1,          exp_doc.monto, ;
									0,         exp_doc.monto_nacional*-1, exp_doc.monto_nacional )
							ENDIF
						ENDIF
					ENDIF
				ENDIF
			ENDIF
		ENDIF
	ENDSCAN
	RETURN

	*---------------------
FUNCTION procesa_ir_tr
	*---------------------
	IF !INLIST(exp_sec.estado,"R","D") THEN
		IF exp_sec.monto < 0 THEN
			SELECT exp_doc
			SEEK lano_eje+lsec_ejec+ls_expediente+ls_ciclo+ls_fase+ls_secuencia+ls_correlativo
			SCAN WHILE ano_eje+sec_ejec+expediente+ciclo+fase+secuencia+correlativo == ;
					lano_eje+lsec_ejec+ls_expediente+ls_ciclo+ls_fase+ls_secuencia+ls_correlativo
				* Documentos Permitidos *
				IF INLIST(exp_doc.cod_doc, '000', '020', '025', '026', '033', '044', '049', ;
						'061', '064', '065', '066', '068', '089', '095') THEN
					ls_num_doc = exp_doc.num_doc
					ls_nombre = exp_doc.nombre
					* ingreso por transferencia *
					*IF FOUND() THEN
					IF SEEK(lano_eje+				;
							lsec_ejec+				;
							exp_fase.expediente+	;
							exp_fase.ciclo+			;
							exp_fase.fase+			;
							exp_fase.secuencia+		;
							exp_sec.correlativo , "exp_ing") THEN
						SELECT exp_ing
						ls_mes_eje = PADL(MONTH(exp_doc.fecha_doc),2,'0')
						*	LLENA CUR_BCO1
						IF !SEEK(ls_expediente+ls_ciclo+exp_doc.cod_doc+;
								exp_doc.num_doc+exp_doc.nombre+;
								'N'+STR(0,19,02)+STR(exp_doc.monto,19,02)+;
								DTOS(exp_doc.fecha_doc),'Cur_bco1','Ord01') THEN
							INSERT INTO cur_bco1 VALUES (ls_mes_eje,					;
								ls_expediente,					;
								ls_ciclo,						;
								exp_doc.cod_doc,				;
								exp_doc.num_doc,				;
								exp_doc.nombre,				;
								'N',							;
								STR(0,19,02),					;
								STR(ABS(exp_doc.monto),19,02),	;
								DTOS(exp_doc.fecha_doc) )
							IF YEAR(exp_doc.fecha_doc)=VAL(gcano_eje) THEN
								* Saldo_Banco
								IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje,'saldo_banco','saldo_bco') THEN
									REPLACE saldo_banco.haber_mes WITH saldo_banco.haber_mes + ABS(exp_doc.monto), ;
										saldo_banco.haber_nac WITH saldo_banco.haber_nac + ABS(exp_doc.monto_nacional)
									REPLACE	saldo_banco.saldo     WITH saldo_banco.debe_mes  - saldo_banco.haber_mes, ;
										saldo_banco.saldo_nac WITH saldo_banco.debe_nac  - saldo_banco.haber_nac
								ELSE
									INSERT INTO saldo_banco VALUES ( lcano_cta,   gs_banco,              gs_cta_cte, ;
										gcano_eje, lsec_ejec,                   ls_mes_eje, ;
										0,         ABS(exp_doc.monto),          ABS(exp_doc.monto)*-1, ;
										0,         ABS(exp_doc.monto_nacional), ABS(exp_doc.monto_nacional)*-1 )
								ENDIF
								* Saldo_Banco_Fuente 29/08/2006
								IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje+ls_origen+ls_fuente,'saldo_banco_fuente','saldo_fte') THEN
									REPLACE saldo_banco_fuente.haber_mes WITH saldo_banco_fuente.haber_mes + ABS(exp_doc.monto), ;
										saldo_banco_fuente.haber_nac WITH saldo_banco_fuente.haber_nac + ABS(exp_doc.monto_nacional)
									REPLACE	saldo_banco_fuente.saldo     WITH saldo_banco_fuente.debe_mes  - saldo_banco_fuente.haber_mes, ;
										saldo_banco_fuente.saldo_nac WITH saldo_banco_fuente.debe_nac  - saldo_banco_fuente.haber_nac
								ELSE
									INSERT INTO saldo_banco_fuente VALUES (  lcano_cta,   gs_banco,                    gs_cta_cte, ;
										gcano_eje, lsec_ejec,                   ls_mes_eje, ;
										ls_origen, ls_fuente, ;
										0,         ABS(exp_doc.monto),          ABS(exp_doc.monto)*-1, ;
										0,         ABS(exp_doc.monto_nacional), ABS(exp_doc.monto_nacional)*-1 )
								ENDIF
							ENDIF
						ENDIF
					ENDIF
					*ENDIF
				ENDIF
			ENDSCAN
		ENDIF
	ENDIF
	RETURN

	*------------------
FUNCTION procesa_gg
	*------------------
	IF INLIST(exp_sec.cod_doc,'009','034') AND gs_banco+gs_cta_cte == exp_sec.banco+exp_sec.cta_cte THEN
		=SEEK(exp_sec.cod_doc,'maestro_documento','cod_doc')
		ls_mes_eje = PADL(MONTH(exp_sec.fecha_doc),2,'0')
		ls_cod_doc = exp_sec.cod_doc
		ls_nombre  = SUBSTR(UPPER(maestro_documento.nombre),01,60)
		ls_num_doc = exp_sec.num_doc
		ls_fec_doc = exp_sec.fecha_doc
		ls_mes_eje = PADL(MONTH(exp_sec.fecha_doc),2,'0')
		*	LLENA CUR_BCO1
		IF !SEEK(ls_mes_eje+ls_expediente+ls_ciclo+ls_cod_doc+ls_num_doc+ls_nombre+'N'+;
				STR(0,19,02)+STR(exp_sec.monto,19,02)+DTOS(ls_fec_doc),'Cur_bco1','Ord01') THEN
			INSERT INTO cur_bco1 VALUES (ls_mes_eje,					;
				ls_expediente,					;
				ls_ciclo,						;
				ls_cod_doc,					;
				ls_num_doc,					;
				ls_nombre,						;
				'N',							;
				STR(0,19,02),					;
				STR(ABS(exp_sec.monto),19,02),	;
				DTOS(ls_fec_doc) )
			IF YEAR(exp_sec.fecha_doc) = VAL(gcano_eje) THEN
				* Saldo_Banco
				IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje,'saldo_banco','saldo_bco') THEN
					REPLACE saldo_banco.debe_mes  WITH saldo_banco.debe_mes + ABS(exp_sec.monto), ;
						saldo_banco.debe_nac  WITH saldo_banco.debe_nac + ABS(exp_sec.monto_nacional)
					REPLACE	saldo_banco.saldo     WITH saldo_banco.debe_mes - saldo_banco.haber_mes, ;
						saldo_banco.saldo_nac WITH saldo_banco.debe_nac - saldo_banco.haber_nac
				ELSE
					INSERT INTO saldo_banco VALUES ( lcano_cta,                     gs_banco,  gs_cta_cte, ;
						gcano_eje,                   lsec_ejec, ls_mes_eje, ;
						ABS(exp_sec.monto),          0,         ABS(exp_sec.monto), ;
						ABS(exp_sec.monto_nacional), 0,         ABS(exp_sec.monto_nacional) )
				ENDIF
				* Saldo_Banco_Fuente 29/08/2006
				IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje+ls_origen+ls_fuente,'saldo_banco_fuente','saldo_fte') THEN
					REPLACE saldo_banco_fuente.debe_mes  WITH saldo_banco_fuente.debe_mes + ABS(exp_sec.monto), ;
						saldo_banco_fuente.debe_nac  WITH saldo_banco_fuente.debe_nac + ABS(exp_sec.monto_nacional)
					REPLACE	saldo_banco_fuente.saldo     WITH saldo_banco_fuente.debe_mes - saldo_banco_fuente.haber_mes, ;
						saldo_banco_fuente.saldo_nac WITH saldo_banco_fuente.debe_nac - saldo_banco_fuente.haber_nac
				ELSE
					INSERT INTO saldo_banco_fuente VALUES (  lcano_cta,                     gs_banco,  gs_cta_cte, ;
						gcano_eje,                   lsec_ejec, ls_mes_eje, ;
						ls_origen,					  ls_fuente, ;
						ABS(exp_sec.monto),          0,         ABS(exp_sec.monto), ;
						ABS(exp_sec.monto_nacional), 0,         ABS(exp_sec.monto_nacional) )
				ENDIF
			ENDIF
		ENDIF
	ENDIF
	RETURN

	*------------------------
FUNCTION procesa_gg_otros
	*------------------------
	* OPeraciones diferentes de Devolucion y Fuente 09
	SELECT exp_doc
	SEEK lano_eje+lsec_ejec+ls_expediente+ls_ciclo+ls_fase+ls_secuencia+ls_correlativo
	SCAN WHILE lano_eje+lsec_ejec+ls_expediente+;
			ls_ciclo+ls_fase+ls_secuencia+ls_correlativo == ;
			ano_eje+sec_ejec+expediente+ciclo+fase+secuencia+correlativo
		* documentos permitidos *
		IF !INLIST(exp_doc.cod_doc, '025', '026', '033', '044', '049', '061', ;
				'064', '065', '066', '068', '077', '081', '084', '087', '088', '089', '095') THEN
			LOOP
		ENDIF
		* no fecha vacia o año diferente a año de ejecucion *
		IF EMPTY(exp_doc.fecha_doc) THEN  && Or Year(exp_doc.fecha_doc) <> Val(gcano_eje)
			LOOP
		ENDIF
		***
		ls_mes_eje = PADL(MONTH(exp_sec.fecha_doc),2,'0')
		ls_num_doc = exp_doc.num_doc
		ls_nombre = exp_doc.nombre
		***
		IF gs_banco+gs_cta_cte == exp_sec.banco+exp_sec.cta_cte THEN
			ls_cod_doc   = exp_doc.cod_doc
			ls_num_doc   = exp_doc.num_doc
			ls_fec_doc = exp_doc.fecha_doc
			ls_nombre = exp_doc.nombre
			* anulacion de cheque *
			IF exp_sec.monto < 00 AND INLIST(exp_sec.estado, "A") THEN
				IF YEAR(exp_sec.fecha_doc) = VAL(gcano_eje) THEN
					IF ( MONTH(exp_sec.fecha_doc) > MONTH(exp_doc.fecha_doc) AND ;
							YEAR(exp_sec.fecha_doc) = YEAR(exp_doc.fecha_doc) ) OR ; && agregado
						( YEAR(exp_sec.fecha_doc) > YEAR(exp_doc.fecha_doc) ) THEN && agrgado
						ls_mes_eje = PADL(MONTH(exp_sec.fecha_doc),2,'0') && exp_doc.fecha_doc ??
						*	LLENA CUR_BCO1
						IF !SEEK(ls_mes_eje+ls_expediente+ls_ciclo+exp_doc.cod_doc+;
								exp_doc.num_doc+exp_doc.nombre+;
								'A'+STR(0,19,02)+STR(exp_doc.monto,19,02)+;
								DTOS(exp_doc.fecha_doc),'Cur_bco1','Ord01') THEN
							INSERT INTO cur_bco1 VALUES (ls_mes_eje,				;
								ls_expediente,				;
								ls_ciclo,					;
								exp_doc.cod_doc,			;
								exp_doc.num_doc,			;
								exp_doc.nombre,			;
								'A',						;
								STR(0,19,02),				;
								STR(exp_doc.monto,19,02),	;
								DTOS(exp_doc.fecha_doc) )
							* Saldo_Banco
							IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje,'saldo_banco','saldo_bco') THEN
								REPLACE saldo_banco.debe_mes  WITH saldo_banco.debe_mes + ABS(exp_doc.monto), ;
									saldo_banco.debe_nac  WITH saldo_banco.debe_nac + ABS(exp_doc.monto_nacional)
								REPLACE	saldo_banco.saldo     WITH saldo_banco.debe_mes - saldo_banco.haber_mes, ;
									saldo_banco.saldo_nac WITH saldo_banco.debe_nac - saldo_banco.haber_nac
							ELSE
								INSERT INTO saldo_banco VALUES ( lcano_cta,                gs_banco,  gs_cta_cte, ;
									gcano_eje,              lsec_ejec, ls_mes_eje, ;
									exp_doc.monto,          0,         exp_doc.monto, ;
									exp_doc.monto_nacional, 0,         exp_doc.monto_nacional )
							ENDIF
							* Saldo_Banco_Fuente 29/08/2006
							IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje+ls_origen+ls_fuente,'saldo_banco_fuente','saldo_fte') THEN
								REPLACE saldo_banco_fuente.debe_mes  WITH saldo_banco_fuente.debe_mes + ABS(exp_doc.monto), ;
									saldo_banco_fuente.debe_nac  WITH saldo_banco_fuente.debe_nac + ABS(exp_doc.monto_nacional)
								REPLACE	saldo_banco_fuente.saldo     WITH saldo_banco_fuente.debe_mes - saldo_banco_fuente.haber_mes, ;
									saldo_banco_fuente.saldo_nac WITH saldo_banco_fuente.debe_nac - saldo_banco_fuente.haber_nac
							ELSE
								INSERT INTO saldo_banco_fuente VALUES (  lcano_cta,                gs_banco,  gs_cta_cte, ;
									gcano_eje,              lsec_ejec, ls_mes_eje, ;
									ls_origen,				 ls_fuente, ;
									exp_doc.monto,          0,         exp_doc.monto, ;
									exp_doc.monto_nacional, 0,         exp_doc.monto_nacional )
							ENDIF
						ENDIF
					ELSE
						ls_mes_eje = PADL(MONTH(exp_sec.fecha_doc),2,'0') && exp_doc.fecha_doc ??
						*	LLENA CUR_BCO1
						IF !SEEK(ls_mes_eje+ls_expediente+ls_ciclo+exp_doc.cod_doc+;
								exp_doc.num_doc+exp_doc.nombre+;
								'A'+STR(0,19,02)+STR(exp_doc.monto*-1,19,02)+;
								DTOS(exp_doc.fecha_doc),'Cur_bco1','Ord01') THEN
							INSERT INTO cur_bco1 VALUES (ls_mes_eje,					;
								ls_expediente,					;
								ls_ciclo,						;
								exp_doc.cod_doc,				;
								exp_doc.num_doc,				;
								exp_doc.nombre,				;
								'A',							;
								STR(0,19,02),					;
								STR(exp_doc.monto*-1,19,02),	;
								DTOS(exp_doc.fecha_doc) )
							* Saldo_Banco
							IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje,'saldo_banco','saldo_bco') THEN
								REPLACE saldo_banco.haber_mes WITH saldo_banco.haber_mes + exp_doc.monto*-1, ;
									saldo_banco.haber_nac WITH saldo_banco.haber_nac + exp_doc.monto_nacional*-1
								REPLACE	saldo_banco.saldo     WITH saldo_banco.debe_mes  - saldo_banco.haber_mes, ;
									saldo_banco.saldo_nac WITH saldo_banco.debe_nac  - saldo_banco.haber_nac
							ELSE
								INSERT INTO saldo_banco VALUES ( lcano_cta,   gs_banco,                  gs_cta_cte, ;
									gcano_eje, lsec_ejec,                 ls_mes_eje, ;
									0,         exp_doc.monto*-1,          exp_doc.monto, ;
									0,         exp_doc.monto_nacional*-1, exp_doc.monto_nacional )
							ENDIF
							* Saldo_Banco_Fuente 29/08/2006
							IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje+ls_origen+ls_fuente,'saldo_banco_fuente','saldo_fte') THEN
								REPLACE saldo_banco_fuente.haber_mes WITH saldo_banco_fuente.haber_mes + exp_doc.monto*-1, ;
									saldo_banco_fuente.haber_nac WITH saldo_banco_fuente.haber_nac + exp_doc.monto_nacional*-1
								REPLACE	saldo_banco_fuente.saldo     WITH saldo_banco_fuente.debe_mes  - saldo_banco_fuente.haber_mes, ;
									saldo_banco_fuente.saldo_nac WITH saldo_banco_fuente.debe_nac  - saldo_banco_fuente.haber_nac
							ELSE
								INSERT INTO saldo_banco_fuente VALUES (  lcano_cta,   gs_banco,                  gs_cta_cte, ;
									gcano_eje, lsec_ejec,                 ls_mes_eje, ;
									ls_origen,	ls_fuente, ;
									0,         exp_doc.monto*-1,          exp_doc.monto, ;
									0,         exp_doc.monto_nacional*-1, exp_doc.monto_nacional )
							ENDIF
						ENDIF
					ENDIF
				ENDIF &&
			ELSE
				* cheque anulado de ejercicios anteriores *
				IF SEEK(lano_eje+				;
						lsec_ejec+				;
						exp_fase.expediente+	;
						exp_fase.ciclo+			;
						exp_fase.fase+			;
						exp_fase.secuencia+		;
						exp_sec.correlativo ,'exp_clasif') THEN
					SELECT exp_clasif
					IF INLIST(exp_clasif.categ_gasto+;
							exp_clasif.grupo_gasto+;
							exp_clasif.modalidad_gasto+;
							exp_clasif.elemento_gasto+;
							exp_clasif.secuencia_to, "000000030", "000000031") THEN
						ls_mes_eje = PADL(MONTH(exp_doc.fecha_doc),2,'0')
						*	LLENA CUR_BCO1
						IF !SEEK(ls_mes_eje+ls_expediente+ls_ciclo+exp_doc.cod_doc+;
								exp_doc.num_doc+exp_doc.nombre+;
								'N'+STR(0,19,02)+STR(exp_doc.monto,19,02)+;
								DTOS(exp_doc.fecha_doc),'Cur_bco1','Ord01') THEN
							INSERT INTO cur_bco1 VALUES (ls_mes_eje,				;
								ls_expediente,				;
								ls_ciclo,					;
								exp_doc.cod_doc,			;
								exp_doc.num_doc,			;
								exp_doc.nombre,			;
								'N',						;
								STR(0,19,02),				;
								STR(exp_doc.monto,19,02),	;
								DTOS(exp_doc.fecha_doc) )
							IF YEAR(exp_doc.fecha_doc) = VAL(gcano_eje) THEN  && agrgado
								* Saldo_Banco
								IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje,'saldo_banco','saldo_bco') THEN
									REPLACE saldo_banco.debe_mes  WITH saldo_banco.debe_mes + exp_doc.monto, ;
										saldo_banco.debe_nac  WITH saldo_banco.debe_nac + exp_doc.monto_nacional
									REPLACE	saldo_banco.saldo     WITH saldo_banco.debe_mes - saldo_banco.haber_mes, ;
										saldo_banco.saldo_nac WITH saldo_banco.debe_nac - saldo_banco.haber_nac
								ELSE
									INSERT INTO saldo_banco VALUES ( lcano_cta,                gs_banco,  gs_cta_cte, ;
										gcano_eje,              lsec_ejec, ls_mes_eje, ;
										exp_doc.monto,          0,         exp_doc.monto, ;
										exp_doc.monto_nacional, 0,         exp_doc.monto_nacional )
								ENDIF
								* Saldo_Banco_Fuente 29/08/2006
								IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje+ls_origen+ls_fuente,'saldo_banco_fuente','saldo_fte') THEN
									REPLACE saldo_banco_fuente.debe_mes  WITH saldo_banco_fuente.debe_mes + exp_doc.monto, ;
										saldo_banco_fuente.debe_nac  WITH saldo_banco_fuente.debe_nac + exp_doc.monto_nacional
									REPLACE	saldo_banco_fuente.saldo     WITH saldo_banco_fuente.debe_mes - saldo_banco_fuente.haber_mes, ;
										saldo_banco_fuente.saldo_nac WITH saldo_banco_fuente.debe_nac - saldo_banco_fuente.haber_nac
								ELSE
									INSERT INTO saldo_banco_fuente VALUES (  lcano_cta,                gs_banco,  gs_cta_cte, ;
										gcano_eje,              lsec_ejec, ls_mes_eje, ;
										ls_origen,				 ls_fuente, ;
										exp_doc.monto,          0,         exp_doc.monto, ;
										exp_doc.monto_nacional, 0,         exp_doc.monto_nacional )
								ENDIF
							ENDIF
						ENDIF
					ELSE
						ls_mes_eje = PADL(MONTH(exp_doc.fecha_doc),2,'0')
						*	LLENA CUR_BCO1
						IF !SEEK(ls_mes_eje+ls_expediente+ls_ciclo+exp_doc.cod_doc+;
								exp_doc.num_doc+exp_doc.nombre+;
								'N'+STR(0,19,02)+STR(exp_doc.monto,19,02)+;
								DTOS(exp_doc.fecha_doc),'Cur_bco1','Ord01') THEN
							INSERT INTO cur_bco1 VALUES (ls_mes_eje,				;
								ls_expediente,				;
								ls_ciclo,					;
								exp_doc.cod_doc,			;
								exp_doc.num_doc,			;
								exp_doc.nombre,			;
								'N',						;
								STR(0,19,02),				;
								STR(exp_doc.monto,19,02),	;
								DTOS(exp_doc.fecha_doc) )
							IF !(exp_sec.estado == "D" AND exp_fase.origen+exp_fase.fuente_financ $ "100/114/116") AND ;
									!(exp_sec.estado == "D" AND exp_fase.origen+exp_fase.fuente_financ+exp_fase.tipo_recurso $ "113S113I") THEN
								* no se consideran las Devoluciones en fuente 00, 14, 16, 13-S *
								IF YEAR(exp_doc.fecha_doc) = VAL(gcano_eje) AND ; && agregado
									YEAR(exp_sec.fecha_doc) = YEAR(exp_doc.fecha_doc) THEN && agregado el 05/09/2002 - Rosa Esquivel
									* Saldo_Banco
									IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje,'saldo_banco','saldo_bco') THEN
										REPLACE saldo_banco.haber_mes WITH saldo_banco.haber_mes + exp_doc.monto, ;
											saldo_banco.haber_nac WITH saldo_banco.haber_nac + exp_doc.monto_nacional
										REPLACE	saldo_banco.saldo     WITH saldo_banco.debe_mes  - saldo_banco.haber_mes, ;
											saldo_banco.saldo_nac WITH saldo_banco.debe_nac  - saldo_banco.haber_nac
									ELSE
										INSERT INTO saldo_banco VALUES ( lcano_cta,   gs_banco,               gs_cta_cte, ;
											gcano_eje, lsec_ejec,              ls_mes_eje, ;
											0,         exp_doc.monto,          exp_doc.monto*-1, ;
											0,         exp_doc.monto_nacional, exp_doc.monto_nacional*-1 )
									ENDIF
									* Saldo_Banco_Fuente 29/08/2006
									IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje+ls_origen+ls_fuente,'saldo_banco_fuente','saldo_fte') THEN
										REPLACE saldo_banco_fuente.haber_mes WITH saldo_banco_fuente.haber_mes + exp_doc.monto, ;
											saldo_banco_fuente.haber_nac WITH saldo_banco_fuente.haber_nac + exp_doc.monto_nacional
										REPLACE	saldo_banco_fuente.saldo     WITH saldo_banco_fuente.debe_mes  - saldo_banco_fuente.haber_mes, ;
											saldo_banco_fuente.saldo_nac WITH saldo_banco_fuente.debe_nac  - saldo_banco_fuente.haber_nac
									ELSE
										INSERT INTO saldo_banco_fuente VALUES (  lcano_cta,   gs_banco,               gs_cta_cte, ;
											gcano_eje, lsec_ejec,              ls_mes_eje, ;
											ls_origen,	ls_fuente, ;
											0,         exp_doc.monto,          exp_doc.monto*-1, ;
											0,         exp_doc.monto_nacional, exp_doc.monto_nacional*-1 )
									ENDIF
								ENDIF
							ENDIF
						ENDIF
					ENDIF
				ENDIF
			ENDIF
		ENDIF
	ENDSCAN
	RETURN

	*--------------------
FUNCTION procesa_gp_n
	*--------------------
	SCAN WHILE lano_eje+lsec_ejec+ls_expediente+ls_ciclo+	;
			ls_fase+ls_secuencia+ls_correlativo == 		;
			ano_eje+sec_ejec+expediente+ciclo+fase+secuencia+correlativo
		* Consistencia año de Entorno
		IF YEAR(exp_doc.fecha_doc) <> VAL(gcano_eje) THEN
			LOOP
		ENDIF
		ls_mes_eje = PADL(MONTH(exp_sec.fecha_doc),2,'0')
		*	LLENA CUR_BCO1
		IF !SEEK(ls_mes_eje+ls_expediente+ls_ciclo+exp_doc.cod_doc+;
				exp_doc.num_doc+exp_doc.nombre+;
				'Z'+STR(exp_doc.monto,19,02)+STR(0,19,02)+;
				DTOS(exp_sec.fecha_doc),'Cur_bco1','Ord01') THEN
			INSERT INTO cur_bco1 VALUES (	ls_mes_eje, 				;
				ls_expediente,				;
				ls_ciclo,					;
				exp_doc.cod_doc,			;
				exp_doc.num_doc,			;
				exp_doc.nombre,				;
				'Z',						;
				STR(exp_doc.monto,19,02),	;
				STR(0,19,02),				;
				DTOS(exp_sec.fecha_doc) )
			IF YEAR(exp_sec.fecha_doc) = VAL(gcano_eje) THEN
				* Saldo_Banco
				IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje,'saldo_banco','saldo_bco') THEN
					REPLACE saldo_banco.debe_mes  WITH saldo_banco.debe_mes + exp_doc.monto, ;
						saldo_banco.debe_nac  WITH saldo_banco.debe_nac + exp_doc.monto_nacional
					REPLACE	saldo_banco.saldo     WITH saldo_banco.debe_mes - saldo_banco.haber_mes, ;
						saldo_banco.saldo_nac WITH saldo_banco.debe_nac - saldo_banco.haber_nac
				ELSE
					INSERT INTO saldo_banco VALUES ( lcano_cta,                gs_banco,  gs_cta_cte, ;
						gcano_eje,              lsec_ejec, ls_mes_eje, ;
						exp_doc.monto,          0,         exp_doc.monto, ;
						exp_doc.monto_nacional, 0,         exp_doc.monto_nacional )
				ENDIF
				* Saldo_Banco_Fuente 29/08/2006
				IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje+ls_origen+ls_fuente,'saldo_banco_fuente','saldo_fte') THEN
					REPLACE saldo_banco_fuente.debe_mes  WITH saldo_banco_fuente.debe_mes + exp_doc.monto, ;
						saldo_banco_fuente.debe_nac  WITH saldo_banco_fuente.debe_nac + exp_doc.monto_nacional
					REPLACE	saldo_banco_fuente.saldo     WITH saldo_banco_fuente.debe_mes - saldo_banco_fuente.haber_mes, ;
						saldo_banco_fuente.saldo_nac WITH saldo_banco_fuente.debe_nac - saldo_banco_fuente.haber_nac
				ELSE
					INSERT INTO saldo_banco_fuente VALUES (  lcano_cta,                gs_banco,  gs_cta_cte, ;
						gcano_eje,              lsec_ejec, ls_mes_eje, ;
						ls_origen,				 ls_fuente, ;
						exp_doc.monto,          0,         exp_doc.monto, ;
						exp_doc.monto_nacional, 0,         exp_doc.monto_nacional )
				ENDIF
			ENDIF
		ENDIF
	ENDSCAN
	RETURN


	*---------------------------
FUNCTION procesa_gp_extorno
	*---------------------------
	ls_correlativo = "0001"
	IF YEAR(exp_sec.fecha_doc) = VAL(gcano_eje) THEN && Consistencia año de Entorno
		SEEK lano_eje+lsec_ejec+ls_expediente+ls_ciclo+ls_fase+ls_secuencia+ls_correlativo
		SCAN WHILE lano_eje+lsec_ejec+ls_expediente+ls_ciclo+;
				ls_fase+ls_secuencia+ls_correlativo == ;
				ano_eje+sec_ejec+expediente+ciclo+fase+secuencia+correlativo
			ls_mes_eje = PADL(MONTH(exp_sec.fecha_doc),2,'0') && exp_doc.fecha_doc ??
			IF ( MONTH(exp_sec.fecha_doc) > MONTH(exp_doc.fecha_doc) AND ;
					YEAR(exp_sec.fecha_doc) = YEAR(exp_doc.fecha_doc) ) OR ; && agregado
				( YEAR(exp_sec.fecha_doc) > YEAR(exp_doc.fecha_doc) ) THEN && agrgado
				*	LLENA CUR_BCO1
				IF !SEEK(ls_mes_eje+ls_expediente+ls_ciclo+exp_doc.cod_doc+;
						exp_doc.num_doc+exp_doc.nombre+;
						'Z'+STR(0,19,02)+STR(exp_doc.monto,19,02)+;
						DTOS(exp_sec.fecha_doc),'Cur_bco1','Ord01') THEN
					INSERT INTO cur_bco1 VALUES (ls_mes_eje,				;
						ls_expediente, 			;
						ls_ciclo, 					;
						exp_doc.cod_doc, 			;
						exp_doc.num_doc, 			;
						exp_doc.nombre, 			;
						'Z', 						;
						STR(0,19,02), 				;
						STR(exp_doc.monto,19,02),	;
						DTOS(exp_sec.fecha_doc) )
					IF YEAR(exp_sec.fecha_doc) = VAL(gcano_eje) THEN
						* Saldo_Banco
						IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje,'saldo_banco','saldo_bco') THEN
							REPLACE saldo_banco.haber_mes  WITH saldo_banco.haber_mes + exp_doc.monto, ;
								saldo_banco.haber_nac  WITH saldo_banco.haber_nac + exp_doc.monto_nacional
							REPLACE	saldo_banco.saldo     WITH saldo_banco.debe_mes - saldo_banco.haber_mes, ;
								saldo_banco.saldo_nac WITH saldo_banco.debe_nac - saldo_banco.haber_nac
						ELSE
							INSERT INTO saldo_banco VALUES ( lcano_cta,   gs_banco,  				gs_cta_cte, ;
								gcano_eje, lsec_ejec, 				ls_mes_eje, ;
								0,         exp_doc.monto,			exp_doc.monto*-1, ;
								0,         exp_doc.monto_nacional,	exp_doc.monto_nacional*-1 )
						ENDIF
						* Saldo_Banco_Fuente 29/08/2006
						IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje+ls_origen+ls_fuente,'saldo_banco_fuente','saldo_fte') THEN
							REPLACE saldo_banco_fuente.haber_mes  WITH saldo_banco_fuente.haber_mes + exp_doc.monto, ;
								saldo_banco_fuente.haber_nac  WITH saldo_banco_fuente.haber_nac + exp_doc.monto_nacional
							REPLACE	saldo_banco_fuente.saldo     WITH saldo_banco_fuente.debe_mes - saldo_banco_fuente.haber_mes, ;
								saldo_banco_fuente.saldo_nac WITH saldo_banco_fuente.debe_nac - saldo_banco_fuente.haber_nac
						ELSE
							INSERT INTO saldo_banco_fuente VALUES (  lcano_cta,   gs_banco,  				gs_cta_cte, ;
								gcano_eje, lsec_ejec, 				ls_mes_eje, ;
								ls_origen, ls_fuente, ;
								0,         exp_doc.monto,			exp_doc.monto*-1, ;
								0,         exp_doc.monto_nacional,	exp_doc.monto_nacional*-1 )
						ENDIF
					ENDIF
				ENDIF
			ELSE
				*	LLENA CUR_BCO1
				IF !SEEK(ls_mes_eje+ls_expediente+ls_ciclo+exp_doc.cod_doc+;
						exp_doc.num_doc+exp_doc.nombre+;
						'Z'+STR(exp_doc.monto*-1,19,02)+STR(0,19,02)+;
						DTOS(exp_sec.fecha_doc),'Cur_bco1','Ord01') THEN
					INSERT INTO cur_bco1 VALUES (ls_mes_eje,				;
						ls_expediente, 			;
						ls_ciclo, 					;
						exp_doc.cod_doc, 			;
						exp_doc.num_doc, 			;
						exp_doc.nombre, 			;
						'Z', 						;
						STR(exp_doc.monto*-1,19,02),;
						STR(0,19,02), 				;
						DTOS(exp_sec.fecha_doc) )
					IF YEAR(exp_sec.fecha_doc) = VAL(gcano_eje) THEN
						* Saldo_Banco
						IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje,'saldo_banco','saldo_bco') THEN
							REPLACE saldo_banco.debe_mes  WITH saldo_banco.debe_mes + exp_doc.monto*-1, ;
								saldo_banco.debe_nac  WITH saldo_banco.debe_nac + exp_doc.monto_nacional*-1
							REPLACE	saldo_banco.saldo     WITH saldo_banco.debe_mes - saldo_banco.haber_mes, ;
								saldo_banco.saldo_nac WITH saldo_banco.debe_nac - saldo_banco.haber_nac
						ELSE
							INSERT INTO saldo_banco VALUES ( lcano_cta,                	gs_banco,	gs_cta_cte, ;
								gcano_eje,              	lsec_ejec,	s_mes_eje, ;
								exp_doc.monto*-1,			0,      	exp_doc.monto*-1, ;
								exp_doc.monto_nacional*-1,	0,			exp_doc.monto_nacional*-1 )
						ENDIF
						* Saldo_Banco_Fuente 29/08/2006
						IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje+ls_origen+ls_fuente,'saldo_banco_fuente','saldo_fte') THEN
							REPLACE saldo_banco_fuente.debe_mes  WITH saldo_banco_fuente.debe_mes + exp_doc.monto*-1, ;
								saldo_banco_fuente.debe_nac  WITH saldo_banco_fuente.debe_nac + exp_doc.monto_nacional*-1
							REPLACE	saldo_banco_fuente.saldo     WITH saldo_banco_fuente.debe_mes - saldo_banco_fuente.haber_mes, ;
								saldo_banco_fuente.saldo_nac WITH saldo_banco_fuente.debe_nac - saldo_banco_fuente.haber_nac
						ELSE
							INSERT INTO saldo_banco_fuente VALUES (  lcano_cta,                	gs_banco,	gs_cta_cte, ;
								gcano_eje,              	lsec_ejec,	s_mes_eje, ;
								ls_origen,					ls_fuente, ;
								exp_doc.monto*-1,			0,      	exp_doc.monto*-1, ;
								exp_doc.monto_nacional*-1,	0,			exp_doc.monto_nacional*-1 )
						ENDIF
					ENDIF
				ENDIF
			ENDIF
		ENDSCAN
	ENDIF
	RETURN


	*------------------------
FUNCTION procesa_gp_reas
	*------------------------
	IF exp_sec.edicion == "E" THEN
		ls_correlativo = "0001"
	ENDIF
	IF YEAR(exp_sec.fecha_doc) = VAL(gcano_eje) THEN && Consistencia año de Entorno
		SEEK lano_eje+lsec_ejec+ls_expediente+ls_ciclo+'G'+ls_secuencia+ls_correlativo
		SCAN WHILE lano_eje+lsec_ejec+ls_expediente+ls_ciclo+;
				'G'+ls_secuencia+ls_correlativo == ;
				ano_eje+sec_ejec+expediente+ciclo+fase+secuencia+correlativo
			ls_mes_eje = PADL(MONTH(exp_sec.fecha_doc),2,'0')
			* LLENA CUR_BCO1
			IF !SEEK(ls_mes_eje+ls_expediente+ls_ciclo+exp_doc.cod_doc+;
					exp_doc.num_doc+exp_doc.nombre+;
					'Z'+STR(exp_doc.monto,19,02)+STR(0,19,02)+;
					DTOS(exp_sec.fecha_doc),'Cur_bco1','Ord01') THEN
				INSERT INTO cur_bco1 VALUES (ls_mes_eje,				;
					ls_expediente, 			;
					ls_ciclo, 					;
					exp_doc.cod_doc, 			;
					exp_doc.num_doc, 			;
					exp_doc.nombre, 			;
					'Z', 						;
					STR(exp_doc.monto,19,02), 	;
					STR(0,19,02), 				;
					DTOS(exp_sec.fecha_doc) )
				IF YEAR(exp_sec.fecha_doc) = VAL(gcano_eje) THEN
					* Saldo_Banco
					IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje,'saldo_banco','saldo_bco') THEN
						REPLACE saldo_banco.debe_mes  WITH saldo_banco.debe_mes + exp_doc.monto, ;
							saldo_banco.debe_nac  WITH saldo_banco.debe_nac + exp_doc.monto_nacional
						REPLACE	saldo_banco.saldo     WITH saldo_banco.debe_mes - saldo_banco.haber_mes, ;
							saldo_banco.saldo_nac WITH saldo_banco.debe_nac - saldo_banco.haber_nac
					ELSE
						INSERT INTO saldo_banco VALUES ( lcano_cta,                gs_banco,  gs_cta_cte, ;
							gcano_eje,              lsec_ejec, ls_mes_eje, ;
							exp_doc.monto,          0,         exp_doc.monto, ;
							exp_doc.monto_nacional, 0,         exp_doc.monto_nacional )
					ENDIF
					* Saldo_Banco_Fuente 29/08/2006
					IF SEEK(lcano_cta+gs_banco+gs_cta_cte+lsec_ejec+gcano_eje+ls_mes_eje+ls_origen+ls_fuente,'saldo_banco_fuente','saldo_fte') THEN
						REPLACE saldo_banco_fuente.debe_mes  WITH saldo_banco_fuente.debe_mes + exp_doc.monto, ;
							saldo_banco_fuente.debe_nac  WITH saldo_banco_fuente.debe_nac + exp_doc.monto_nacional
						REPLACE	saldo_banco_fuente.saldo     WITH saldo_banco_fuente.debe_mes - saldo_banco_fuente.haber_mes, ;
							saldo_banco_fuente.saldo_nac WITH saldo_banco_fuente.debe_nac - saldo_banco_fuente.haber_nac
					ELSE
						INSERT INTO saldo_banco_fuente VALUES (  lcano_cta,                gs_banco,  gs_cta_cte, ;
							gcano_eje,              lsec_ejec, ls_mes_eje, ;
							ls_origen,				 ls_fuente, ;
							exp_doc.monto,          0,         exp_doc.monto, ;
							exp_doc.monto_nacional, 0,         exp_doc.monto_nacional )
					ENDIF
				ENDIF
			ENDIF
		ENDSCAN
	ENDIF
	RETURN
