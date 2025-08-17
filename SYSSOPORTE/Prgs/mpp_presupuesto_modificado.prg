
**************************************************************************************************************
*******************JMDP***************************************************************************************
USE siaf!ESPECIFICA_DET					IN 0 SHARED ALIAS ESP_DET		AGAIN ORDER TAG IDCLASIF   	&& ANO_EJE+ID_CLASIFICADOR
USE siaf!SUBGENERICA_DET				IN 0 SHARED ALIAS SUB_DET		AGAIN ORDER TAG sgenericad   && ANO_EJE+TIPO_TRANSACCION+GENERICA+SUBGENERICA
USE siaf!nota_modificatoria_cab			IN 0 SHARED ALIAS nota_cab		AGAIN ORDER TAG MP_NOTACAB  && ANO_EJE+SEC_EJEC+SEC_EJEC2+SEC_NOTA
USE siaf!nota_modificatoria_fte			IN 0 SHARED ALIAS nota_fte		AGAIN ORDER TAG MP_NOTAFTE  && ANO_EJE+SEC_EJEC+SEC_EJEC2+SEC_NOTA+ORIGEN+FUENTE_FINANC
USE siaf!nota_modificatoria_ing 		IN 0 SHARED ALIAS nota_ing		AGAIN ORDER TAG MPNOTAINGP  && ANO_EJE+SEC_EJEC+SEC_EJEC2+SEC_NOTA+ORIGEN+FUENTE_FINANC+TIPO_RECURSO+CLASE_INGRESO+TIPO_INGRESO+SUB_TIPO_INGRESO+ELEMENTO_INGRESO
USE siaf!nota_modificatoria_det 		IN 0 SHARED ALIAS nota_det		AGAIN ORDER TAG MPNOTADETP  && ANO_EJE+SEC_EJEC+SEC_EJEC2+SEC_NOTA+ORIGEN+FUENTE_FINANC+TIPO_RECURSO+SEC_FUNC+CATEG_GASTO+GRUPO_GASTO+MODALIDAD_GASTO+ELEMENTO_GASTO
USE siaf!nota_modificatoria_sec 		IN 0 SHARED ALIAS nota_sec		AGAIN ORDER TAG MP_NOTASEC  && ANO_EJE+SEC_EJEC+SEC_EJEC2+SEC_NOTA+SECUENCIA
USE siaf!nota_modificatoria_doc_sec 	IN 0 SHARED ALIAS nota_docsec	AGAIN ORDER TAG MP_NOTADOS  && ANO_EJE+SEC_EJEC+SEC_DOC+SEC_EJEC2+SEC_NOTA+SECUENCIA
USE siaf!nota_modificatoria_doc 		IN 0 SHARED ALIAS nota_doc  	AGAIN ORDER TAG MP_NOTADOC  && ANO_EJE+SEC_EJEC+SEC_DOC
USE siaf!gasto							IN 0 SHARED ALIAS gto			AGAIN ORDER TAG MP_GASTOP   && ANO_EJE+SEC_EJEC+ORIGEN+FUENTE_FINANC+TIPO_RECURSO+SEC_FUNC+ID_CLASIFICADOR
USE siaf!ingreso						IN 0 SHARED ALIAS ing			AGAIN ORDER TAG MPINGRESOP  && ANO_EJE+SEC_EJEC+ORIGEN+FUENTE_FINANC+TIPO_RECURSO+ID_CLASIFICADOR
USE siaf!PLIEGO_EJECUTORA				IN 0 SHARED ALIAS PLI_EJEC		AGAIN ORDER TAG SEC_EJEC2  && ANO_EJE+SEC_EJEC+ORIGEN+FUENTE_FINANC+TIPO_RECURSO+ID_CLASIFICADOR



CREATE CURSOR curgen (	ano_ctb           		c(4)	,;
						sec_ejec		  		c(6)	,;
						origen					c(1)	,;
						fuente_financ			c(2)	,;
						tipo_recurso			c(2)	,;
						sec_func				c(4)	,;
						id_clasificador			c(7)	,;
						tipo_transaccion		c(1)	,;
						generica				c(1)	,;
						subgenerica				c(2)	,;
						subgenerica_det			c(2)	,;
						especifica				c(2)	,;
						especifica_det			c(2)	,;
						categoria_gasto			c(1)	,;
						presupuesto				N(19)	,;
						anulacion				N(19)	,;
						credito					N(19)	)
SELECT curgen
INDEX ON ano_ctb+ALLTRIM(sec_ejec)+ALLTRIM(origen)+;
	ALLTRIM(fuente_financ)+id_clasificador TAG NOTA

SET ORDER TO NOTA

*!* Inicio de la creacion del cursor con las notas modificatorias que se van a usar en el reporte
lccond	= ''
lccond	= lccond + ' and b.ano_eje=gcano_eje '



SELECT	a.ano_eje, a.sec_ejec , a.sec_ejec2, a.sec_nota, a.secuencia, a.fecha, a.notas, ;
	a.estado est_ue, a.estado_envio est_env_ue, ;
	b.ind_habilita, b.tipo_modificacion, b.mes_eje, ;
	SPACE(03) 	pli_cod_doc			,;
	SPACE(20) 	pli_num_doc			,;
	CTOD('//') 	pli_fecha_doc		,;
	SPACE(01)	pli_estado			,;
	SPACE(01)	pli_estado_envio	,;
	SPACE(03) 	regu_cod_doc  		,;
	SPACE(20) 	regu_num_doc		,;
	CTOD('//') 	regu_fecha_doc		,;
	SPACE(01)	regu_estado			,;
	SPACE(01)	regu_estado_envio	,;
	SPACE(150)	observacion			,;
	a.sec_doc						,;
	a.sec_doc_r						,;
	b.tipo_procedencia				;
	FROM nota_sec a, nota_cab b		;
	WHERE a.ano_eje = b.ano_eje 	;
	AND a.sec_ejec 	= b.sec_ejec 	;
	AND a.sec_nota	= b.sec_nota 	;
	AND IIF(GCfuncion_entidad='01',a.sec_ejec=Gcsec_ejec,1=1) ;
	AND a.secuencia = '0001' 		;
	AND !EMPTY(a.sec_doc)			;
	AND INLIST(b.tipo_modificacion,'001','002','003','004','006','007'); &&solo presenta las notas que tienen anulacion y crèdito
	&lccond ;
	oRDER BY 1,2,3 ;
	INTO CURSOR curcab READWRITE
	

SELECT curcab
INDEX ON ano_eje+sec_ejec+sec_nota+secuencia TAG NOTA

SELECT	a.ano_eje, a.sec_ejec sec_pliego, a.sec_ejec2 sec_ejec,;
	a.sec_nota, a.secuencia, ;
	a.sec_doc, a.estado est_doc, a.actual,;
	b.ind_doc, b.cod_doc, b.num_doc, b.fecha_doc, ;
	b.estado, b.estado_envio , b.observacion 	;
	FROM nota_docsec a, nota_doc b ;
	WHERE 	a.ano_eje 	= b.ano_eje ;
	AND a.sec_ejec 	= b.sec_ejec ;
	AND a.sec_doc	= b.sec_doc ;
	AND a.ano_eje	= gcano_eje ;
	AND NOT INLIST(a.estado,'R') ;		&&solo las notas que estan con estado diferente de Aprobado y Rechazado
	INTO CURSOR cdoc

SELECT cdoc
INDEX ON ano_eje+sec_ejec+sec_nota+secuencia+sec_doc+ind_doc+estado TAG DOC

SELECT curcab
SCAN ALL
	SCATTER MEMVAR
	*!*	buscando si tiene documento de aprobacion en la nota inicial
	lckey=M.ano_eje+M.sec_ejec+M.sec_nota+M.secuencia+m.sec_doc+'1'+'A'
	IF SEEK(lckey,'CDOC','DOC')
		IF NOT INLIST(cdoc.estado,'R')	&&solo las notas que estan con estado diferente de Aprobado y Rechazado
			REPLACE pli_cod_doc 		WITH cdoc.cod_doc	,;
					pli_num_doc 		WITH cdoc.num_doc 	,;
					pli_fecha_doc 		WITH cdoc.fecha_doc	,;
					pli_estado  		WITH cdoc.estado	,;
					pli_estado_envio 	WITH cdoc.estado_envio IN curcab
		ELSE
			DELETE
			LOOP
		ENDIF
	ELSE
		DELETE
		LOOP
	ENDIF
	*!*	buscando si tiene documento de regularizacion
	IF !EMPTY(m.sec_doc_r) AND m.tipo_modificacion='003'
		lckey=M.ano_eje+M.sec_ejec+M.sec_nota+M.secuencia+m.sec_doc_r+'2'
		IF SEEK(lckey,'CDOC','DOC')
			REPLACE regu_cod_doc		WITH cdoc.cod_doc	,;
					regu_num_doc 		WITH cdoc.num_doc	,;
					regu_fecha_doc 		WITH cdoc.fecha_doc	,;
					regu_estado  		WITH cdoc.estado	,;
					regu_estado_envio 	WITH cdoc.estado_envio IN curcab
		ENDIF
	ENDIF
	*!*	buscando si tiene anulacion en la nota
	lckey=M.ano_eje+M.sec_ejec+m.sec_ejec2+M.sec_nota+'0002'
	IF SEEK(lckey,'nota_sec','mp_notasec')
		IF !EMPTY(nota_sec.sec_doc)
			*ano_eje+sec_ejec+sec_nota+secuencia+sec_doc+ind_doc+est_doc
			lckey=M.ano_eje+M.sec_ejec+M.sec_nota+'0002'+nota_sec.sec_doc+'1'+'A'
			IF SEEK(lckey,'CDOC','DOC')
				DELETE IN curcab
			ENDIF
		ENDIF
	ENDIF
ENDSCAN

	
SET DELETED OFF
IF Gcfuncion_entidad='02' 
	SELECT gto
	SEEK(gcano_eje)
	SCAN WHILE ano_eje = gcano_eje
		SCATTER MEMVAR
		IF DELETED('gto') THEN 
		 	LOOP 
		ENDIF 
		lckey = gcano_eje+m.sec_ejec+m.origen+m.fuente_financ+m.id_clasificador
		SELECT ESP_DET && ANO_EJE+ID_CLASIFICADOR
		=SEEK(gcano_eje+m.id_clasificador)
		lctipo_transaccion 	= ESP_DET.tipo_transaccion
		lcgenerica 			= ESP_DET.generica
		lcsubgenerica 		= ESP_DET.subgenerica
		lcsubgenerica_det 	= ESP_DET.subgenerica_det
		lcespecifica		= ESP_DET.especifica
		lcespecifica_det	= ESP_DET.especifica_det
		SELECT SUB_DET	&& ANO_EJE+TIPO_TRANSACCION+GENERICA+SUBGENERICA
		=SEEK(gcano_eje+ESP_DET.tipo_transaccion+ESP_DET.generica+ESP_DET.subgenerica+ESP_DET.subgenerica_det)
		lccateg_gasto 		= SUB_DET.categoria_gasto	
		SELECT pli_ejec	&& ANO_EJE+SEC_EJEC+SEC_EJEC2
		IF !SEEK(gcano_eje+gcsec_ejec+m.sec_ejec)
			LOOP 
		ELSE 
			IF pli_ejec.estado<> 'A'
				LOOP 
			ENDIF 
		ENDIF 
		IF SEEK(lckey,'curgen','nota')
			IF DELETED('curgen')
				RECALL IN curgen
			ENDIF 
			REPLACE curgen.presupuesto WITH curgen.presupuesto+gto.presupuesto IN curgen
		ELSE
			INSERT INTO curgen(ano_ctb,sec_ejec,origen,fuente_financ,tipo_recurso,;
				sec_func,id_clasificador,tipo_transaccion,generica,subgenerica,subgenerica_det,;
				especifica,especifica_det,categoria_gasto,presupuesto) VALUES(gcano_eje,m.sec_ejec,;
				m.origen,m.fuente_financ,PADR(m.tipo_recurso,2,' '),m.sec_func,m.id_clasificador,lctipo_transaccion,;
				lcgenerica,lcsubgenerica,lcsubgenerica_det,lcespecifica,lcespecifica_det,lccateg_gasto,gto.presupuesto)
		ENDIF
		
	ENDSCAN
	SELECT curgen
	GO TOP

	SELECT curcab
	GO TOP
	SCAN ALL
		SCATTER MEMVAR
		IF DELETED('curcab') THEN 
		 	LOOP 
		ENDIF 
		lckey_det	= m.ano_eje+m.sec_ejec+m.sec_ejec2+m.sec_nota
		lccond_det	= 'ano_eje+sec_ejec+sec_ejec2+sec_nota = "'+lckey_det+'" '
		IF SEEK(lckey_det,'nota_det','MPNOTADETP')
			SELECT nota_det
			SCAN WHILE &lccond_det
				IF DELETED('nota_det')
					LOOP
				ENDIF 	
				SELECT pli_ejec	&& ANO_EJE+SEC_EJEC+SEC_EJEC2
				IF !SEEK(gcano_eje+gcsec_ejec+m.sec_ejec)
					LOOP 
				ELSE 
					IF pli_ejec.estado<> 'A'
						LOOP 
					ENDIF 
				ENDIF 			
				lckey_mes = gcano_eje+m.sec_ejec+'005000'+nota_det.sec_nota+nota_det.origen+nota_det.fuente_financ
				lckey_gto = gcano_eje+m.sec_ejec+nota_det.origen+nota_det.fuente_financ+nota_det.id_clasificador
				SELECT nota_fte
				=SEEK(lckey_mes,'nota_fte','MP_NOTAFTE')
				lctipo_tipo_recurso = nota_fte.tipo_recurso
				SELECT ESP_DET && ANO_EJE+ID_CLASIFICADOR
				=SEEK(gcano_eje+nota_det.id_clasificador)
				lctipo_transaccion 	= ESP_DET.tipo_transaccion
				lcgenerica 			= ESP_DET.generica
				lcsubgenerica 		= ESP_DET.subgenerica
				lcsubgenerica_det 	= ESP_DET.subgenerica_det
				lcespecifica		= ESP_DET.especifica
				lcespecifica_det	= ESP_DET.especifica_det
				SELECT SUB_DET	&& ANO_EJE+TIPO_TRANSACCION+GENERICA+SUBGENERICA
				** MGHN :20120905 SE AGREGA +ESP_DET.subgenerica_det A LA LLAVE OBS 123 CC
				=SEEK(gcano_eje+ESP_DET.tipo_transaccion+ESP_DET.generica+ESP_DET.subgenerica+ESP_DET.subgenerica_det)
				lccateg_gasto 		= SUB_DET.categoria_gasto
				IF SEEK(lckey_gto,'curgen','nota')
					IF DELETED('curgen')
						RECALL IN curgen
					ENDIF 
				ELSE
					INSERT INTO curgen(ano_ctb,sec_ejec,origen,fuente_financ,tipo_recurso,;
						sec_func,id_clasificador,tipo_transaccion,generica,subgenerica,;
						subgenerica_det,especifica,especifica_det,categoria_gasto) VALUES(gcano_eje,m.sec_ejec,;
						nota_fte.origen,nota_fte.fuente_financ,PADR(lctipo_tipo_recurso,2,' '),nota_det.sec_func,nota_det.id_clasificador,;
						lctipo_transaccion,lcgenerica,lcsubgenerica,lcsubgenerica_det,lcespecifica,lcespecifica_det,lccateg_gasto)
				ENDIF 			
				REPLACE curgen.anulacion 	WITH curgen.anulacion 	+ nota_det.monto_de
				REPLACE curgen.credito 		WITH curgen.credito 	+ nota_det.monto_a
			ENDSCAN 
		ENDIF 
		SELECT curcab
	ENDSCAN  
ELSE
	SELECT gto
	SEEK(gcano_eje+Gcsec_ejec)
	SCAN WHILE ano_eje+sec_ejec = gcano_eje+gcsec_ejec
		SCATTER MEMVAR
		IF DELETED('gto') THEN 
		 	LOOP 
		ENDIF 
		lckey = gcano_eje+m.sec_ejec+m.origen+m.fuente_financ+m.id_clasificador
		SELECT ESP_DET && ANO_EJE+ID_CLASIFICADOR
		=SEEK(gcano_eje+m.id_clasificador)
		lctipo_transaccion 	= ESP_DET.tipo_transaccion
		lcgenerica 			= ESP_DET.generica
		lcsubgenerica 		= ESP_DET.subgenerica
		lcsubgenerica_det 	= ESP_DET.subgenerica_det
		lcespecifica		= ESP_DET.especifica
		lcespecifica_det	= ESP_DET.especifica_det
		SELECT SUB_DET	&& ANO_EJE+TIPO_TRANSACCION+GENERICA+SUBGENERICA
		=SEEK(gcano_eje+ESP_DET.tipo_transaccion+ESP_DET.generica+ESP_DET.subgenerica+ESP_DET.subgenerica_det)
		lccateg_gasto 		= SUB_DET.categoria_gasto	
		SELECT pli_ejec	&& ANO_EJE+SEC_EJEC+SEC_EJEC2
		IF !SEEK(gcano_eje+gcsec_ejec+m.sec_ejec)
			LOOP 
		ELSE 
			IF pli_ejec.estado<> 'A'
				LOOP 
			ENDIF 
		ENDIF 
		IF SEEK(lckey,'curgen','nota')
			IF DELETED('curgen')
				RECALL IN curgen
			ENDIF 
			REPLACE curgen.presupuesto WITH curgen.presupuesto+gto.presupuesto IN curgen
		ELSE
			INSERT INTO curgen(ano_ctb,sec_ejec,origen,fuente_financ,tipo_recurso,;
				sec_func,id_clasificador,tipo_transaccion,generica,subgenerica,subgenerica_det,;
				especifica,especifica_det,categoria_gasto,presupuesto) VALUES(gcano_eje,m.sec_ejec,;
				m.origen,m.fuente_financ,PADR(m.tipo_recurso,2,' '),m.sec_func,m.id_clasificador,lctipo_transaccion,;
				lcgenerica,lcsubgenerica,lcsubgenerica_det,lcespecifica,lcespecifica_det,lccateg_gasto,gto.presupuesto)
		ENDIF
		
	ENDSCAN
	SELECT curgen
	GO TOP

	SELECT curcab
	GO TOP
	SCAN ALL
		SCATTER MEMVAR
		IF DELETED('curcab') THEN 
		 	LOOP 
		ENDIF 
		lckey_det	= m.ano_eje+m.sec_ejec+m.sec_ejec2+m.sec_nota
		lccond_det	= 'ano_eje+sec_ejec+sec_ejec2+sec_nota = "'+lckey_det+'" '
		IF SEEK(lckey_det,'nota_det','MPNOTADETP')
			SELECT nota_det
			SCAN WHILE &lccond_det
				IF DELETED('nota_det')
					LOOP
				ENDIF 	
				SELECT pli_ejec	&& ANO_EJE+SEC_EJEC+SEC_EJEC2
				IF !SEEK(gcano_eje+gcsec_ejec+m.sec_ejec)
					LOOP 
				ELSE 
					IF pli_ejec.estado<> 'A'
						LOOP 
					ENDIF 
				ENDIF 			
				lckey_mes = gcano_eje+m.sec_ejec+'005000'+nota_det.sec_nota+nota_det.origen+nota_det.fuente_financ
				lckey_gto = gcano_eje+m.sec_ejec+nota_det.origen+nota_det.fuente_financ+nota_det.id_clasificador
				SELECT nota_fte
				=SEEK(lckey_mes,'nota_fte','MP_NOTAFTE')
				lctipo_tipo_recurso = nota_fte.tipo_recurso
				SELECT ESP_DET && ANO_EJE+ID_CLASIFICADOR
				=SEEK(gcano_eje+nota_det.id_clasificador)
				lctipo_transaccion 	= ESP_DET.tipo_transaccion
				lcgenerica 			= ESP_DET.generica
				lcsubgenerica 		= ESP_DET.subgenerica
				lcsubgenerica_det 	= ESP_DET.subgenerica_det
				lcespecifica		= ESP_DET.especifica
				lcespecifica_det	= ESP_DET.especifica_det
				SELECT SUB_DET	&& ANO_EJE+TIPO_TRANSACCION+GENERICA+SUBGENERICA
				**MGHN : 20120905
				=SEEK(gcano_eje+ESP_DET.tipo_transaccion+ESP_DET.generica+ESP_DET.subgenerica+ESP_DET.subgenerica_det)
				lccateg_gasto 		= SUB_DET.categoria_gasto
				IF SEEK(lckey_gto,'curgen','nota')
					IF DELETED('curgen')
						RECALL IN curgen
					ENDIF 
				ELSE
					INSERT INTO curgen(ano_ctb,sec_ejec,origen,fuente_financ,tipo_recurso,;
						sec_func,id_clasificador,tipo_transaccion,generica,subgenerica,;
						subgenerica_det,especifica,especifica_det,categoria_gasto) VALUES(gcano_eje,m.sec_ejec,;
						nota_fte.origen,nota_fte.fuente_financ,PADR(lctipo_tipo_recurso,2,' '),nota_det.sec_func,nota_det.id_clasificador,;
						lctipo_transaccion,lcgenerica,lcsubgenerica,lcsubgenerica_det,lcespecifica,lcespecifica_det,lccateg_gasto)
				ENDIF 			
				REPLACE curgen.anulacion 	WITH curgen.anulacion 	+ nota_det.monto_de
				REPLACE curgen.credito 		WITH curgen.credito 	+ nota_det.monto_a
			ENDSCAN 
		ENDIF 
		SELECT curcab
	ENDSCAN  


ENDIF
	
SET DELETED ON 
SELECT * FROM curcab INTO CURSOR curcabdet

SELECT curgen



USE IN cdoc
USE IN curcab
USE IN ing
USE IN gto
USE IN nota_doc
USE IN nota_docsec
USE IN nota_sec
USE IN nota_det
USE IN nota_ing
USE IN nota_fte
USE IN nota_cab
USE IN sub_det
USE IN esp_det
USE IN PLI_EJEC

