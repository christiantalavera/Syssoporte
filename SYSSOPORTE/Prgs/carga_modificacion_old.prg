PARAMETERS pPeriodo, pSec_ejec

lcsec_ejec= psec_ejec
USE siaf!ejecutora 					IN 0 SHARED AGAIN ORDER TAG sec_ejec 	ALIAS r_ejecutora
USE siaf!meta 						IN 0 SHARED AGAIN ORDER TAG meta 		ALIAS r_meta
USE siaf!act_proy_nombre			IN 0 SHARED AGAIN ORDER TAG act_proy 	ALIAS r_act_proy 
USE siaf!programa_ppto_nombre		IN 0 SHARED AGAIN ORDER TAG progppto_n 	ALIAS programa_ppto_nombre1 
USE siaf!nota_modificatoria_cab		IN 0 SHARED AGAIN ORDER TAG mp_notacab	ALIAS nota_cab
USE siaf!nota_modificatoria_sec		IN 0 SHARED AGAIN ORDER TAG mp_notasec 	ALIAS nota_sec
USE siaf!nota_modificatoria_det		IN 0 SHARED AGAIN ORDER TAG mp_notadet 	ALIAS nota_det
USE siaf!nota_modificatoria_doc 	IN 0 SHARED AGAIN ORDER tag mp_notadoc  ALIAS nota_doc
USE siaf!nota_modificatoria_doc_sec IN 0 SHARED AGAIN ORDER tag mp_notados  ALIAS nota_doc_sec


CREATE CURSOR curModificacion (	ano_eje			c(0004), ;
								sec_ejec		c(0006), ;
								origen			c(0001), ;
								fuente_financ	c(0002), ;
								tipo_recurso	c(0002), ;
								sec_func		c(0004), ;
								id_clasificador c(0007), ;
								clasificador	c(0010), ;
								modificacion	N(19))
								
INDEX ON ANO_EJE + SEC_EJEC + FUENTE_FINANC + TIPO_RECURSO  + SEC_FUNC  TAG SECFUNC ADDITIVE  	
INDEX ON ANO_EJE + SEC_EJEC + ORIGEN + FUENTE_FINANC + TIPO_RECURSO + SEC_FUNC + ID_CLASIFICADOR TAG GTO ADDITIVE  
INDEX ON ANO_EJE + ID_CLASIFICADOR TAG IDCLASIF   ADDITIVE

SELECT curModificacion 
SET ORDER TO gto

CREATE CURSOR curModificacion_det (	ano_eje				c(0004), ;
									sec_ejec			c(0006), ;
									origen				c(0001), ;
									fuente_financ		c(0002), ;
									tipo_modificacion	c(003) , ;
									tipo_recurso		c(0002), ;
									sec_func			c(0004), ;
									id_clasificador		c(0007), ;
									clasificador		c(0010), ;
									modificacion		N(19))

INDEX ON ano_eje + sec_ejec + origen + fuente_financ + tipo_modificacion + tipo_recurso + sec_func + id_clasificador TAG gto ADDITIVE  

SELECT curModificacion_det 
SET ORDER TO GTO   && ANO_EJE+SEC_EJEC+ORIGEN+FUENTE_FINANC+TIPO_RECURSO+SEC_FUNC+ID_CLASIFICADOR 

CREATE CURSOR curcab ;
	(	ano_eje           C(0004) 	NULL, ;
		sec_ejec		  C(0006) 	NULL, ;
		sec_ejec2		  C(0006) 	NULL, ;
		sec_nota		  C(0010) 	NULL, ;
		secuencia		  C(0004)   NULL, ;
		mes_eje			  C(0002)	NULL, ;
		ind_habilita	  C(0001)	NULL, ;
		tipo_modificacion C(0003)	NULL, ;
		cod_doc			  C(0003)	NULL, ;
		num_doc			  C(0020)	NULL, ;
		fecha_doc		  D			NULL, ;
		cod_doc2		  C(0003)	NULL, ;
		num_doc2		  C(0020)	NULL, ;
		fecha_doc2		  D			NULL, ;
		monto			  N(0019)	NULL, ;
		notas			  C(0150)	NULL, ;
		estado			  C(0001)	NULL, ;
		estado2			  C(0001)	NULL, ;
		estado_envio	  C(0001)	NULL, ;
		estado_envio2	  C(0001)	NULL, ;
		estado_envio3	  C(0001)	NULL, ;
		seleccionar 	  N(0001)		, ;
		detalle			  N(0001)	NULL, ;
		fecha			  D		  	,;
		observacion		  C(0150)	,;
		unico			  C(0001)	)


IF lcFuncionEntidad = '03' THEN 
	lcKeyA = 'gcano_eje'
	lccond = 'ano_eje = gcano_eje'
ELSE
	lcKeyA = 'gcano_eje+Psec_ejec'
	lccond = 'ano_eje+sec_ejec = gcano_eje+psec_ejec'
ENDIF
		
		
SELECT curcab
INDEX ON  ano_eje+sec_ejec+sec_ejec2+sec_nota+secuencia TAG NOTA
INDEX ON ALLTRIM(STR(seleccionar)) TAG seleccion
SET ORDER TO NOTA
*!* CARGANDO EL CURSOR CON LAS NOTAS MODIFICATORIAS
SELEC AUX_NOTA_CAB
SEEK &lcKeyA.
SCAN WHILE &lccond. FOR !DELETED()
	SCATTER MEMVAR
	lckey=m.ano_eje+m.sec_ejec+m.sec_ejec2+m.sec_nota
	**&& ANO_EJE+SEC_EJEC+SEC_EJEC2+SEC_NOTA+SECUENCIA
	IF SEEK(lckey,'AUX_NOTA_SEC','mp_notasec')
		SELECT AUX_NOTA_SEC
		SCAN WHILE ano_eje+sec_ejec+sec_ejec2+sec_nota=lckey FOR !DELETED()
			** VERIFICANDO SI SE ENCUENTRA APROBADA Y EXISTE DOCUMENTO
			SCATTER NAME oNSec	
			**lckeyNSec= 	oNSec.ANO_EJE+oNSec.SEC_EJEC+oNSec.SEC_DOC+oNSec.SEC_EJEC2+oNSec.SEC_NOTA+oNSec.SECUENCIA
			lckeyNSec =	oNSec.ANO_EJE+oNSec.SEC_EJEC+oNSec.SEC_NOTA+oNSec.SECUENCIA
			lcCodDoc  = oNSec.Sec_Doc
			*** LLAVE DEL DOC_SEC MP_NOTADOS   && ANO_EJE+SEC_EJEC+SEC_DOC+SEC_EJEC2+SEC_NOTA+SECUENCIA
			*** VERIFICANDO QUE NOTA CORRESPONDA AL PERIODO
			IF lcModulo = 'MPP'
				lcmes_nota = AUX_NOTA_CAB.MES_EJE
			ELSE
				lcmes_nota = PADL(month(oNSec.fecha),2,"0")	
			ENDIF
			IF EMPTY(alltrim(lcCodDoc)) THEN 
				LOOP
			ENDIF	
			IF lcmes_nota > lcmes THEN 
				LOOP
			ENDIF	
			** VERIFICANDO ESTADO DE APROBACION
			IF oNSec.ESTADO <>'A'
				LOOP
			ENDIF
			
			IF SEEK(lckeyNSec,"AUX_NOTA_DOC_SEC","SECUENCIA")
				SELE AUX_NOTA_DOC_SEC
				SCAN WHILE ano_eje+sec_ejec2+sec_nota+secuencia = lckeyNSec FOR !DELETED()
					IF AUX_NOTA_DOC_SEC.sec_doc = lcCodDoc
				
						SELECT AUX_NOTA_DOC_SEC
						SCATTER NAME oNDSec
						lckeyNDoc= oNDSec.ANO_EJE+oNDSec.SEC_EJEC+oNDSec.SEC_DOC
						IF SEEK(lckeyNDoc,"AUX_NOTA_DOC","MP_NOTADOC") 
							IF AUX_NOTA_DOC.ESTADO = 'A' THEN
								m.estado 	= 'A'
								m.secuencia = oNSec.secuencia
								SELECT curcab
								APPEND BLANK
								GATHER MEMVAR
								exit 
							ELSE
								LOOP
							ENDIF
						ENDIF
					ELSE
						LOOP
					ENDIF
						
				ENDSCAN
			ELSE
				LOOP
			ENDIF	
			SELECT AUX_NOTA_SEC			
		ENDSCAN
	ELSE
		LOOP
	ENDIF
ENDSCAN

*** ELIMINANDO SECUENCIAS DE NOTAS QUE FUERON ANULADAS
SELECT * FROM curcab WHERE secuencia='0002' INTO CURSOR caux
SELECT caux
SCAN ALL
	SCATTER MEMVAR
	IF m.estado='A'
		lckey=M.ano_eje+M.sec_ejec+M.sec_ejec2+m.sec_nota
		=SEEK (lckey,'curcab','NOTA')
		SELECT curcab
		DELE WHILE ano_eje+sec_ejec+sec_ejec2+sec_nota=lckey
	ENDIF
	SELECT caux
ENDSCAN

*** CARGANDO EL DETALLE DE LAS MODIFICACIONES
SELECT curcab
SCAN ALL FOR !DELETED()
	SCATTER NAME oNot
	*** ANO_EJE+SEC_EJEC+SEC_EJEC2+SEC_NOTA+ORIGEN+FUENTE_FINANC+TIPO_RECURSO+SEC_FUNC+ID_CLASIFICADOR
	lcKeyNota = oNot.ANO_EJE + oNot.SEC_EJEC + oNot.SEC_EJEC2 + oNot.SEC_NOTA 
	IF SEEK(lcKeyNota,"AUX_NOTA_DET","MPNOTADETP")
		SELE AUX_NOTA_DET
		SCAN WHILE ANO_EJE+SEC_EJEC+SEC_EJEC2+SEC_NOTA=lcKeyNota
			SCATTER NAME oNotDet
			lckey			= oNotDet.ano_eje+oNotDet.sec_ejec+oNotDet.origen+oNotDet.fuente_financ+oNotDet.tipo_recurso+oNotDet.sec_func+oNotDet.id_clasificador
			lcClasificador  = ""
			IF SEEK(oNotDet.ano_eje+oNotDet.id_clasificador, 'CTB_Mae_Cla')
				lcClasificador = CTB_Mae_Cla.Clasificador
			ENDIF
			
			IF !SEEK(lckey,'curModificacion','gto')
				INSERT INTO curModificacion  ( ano_eje, sec_ejec, origen, fuente_financ, tipo_recurso,sec_func, id_clasificador, modificacion,clasificador) ;
				VALUES(oNotDet.ano_eje, oNotDet.sec_ejec,	oNotDet.origen, oNotDet.fuente_financ, oNotDet.tipo_recurso,oNotDet.sec_func, oNotDet.id_clasificador, (oNotDet.monto_a - oNotDet.monto_de),lcClasificador)
			ELSE
				REPLACE curModificacion.modificacion WITH curModificacion.modificacion + (oNotDet.monto_a - oNotDet.monto_de) IN curModificacion
			ENDIF
			lcKey_det = oNotDet.ano_eje+oNotDet.sec_ejec+oNotDet.origen+oNotDet.fuente_financ+oNot.tipo_modificacion+oNotDet.tipo_recurso+oNotDet.sec_func+oNotDet.id_clasificador
			IF !SEEK(lckey_det,'curModificacion_det','gto')
				INSERT INTO curModificacion_det  ( ano_eje, sec_ejec, origen, fuente_financ, tipo_modificacion, tipo_recurso,sec_func, id_clasificador, modificacion,clasificador) ;
					VALUES(	oNotDet.ano_eje, oNotDet.sec_ejec,	;
					oNotDet.origen, oNotDet.fuente_financ, oNot.tipo_modificacion, oNotDet.tipo_recurso,oNotDet.sec_func, oNotDet.id_clasificador, (oNotDet.monto_a - oNotDet.monto_de),lcClasificador)
			ELSE
				REPLACE curModificacion_det.modificacion		WITH curModificacion_det.modificacion + (oNotDet.monto_a - oNotDet.monto_de) IN curModificacion_det
			ENDIF
		ENDSCAN
	ENDIF
	SELE curcab
ENDSCAN
USE IN r_ejecutora
USE IN r_meta
USE IN r_act_proy
USE IN programa_ppto_nombre1 
USE IN NOTA_CAB 			
USE IN NOTA_SEC 			
USE IN NOTA_DET 			
USE IN NOTA_DOC_SEC 	
USE IN NOTA_DOC	
