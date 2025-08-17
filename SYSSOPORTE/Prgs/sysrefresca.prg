** RM  : REFRESCAR MARCO PRESUPUESTAL Y PCA
** RC  : REFRESCAR CERTIFICACION
** RE  : REFRESCAR EXPEDIENTE

CREATE CURSOR curGenerica (ano_eje			c(4),;
						   sec_ejec			c(6),;
						   origen			c(1),;
						   fuente_financ	c(2),;
						   categoria_gasto	c(1),;
						   tipo_transaccion	c(1),;
						   generica			c(1))

SELECT curGenerica
INDEX on ano_eje+sec_ejec+origen+fuente_financ+categoria_gasto+tipo_transaccion+generica TAG inxgen

CREATE CURSOR curCertificado (ano_eje		c(4),;
							  sec_ejec		c(6),;
							  certificado	c(10),;
							  secuencia		c(4),;
							  seleccionado	n(1) DEFAULT 1)

SELECT curCertificado
INDEX on ano_eje+sec_ejec+certificado+secuencia TAG inx1 
							
CREATE CURSOR CurExpediente  (ano_eje		c(4),;
							  sec_ejec		c(6),;
							  expediente	c(10),;
							  ciclo			c(1),;
							  fase			c(1),;
							  seleccionado	N(1))		

SELECT curExpediente							  					  
INDEX ON ano_eje+sec_ejec+expediente+ciclo+fase TAG inx1 
						   	
USE syssoporte!log_proceso IN 0 ORDER tag ueestado

SELECT log_proceso
SEEK 'NRM'
SCAN WHILE ue_estado+tipo_operacion = "NRM" 
	SCATTER MEMVAR 
	IF !SEEK(m.ano_eje+m.sec_ejec+m.origen+m.fuente_financ+m.categoria_gasto+m.tipo_transaccion+m.generica,'curGenerica') THEN 
		INSERT INTO curGenerica (ano_eje,sec_ejec, origen, fuente_financ, categoria_gasto, tipo_transaccion, generica) ;
						VALUES (m.ano_eje, m.sec_ejec, m.origen, m.fuente_financ, m.categoria_gasto, m.tipo_transaccion, m.generica)
	ENDIF 
	REPLACE log_proceso.ue_estado WITH 'A'
ENDSCAN  
DO REFRESCA_PCA_PIM

*!*	SELECT log_proceso
*!*	SEEK 'NRC'
*!*	SCAN WHILE ue_estado+tipo_operacion = "NRC" 
*!*		SCATTER MEMVAR 
*!*		IF !SEEK(m.ano_eje+m.sec_ejec+m.certificado+m.secuencia,'curCertificado','inx1') THEN 
*!*			INSERT INTO curCertificado (ano_eje,sec_ejec, certificado, secuencia) ;
*!*							VALUES (m.ano_eje, m.sec_ejec, m.certificado,m.secuencia)
*!*		ENDIF 
*!*		REPLACE log_proceso.ue_estado WITH 'A'	
*!*	ENDSCAN  
*!*	DO Refresca_certificado


*!*	SELECT log_proceso
*!*	SEEK 'NRE'
*!*	SCAN WHILE ue_estado+tipo_operacion = "NRE" 
*!*		SCATTER MEMVAR 
*!*		IF !SEEK(m.ano_eje+m.sec_ejec+m.expediente+m.ciclo+m.fase,'curExpediente','inx1') THEN 
*!*			INSERT INTO curExpediente (ano_eje,sec_ejec, expediente, ciclo, fase) ;
*!*							VALUES (m.ano_eje, m.sec_ejec, m.expediente,m.ciclo, m.fase)
*!*		ENDIF 
*!*		REPLACE log_proceso.ue_estado WITH 'A'	
*!*	ENDSCAN  
*!*	DO Refresca_Expediente