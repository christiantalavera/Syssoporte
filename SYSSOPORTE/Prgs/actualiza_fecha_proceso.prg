USE siaf!expediente_secuencia ORDER tag exp_sec ALIAS cExpediente_secuencia
SELECT cExpediente_secuencia
SEEK gcano_eje+gcsec_ejec
SCAN WHILE ano_eje+sec_ejec = gcano_eje+gcsec_ejec
	IF EMPTY(ano_proceso) OR EMPTY(mes_proceso) OR EMPTY(dia_proceso) THEN 
		REPLACE ano_proceso WITH SUBSTR(DTOS(cExpediente_secuencia.fecha_doc),1,4) , ;
			mes_proceso WITH SUBSTR(DTOS(cExpediente_secuencia.fecha_doc),5,2) , ;
			dia_proceso WITH SUBSTR(DTOS(cExpediente_secuencia.fecha_doc),7,2)			
	ENDIF 
ENDSCAN
USE IN cExpediente_secuencia