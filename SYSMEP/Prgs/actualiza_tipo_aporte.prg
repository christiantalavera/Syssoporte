WAIT WINDOW 'Iniciando Proceso' nowait
USE sysmep!proyecto_detalle			IN 0 AGAIN ORDER tag sec_det
USE sysmep!proyecto_nota			IN 0 AGAIN ORDER tag nota
USE siaf!expediente_fase			IN 0 AGAIN ORDER tag exp_fase

SELECT proyecto_detalle
SET RELATION TO ano_eje+sec_ejec+expediente+ciclo+fase+secuencia INTO expediente_fase ADDITIVE 

SELECT proyecto_detalle
SCAN ALL 
	DO CASE 
		CASE expediente_fase.fuente_financ = '00' 
			replace proyecto_detalle.tipo_aporte WITH '00'	
		CASE expediente_fase.fuente_financ = '19' AND expediente_fase.tipo_financiamiento = 'E'
			replace proyecto_detalle.tipo_aporte WITH '01'	
		CASE expediente_fase.fuente_financ = '19' AND expediente_fase.tipo_financiamiento = 'I'		
			replace proyecto_detalle.tipo_aporte WITH '99'			
	ENDCASE 
ENDSCAN 

SELECT proyecto_nota
SCAN ALL 
	DO CASE 
		CASE proyecto_nota.fuente_financ = '00'
			replace proyecto_nota.tipo_aporte WITH '00'
		CASE proyecto_nota.fuente_financ = '19'
			replace proyecto_nota.tipo_aporte WITH '01'	
	ENDCASE 

ENDSCAN 

USE IN proyecto_detalle
USE IN proyecto_nota
USE IN expediente_fase 
WAIT WINDOW 'Proceso Terminado' nowait