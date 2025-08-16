xRutaOrigen = RTRIM(gcRutaSIAF)
xRutaDestino = RTRIM(gcRutacERT)
xBDOrigen = xRutaOrigen + 'SIAF.dbc'
xBDDestino = xRutaDestino +'SYSMEP.dbc'
OPEN DATABASE &xBdOrigen

*!*	SELECT * ;
*!*	   from &xBdOrigen ;
*!*	   where objecttype='Table' AND !DELETED() ;
*!*	   into cursor curTabla READWRITE ORDER BY 4
   
CREATE CURSOR curTabla (objectname          c(50))

INSERT INTO curTabla VALUES ('BENEFICIARIO')
INSERT INTO curTabla VALUES ('CONVENIO_ORGANISMO')
INSERT INTO curTabla VALUES ('CONVENIO_PROYECTO')
INSERT INTO curTabla VALUES ('CONTROL_PROYECTO')
INSERT INTO curTabla VALUES ('EJECUTORA_SECUENCIA')
INSERT INTO curTabla VALUES ('JUSTIFICACION')
INSERT INTO curTabla VALUES ('JUSTIFICACION_DETALLE')
INSERT INTO curTabla VALUES ('PROYECTO_ACTIVIDAD')
INSERT INTO curTabla VALUES ('PROYECTO_CATEGORIA_GASTO')
INSERT INTO curTabla VALUES ('PROYECTO_COMPONENTE')
INSERT INTO curTabla VALUES ('PROYECTO_DETALLE')
INSERT INTO curTabla VALUES ('PROYECTO_ELEMENTO_GASTO')
INSERT INTO curTabla VALUES ('PROYECTO_ESPECIFICA_GASTO')
INSERT INTO curTabla VALUES ('PROYECTO_GRUPO_GASTO')
INSERT INTO curTabla VALUES ('PROYECTO_NOTA')
INSERT INTO curTabla VALUES ('PROYECTO_PRESUPUESTO')
INSERT INTO curTabla VALUES ('PROYECTO_PRESUPUESTO_PAD')
INSERT INTO curTabla VALUES ('PROYECTO_SUB_ACTIVIDAD')
INSERT INTO curTabla VALUES ('PROYECTO_SUB_COMPONENTE')
INSERT INTO curTabla VALUES ('PROYECTO_SUB_ESPECIFICA_GASTO')
INSERT INTO curTabla VALUES ('PRY_HT_CONVENIO_CAB')
INSERT INTO curTabla VALUES ('PRY_HT_CONVENIO_DET')
INSERT INTO curTabla VALUES ('PRY_HT_CONVENIO_DIF_CAMBIO')
INSERT INTO curTabla VALUES ('PRY_HT_INFORME_CAB')
INSERT INTO curTabla VALUES ('PRY_HT_INFORME_DET')
INSERT INTO curTabla VALUES ('PROYECTO_META_FISICA')
INSERT INTO curTabla VALUES ('PROYECTO_META_FISICA_DET')
INSERT INTO curTabla VALUES ('UNIDAD_MEDIDA_INDICADOR')
INSERT INTO curTabla VALUES ('PROYECTO_ACUMULADO')
INSERT INTO curTabla VALUES ('TIPO_CAMBIO_MEP')
INSERT INTO curTabla VALUES ('PROYECTO_TIPO_CAMBIO')
 

WAIT WINDOW 'Iniciando carga....' NOWAIT 

SELECT  curTabla
SCAN all
      lcTabla=alltrim(curTabla.objectname)
      WAIT WINDOW "Procesando Tabla '" + lcTabla + "'" NOWAIT 
      lcOrigen = xRutaOrigen  + alltrim(curTabla.objectname) +'.dbf'
      lcDestino = xRutaDestino + alltrim(curTabla.objectname)+'.dbf'
	
      IF FILE(lcDestino) THEN 
		    USE  &lcOrigen  IN 0 AGAIN ALIAS tablaorigen 
		    USE  &lcDestino IN 0 AGAIN ALIAS tablaDestino exclusive 
    		SELECT TablaDestino
    		ZAP 
      		SELECT tablaOrigen
      		SCAN ALL 
      			SCATTER MEMVAR 
      			INSERT INTO TablaDestino FROM memvar
      		ENDSCAN 

      		USE IN tablaDestino
      		USE IN tablaOrigen
      ENDIF 
ENDSCAN 

*CLOSE ALL 


*----------------------
procedure sf_reindexa
*----------------------
 
 select * ;
   from sysmep.dbc ;
   where objecttype='Table' AND !DELETED() ;
   into cursor cursiaf_tablas READWRITE 
    
   index on objectid tag ind02  
   
USE IN sysmep
   select cursiaf_tablas 
 
   scan all        
     SCATTER MEMVAR 
      lcTabla=alltrim(m.objectname)
      WAIT WINDOW "Reindexando Tabla '" + lcTabla + "'" NOWAIT 
      lcArchivo=gcrutacert+alltrim(m.objectname)+'.dbf'
      IF FILE(lcArchivo) THEN 
	      USE  &LcArchivo IN 0 AGAIN ALIAS &LcTabla EXCLUSIVE 
	      SELECT &LcTabla
	      Reindex        
	      USE IN &LcTabla
	    ENDIF 
      select cursiaf_tablas       
   endscan 
   WAIT WINDOW 'Proceso de reindexado finalizado exitosamente...'
  CLOSE DATABASES ALL  
RETURN  

*---------------------- 
procedure sf_Packea
*----------------------
 

 select * ;
   from sysmep.dbc ;
   where objecttype='Table' AND !DELETED();
   into cursor cursiaf_tablas READWRITE 
    
   index on objectid tag ind02  
   
USE IN sysmep
   SELECT cursiaf_tablas 
 
   SCAN ALL          
     SCATTER MEMVAR 
	  lcTabla=alltrim(m.objectname)
      lcArchivo=gcrutacert+alltrim(m.objectname)+'.dbf'
      IF FILE(lcArchivo) THEN 
	      WAIT WINDOW "Pack en Tabla '" + lcTabla + "'" NOWAIT 
	      USE  &lcArchivo IN 0 AGAIN ALIAS &LcTabla EXCLUSIVE 
	      SELECT &LcTabla
	      Pack
	      USE IN &LcTabla
      ENDIF 
      SELECT cursiaf_tablas       
   endscan 
   WAIT WINDOW 'Proceso de eliminación finalizado exitosamente...'
  CLOSE DATABASES ALL  
RETURN  


