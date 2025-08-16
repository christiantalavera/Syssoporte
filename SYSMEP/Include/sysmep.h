* CERT/Ass

#INCLUDE FOXPRO.H
#DEFINE LLKEY				"SPIDERMANHYPE"
#DEFINE LLKEY_MENU			"SPIDERMANHOMECOMING"


*-- Títulos de Caja de Mensajes y otros mensajes
#DEFINE MSG_ERRORTITLE      "Mensaje SYSMEP"
#DEFINE MSG_APP             "SYSMEP"
#DEFINE MSG_ENTERADDMODE    "Archivo vacío. Se inicia modo 'Crear'."
#DEFINE MSG_TABLERULEFAIL   "Regla de Archivo falló!"
#DEFINE MSG_SAVECHANGES     "Desea guardar sus cambios primero?"
#DEFINE MSG_DELETEREC       "Desea eliminar este registro?"
#DEFINE MSG_DELETEWARN      "Aviso de eliminación"
#DEFINE MSG_ADDNEWREC       "Archivo vacío. Desea agregar un registro nuevo?"
#DEFINE MSG_INFSAVED        "Información guardada."
#DEFINE MSG_RECCOPIED       "Registro copiado."
#DEFINE MSG_EMPTYPICKLIST   "Lista vacía"
#DEFINE MSG_METHOD          "Método: "
#DEFINE MSG_LINENUM         "Línea: "
#DEFINE MSG_CONFLICT        "Sobreescribe cambios hechos por otro usuario? "
#DEFINE MSG_PRINTPREVIEW    "Ver la impresión en pantalla?"
#DEFINE MSG_PRINTNOW        "Imprimir Ahora?"
#DEFINE MSG_EXPORTNOW       "Exportar Registros Ahora?"
#DEFINE MSG_ERRORCICLO		"Ciclo errado, reingrese."
#DEFINE MSG_ERRORFASE		"Fase errada, reingrese."
#DEFINE MSG_ERRORCLASIF		"Clasificador errado, reingrese."
#DEFINE MSG_ERRORMETA		"Meta errada, reingrese."
#DEFINE MSG_SOBREGIRO		"Monto genera sobregiro, reingrese."
#DEFINE _GridTreeViewVCX    "GridTreeview.vcx"

*-- Indica el estado del alias actual en sfEditform
#DEFINE FILE_OK      0
#DEFINE FILE_BOF     1
#DEFINE FILE_EOF     2
#DEFINE FILE_CANCEL  3

*-- Constantes para identificar que trigger falló usando elemento 5 del array retornado por
*-- AERROR(), asi como para apuntar al elemento del array de mensaje de error: aErrorMsg[]
#DEFINE INSERTTRIG  1
#DEFINE UPDATETRIG  2
#DEFINE DELETETRIG  3

*-- Trigger mensajes de error
#DEFINE TRIG_INSERTTRIGFAIL  "Trigger de inserción falló!"
#DEFINE TRIG_UPDATETRIGFAIL  "Trigger de actualización falló!"
#DEFINE TRIG_DELETETRIGFAIL  "Trigger de eliminación falló!"

*-- Teclado
#DEFINE CRLF   CHR(13) + CHR(10)
#DEFINE CR     CHR(13)
#DEFINE TAB    CHR(9)

*-- Variables de memoria diversas
#DEFINE DEBUGMODE    .F.
#DEFINE CURRENCY     "S/."
#DEFINE AERRORARRAY  7
#DEFINE CUSTSTRUARRAY  [1,1]
* Insert assertion function call into program files
#DEFINE ccAssertCommand =g_assert

*-- Uncomment this line to remove assertion calls when desired
*#DEFINE ccAssertCommand **

*-- Event trapping function.
#DEFINE ccEventTrapInit =g_inievt()

*-- Uncomment this line to remove event trap initialization
*#DEFINE ccEventTrapInit **

#DEFINE ccEventTrapTrapEvent =g_recevt(program(), lineno())

**-- Uncomment this line to remove event trap recording
*#DEFINE ccEventTrapTrapEvent **

*-- CONSTANTES TIPOS DE PROCESOS DE SELECCION
#DEFINE LP 	    		'01'
#DEFINE CP 	 			'02'
#DEFINE ADP				'03'
#DEFINE ADS				'04'
#DEFINE AMCD			'05'
#DEFINE LPN				'06'
#DEFINE LPI				'07'
#DEFINE CPN	 			'08'
#DEFINE CPI	   			'09'
#DEFINE AMC 			'10'
#DEFINE AMCE			'11'
#DEFINE AMCEI			'12'
#DEFINE AMCTU			'13'


*-- CONSTANTES ETAPAS DE LOS PROCESOS DE SELECCION
#DEFINE CONV 	   		'01'
#DEFINE VB				'02'
#DEFINE PC				'03'
#DEFINE AC				'04'
#DEFINE OBS				'05'
#DEFINE PAP				'08'
#DEFINE BP				'10'
#DEFINE BPC				'11'
#DEFINE CF				'12'

*-- CONSTANTES SITUACION DEL CONTRATO
#DEFINE CEJ		   		'01'
#DEFINE CIN				'02'
#DEFINE CNU				'03'
#DEFINE CRE				'04'
#DEFINE CLI				'05'

