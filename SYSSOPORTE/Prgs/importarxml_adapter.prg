*-------------------------------------------------------------------------
* PROGRAMA: ImportarXML_Robusto.prg
* DESCRIPCIÓN: Intenta importar XML incluso si tiene problemas de codificación (tildes)
*-------------------------------------------------------------------------
LPARAMETERS tcArchivoXML, tcNombreCursor

* 1. VALIDACIONES BÁSICAS
IF PCOUNT() < 2
    MESSAGEBOX("Sintaxis: DO ImportarXML_Robusto WITH 'archivo.xml', 'alias'", 16)
    RETURN .F.
ENDIF

IF !FILE(tcArchivoXML)
    MESSAGEBOX("Archivo no encontrado: " + tcArchivoXML, 16)
    RETURN .F.
ENDIF

* Limpieza de cursor previo
IF USED(tcNombreCursor)
    USE IN (tcNombreCursor)
ENDIF

LOCAL loAdapter as XMLAdapter
LOCAL loTabla as XMLTable
LOCAL lcXMLString, llExito
llExito = .F.

TRY
    loAdapter = CREATEOBJECT("XMLAdapter")
    loAdapter.RespectNesting = .T.
    
    * INTENTO 1: Carga Estándar (Suponiendo que el XML está perfecto)
    TRY
        loAdapter.LoadXML(tcArchivoXML, .T.)
    CATCH
        * --- ZONA DE RECUPERACIÓN DE ERRORES ---
        * Si falla, asumimos que es por culpa de la tilde (Encoding).
        * Leemos el archivo crudo a una variable.
        lcXMLString = FILETOSTR(tcArchivoXML)
        
        * TRUCO DE EXPERTO:
        * Reemplazamos la declaración "UTF-8" por "Windows-1252" en la cabecera.
        * Esto obliga al parser a aceptar la 'Ó' de COMISIÓN como válida.
        lcXMLString = STRTRAN(lcXMLString, 'encoding="UTF-8"', 'encoding="Windows-1252"', 1, 1, 1)
        lcXMLString = STRTRAN(lcXMLString, "encoding='UTF-8'", "encoding='Windows-1252'", 1, 1, 1)
        
        * Intentamos cargar de nuevo, pero esta vez desde la VARIABLE (.F.)
        loAdapter.LoadXML(lcXMLString, .F.)
    ENDTRY
    
    * 2. PROCESAR SI CARGÓ
    IF loAdapter.Tables.Count > 0
        loTabla = loAdapter.Tables(1)
        
        * Convertimos a cursor
        loTabla.ToCursor(.F., tcNombreCursor)
        llExito = .T.
    ELSE
        MESSAGEBOX("El XML no contiene tablas.", 48)
    ENDIF

CATCH TO loEx
    * Si falló incluso con el truco, mostramos el error original
    MESSAGEBOX("Error Fatal: No se pudo reparar el XML." + CHR(13) + ;
               "Razón: " + loEx.Message, 16)
ENDTRY

*!*	IF llExito
*!*	    SELECT (tcNombreCursor)
*!*	    MESSAGEBOX("Importación Exitosa (con corrección de tildes).", 64)
*!*	    BROWSE NORMAL NOWAIT
*!*	ENDIF