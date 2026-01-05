****************************************
*-- Se conecta con la base de datos
FUNCTION Conecta
PARAMETERS glUsuario, glClave, glBDatos, glTypeBD 
*LOS PARAMETROS LLEGAN YA INICIADOS DESDE EL MAIN
Private OK 
	WAIT WINDOW "Conectando con la Base de Datos ("+ ALLTRIM(glTypeBD)+")" NOWAIT 
    OK = .T.
    IF glTypeBD  = "ORACLE" Then
       pc_ConnString= "Provider=ORAOLEDB.ORACLE;Persist Security Info=False;" + ;
                      "User Id=" + glUsuario +";" + ;
                      "Password=" + glClave +";Data Source=" + glBDatos +";"
    EndIf
    pn_Conexion  = createobject('ADODB.Connection')
    TRY
    	pn_Conexion.Attributes=262144 &&adXactAbortRetaining
   	    pn_Conexion.Open(pc_ConnString)
	    pn_Conexion.BeginTrans
    CATCH TO oException
	    OK = .F.
	    =ReporteErrores(oException, 0)
	    MESSAGEBOX("No se puede Realizar la Conexión a la B.D., Revise su conexión.",16,"Error en la Conexión a la B.D.")
    	*MESSAGEBOX(oException.Message)
    ENDTRY
RETURN OK