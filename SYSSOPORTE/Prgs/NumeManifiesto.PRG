SET DELETED ON 

Public m.nhandle, controlador, strConSQL, strSQL, TmpCursor, mDataSource
Public vUsuario, vRutaDBF, cnxQry

Wait Window 'Conectandose a la BD. ADUANA ....' nowait

vUsuario='repusr'
vpasword='repusr'
mDataSource='aduana'

Store SQLConnect(mDataSource,vUsuario,vpasword ) To m.nhandle
If m.nhandle <= 0
  =messagebox("Falló la conexión inicial",64,"Mensaje del Sistema")
  sTerminar = .t.
  cancel
Else
  controlador = m.nhandle
EndIf

pNroMaster = '07582316544'
pNroMaster = '72968003935'
pAnoOrden = "2011"
pNroOrden = "100001"
pAduOrden = "235"
pTerminal = "3272"
p="4173"

strConSQL = "Select pmnf0000.nawb000, pmnf0000.cori000, pmnf0000.ccli000, pmnf0000.cdst000, pmnf0000.cprd000, pmnf0000.nvue000, pmnf0000.fvue000, "+;
			"pmnf0000.drte000, pmnf0000.dcsi000, pmnf0000.qpesarr, pmnf0000.qpzaarr, " + ;
			"(Select Darrawb0.QPzaArr " +;
			"   From Darrawb0 " + ;
			"  Where Darrawb0.Nmnf000 = Pmnf0000.Nmnf000 " + ;
     		"	 And Darrawb0.Nawbmas = Pmnf0000.Nawbmas " + ;
			"	 And Darrawb0.Nawb000 = Pmnf0000.Nawb000) As PiezaDHL, " + ;
			"(Select Darrawb0.QPesArr " + ;
			"	From Darrawb0 " + ;
			"  Where Darrawb0.Nmnf000 = Pmnf0000.Nmnf000 " + ;
			"	 And Darrawb0.Nawbmas = Pmnf0000.Nawbmas " + ;
			"	 And Darrawb0.Nawb000 = Pmnf0000.Nawb000) As PesoDHL, " + ;
	  "Nvl((Select 1 " + ;
			"   From Darrawb0 " + ;
			"  Where Darrawb0.Nmnf000 = Pmnf0000.Nmnf000  " + ;
			"	 And Darrawb0.Nawbmas = Pmnf0000.Nawbmas  " + ;
			"	 And Darrawb0.Nawb000 = Pmnf0000.Nawb000), 0) As DHL, " + ;
			"pmnf0000.imonbas, " + ;
	  "Nvl((Select 1 " + ;
      "      From POpeArr0, Darrawb0 " + ;
      "		  Where POpeArr0.Nawb000 = Darrawb0.Nawb000  " + ;
	  "			 And POpeArr0.NarrPza = Darrawb0.NarrPza " + ;
	  "			 And Darrawb0.Nawb000 = Pmnf0000.Nawb000 " + ;
	  "			 And Darrawb0.Nawbmas = Pmnf0000.Nawbmas " + ;
	  "			 And Darrawb0.Nmnf000 = Pmnf0000.Nmnf000 " + ;
	  "			 And POpeArr0.CTrs000 = '01' " + ;
	  "			 And POpeArr0.COpe000 = '02' " + ;
	  "			 And Trim(POpeArr0.NOpe000) in ('010', '105')), 0) As CR, " + ;
	  "Nvl((Select 1 " + ;
  	  "			From TabMMG " + ;
 	  "		  Where TabMMG.Fld_Master = Pmnf0000.Nawbmas " + ;
	  "			 And TabMMG.Fld_Manif = Pmnf0000.Nmnf000 " + ;
	  "			 And TabMMG.Fld_Guia  = Pmnf0000.Nawb000 " + ;
		"		 And TabMMG.Fld_EstSbr = '1'), 0) As Sbr, " + ;
	  "Nvl((Select 1 " + ;
		"	   From POpeArr0, Darrawb0 " + ;
		"	  Where POpeArr0.Nawb000 = Darrawb0.Nawb000 " + ;
		"		 And POpeArr0.NarrPza = Darrawb0.NarrPza " + ;
		"		 And Darrawb0.Nawb000 = Pmnf0000.Nawb000 " + ;
		"		 And Darrawb0.Nawbmas = Pmnf0000.Nawbmas " + ;
		"		 And Darrawb0.Nmnf000 = Pmnf0000.Nmnf000 " + ;
		"		 And POpeArr0.CTrs000 = '01' " + ;
		"		 And POpeArr0.COpe000 = '02' " + ;
		"		 And Trim(POpeArr0.NOpe000) <> '010' " + ;
		"		 And Trim(POpeArr0.Nope000) <> ''), 0) As Ret, " + ;
	  "Nvl((Select Distinct Trim(Papbadu0.Nume_Corre)||','||Trim(Papbadu0.Tipo_Aforo) " + ;
		"	   from Papbadu0, TTeleman, Darrawb0 " + ;
		"	  Where Darrawb0.Nawbmas = Pmnf0000.Nawbmas " + ;
		"		 And Darrawb0.Nmnf000 = Pmnf0000.Nmnf000 " + ;
		"		 And Darrawb0.Nawb000 = Pmnf0000.Nawb000 " + ;
		"		 and Darrawb0.Nawb000 = TTeleman.Nawb000 " + ;
		"		 And Darrawb0.Narrpza = TTeleman.Narrpza " + ;
		"		 And LPad(TTeleman.NOrden0, 6, '0') = Papbadu0.Nume_Orden " + ;
		"		 and TTeleman.FAprob0 = Papbadu0.Ano_Prese), " + ;
	  	"	(Select Distinct Trim(Papbadu0.Nume_Corre)||','||Trim(Papbadu0.Tipo_Aforo) " + ;
		"		From Papbadu0, TabTdspGDCab C, TabTdspGDDet D " + ;
		"	  Where Pmnf0000.Nawbmas = C.Fld_Master " + ;
		"		 And Pmnf0000.Nmnf000 = C.Fld_Manif " + ;
		"		 And C.Fld_Master = D.Fld_Master " + ;
		"		 And C.Fld_Manif = D.Fld_Manif " + ;
		"		 And C.Fld_NumOrd = D.Fld_NumOrd " + ;
		"		 And C.Fld_AnnoOrd = D.Fld_AnnoOrd " + ;
		"		 And D.Fld_Guia = Pmnf0000.Nawb000 " + ;
		"		 And C.Fld_NumOrd = Papbadu0.Nume_Orden " + ;
		"		 And C.Fld_AnnoOrd = Papbadu0.Ano_Prese " + ;
		"		 And D.Fld_EstTdsp = 1)) As Poliza, " + ;
		"	(Select TabTipCond.Fld_DesTCon " + ;
 		"		from TabTipCond " + ;
		"	  Where TabTipCond.Fld_CodTCon = Pmnf0000.TipCon) As TipCon, " + ;
		"	(Select TabTipCarga.Fld_DesTCar " + ;
		"		From TabTipCarga " + ;
		"	  Where TabTipCarga.Fld_CodTCar = Pmnf0000.TipCar) As TipCar, " + ;
	  "Nvl((Select Lpad(TabActInv.Fld_NumAI,8,'0')||' - '||TabActInv.Fld_AnnoAI " + ;
		"		From TabActInv " + ;
		"	  Where TabActInv.Fld_Master = Pmnf0000.Nawbmas  " + ;
		"		 And TabActInv.Fld_Manif = Pmnf0000.Nmnf000 " + ;
		"		 and TabActInv.Fld_Guia = Pmnf0000.Nawb000),'') As NAAI, " + ;
	  "Nvl((Select TabActInv.Fld_PzaME " + ;
		"		From TabActInv " + ;
		"	  Where TabActInv.Fld_Master = Pmnf0000.Nawbmas " + ;
		"		 And TabActInv.Fld_Manif = Pmnf0000.Nmnf000 " + ;
		"		 And TabActInv.Fld_Guia = Pmnf0000.Nawb000),0) As PzaME, " + ;
	  "Nvl((Select TabActInv.Fld_PesoME " + ;
		"		From TabActInv " + ;
  		"	  Where TabActInv.Fld_Master = Pmnf0000.Nawbmas " + ;
		"		 And TabActInv.Fld_Manif = Pmnf0000.Nmnf000 " + ;
		"		 And TabActInv.Fld_Guia = Pmnf0000.Nawb000),0) As PesoME, " + ;
	  "Nvl((Select 1 " + ;
		"		From GuiaImpCash " + ;
		"	  Where GuiaImpCash.Guia = Pmnf0000.Nawb000),0) as IMPCash " + ;
	"From pmnf0000  " + ;
  "Where Pmnf0000.NawbMas = '"+pNroMaster+"'" 

SQLExec(Controlador, strConSQL, "qDatoVuelo")
SELECT qDatoVuelo
COPY TO C:\DATAVUELO1.DBF 

*** Para cada Guia:
SELECT qDatoVuelo
GO TOP
DO While!EOF()
	pNroGuia = Nawb000
	strConSQL = "Select pmnf0000.nawbmas,pmnf0000.nawb000,pmnf0000.nmnf000,pmnf0000.cori000,pmnf0000.corisis,pmnf0000.cdst000,pmnf0000.cpueetd,pmnf0000.dfaccom," + ;
			"pmnf0000.ffaccom,pmnf0000.nvue000,pmnf0000.fvue000,pmnf0000.cprd000,pmnf0000.dced000,pmnf0000.idclawb,pmnf0000.cmonorg,pmnf0000.imonbas,pmnf0000.iflete,"+;
			"pmnf0000.iseguro,pmnf0000.qpzaawb,pmnf0000.qpesawb,pmnf0000.qpzaarr,pmnf0000.qpesarr,pmnf0000.drte000,pmnf0000.ddirrt1,pmnf0000.ddirrt2,pmnf0000.ddirrt3,"+;
			"pmnf0000.cposrte,pmnf0000.ntelrte,pmnf0000.ccli000,pmnf0000.dcsi000,pmnf0000.ddircs1,pmnf0000.ddircs2,pmnf0000.ddircs3,pmnf0000.cposcsi,pmnf0000.ntelcsi,"+;
			"pmnf0000.tipcon,pmnf0000.tipcar, " + ;
			"(Select MCli0000.NRUC000 " + ;
			"From MCli0000 " + ;
			"Where MCli0000.CCli000 = Pmnf0000.CCli000) As NumDoc, " + ;
			"'' As MotCod, " + ;
			"(Select PopeArr0.Nope000 " + ;
			"From Darrawb0, PopeArr0 " + ;
			"Where Darrawb0.Nawb000 = Pmnf0000.Nawb000 And " + ;
			"			Darrawb0.Nmnf000 = Pmnf0000.Nmnf000 And " + ;
			"			Darrawb0.Nawb000 = PopeArr0.Nawb000 And " + ;
			"			Darrawb0.NarrPza = PopeArr0.NarrPza And " + ;
			"			PopeArr0.Cope000 = '02')  As MotRet, " + ;
			"0 As NumArr, " + ;
			"(Select Darrawb0.NarrPza " + ;
			"		From Darrawb0 " + ;
			"		Where Darrawb0.Nawbmas = Pmnf0000.Nawbmas And " + ;
			"					Darrawb0.Nawb000 = Pmnf0000.Nawb000 And " + ;
			"					Darrawb0.Nmnf000 = Pmnf0000.Nmnf000 And " + ;
			"					Darrawb0.Nanomnf > 0) As Pase, " + ;
			"Nvl((Select Trim(TabMMG.Fld_EstSbr) " + ;
			"		From TabMMG " + ;
			"		Where TabMMG.Fld_Master = Pmnf0000.Nawbmas And " + ;
			"					TabMMG.Fld_Manif = Pmnf0000.Nmnf000 And " + ;
			"					TabMMG.Fld_Guia = Pmnf0000.Nawb000),'0') As Sbr, " + ;
			"0 As FlgSbr, " + ;
			"Nvl((Select Trim(TabMMG.Fld_EstChkSbr) " + ;
			"		From TabMMG " + ;
			"		Where TabMMG.Fld_Master = Pmnf0000.Nawbmas And " + ;
			"					TabMMG.Fld_Manif = Pmnf0000.Nmnf000 And " + ;
			"					TabMMG.Fld_Guia = Pmnf0000.Nawb000),'0') As EstChkSbr, " + ;
			"Nvl((Select Trim(TabMMG.Fld_UsuCL) " + ;
			"	From TabMMG  " + ;
			"	Where TabMMG.Fld_Master = Pmnf0000.Nawbmas And " + ;
			"					TabMMG.Fld_Manif = Pmnf0000.Nmnf000 And " + ;
			"					TabMMG.Fld_Guia = Pmnf0000.Nawb000),'') As UsuCL, " + ;
			"Nvl((Select 1 " + ;
			"		From GuiaIMPCash " + ;
			"		Where GuiaIMPCash.Guia = Pmnf0000.Nawb000),0) As IMPCash, " + ;
			"Nvl((Select 1 " + ;
			"		From dawbdesg " + ;
			"		Where dawbdesg.nawb000 = pmnf0000.nawb000 and dawbdesg.nawbmas = pmnf0000.nawbmas and dawbdesg.nmnf000 = pmnf0000.nmnf000),0) As desglose, " + ;
         	"(SELECT pa.codtipawb " + ;
         	"   FROM pmnfawb pa " + ;
         	"  WHERE pa.nawbmas = Pmnf0000.Nawbmas " + ;
         	"    and pa.nawb000 = Pmnf0000.Nawb000)  As EstTipo " + ;
		 	"From Pmnf0000  " + ;
		 	"Where PMnf0000.NawbMas = '" + pNroMaster + "'"
		 	**And PMnf0000.Nawb000 In ('" + pNroGuia + "') "
		 	SQLExec(Controlador, strConSQL, "qDatoGuia")

			*: Grabamos en DBFS de Estructura
		    *SELECT DUIMGUIA
		    *GO Bottom
		    *APPEND BLANK		    
		    *REPLACE codi_aduan WITH pAduOrden
		    *Replace ano_orden  WITH pAnoOrden
		    *REPLACE nume_orden WITH pNroOrden
		    *REPLACE CODI_REGI  WITH pRegOrden
			*REPLACE VIA_TRANS  WITH pViaTrans
			*REPLACE NUMCON	   WITH qDatoGuia->nawb000				&& CARACTER	25		N?MERO DE GUIA EER	---	M
			*REPLACE PUER_EMBAR WITH	"" 								&& CARACTER	5		C?DIGO DEL PUERTO DE EMBARQUE	---	M
			*REPLACE PUER_DESCA WITH ""								&& CARACTER	5		C?DIGO DEL PUERTO DE DESCARGA	---	M
			*REPLACE PUER_DESTI WITH ""								&& CARACTER	5		C?DIGO DEL PUERTO DESTINO FINAL	---	M
			*REPLACE CANT_BULTO WITH qDatoGuia->qpzaawb				&& NUMERICO	15,3	CANTIDAD DE BULTOS MANIFESTADOS	---	M
			*REPLACE TPESO_BRUT WITH qDatoGuia->qpesawb				&& NUMERICO	15,3	PESO BRUTO MANIFESTADOS	---	M

			*: Extraemos los Datos del Cliente
			*REPLACE CONSIGNA   WITH ALLTRIM(qDatoGuia->dcsi000)		&& CARACTER	60		NOMBRE DEL CONSIGNATARIO	---	M
			*REPLACE DIRECCION  WITH	ALLTRIM(qDatoGuia->ddircs1)		&& CARACTER	60		DIRECCI?N DEL CONSIGNATARIO	---	M

			*REPLACE DESCRIP	   WITH	ALLTRIM(qDatoGuia->dced000)		&& CARACTER	70		DESCRIPCI?N DE LA MERCADER?A	---	M
			*REPLACE FOB_CONO   WITH	qDatoGuia->imonbas 				&& NUMERICO	12,2	VALOR FOB DE LA MERCANC?A
			*REPLACE OBS1	   WITH	""								&& CARACTER	70		OBSERVACIONES 1	---	C
			*REPLACE OBS2	   WITH	""								&& CARACTER	70		OBSERVACIONES 2	---	C
			*REPLACE NUMCONM	   WITH	pNroMaster						&& CARACTER	25		NUMERO DE DOCUMENTO DE TRANSPORTE (HIJO O DIRECTO)	---	M
			*REPLACE TIPO_CONDI WITH	qDatoGuia->tipcon				&& CARACTER	3		CONDICION DE LA CARGA
			*REPLACE T_CARGA	   WITH	qDatoGuia->tipcar				&& CARACTER	1		TIPO DE CARGA

			*: Calculo de Codigo de Categoria
			*REPLACE COD_CATEG  WITH									&& CARACTER	4		CODIGO DE CATEGORIA
			SELECT qDatoMasGuia
			COPY TO C:\DATAGUIA1.DBF 
 SET STEP ON 
	SELECT qDatoVuelo
	SKIP	
ENDDO
