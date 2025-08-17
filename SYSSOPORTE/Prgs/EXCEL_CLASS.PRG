*---------------------------------------------------------------------------------------------------------------------------
* Nombre del Sistema       : 
* Nombre del Programa      : EXCEL_CLASS.PRG
* Nombre del Programador   : WALTER R. OJEDA VALIENTE
* Fecha de inicio          : 19/OCT/2012
* Fecha última modificación: 20/OCT/2012
* Archivos de entrada      : ---
* Archivos de salida       : ---
* Tarea realizada          : CLASE PARA AUTOMATIZAR EXCEL
*---------------------------------------------------------------------------------------------------------------------------

DEFINE CLASS aExcel AS CUSTOM
  
  cCarpetaExcel   = ""          && Carpeta en la cual se creará la planilla 
  cMensajeError   = ""          && Mensaje diciendo cual fue el error ocurrido
  cNombreModelo   = ""          && Nombre de una planilla Excel que servirá como modelo
  cNombrePlanilla = ""          && Nombre de la planilla Excel que se desea crear ahora
  oExcel          = NULL
  oApplication    = NULL
  
  *--- Abre Excel para que se pueda modificar el contenido de las celdas u obtener los valores que tienen las celdas
  FUNCTION AbrirExcel
    Local lcCarpetaActual, lcCarpetaExcel, lcNombreModelo, lcNombrePlanilla
    lcCarpetaActual = Sys(5) + CurDir()
    with This
      lcCarpetaExcel = Apostrofos(iif(Empty(.cCarpetaExcel), lcCarpetaActual + "EXCEL\", .cCarpetaExcel), 1)
      TRY
        if !Directory(Apostrofos(lcCarpetaExcel, 0))     && Si no existe la carpeta Excel se trata de crearla
          MD &lcCarpetaExcel
        endif
        .cCarpetaExcel = Apostrofos(lcCarpetaExcel, 0)
      CATCH
        .cMensajeError = "No pude acceder a la carpeta Excel"
      ENDTRY
      if Empty(.cMensajeError)     && Si está todo OK
        do case
          case Empty(.cNombreModelo) .and. Empty(.cNombrePlanilla)     && Si quiere crear una planilla totalmente en blanco
            TRY
              .oExcel = CreateObject("EXCEL.APPLICATION")
            CATCH
              .cMensajeError = "No pude crear la planilla Excel*Posiblemente EXCEL no está instalado"
            ENDTRY
          case !Empty(.cNombreModelo) .and. !Empty(.cNombrePlanilla)     && Si quiere crear una planilla basada en otra ya existente
            if File(Apostrofos(.cNombreModelo, 0))     && Si existe la planilla que servirá de modelo
              lcNombreModelo   = Apostrofos(.cNombreModelo, 1)
              lcNombrePlanilla = Apostrofos(lcCarpetaExcel + .cNombrePlanilla, 1)
              TRY
                COPY FILE &lcNombreModelo TO &lcNombrePlanilla
              CATCH
                .cMensajeError = "No pude copiar el modelo de planilla"
              ENDTRY
              if Empty(.cMensajeError)     && Si se pudo copiar la planilla
                lcNombrePlanilla = Apostrofos(lcNombrePlanilla, 0)
                TRY
                  .oExcel = GetObject(lcNombrePlanilla)
                CATCH
                  .cMensajeError = "No pude abrir la planilla Excel:*" + lcNombrePlanilla + "*Creación de la planilla Excel NO realizada"
                ENDTRY
              endif
            else
              .cMensajeError = "No existe la planilla:*" + .cNombreModelo
            endif  
          case Empty(.cNombreModelo) .and. !Empty(.cNombrePlanilla)     && Quiere abrir una planilla para lectura
            .cNombrePlanilla = Apostrofos(.cNombrePlanilla, 0)
            .cNombrePlanilla = JustFName(.cNombrePlanilla) + iif(Empty(JustExt(.cNombrePlanilla)), ".XLS", "")
            .cNombrePlanilla = iif(Empty(JustPath(.cNombrePlanilla)), Sys(5) + CurDir(), "") + .cNombrePlanilla
            TRY
              .oExcel = GetObject(Apostrofos(.cNombrePlanilla, 0))
            CATCH
              .cMensajeError = "No pude abrir la planilla Excel:*" + .cNombrePlanilla + "*Creación de la planilla Excel NO realizada"
            ENDTRY
          otherwise
            .cMensajeError = "Modo de apertura no contemplado"
        endcase
      endif
      if Empty(.cMensajeError)
        .oApplication = .oExcel.Application
        with .oApplication
          .Visible = .F.
          if Empty(This.cNombrePlanilla)     && Se quiere crear una planilla en blanco
            TRY
              .WorkBooks.Add()
            CATCH
              .cMensajeError = "No pude crear una nueva planilla Excel"
            ENDTRY
          else                                         && Se quiere abrir una planilla existente
            TRY
              This.cNombrePlanilla = iif(Empty(JustPath(This.cNombrePlanilla)), This.cCarpetaExcel, "") + This.cNombrePlanilla
              .WorkBooks.Open(This.cNombrePlanilla)
            CATCH
              .cMensajeError = "No pude abrir la planilla*" + This.cNombrePlanilla
            ENDTRY
          endif
        endwith
      endif
    endwith
  ENDFUNC
  
  *--- Cierra Excel, si hay una planilla aún no grabada entonces pedirá confirmación
  FUNCTION CerrarExcel
    This.oApplication.QUIT
  ENDFUNC
  
  *--- Convierte una columna de la forma A, B, C, D, etc. a la forma 1, 2, 3, 4, etc.
  HIDDEN FUNCTION ConvertirColumna
    LParameters tuColumna
    Local ln1, ln2, lnColumna
    do case
      case VarType(tuColumna) == "C"
        tuColumna = AllTrim(tuColumna)
        ln1 = (Asc(Left(tuColumna, 1)) - 64) * iif(Len(tuColumna) == 1, 1, 26)
        ln2 = iif(Len(tuColumna) == 2, Asc(Right(tuColumna, 1)) - 64, 0)
        lnColumna = ln1 + ln2
      case VarType(tuColumna) == "N"
        lnColumna = tuColumna
      otherwise
        This.cMensajeError = "El tipo de la columna debe ser 'C' o 'N'"
        lnColumna = 0
    endcase
    Return(lnColumna)
  ENDFUNC  
  
  *--- Extrae los distintos formatos
  HIDDEN FUNCTION ExtraerFormato
    LParameters tcFormato, tuVariable
    Local lnPosicion, lnI, lcX, luValor
    tcFormato = StrTran(tcFormato, " =", "=")
    tcFormato = StrTran(tcFormato, "= ", "=")
    tuVariable = tuVariable + iif(Right(tuVariable, 1) <> "=", "=", "")
    lnPosicion = At(tuVariable, tcFormato)
    luValor = ""
    if lnPosicion > 0
      lnI = lnPosicion + 3
      lcX = SubStr(tcFormato, lnI, 1)
      do while lcX <> ";" .and. lnI <= Len(tcFormato)
        luValor = luValor + lcX
        lnI = lnI + 1
        lcX = SubStr(tcFormato, lnI, 1)
      enddo
    endif
    if Left(luValor, 1) $ "0123456789"
      luValor = Val(luValor)
    else
      if luValor == ".F."
        luValor = .F.
      else
        if luValor == ".T."
          luValor = .T.
        endif
      endif
    endif
    Return(luValor)
  ENDFUNC
  
  *--- Graba la planilla
  FUNCTION Grabar
    with This
      .oApplication.ActiveWorkBook.Save()
    endwith
  ENDFUNC
  
  *--- Graba la planilla, con el mismo nombre que tenía o con otro nombre
  FUNCTION GrabarComo
    LParameters tcNombrePlanilla
    Local lcNombreGrabado
    with This
      if VarType(tcNombrePlanilla) == "C"     && Si se especificó un nuevo nombre para la planilla
        lcNombreGrabado = iif(Empty(JustPath(tcNombrePlanilla)), .cCarpetaExcel, "") + JustFName(tcNombrePlanilla) + iif(Empty(JustExt(tcNombrePlanilla)), ".XLS", "")
      else                                    && Si se quiere grabar con el nombre actual
        lcNombreGrabado = .cNombrePlanilla
      endif
      .oApplication.ActiveWorkBook.SaveAs(lcNombreGrabado)
    endwith
  ENDFUNC
  
  *--- Inserta datos en la celda especificada, opcionalmente con un formato
  FUNCTION InsertarDatos
    LParameters tnFila, tnColumna, tuValor, tcFormato
    Local lcCol, FN, FS, FB, HA, NF, CI, PA, BO, LS, CW
    if !Empty(This.cMensajeError)     && Si hay algún error, no debe continuar
      Return .F.
    endif
    with This.oApplication
      .Cells(tnFila, This.ConvertirColumna(tnColumna)).Value = tuValor
      if !Empty(tcFormato)
        *  Formato Celda
        *  -------------  
        *  FN = FontName (Arial, Courier New, etc.)
        *  FS = FontSize (8, 10, 12, etc.)
        *  FB = FontBold (.F., .T.)
        *  HA = HorizontalAlignment (1 = General, 2 = Izquierda, 3 = Centrar, 4 = Derecha)
        *  NF = NumberFormat ("#,##0.00_)", "m/d/yy", "$#,##0.00_);[Rojo]($#,##0.00)")
        *  CI = .Interior.ColorIndex (1=negro, 2=blanco, 3=rojo, etc.) 
        *  PA = .Interior.Pattern (1 = Sólido, ...)
        *  BO = Borders(1=izquierda
        *  LS = LineStyle(1=contínua, 2=rayada, 3=punteada, 4=punto y raya, 5=inclinada, etc.)
        *  CW = ColumnWidth
        FN = This.ExtraerFormato(tcFormato, "FN")     && FontName
        FS = This.ExtraerFormato(tcFormato, "FS")     && FontSize
        FB = This.ExtraerFormato(tcFormato, "FB")     && FontBold
        HA = This.ExtraerFormato(tcFormato, "HA")     && HorizontalAlignment
        NF = This.ExtraerFormato(tcFormato, "NF")     && NumberFormat
        CI = This.ExtraerFormato(tcFormato, "CI")     && ColorIndex
        PA = This.ExtraerFormato(tcFormato, "PA")     && Pattern
        BO = This.ExtraerFormato(tcFormato, "BO")     && Borders
        LS = This.ExtraerFormato(tcFormato, "LS")     && LineStyle
        CW = This.ExtraerFormato(tcFormato, "CW")     && ColumnWidth
        if !Empty(FN)
          .Cells(tnFila, This.ConvertirColumna(tnColumna)).Font.Name = FN
        endif
        if !Empty(FS)
          .Cells(tnFila, This.ConvertirColumna(tnColumna)).Font.Size = FS
        endif
        if !Empty(FB)
          .Cells(tnFila, This.ConvertirColumna(tnColumna)).Font.Bold = FB
        endif
        if !Empty(HA)
          .Cells(tnFila, This.ConvertirColumna(tnColumna)).HorizontalAlignment = HA
        endif
        if !Empty(NF)
          .Cells(tnFila, This.ConvertirColumna(tnColumna)).NumberFormat = NF
        endif
        if !Empty(CI)
          .Cells(tnFila, This.ConvertirColumna(tnColumna)).Interior.ColorIndex = CI
        endif
        if !Empty(PA)
          .Cells(tnFila, This.ConvertirColumna(tnColumna)).Interior.Pattern = PA
        endif
        if !Empty(BO)
          * BO = 1 (izquierda), 2 (derecha), 4 (arriba), 8 (abajo)
          * LS = 0 (ninguno), 1 (contínuo), 2 (rayada), 3 (punteada), 4 (punto y raya), 5 (inclinada 1), 6 (inclinada 2), 9 (doble raya)
          LS = iif(VarType(LS) == "N", LS, 1)
          LS = iif(LS <  1,  1, LS)
          LS = iif(LS > 10, 10, LS)
          BO = iif(BO > 15, BO % 15, BO)
          if BO >= 8
            .Cells(tnFila, This.ConvertirColumna(tnColumna)).Borders(4).LineStyle = LS
            BO = BO - 8
          endif
          if BO >= 4
            .Cells(tnFila, This.ConvertirColumna(tnColumna)).Borders(3).LineStyle = LS
            BO = BO - 4
          endif
          if BO >= 2
            .Cells(tnFila, This.ConvertirColumna(tnColumna)).Borders(2).LineStyle = LS
            BO = BO - 2
          endif
          if BO >= 1
            .Cells(tnFila, This.ConvertirColumna(tnColumna)).Borders(1).LineStyle = LS
            BO = BO - 1
          endif
        endif
        if VarType(CW) == "N"
          lcCol = CHR(This.ConvertirColumna(tnColumna) + 64) + ":" + CHR(This.ConvertirColumna(tnColumna) + 64)
          oSheet = .Parent.ActiveSheet
          oSheet.Columns(lcCol).ColumnWidth = CW
        endif
      endif
    endwith
  ENDFUNC
  
  *--- Hace la planilla visible para el usuario
  FUNCTION MostrarPlanilla
    if !Empty(This.cMensajeError)     && Si hay algún error, no debe continuar
      Return .F.
    endif
    with This
      .oApplication.Visible = .T.
      .oExcel = NULL
    endwith
  ENDFUNC
  
  *--- Obtiene el valor que hay en una celda
  FUNCTION ObtenerValor
    LParameters tnFila, tnColumna
    Local luValor
    tnFila    = iif(VarType(tnFila) == "N", tnFila, 0)
    tnColumna = iif(VarType(This.ConvertirColumna(tnColumna)) == "N", This.ConvertirColumna(tnColumna), 0)
    TRY
      luValor = This.oApplication.Cells(tnFila, This.ConvertirColumna(tnColumna)).Value
    CATCH
      This.cMensajeError = "No pude obtener el valor de la celda*Fila=" + Transform(tnFila) + ", Columna=" + Transform(tnColumna)
      luValor = ""
    ENDTRY
    Return(luValor)
  ENDFUNC
  
ENDDEFINE
*
*
