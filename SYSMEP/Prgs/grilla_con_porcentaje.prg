oForm = CREATEOBJECT("myForm")
oForm.SHOW(1)

DEFINE CLASS myForm AS FORM
  AUTOCENTER = .T.
  CAPTION = "Ejemplo de Grid con barras de porcentajes"
  ADD OBJECT myGrid AS GRID

  PROCEDURE LOAD
    CREATE CURSOR myProcess (percent i, NAME c(10))
    RAND(-1)
    FOR ix = 1 TO 100
      INSERT INTO myProcess VALUES (INT(RAND()*100), SYS(2015))
    ENDFOR
    LOCATE
  ENDPROC

  PROCEDURE INIT
    WITH THIS.myGrid
      .ADDCOLUMN(.COLUMNCOUNT+1)
      .COLUMNS(.COLUMNCOUNT).CONTROLSOURCE = "myProcess.Percent"
      .COLUMNS(.COLUMNCOUNT).DYNAMICFONTBOLD = "!(thisform.UpdateContainer(this.columns(1).myPercent))"
      .COLUMNS(.COLUMNCOUNT).Header1.CAPTION = "Porcentaje"

      .WIDTH = THISFORM.WIDTH
      .HEIGHT = THISFORM.HEIGHT
      .ANCHOR = 15
    ENDWITH

    WITH THIS.myGrid.COLUMNS(1)
      .ADDOBJECT("myPercent","myContainer")
      .myPercent.WIDTH = .WIDTH
      .myPercent.lblPercent.WIDTH = .WIDTH
      .myPercent.VISIBLE = .T.
      .DYNAMICFONTBOLD = "Thisform.UpdateContainer(this.columns(1).myPercent)"
      .CURRENTCONTROL = "myPercent"
      .SPARSE = .F.
    ENDWITH

    THIS.myGrid.COLUMNS(1).ENABLED = .F.
  ENDPROC

  PROCEDURE UpdateContainer(toContainer)
    WITH toContainer
      .shpPercent.BACKCOLOR = IIF(percent > 90, 0xFF, IIF(percent > 70, 0x00FFFF, 0x00FF00))
      .shpPercent.WIDTH = toContainer.PARENT.WIDTH * percent/100
      .lblPercent.CAPTION = TRANSFORM(percent) + "%"
    ENDWITH
  ENDPROC
ENDDEFINE

DEFINE CLASS myContainer AS CONTAINER
  BACKCOLOR = 0xFFFFFF
  BORDERWIDTH = 0
  ADD OBJECT shpPercent AS SHAPE WITH BORDERSTYLE = 0
  ADD OBJECT lblPercent AS LABEL WITH ALIGNMENT = 2 ,BACKSTYLE=0
ENDDEFINE