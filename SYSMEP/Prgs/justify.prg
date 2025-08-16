**************************************************

*-- Class:        justify

*-- ParentClass:  custom

*-- BaseClass:    custom

*-- Author: Oscar Gonzalez Hernandez

*

DEFINE CLASS justify AS CUSTOM

  HEIGHT = 15

  WIDTH = 16

  *-- Contiene el texto ya justificado y se va acumulando

  *-- hasta contener todo el texto justificado.

  ctextoacumulado = ""

  NAME = "justify"

  *-- Contiene el nombre del archivo que se

  *-- creará con el texto justificado.

  csalidatexto = .F.

  *-- Separa el texto pasado como parámetro en diferentes

  *-- bloques si ses que se encuentran retornos de carro,

  *-- sino regresa el texto completo.

  PROCEDURE SEPARA

    LPARAMETERS tcParrafo,taBloques,tbBloques

    lnDimension = 0

    FOR i = 1 TO LEN(tcParrafo)

      IF ASC(SUBSTR(tcParrafo,i,1)) = 13

        lnDimension = lnDimension + 1

        DIMENSION laRetornos(lnDimension)

        laRetornos(lnDimension) = i

      ENDIF

    ENDFOR

    IF VARTYPE(laRetornos) = "U"

      tbBloques = .F.

      taBloques = tcParrafo

      RETURN @taBloques

    ENDIF

    lnInicio = 1

    FOR k = 1 TO ALEN(laRetornos)

      DIMENSION taBloques(k)

      taBloques(k) = SUBSTR(tcParrafo,lnInicio,laRetornos(k)-lnInicio)

      lnInicio = laRetornos(k)+1

    ENDFOR

    DIMENSION taBloques(k)

    taBloques(k) = SUBSTR(tcParrafo,lnInicio,LEN(tcParrafo)lnInicio)

    tbBloques = .T.

    RETURN @taBloques

  ENDPROC

  *-- Ejecuta el proceso de justificado del texto separado

  *-- en bloques o texto único regresado por el método separa().

  PROCEDURE dojustify

    LPARAMETERS tcTextoJustificar,tnLongJustificado

    LOCAL lgBloques,lbBloques

    THIS.SEPARA(tcTextoJustificar,@lgBloques,@lbBloques)

    IF lbBloques && Se ha partido el exto en bloques

      FOR lnCont = 1 TO ALEN(lgBloques)

        THIS.ctextoacumulado = ""

        THIS.Justificar(lgBloques(lnCont),tnLongJustificado)

         TEXT

           <<This.ctextoacumulado>>

         ENDTEXT

      ENDFOR

    ELSE

      THIS.ctextoacumulado = ""

      THIS.Justificar(lgBloques,tnLongJustificado)

      TEXT

        <<This.ctextoacumulado>>

      ENDTEXT

    ENDIF

    RETURN

  ENDPROC

  *-- Método recursivo que justifica párrafo por párrafo

  *-- según la longitud de caracteres que se le indique.

  PROCEDURE justificar

    LPARAMETERS tcTexto,tnTamaño

    IF EMPTY(SUBSTR(tcTexto,tnTamaño,1))

      lcTextoJ = ALLTRIM(SUBSTR(tcTexto,1,tnTamaño-1))

      IF EMPTY(lcTextoJ)

        RETURN

      ENDIF

      tcTextoAlterno = SUBSTR(tcTexto,tnTamaño+1,LEN(tcTexto))

      IF !EMPTY(tcTextoAlterno)

        lcTextoAcumular = THIS.rellena(lcTextoJ,tnTamaño)

        THIS.ctextoacumulado = THIS.ctextoacumulado + lcTextoAcumular + CHR(13)

        tcTexto = SUBSTR(tcTexto,tnTamaño+1,LEN(tcTexto))

        THIS.justificar(tcTexto,tnTamaño)

      ELSE

        lcTextoAcumular = lcTextoJ

        THIS.ctextoacumulado = THIS.ctextoacumulado + lcTextoAcumular + CHR(13)

        tcTexto = SUBSTR(tcTexto,tnTamaño+1,LEN(tcTexto))

        THIS.justificar(tcTexto,tnTamaño)

      ENDIF

    ELSE

      lcChar = SUBSTR(tcTexto,tnTamaño,1)

      lnContador = tnTamaño

      DO WHILE !EMPTY(lcChar)

        lnContador = lnContador - 1

        lcChar = SUBSTR(tcTexto,lnContador,1)

      ENDDO

      lcTextoJ = ALLTRIM(SUBSTR(tcTexto,1,lnContador))

      tcTextoAlterno = SUBSTR(tcTexto,tnTamaño+1,LEN(tcTexto))

      IF !EMPTY(tcTextoAlterno)

        lcTextoAcumular = THIS.rellena(lcTextoJ,tnTamaño)

        THIS.ctextoacumulado = THIS.ctextoacumulado  + lcTextoAcumular + CHR(13)

        tcTexto = SUBSTR(tcTexto,lnContador+1,LEN(tcTexto))

        THIS.justificar(tcTexto,tnTamaño)

      ELSE

        lcTextoAcumular = lcTextoJ

        THIS.ctextoacumulado = THIS.ctextoacumulado + lcTextoAcumular + CHR(13)

        tcTexto = SUBSTR(tcTexto,lnContador+1,LEN(tcTexto))

        THIS.justificar(tcTexto,tnTamaño)

      ENDIF

    ENDIF

  ENDPROC

  *-- Rellena el párrafo cortado por el método justificar()

  *-- con el número de espacios correspondientes para crear

  *-- un parrafo de la longitud deseada.

  PROCEDURE rellena

    LPARAMETERS tcParrafo,tnLong

    IF LEN(tcParrafo) = tnLong

      RETURN tcParrafo

    ENDIF

    lnDimension = 0

    lcPalabra = ""

    FOR i = 1 TO LEN(tcParrafo)

      IF ASC(SUBSTR(tcParrafo,i,1)) = 32

        lnDimension = lnDimension + 1

        DIMENSION laEspacios(lnDimension)

        laEspacios(lnDimension) = i

      ENDIF

    ENDFOR

    IF VARTYPE(laEspacios) = "U"

      RETURN tcParrafo

    ENDIF

    lnInicio = 1

    FOR k = 1 TO ALEN(laEspacios)

      DIMENSION laPalabras(k)

      laPalabras(k) = SUBSTR(tcParrafo,lnInicio,laEspacios(k)-lnInicio)

      lnInicio = laEspacios(k)+1

    ENDFOR

    DIMENSION laPalabras(k)

    laPalabras(k) = SUBSTR(tcParrafo,lnInicio,LEN(tcParrafo)lnInicio)

    lnTotalEspacios = tnLong - LEN(tcParrafo)

    lnEspaciosContados = 0

    DO WHILE !EMPTY(lnTotalEspacios)

      FOR l = 1 TO ALEN(laPalabras)-1

        laPalabras(l) = laPalabras(l) + " "

        lnTotalEspacios = lnTotalEspacios - 1

        IF EMPTY(lnTotalEspacios)

          EXIT

        ENDIF

      ENDFOR

    ENDDO

    lcParrafoFormateado = ""

    lcParrafoFormateado = laPalabras(1)

    FOR j = 2 TO ALEN(laPalabras)

      lcParrafoFormateado = lcParrafoFormateado + " " + laPalabras(j)

    ENDFOR

    RETURN lcParrafoFormateado

  ENDPROC

  *-- Activa la configuración de salida del texto justificado.

  PROCEDURE set_textmerge_on

    SET TEXTMERGE TO (THIS.csalidatexto) NOSHOW

    SET TEXTMERGE ON

  ENDPROC

  *-- Desactiva la configuración de salida del texto justificado.

  PROCEDURE set_textmerge_off

    SET TEXTMERGE TO

    SET TEXTMERGE OFF

  ENDPROC

ENDDEFINE