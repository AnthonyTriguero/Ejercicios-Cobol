      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIVALIDE.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  WS-CEDEULA PIC X(10).
       01  WS-CEDULA-REDIFINE REDEFINES WS-CEDEULA.
           05 WS-9-PRIMEROS-DIGI PIC 9 OCCURS 10 TIMES INDEXED BY S-1.
.
       01  WS-ULTIMO-DIGTO-CI PIC 9.


       01  WS-VALOR-2-1 PIC 9(9) VALUE 212121212.
       01  WS-CONTEIN-2 REDEFINES WS-VALOR-2-1.
           05 WS-LISTA-1 PIC 9 OCCURS 9 TIMES INDEXED BY S-2.


       01  WS-PRE-RESULTADO PIC 9(2).


       01  WS-SUMA PIC 9(2).
       01  WS-SUMA-POSESO REDEFINES WS-SUMA.
           02 WS-SUMA-1 PIC 9.
           02 WS-SUMA-2 PIC 9.

       01  WS-RECEPTOR-SUMA.
           02 WS-VALOR-1 PIC 9.
           02 WS-VALOR-2 PIC 9.

       01  WS-RESTA PIC 99.

       01  WS-CONS PIC 99 VALUE 10.
       01  WS-CONS-2 PIC 9 VALUE 1.

       PROCEDURE DIVISION.



       MAIN-PROCEDURE.

           DISPLAY "INGRESE LA CEDULA".

           ACCEPT WS-CEDEULA.
           SET  S-2 TO WS-CONS-2.
           PERFORM 3000-ULTIMO-ENTERO VARYING S-1 FROM WS-CONS-2
           BY 9 UNTIL
           S-1 = WS-CONS.
           PERFORM 3000-ULTIMO-ENTERO


      *    1234567897


           PERFORM 1000-MULTIPLICADOR VARYING S-2 FROM WS-CONS-2
           BY WS-CONS-2 UNTIL
               S-2 >= WS-CONS.

           PERFORM 6000-VALIDAR.




           STOP RUN.

       1000-MULTIPLICADOR.
           COMPUTE WS-PRE-RESULTADO = WS-LISTA-1(S-2) *
           WS-9-PRIMEROS-DIGI(S-2).
           PERFORM 2000-SUMA.

       2000-SUMA.
           IF WS-PRE-RESULTADO >= WS-CONS
              MOVE WS-PRE-RESULTADO TO WS-RECEPTOR-SUMA
              COMPUTE WS-SUMA = WS-SUMA + WS-VALOR-1
              + WS-VALOR-2

           ELSE
              COMPUTE WS-SUMA = WS-SUMA + WS-PRE-RESULTADO



           END-IF.
       3000-ULTIMO-ENTERO.

           MOVE WS-9-PRIMEROS-DIGI(S-1) TO WS-ULTIMO-DIGTO-CI.



       6000-VALIDAR.

           EVALUATE TRUE
               WHEN WS-SUMA-2 >= WS-CONS-2
               DISPLAY WS-SUMA-2
                   COMPUTE WS-RESTA = (WS-SUMA-1 + WS-CONS-2)*WS-CONS
                   - WS-SUMA
                   IF WS-RESTA = WS-ULTIMO-DIGTO-CI
                      DISPLAY "EL CODIGO INGRESADO ES CORRECTO"
                      DISPLAY WS-SUMA
                      DISPLAY "EL CODIGO IDENTIFICADOR: " WS-RESTA
                   ELSE IF WS-RESTA  = WS-CONS AND
                       WS-ULTIMO-DIGTO-CI = 0
                       DISPLAY "EL CODIGO CALCULADO ES CORECTO: "
                       DISPLAY "EL CODIGO IDENTIFICADOR: " WS-RESTA
                   ELSE
                       DISPLAY "CEDULA INVALIDA"
                       DISPLAY "EL CODIGO IDENTIFICADOR: " WS-RESTA
                       DISPLAY "CODIGO INGRESADO: " WS-ULTIMO-DIGTO-CI
                   END-IF
                WHEN WS-SUMA-2 = 0
                DISPLAY WS-SUMA-2
                DISPLAY "ERROR"
                DISPLAY WS-SUMA-2
           END-EVALUATE.

       END PROGRAM CIVALIDE.
