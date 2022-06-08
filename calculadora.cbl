      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-NUM-1 PIC S9(5)V99.
       01  WS-NUM-2 PIC S9(5)V9(2).
       01  WS-RESULT PIC S9(11)V99.
       01  WS-PRE-ENTRADA PIC 9(1).
       01  WS-ENTRADA PIC A(7).
       01  WS-FORMATO PIC -9999999999.99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM CALCULADORA UNTIL WS-PRE-ENTRADA=2.


           STOP RUN.


           CALCULADORA.
               DISPLAY "1.USAR LA CALCULADORA".
               DISPLAY "2.SALIR".
               ACCEPT WS-PRE-ENTRADA.
               IF WS-PRE-ENTRADA = 1
                   PERFORM OBTENER-DATOS
                   PERFORM PROCEDIMIENTO
               ELSE
               IF WS-PRE-ENTRADA NOT =2
                    DISPLAY "VACIO"

               ELSE
                   DISPLAY "ALGO SALIO MAL".


           OBTENER-DATOS.
      *    CABEZERA
               DISPLAY "*******************************************"
               DISPLAY "SELECCIONE UN NUMERO"
               DISPLAY "1.SUMA"
               DISPLAY "2.RESTA"
               DISPLAY "3.MULTIPLICACION"
               DISPLAY "4.DIVICION"
               DISPLAY "*******************************************"
      *        CABEZERA
               DISPLAY "INGRESE UN NUMERO"
               ACCEPT WS-NUM-1
               DISPLAY "INGRESE OTRO NUMERO"
               ACCEPT WS-NUM-2

               DISPLAY "INDIQUE LA OPERACION"
               ACCEPT WS-ENTRADA.


           PROCEDIMIENTO.
           IF WS-ENTRADA = "SUMA"
                DISPLAY "HAZ SELECCIONADO SUMA"
                ADD WS-NUM-1 TO WS-NUM-2 GIVING WS-RESULT
                MOVE WS-RESULT TO WS-FORMATO
                DISPLAY "EL RESULTADO ES:  " WS-FORMATO
           ELSE
           IF WS-ENTRADA = "RESTA"
                DISPLAY "HAZ SELECCIONADO RESTA"
                SUBTRACT WS-NUM-1 FROM WS-NUM-2 GIVING WS-RESULT
                MOVE WS-RESULT TO WS-FORMATO
                DISPLAY "EL RESULTADO ES:  " WS-FORMATO
           ELSE
           IF WS-ENTRADA = "MULTIPLICACION"
                DISPLAY "HAZ SELECCIONADO MULTIPLICACION"
                MULTIPLY WS-NUM-1 BY WS-NUM-2 GIVING WS-RESULT
                MOVE WS-RESULT TO WS-FORMATO
                DISPLAY "EL RESULTADO ES:  " WS-FORMATO
           ELSE
           IF WS-ENTRADA = "DIVICION"
                DISPLAY "HAZ SELECCIONADO DIVICION"
                DIVIDE WS-NUM-1 INTO WS-NUM-2 GIVING WS-RESULT
                MOVE WS-RESULT TO WS-FORMATO
                DISPLAY "EL RESULTADO ES:  " WS-FORMATO
           ELSE
               DISPLAY "¡¡OOPS!! ALGO SALIO MAL ".


       END PROGRAM YOUR-PROGRAM-NAME.
