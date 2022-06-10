      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VSFACTU.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-PRODUCTS.
           05 WS-PRODUCTO-1         PIC A(10).
           05 WS-PRODUCTO-2         PIC A(10).
           05 WS-PRODUCTO-3         PIC A(10).
           05 WS-PRODUCTO-4         PIC A(10).
           05 WS-PRODUCTO-5         PIC A(10).
           05 WS-PRODUCTO-6         PIC A(10).
           05 WS-PRODUCTO-7         PIC A(10).
       01  ENTRADA-VALOR.
           05 WS-VALOR-1               PIC 9(5)V99.
           05 WS-VALOR-2               PIC 9(5)V99.
           05 WS-VALOR-3               PIC 9(5)V99.
           05 WS-VALOR-4               PIC 9(5)V99.
           05 WS-VALOR-5               PIC 9(5)V99.
           05 WS-VALOR-6               PIC 9(5)V99.
           05 WS-VALOR-7               PIC 9(5)V99.
           05 WS-CANTIDAD.
               10 WS-CANTIDAD-1        PIC 9(5).
               10 WS-CANTIDAD-2        PIC 9(5).
               10 WS-CANTIDAD-3        PIC 9(5).
               10 WS-CANTIDAD-4        PIC 9(5).
               10 WS-CANTIDAD-5        PIC 9(5).
               10 WS-CANTIDAD-6        PIC 9(5).
               10 WS-CANTIDAD-7        PIC 9(5).
               10 WS-TOTAL.
                   15 WS-TOTAL-1       PIC 9(5)V99.
                   15 WS-TOTAL-2       PIC 9(5)V99.
                   15 WS-TOTAL-3       PIC 9(5)V99.
                   15 WS-TOTAL-4       PIC 9(5)V99.
                   15 WS-TOTAL-5       PIC 9(5)V99.
                   15 WS-TOTAL-6       PIC 9(5)V99.
                   15 WS-TOTAL-7       PIC 9(5)V99.


       01  WS-PRE-ENTRADA           PIC 9.

       PROCEDURE DIVISION.
       00001MAIN-PROCEDURE.
           PERFORM 1000-CABEZERA-1 UNTIL WS-PRE-ENTRADA=3.
       1000-CABEZERA-1.
               DISPLAY "1.USAR LA CALCULADORA".
               DISPLAY "2.DATOS DEL CLIENTE".
               DISPLAY "3.SALIR".

               ACCEPT WS-PRE-ENTRADA.
               IF WS-PRE-ENTRADA = 1
                   PERFORM 2000-OBTENER-DATOS
                   PERFORM 2010-ACCEPT-PRODUCTO-1
                   PERFORM 2015-PROCESO-PRODUCTO-1


               ELSE
               IF WS-PRE-ENTRADA =2

                    DISPLAY "HOLA"

               ELSE

               IF WS-PRE-ENTRADA = 3
                    DISPLAY "SALIDA".
                   PERFORM 1000-GENERAL.
       1000-FINAL-CABEZERA-1.

       1010-DISPLAY-PRODUCTO SECTION.
               DISPLAY "INGRESE EL NOMBRE DEL PRODUCTO".
       1010-FIN-DISPLAY-PRODUCTO SECTION.

       1020-DISPLAY-PRECIO-PRODUCTO SECTION.
               DISPLAY "PRECIO DEL PRODCUTO".
       1020-FINAL-DISP-PRECIO-PRODCUTO SECTION.

       1030-CANTIDAD-PRODUCTOS SECTION.
               DISPLAY "CANTIDAD: ".

       1030-FINAL-CANTIDAD-PRODUCTO SECTION.

       2000-OBTENER-DATOS SECTION.


      *    CABEZERA
           DISPLAY "*******************************************"
           DISPLAY "FACTURA DE 7 PRODCTOS"
           DISPLAY "1.INGRESE PRODUCTOS"
           DISPLAY "*******************************************".
      *        CABEZERA
       2010-ACCEPT-PRODUCTO-1 SECTION.
           PERFORM 1010-DISPLAY-PRODUCTO.
           ACCEPT WS-PRODUCTO-1.
           PERFORM 1020-DISPLAY-PRECIO-PRODUCTO.
           ACCEPT WS-VALOR-1
           PERFORM 1030-CANTIDAD-PRODUCTOS.
           ACCEPT WS-CANTIDAD-1.
       2015-PROCESO-PRODUCTO-1 SECTION.
           COMPUTE WS-TOTAL-1 = WS-VALOR-1 * WS-CANTIDAD-1
           DISPLAY WS-TOTAL-1.

       2015-FINAL-PROCESO-PRODUCTO-1 SECTION.
       2010-FINAL-ACCEPT-PRODUCTO-1 SECTION.




       2000-FIN-OBTENER-DATOS SECTION.





















       1000-GENERAL SECTION.
            STOP RUN.
       1000-FIANL-GENERAL SECTION.
           EXIT.

       0000FINAL-MAIN-PROCEDURE.
           EXIT.

       END PROGRAM VSFACTU.
