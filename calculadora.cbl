         ******************************************************************
      * Author:ANTHONY
      * Date:6/8/22
      * Purpose:CALCULADORA
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULADORA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *VARIABLES CALCULADORA
      *-----------------------------------
       01  WS-NUM-1                        PIC S9(5)V99.
       01  WS-NUM-2                        PIC S9(5)V9(2).
       01  WS-RESULT                       PIC S9(11)V99.
       01  WS-PRE-ENTRADA                  PIC 9(1).
       01  WS-ENTRADA                      PIC 9(1).
       01  WS-FORMATO                      PIC -99999.99.
      *-------------------------------------
      *DATOS DE CLIENTE
      *-------------------------------------
        01  WS-CLIENTE.
           05 WS-DATOS.
               10 WS-CLIENTE-NAME         PIC A(10).
               10 WS-CLIENTE-LASTNAME     PIC A(10).
               10 WS-CLIENTE-CODE         PIC ZZZZZ.ZZ.
               10 WS-CLIENTE-CELL.
                   15 WS-CLIENTE-BALANCE  PIC S9(11)V99 COMP-3.
                   15 WS-CLIENTE-ADDRES   PIC A(10).
                   15 WS-CLIENTE-STREET-1 PIC X(10).
                   15 WS-CLIENTE-STREET-2 PIC X(10).
      *------------------------------------- 
       PROCEDURE DIVISION.
       0000-MAIN-PROCEDURE SECTION.

           PERFORM 1000-CALCULADORA UNTIL WS-PRE-ENTRADA=3.
               PERFORM 4001-SALIR.


           


       1000-CALCULADORA SECTION.
           
               DISPLAY "1.USAR LA CALCULADORA".
               DISPLAY "2.DATOS DEL CLIENTE".
               DISPLAY "3.SALIR".
              
               ACCEPT WS-PRE-ENTRADA.
               IF WS-PRE-ENTRADA = 1
                   PERFORM 2000-OBTENER-DATOS
                   PERFORM 2000-PROCEDIMIENTO
                   
               ELSE
               IF WS-PRE-ENTRADA =2
                    
                    PERFORM 3000-DATOS-CLIENTE

               ELSE
                  
               IF WS-PRE-ENTRADA NOT =3
                    DISPLAY "SALIDA".
                    
                   
       1000-FIN-CALCULADORA SECTION.
       
       2000-OBTENER-DATOS SECTION.          
           
           
      *    CABEZERA
               DISPLAY "*******************************************"
               DISPLAY "SELECCIONE UN NUMERO PARA LA OPERACIÓN"
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
       1000-FIN-OBTENER-DATOS SECTION.
       
       2000-PROCEDIMIENTO SECTION.

           PROCEDIMIENTO.
           IF WS-ENTRADA = 1
               
                DISPLAY "HAZ SELECCIONADO SUMA"
                ADD WS-NUM-1 TO WS-NUM-2 GIVING WS-RESULT
                MOVE WS-RESULT TO WS-FORMATO
                DISPLAY "EL RESULTADO ES:  " WS-FORMATO
           ELSE
           IF WS-ENTRADA = 2
                DISPLAY "HAZ SELECCIONADO RESTA"
                SUBTRACT WS-NUM-1 FROM WS-NUM-2 GIVING WS-RESULT
                MOVE WS-RESULT TO WS-FORMATO
                DISPLAY "EL RESULTADO ES:  " WS-FORMATO
           ELSE
           IF WS-ENTRADA = 3
                DISPLAY "HAZ SELECCIONADO MULTIPLICACION"
                MULTIPLY WS-NUM-1 BY WS-NUM-2 GIVING WS-RESULT
                MOVE WS-RESULT TO WS-FORMATO
                DISPLAY "EL RESULTADO ES:  " WS-FORMATO
           ELSE
           IF WS-ENTRADA = 4
                DISPLAY "HAZ SELECCIONADO DIVICION"
                DIVIDE WS-NUM-1 INTO WS-NUM-2 GIVING WS-RESULT
                MOVE WS-RESULT TO WS-FORMATO
                DISPLAY "EL RESULTADO ES:  " WS-FORMATO
           ELSE
               DISPLAY "¡¡OOPS!! ALGO SALIO MAL ".
       2000-FIN-PROCEDIMIENTO SECTION.
       
       3000-DATOS-CLIENTE SECTION.
            
            MOVE "ANTHONY" TO WS-CLIENTE-NAME.
            MOVE "TRIGUERO" TO WS-CLIENTE-LASTNAME.
            MOVE "......." TO WS-CLIENTE-ADDRES.
            MOVE "VILLA 1" TO WS-CLIENTE-STREET-1.
            MOVE "VILLA 1" TO WS-CLIENTE-STREET-2.

            
            

            DISPLAY "INGRESE EL CODIGO: ".
            ACCEPT WS-CLIENTE-CODE
            DISPLAY "INGRESE EL BALANCE: ".
            ACCEPT WS-CLIENTE-BALANCE.
           
            DISPLAY "*******************************************".
            DISPLAY "NOMBRE Y APELLEDO DEL CLIENTE: ".
            DISPLAY WS-CLIENTE-NAME ' ' WS-CLIENTE-LASTNAME.
            DISPLAY "DIRECCION: " WS-CLIENTE-ADDRES "/" 
               WS-CLIENTE-STREET-1 "/" WS-CLIENTE-STREET-2.
            
            DISPLAY "EL CODIGO ES: " WS-CLIENTE-CODE.
            DISPLAY "EL BALANCE SELECIONADO ES: " WS-CLIENTE-BALANCE.
            DISPLAY "*******************************************".
            
       
       3000-FIN-DATOS-CLIENTE SECTION.
           
            
           
            
            
       4000-GENERAL SECTION.
       4001-SALIR.
               STOP RUN.
       
       4000-FIN-GENERAL SECTION.
               
           
       0000-FIN-MAIN-PROCEDURE SECTION.
       
       END PROGRAM CALCULADORA.

       
