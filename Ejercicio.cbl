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
       01  WS-CLIENTE.
           05 WS-DATOS.
               10 WS-CLIENTE-NAME         PIC A(50).
               10 WS-CLIENTE-LASTNAME     PIC A(50).
               10 WS-CLIENTE-CODE.
                   15 WS-CLIENTE-ADDRES   PIC A(100).
                   15 WS-CLIENTE-STREET-1 PIC X(50).
                   15 WS-CLIENTE-STREET-2 PIC X(50).



       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            MOVE "ANTHONY" TO WS-CLIENTE-NAME.
            MOVE "TRIGUERO" TO WS-CLIENTE-LASTNAME.
            MOVE "0446" TO WS-CLIENTE-CODE.
            MOVE "CDLA MARTHA ROLDOS" TO WS-CLIENTE-ADDRES.
            MOVE "VILLA 1" TO WS-CLIENTE-STREET-1.
            MOVE "VILLA 1" TO WS-CLIENTE-STREET-2.

            DISPLAY "NOMBRES Y A PELLEDOS DEL CLIENTE: ".
            DISPLAY WS-CLIENTE-NAME ' ' WS-CLIENTE-LASTNAME.
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
