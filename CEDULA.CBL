     ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EVALU2.
       AUTHOR.  ANTHONY.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
      *     VARIABLES FECHA
      *---------------------------------------------------------------
           01  FECHA-DATOS.
           02  FECHA-DATE.
               10  FECHA-ANIO         PIC 9(04).
               10  FECHA-MES          PIC 9(02).
               10  FECHA-DIA          PIC 9(02).

      *     ALUMNO VARIABLES
      * --------------------------------------------------------------
       01  WS-ALUMNOS.
           05 WS-DATOS-ALUM       OCCURS 20 TIMES INDEXED BY ALUM.
               10 WS-NOMBRE           PIC X(40).
               10 WS-CEDULA           PIC X(10).
               10 WS-FECHA-NACIMIENTO.
                   15 WS-DIA          PIC 99.
                   15 WS-MES          PIC 9(2).
                   15 WS-ANIO         PIC 9(4).
               10 WS-NOTAS-1          PIC 99V99.
               10 WS-NOTAS-2          PIC 99V99.
               10 WS-NOTAS-3          PIC 99V99.
               10 WS-PROMEDIO         PIC 9(2)V99.
       01  WS-ALUMNO-CONT             PIC 9(02) VALUE 0.
       01  WS-CONT-2                  PIC 9(02) VALUE 0.
       01  WS-BUSC-ALUMN              PIC X(40).
      * --------------------------------------------------------------
       01  WS-CONS-3                  PIC 9     VALUE 3.
      *---------------------------------------------------------------- 
      *    MESES-CONTADOR
       01  WS-MESES.
           05  FILLER              PIC X(10)     VALUE 'ENERO'.
           05  FILLER              PIC X(10)     VALUE 'FEBRERO'.
           05  FILLER              PIC X(10)     VALUE 'MARZO'.
           05  FILLER              PIC X(10)     VALUE 'ABRIL'.
           05  FILLER              PIC X(10)     VALUE 'MAYO'.
           05  FILLER              PIC X(10)     VALUE 'JUNIO'.
           05  FILLER              PIC X(10)     VALUE 'JULIO'.
           05  FILLER              PIC X(10)     VALUE 'AGOSTO'.
           05  FILLER              PIC X(10)     VALUE'SEPTIEMBRE'.
           05  FILLER              PIC X(10)     VALUE 'OCTUBRE'.
           05  FILLER              PIC X(10)     VALUE 'NOVIEMBRE'.
           05  FILLER              PIC X(10)     VALUE 'DICIEMBRE'.
       01  FILLER REDEFINES WS-MESES.
           05 WS-STR-MESES    OCCURS 12 TIMES.
              10 WS-STR-MES        PIC X(10).
      *    RECEPTOP DE MES          ------------        
       01   WS-FECHA-MES-ONLY.
           05  WS-MES-OBTEN         PIC X(10).
      *---------------------------------------------------------------
      *    CEDULA VARIABLES VALIDE 
       01  WS-CEDEULA-ACEEPT PIC X(10).
       01  WS-CEDULA-REDIFINE REDEFINES WS-CEDEULA-ACEEPT.
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
       
      *-----------------------
       PROCEDURE DIVISION.
           PERFORM 0000-MAIN-PROCEDURE.
           PERFORM 1000-INICIALIZAR.
           PERFORM 2000-INGRESO-DATOS.
           PERFORM 7000-VALIDAR-CEDULA.
           PERFORM 3000-CALCULOS-NOTAS.
           PERFORM 4000-BUSCAR-ALUMNO.
           PERFORM 9999-GENERAL.



       0000-MAIN-PROCEDURE SECTION.

           DISPLAY "INGRESE EL NUMERO DE ALUMNOS A INGRESAR".
           ACCEPT WS-ALUMNO-CONT.
           IF WS-ALUMNO-CONT <= 0
               DISPLAY "ERROR DATOS NO PERMIDOS"
               PERFORM 9999-GENERAL
               
           ELSE IF WS-ALUMNO-CONT > 20
               DISPLAY "ERROR DATOS NO PERMIDOS"
               PERFORM 9999-GENERAL.

       1000-INICIALIZAR SECTION.
           SET ALUM TO 1
           INITIALIZE WS-ALUMNOS
           PERFORM 2000-INGRESO-DATOS VARYING ALUM FROM 1
           BY 1 UNTIL ALUM >= WS-ALUMNO-CONT.
           

       1000-IND-INICIALIZAR SECTION.




       2000-INGRESO-DATOS SECTION.

            MOVE FUNCTION CURRENT-DATE TO FECHA-DATOS


               DISPLAY "INGRESO DE DATOS"
               DISPLAY "INGRESE NOMBRE"
               ACCEPT WS-NOMBRE(ALUM)
               IF WS-NOMBRE(ALUM) NOT> SPACE
                  DISPLAY "ERROR EN LOS ESPACIOS"
                  PERFORM 9999-GENERAL
               ELSE
               DISPLAY "INGRESO DE CEDULA"
               ACCEPT WS-CEDULA(ALUM)

               
               DISPLAY "INGRESE LA FECHA NACIMIENTO"
               DISPLAY "DIA"
               ACCEPT WS-DIA(ALUM)
               IF WS-DIA(ALUM) >=29
                   DISPLAY "FECHA NO VALIDA"
                   PERFORM 9999-GENERAL
               ELSE
               DISPLAY "MES"
               ACCEPT WS-MES(ALUM)
               IF WS-MES(ALUM) >12
                   DISPLAY "NO VALIDO"
                   PERFORM 9999-GENERAL
               ELSE     
               DISPLAY "ANIO"
               ACCEPT WS-ANIO(ALUM)
               DISPLAY "INGRESE LAS NOTAS"
               DISPLAY "PRIMERA NOTA"
               ACCEPT WS-NOTAS-1(ALUM)
               IF WS-NOTAS-1(ALUM) >=11
                   DISPLAY "NO VALIDO"
                   PERFORM 9999-GENERAL
               ELSE
               DISPLAY "SEGUNDA NOTA"
               ACCEPT WS-NOTAS-2(ALUM)
               IF WS-NOTAS-2(ALUM) >=11
                   DISPLAY "NO VALIDO"
                   PERFORM 9999-GENERAL
               ELSE
               DISPLAY "TERCERA NOTA"
               ACCEPT WS-NOTAS-3(ALUM).
               IF WS-NOTAS-3(ALUM) >=11
                   DISPLAY "INCORRECTO"
                   PERFORM 9999-GENERAL.
               
               
               
       2000-END-INGRESO-DATOS SECTION.
           

       3000-CALCULOS-NOTAS SECTION.

           COMPUTE WS-PROMEDIO(ALUM) ROUNDED = (WS-NOTAS-1(ALUM)
           + WS-NOTAS-2(ALUM)
           + WS-NOTAS-1(ALUM))/WS-CONS-3.

       3000-END-CALCULO-NOTAS.
           
           





       4000-BUSCAR-ALUMNO SECTION.
           DISPLAY'DIGITE NOMBRE A BUSCAR : '
           ACCEPT WS-BUSC-ALUMN
|          SET ALUM TO 1
           SEARCH  WS-DATOS-ALUM
               AT END
                   DISPLAY 'EL ALUMNO NO EXISTE'
               WHEN  WS-NOMBRE(ALUM) = WS-BUSC-ALUMN 
               PERFORM 5000-PRESETACION-DATOS.
                   
                   

       4000-END-BUSCAR SECTION.
       
       
       5000-PRESETACION-DATOS SECTION.
           
           MOVE WS-STR-MES (WS-MES(ALUM)) TO WS-MES-OBTEN.
           DISPLAY "DATOS ESTUDIANTES"
           DISPLAY "FECHA ACTUAL       :" FECHA-ANIO '-'FECHA-MES
                                                 '-' FECHA-DIA
           DISPLAY "NOMBRE COMPLETO    : " WS-NOMBRE(ALUM)
           DISPLAY "CEDULA             : " WS-CEDULA(ALUM)
           DISPLAY "FECHA NACIMIENTO   : " WS-DIA(ALUM) 
                                          "/" WS-MES-OBTEN
                                          "/" WS-ANIO(ALUM)
           DISPLAY "PROMEDIO           : " WS-PROMEDIO(ALUM).

       5000-END-PRESENTACION-DATOS SECTION.
       
       7000-VALIDAR-CEDULA SECTION.
           MOVE WS-CEDULA(ALUM) TO WS-CEDEULA-ACEEPT.
           SET  S-2 TO WS-CONS-2.
           PERFORM 2000-ULTIMO-ENTERO VARYING S-1 FROM WS-CONS-2
           BY 9 UNTIL
           S-1 = WS-CONS.
           PERFORM 2000-ULTIMO-ENTERO
      *    1234567897
           PERFORM 1000-MULTIPLICADOR VARYING S-2 FROM WS-CONS-2
           BY WS-CONS-2 UNTIL
               S-2 >= WS-CONS.
           PERFORM 6000-VALIDAR.
     
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
       2000-ULTIMO-ENTERO.

           MOVE WS-9-PRIMEROS-DIGI(S-1) TO WS-ULTIMO-DIGTO-CI.

       6000-VALIDAR.
           EVALUATE TRUE
               WHEN WS-SUMA-2 >= WS-CONS-2
               DISPLAY WS-SUMA-2
                   COMPUTE WS-RESTA = (WS-SUMA-1 + WS-CONS-2)*WS-CONS
                   - WS-SUMA
                   IF WS-RESTA = WS-ULTIMO-DIGTO-CI
                      DISPLAY "CEDULA CORRECTA"

                   ELSE IF WS-RESTA  = WS-CONS AND
                       WS-ULTIMO-DIGTO-CI = 0
                       DISPLAY "CEDULA CORRECTA: "
                       
                   ELSE
                       DISPLAY "CEDULA INVALIDA"
                       PERFORM 9999-GENERAL
                   END-IF
                WHEN WS-SUMA-2 = 0
                DISPLAY WS-SUMA-2
                DISPLAY "ERROR"
                DISPLAY WS-SUMA-2.


       6000-END-VALIDE-CEDULA SECTION.
           




       0000-END-MAIN.

       9999-GENERAL SECTION.
            STOP RUN.
 
  
       END PROGRAM EVALU2.
