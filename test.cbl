       ID DIVISION.
       PROGRAM-ID. JKEMLISS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01 INTERNAL-PROGRAM-VARIABLES.
           05 DISP-COUNT                      PIC S9(4) COMP.
           05 MAX-LOOP                        PIC S9(4) COMP
                                              VALUE IS 8.

           COPY MORTGAGE.

           EXEC SQL INCLUDE SQLCA END-EXEC.

       01 SQLCOMMAREA.
           05 SQLINPUT.
              10 INPUTLOAN                 PIC S9(9)V99 COMP-3.
              10 INPUTYEARS                PIC S9(4)    COMP-3.
              10 INPUTRATE                 PIC S9(2)V9(3) COMP-3.
           05 SQLOUTPUT OCCURS 8 TIMES.
              10 SQLCOMPANY                PIC X(24).
              10 SQLPHONE                  PIC X(13).
              10 SQLRATE                   PIC S9(3)V9(2) USAGE COMP-3.
              10 SQLLOAN                   PIC X(12).
              10 SQLYEARS                  PIC X(2).
           05 SQLMESSAGE                   PIC X(24).
           05 SQLRC                        PIC X(12).

           EXEC SQL
           DECLARE ICURSOR CURSOR FOR
               SELECT COMPANY, PHONE, RATE, LOAN, YEARS
                   FROM JKEMORT.MORTGAGE
                   WHERE LOAN >= :INPUTLOAN AND
                         RATE <= :INPUTRATE AND
                         YEARS = :INPUTYEARS
           END-EXEC.

       LINKAGE SECTION.
       01 DFHCOMMAREA.
          05 DFHINPUT.
              10 DFHINPUTLOAN              PIC S9(9)V99 COMP-3.
              10 DFHINPUTYEARS             PIC S9(4)    COMP-3.
              10 DFGINPUTRATE              PIC S9(2)V9(3) COMP-3.
           05 DFHOUTPUT OCCURS 8 TIMES.
              10 DFHSQLCOMPANY             PIC X(24).
              10 DFHSQLPHONE               PIC X(13).
              10 DFHSQLRATE                PIC S9(3)V9(2) USAGE COMP-3.
              10 DFHSQLLOAN                PIC X(12).
              10 DFHSQLYEARS               PIC X(2).
           05 DFHSQLMESSAGE                PIC X(24).
           05 DFHSQLRC                     PIC X(12).

       PROCEDURE DIVISION.

           INITIALIZE SQLCOMMAREA.
           INITIALIZE DISP-COUNT.

           MOVE LOW-VALUES TO SQLCOMMAREA.

           IF EIBCALEN = LENGTH OF DFHCOMMAREA
               MOVE DFHCOMMAREA TO SQLCOMMAREA
           ELSE
               EXEC CICS RETURN
                   END-EXEC
           END-IF.

           MOVE 1 TO DISP-COUNT

           EXEC SQL OPEN ICURSOR END-EXEC.

           IF SQLCODE = 0
              PERFORM A150-PROCESS-FILE
                   UNTIL SQLCODE NOT = 0
                   OR DISP-COUNT > MAX-LOOP
           ELSE
                MOVE 'ERROR WITH START'      TO SQLMESSAGE
                MOVE SQLCODE                 TO SQLRC
           END-IF
           .

           EXEC SQL CLOSE ICURSOR END-EXEC.

           MOVE SQLCOMMAREA TO DFHCOMMAREA.

           EXEC CICS RETURN END-EXEC.

       A150-PROCESS-FILE.

           EXEC SQL FETCH ICURSOR
                INTO :COMPANY,
                     :PHONE,
                     :RATE,
                     :LOAN,
                     :YEARS
           END-EXEC.


           IF SQLCODE = 0

                 MOVE COMPANY TO
                       SQLCOMPANY (DISP-COUNT)
      * Move LOAN COMPANY PHONE NUMBER to output map
                 MOVE PHONE TO
                       SQLPHONE (DISP-COUNT)
      * Move LOAN COMPANY RATE offered to output map
                 MOVE RATE TO SQLRATE(DISP-COUNT)
      * Move LOAN COMPANY LOAN TERM to output map
                 MOVE YEARS TO SQLYEARS(DISP-COUNT)

                 ADD 1 TO DISP-COUNT

           ELSE

              IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
                 MOVE 'ERROR WITH CURSOR READ' TO SQLMESSAGE
                 MOVE SQLCODE                  TO SQLRC
              END-IF

           END-IF
           .
