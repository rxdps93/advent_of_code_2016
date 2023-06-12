       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2016-D11P1.

       ENVIRONMENT DIVISION. 
       CONFIGURATION SECTION. 
       REPOSITORY. FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT INPUTFILE ASSIGN TO 'INPUT'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUTFILE IS EXTERNAL 
           RECORD IS VARYING IN SIZE
           DATA RECORD IS INPUT-LINE.
       01  INPUT-LINE  PIC X(999).

       WORKING-STORAGE SECTION. 
       01  LOOP        PIC 9    VALUE 1.
       01  LEN         PIC 999.
       01  LEN2        PIC 999.
       01  CURRENT     PIC 9    VALUE 1.
       01  FL-1        PIC AA   OCCURS 10 TIMES.
       01  FL-2        PIC AA   OCCURS 10 TIMES.
       01  FL-3        PIC AA   OCCURS 10 TIMES.
       01  FL-4        PIC AA   OCCURS 10 TIMES.
              

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM UNTIL LOOP = 0
              READ INPUTFILE NEXT RECORD INTO INPUT-LINE
              AT END
                 MOVE 0 TO LOOP
              NOT AT END
                 PERFORM PARSE-LINE
              END-READ
           END-PERFORM
           CLOSE INPUTFILE

           GOBACK.

       PARSE-LINE.
           MOVE 0 TO LEN
           INSPECT TRIM(INPUT-LINE) TALLYING LEN FOR CHARACTERS 
              AFTER INITIAL 'contains'
           INSPECT INPUT-LINE CONVERTING '.' TO SPACE
           MOVE TRIM(INPUT-LINE(26:LEN)) TO INPUT-LINE
           DISPLAY TRIM(INPUT-LINE)
           DISPLAY 'END'.

       END PROGRAM AOC-2016-D11P1.