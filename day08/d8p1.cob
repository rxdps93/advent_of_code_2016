       IDENTIFICATION DIVISION. 
       PROGRAM-ID. AOC-2016-D8P1.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
           SELECT INPUTFILE ASSIGN TO 'TEST'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION. 
       FILE SECTION. 
       FD INPUTFILE IS EXTERNAL 
           RECORD IS VARYING IN SIZE
           DATA RECORD IS INPUT-LINE.
       01 INPUT-LINE   PIC X(99).

       WORKING-STORAGE SECTION. 
       01  LOOP        PIC 9    VALUE 1.
       01  LEN         PIC 99.
       01  X           PIC 99.
       01  Y           PIC 99.
       01  TMP         PIC 99.
       01  LCD OCCURS 6 TIMES.
           05 ROW OCCURS 50 TIMES.
              10 PIXEL PIC X VALUE '.'.

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
           PERFORM PRT-SC
           GOBACK.

       PARSE-LINE.
           DISPLAY FUNCTION TRIM(INPUT-LINE)

           IF INPUT-LINE(2:1) = 'e' THEN
              PERFORM RECT
           ELSE IF INPUT-LINE(8:1) = 'c' THEN
              PERFORM ROT-COL
           ELSE IF INPUT-LINE(8:1) = 'r' THEN
              PERFORM ROT-ROW
           END-IF.

       RECT.
           MOVE 0 TO LEN
           MOVE 0 TO TMP
           INSPECT INPUT-LINE TALLYING LEN FOR CHARACTERS BEFORE 'x'
           INSPECT FUNCTION TRIM(INPUT-LINE) TALLYING TMP
              FOR CHARACTERS AFTER 'x'

           MOVE INPUT-LINE(6:LEN - 5) TO X
           MOVE INPUT-LINE(LEN + 2:TMP) TO Y

           PERFORM VARYING LEN FROM 1 BY 1 UNTIL LEN > Y
              PERFORM VARYING TMP FROM 1 BY 1 UNTIL TMP > X
                 MOVE '#' TO ROW(LEN TMP)
              END-PERFORM
           END-PERFORM.

       ROT-COL.
           DISPLAY '    ROT-COL'.

       ROT-ROW.
           DISPLAY '    ROT-ROW'.

       PRT-SC.
           PERFORM VARYING TMP FROM 1 BY 1 UNTIL TMP > 6
              DISPLAY LCD(TMP)
           END-PERFORM.

       END PROGRAM AOC-2016-D8P1.
