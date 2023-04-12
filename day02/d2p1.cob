       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2016-D2P1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO 'INPUT'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUTFILE IS EXTERNAL
           RECORD IS VARYING IN SIZE
           DATA RECORD IS INPUT-LINE.
       01 INPUT-LINE  PIC X(999).

       WORKING-STORAGE SECTION.
       01 INDX        PIC 999    VALUE 001.
       01 OFFSET      PIC 999.
       01 LEN         PIC 999.
       01 LOOP        PIC 9      VALUE 1.
       01 X           PIC 9      VALUE 2.
       01 Y           PIC 9      VALUE 2.
       01 KEYPAD.
          05 ROW OCCURS 3 TIMES.
             10 CLM   PIC X OCCURS 3 TIMES.

       PROCEDURE DIVISION.
       MAIN.
           MOVE '123' TO ROW(1)
           MOVE '456' TO ROW(2)
           MOVE '789' TO ROW(3)

           OPEN INPUT INPUTFILE.
           PERFORM UNTIL LOOP = 0
                   READ INPUTFILE NEXT RECORD INTO INPUT-LINE
                   AT END
                      MOVE 0 TO LOOP
                      DISPLAY ' '
                   NOT AT END
                       PERFORM
                          PARSE-INPUT
                       DISPLAY CLM(Y, X) WITH NO ADVANCING 
                   END-READ
           END-PERFORM.
           CLOSE INPUTFILE.

           GOBACK.

       PARSE-INPUT.
           MOVE 0 TO LEN
           MOVE 1 TO OFFSET

           INSPECT INPUT-LINE TALLYING LEN FOR CHARACTERS BEFORE SPACE.
           PERFORM LEN TIMES
              
                   EVALUATE INPUT-LINE(OFFSET:1)
                   WHEN 'U'
                        SUBTRACT 1 FROM Y
                   WHEN 'D'
                        ADD 1 TO Y
                   WHEN 'L'
                        SUBTRACT 1 FROM X
                   WHEN 'R'
                        ADD 1 TO X
                   END-EVALUATE

                   ADD 1 TO OFFSET

                   IF Y < 1 THEN
                      MOVE 1 TO Y
                   ELSE
                      IF Y > 3 THEN
                         MOVE 3 TO Y
                      END-IF
                   END-IF

                   IF X < 1 THEN
                      MOVE 1 TO X
                   ELSE
                      IF X > 3 THEN
                         MOVE 3 TO X
                      END-IF
                   END-IF
           END-PERFORM.
           
       END PROGRAM AOC-2016-D2P1.
