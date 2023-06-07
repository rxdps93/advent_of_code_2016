       IDENTIFICATION DIVISION. 
       PROGRAM-ID. AOC-2016-D10P1.

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
       01 INPUT-LINE   PIC X(99).

       WORKING-STORAGE SECTION. 
       01 LOOP            PIC 9       VALUE 1.
       01 LEN             PIC 99.
       01 INSTR           PIC X(99).
       01 TMP             PIC 999.
       01 TMP2            PIC 999.
       01 TMP-L           PIC 999.
       01 TMP-R           PIC 999.
       01 BOTS.
           05 CHIPS       OCCURS 250 TIMES.
              10 BOT      PIC 999     VALUE 999.
              10 L-VAL    PIC 999     VALUE 0.
              10 R-VAL    PIC 999     VALUE 0.
              10 LO       PIC 999     VALUE 0.
              10 HI       PIC 999     VALUE 0.

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

           DISPLAY 'BEGIN'
           MOVE 1 TO LOOP
           PERFORM UNTIL LOOP = 0
              MOVE 0 TO LOOP
              PERFORM VARYING TMP FROM 1 BY 1 UNTIL TMP > 250
                 IF BOT IN CHIPS(TMP) IS NOT EQUAL TO 999 THEN

                    IF L-VAL IN CHIPS(TMP) IS NOT EQUAL TO 0 AND 
                       R-VAL IN CHIPS(TMP) IS NOT EQUAL TO 0 THEN
                       PERFORM ASSIGN-CHIPS
                    END-IF

                    IF L-VAL IN CHIPS(TMP) = 0 OR
                       R-VAL IN CHIPS(TMP) = 0 THEN
                       MOVE 1 TO LOOP
                    END-IF 
                 END-IF
              END-PERFORM
              PERFORM SORT-CHIPS
           END-PERFORM

           PERFORM VARYING TMP FROM 1 BY 1 UNTIL TMP > 250
      *       MOVE L-VAL IN CHIPS(TMP) TO TMP-L
      *       MOVE R-VAL IN CHIPS(TMP) TO TMP-R
      *       IF TMP-L = 61 AND TMP-R = 17 THEN

      *          DISPLAY 'BOT 'BOT IN CHIPS(TMP)
      *       ELSE IF TMP-L = 17 AND TMP-R = 61 THEN
      *          DISPLAY 'BOT 'BOT IN CHIPS(TMP)
      *       END-IF
              IF BOT IN CHIPS(TMP) IS NOT EQUAL TO 999 THEN
                 
                 DISPLAY BOT IN CHIPS(TMP)', ['
                          L-VAL IN CHIPS(TMP)', '
                          R-VAL IN CHIPS(TMP)']'
      *          DISPLAY '    'LO IN CHIPS(TMP) ' & '
      *                HI IN CHIPS(TMP)
              END-IF
           END-PERFORM

           GOBACK.

       SORT-CHIPS.
           PERFORM VARYING TMP FROM 1 BY 1 UNTIL TMP > 250
              IF BOT IN CHIPS(TMP) IS NOT EQUAL TO 999 THEN
                 MOVE L-VAL IN CHIPS(TMP) TO TMP-L
                 MOVE R-VAL IN CHIPS(TMP) TO TMP-R
                 MOVE FUNCTION MIN(TMP-L TMP-R) TO L-VAL
                 MOVE FUNCTION MAX(TMP-L TMP-R) TO R-VAL
              END-IF
           END-PERFORM.

       ASSIGN-CHIPS.
      *    LOW
           MOVE FUNCTION MIN(L-VAL IN CHIPS(TMP) R-VAL IN CHIPS(TMP))
              TO TMP-L
           MOVE LO IN CHIPS(TMP) TO TMP-R
           
           COMPUTE TMP2 = TMP - 1 END-COMPUTE
           DISPLAY TMP2' -> 'L-VAL IN CHIPS(TMP)', 'R-VAL IN CHIPS(TMP)
           IF TMP-R IS NOT EQUAL TO 999 AND TMP-L IS NOT EQUAL TO 0 THEN
              IF L-VAL IN CHIPS(TMP-R + 1) = 0 THEN
                 DISPLAY 'LL    ASSIGNING 'TMP-L' FROM 'TMP2' TO 'TMP-R
                 MOVE TMP-L TO L-VAL IN CHIPS(TMP-R + 1)
              ELSE IF R-VAL IN CHIPS(TMP-R + 1) = 0 THEN
                 DISPLAY 'LR    ASSIGNING 'TMP-L' FROM 'TMP2' TO 'TMP-R
                 MOVE TMP-L TO R-VAL IN CHIPS(TMP-R + 1)
              ELSE
                 DISPLAY 'L     OVERWRITE DETECTED!'
              END-IF
           END-IF

      *    HIGH
           MOVE FUNCTION MAX(L-VAL IN CHIPS(TMP) R-VAL IN CHIPS(TMP))
              TO TMP-L
           MOVE HI IN CHIPS(TMP) TO TMP-R

           IF TMP-R IS NOT EQUAL TO 999 AND TMP-L IS NOT EQUAL TO 0 THEN
              IF L-VAL IN CHIPS(TMP-R + 1) = 0 THEN
                 DISPLAY 'HL    ASSIGNING 'TMP-L' FROM 'TMP2' TO 'TMP-R
                 MOVE TMP-L TO L-VAL IN CHIPS(TMP-R + 1)
              ELSE IF R-VAL IN CHIPS(TMP-R + 1) = 0 THEN
                 DISPLAY 'HR    ASSIGNING 'TMP-L' FROM 'TMP2' TO 'TMP-R
                 MOVE TMP-L TO R-VAL IN CHIPS(TMP-R + 1)
              ELSE 
                 DISPLAY 'H     OVERWRITE DETECTED!'
              END-IF
           END-IF.

       PARSE-LINE.
           INSPECT INSTR REPLACING CHARACTERS BY SPACE
      *    IMMEDIATE ASSIGNMENT CASE
           IF INPUT-LINE(1:1) = 'v' THEN
              MOVE INPUT-LINE TO INSTR
              MOVE 0 TO LEN
              INSPECT INSTR CONVERTING 
                 "abcdefghijklmnopqrstuvwxyz" TO SPACE
              MOVE FUNCTION TRIM(INSTR) TO INSTR
              INSPECT INSTR TALLYING LEN FOR
                 CHARACTERS BEFORE INITIAL SPACE 

              MOVE INSTR(1:LEN) TO TMP-L

              INSPECT INSTR REPLACING CHARACTERS BY SPACE 
                 BEFORE SPACE
              MOVE INSTR TO TMP-R

              MOVE TMP-R TO BOT IN CHIPS(TMP-R + 1)
              
              IF L-VAL IN CHIPS(TMP-R + 1) = 0 THEN
                 MOVE TMP-L TO L-VAL IN CHIPS(TMP-R + 1)
              ELSE
                 MOVE TMP-L TO R-VAL IN CHIPS(TMP-R + 1)
              END-IF
              
           ELSE IF INPUT-LINE(1:1) = 'b' THEN
      *       DISPLAY INPUT-LINE
              INSPECT INPUT-LINE CONVERTING 
                 "acdefghijklmnoqrstuvwxyz" TO SPACE 
              MOVE FUNCTION TRIM(INPUT-LINE(2:)) TO INPUT-LINE

              MOVE 1 TO LEN
              MOVE 1 TO TMP-L TMP-R
              PERFORM UNTIL TMP-L > LENGTH OF FUNCTION TRIM(INPUT-LINE)
                 IF INPUT-LINE(TMP-L:1) IS NOT EQUAL TO SPACE THEN
                    MOVE INPUT-LINE(TMP-L:1) TO INSTR(TMP-R:1)
                    ADD 1 TO TMP-R
                    MOVE 1 TO LEN
                 ELSE
                    IF LEN = 1 THEN
                       MOVE INPUT-LINE(TMP-L:1) TO INSTR(TMP-R:1)
                       ADD 1 TO TMP-R
                       MOVE 0 TO LEN
                    END-IF
                 END-IF
                 ADD 1 TO TMP-L
              END-PERFORM
              
              MOVE 0 TO LEN
              INSPECT INSTR TALLYING LEN FOR 
                 CHARACTERS BEFORE INITIAL SPACE
              MOVE INSTR(1:LEN) TO TMP
              MOVE TMP TO BOT IN CHIPS(TMP + 1)
              MOVE INSTR(LEN + 2:) TO INSTR

      *       CASE FOR LOW; SKIP OUTPUT FOR NOW
              MOVE 0 TO LEN
              INSPECT FUNCTION TRIM(INSTR(3:)) TALLYING LEN FOR 
                 CHARACTERS BEFORE INITIAL SPACE

              IF INSTR(1:1) = 'b' THEN
                 MOVE INSTR(3:LEN) TO LO IN CHIPS(TMP + 1)
              ELSE
                 MOVE 999 TO LO IN CHIPS(TMP + 1)
              END-IF

              MOVE INSTR(4 + LEN:) TO INSTR

      *       CASE FOR HIGH; SKIP OUTPUT FOR NOW
              MOVE 0 TO LEN
              INSPECT FUNCTION TRIM(INSTR(3:)) TALLYING LEN FOR 
                 CHARACTERS BEFORE INITIAL SPACE

              IF INSTR(1:1) = 'b' THEN
                 MOVE INSTR(3:LEN) TO HI IN CHIPS(TMP + 1)
              ELSE
                 MOVE 999 TO HI IN CHIPS(TMP + 1)
              END-IF

      *       DISPLAY '    BOT 'BOT IN CHIPS(TMP + 1)
      *          ' L: 'LO IN CHIPS(TMP + 1)
      *          ' H: 'HI IN CHIPS(TMP + 1)
              
           END-IF.

       END PROGRAM AOC-2016-D10P1.
