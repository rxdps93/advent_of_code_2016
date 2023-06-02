       IDENTIFICATION DIVISION. 
       PROGRAM-ID. AOC-2016-D9P1.

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
       01 INPUT-LINE   PIC X(99999).

       WORKING-STORAGE SECTION. 
       01  LOOP     PIC 9       VALUE 1.
       01  LEN      PIC 9(5)    VALUE 0.
       01  PTR      PIC 9(5)    VALUE 0.
       01  PTR2     PIC 9(5)    VALUE 0.
       01  MARK     PIC 99      VALUE 0.
       01  CHRS     PIC 999     VALUE 0.
       01  REPS     PIC 999     VALUE 0.
       01  MESG     PIC X       OCCURS 99999 TIMES.

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM UNTIL LOOP = 0
              READ INPUTFILE NEXT RECORD INTO INPUT-LINE
              AT END
                 MOVE 0 TO LOOP
              NOT AT END
                 PERFORM DECRYPT
              END-READ
           END-PERFORM
           CLOSE INPUTFILE
           GOBACK.

       DECRYPT.
           DISPLAY FUNCTION TRIM(INPUT-LINE)
           MOVE 0 TO LEN
           INSPECT FUNCTION TRIM(INPUT-LINE)
              TALLYING LEN FOR CHARACTERS BEFORE SPACES 

           DISPLAY '    'LEN
           MOVE 1 TO PTR
           MOVE 1 TO PTR2
           MOVE 0 TO MARK
           PERFORM UNTIL PTR > LEN

              IF INPUT-LINE(PTR:1) IS EQUAL TO '(' THEN
      *          GET NEXT )
      *          MOVE NEXT X CHARACTERS Y TIMES TO MESG
      *          JUMP PTR AHEAD AN APPROPRIATE AMOUNT
                 MOVE 0 TO CHRS
                 MOVE 0 TO REPS
                 INSPECT INPUT-LINE(PTR + 1:10) TALLYING CHRS
                    FOR CHARACTERS BEFORE INITIAL 'x'

                 INSPECT INPUT-LINE(PTR + CHRS + 2:10) TALLYING REPS
                    FOR CHARACTERS BEFORE INITIAL ')'
              ELSE
      *          MOVE TO MESG2
                 DISPLAY 'TODO'
              END-IF

      *       IF INPUT-LINE(PTR:1) IS EQUAL TO ')' THEN
      *          DISPLAY '    'INPUT-LINE(PTR:1)
      *       END-IF

      *       IF INPUT-LINE(PTR:1) IS ALPHABETIC OR
      *          INPUT-LINE(PTR:1) IS NUMERIC THEN
                 
      *          IF MARK = 0 THEN
      *             DISPLAY '    'INPUT-LINE(PTR:1)
      *          ELSE
      *             DISPLAY '        'INPUT-LINE(PTR:1)
      *          END-IF
      *       END-IF

      *       DISPLAY PTR':'LEN
              ADD 1 TO PTR
           END-PERFORM.

       END PROGRAM AOC-2016-D9P1.
