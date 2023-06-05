       IDENTIFICATION DIVISION. 
       PROGRAM-ID. AOC-2016-D9P2.
      
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
       01 INPUT-LINE   PIC X(99999).
      
       WORKING-STORAGE SECTION. 
       01  LOOP        PIC 9       VALUE 1.
       01  RET         PIC 9(11)   VALUE 0.
       01  PSN         PIC 9(11)   VALUE 1.
       01  M-PSN       PIC 9(11)   VALUE 1.
       01  TMP         PIC 9(11).
       01  LEN         PIC 9(11).
       01  CHRS        PIC 9(11).
       01  REPS        PIC 9(11).
       01  IN-LEN      PIC 9(11).
       01  MULT.
           05 CH-MUL   OCCURS 1 TO 99999 TIMES DEPENDING ON IN-LEN.
              10 M-VAL PIC 9(11) VALUE 1.
       01  M-TMP       PIC 9(11).
      
       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM UNTIL LOOP = 0
              READ INPUTFILE NEXT RECORD INTO INPUT-LINE
              AT END
                 MOVE 0 TO LOOP
              NOT AT END
                 MOVE 0 TO RET
                 INITIALIZE MULT REPLACING NUMERIC DATA BY 1
                 PERFORM DECRYPT
                 DISPLAY RET
              END-READ
           END-PERFORM
           CLOSE INPUTFILE
           GOBACK.
      
       DECRYPT.
           MOVE LENGTH OF FUNCTION TRIM(INPUT-LINE) TO IN-LEN
           PERFORM VARYING PSN FROM 1 BY 1 UNTIL PSN > IN-LEN
      *    UPDATE WEIGHTS IF (
              IF INPUT-LINE(PSN:1) = '(' THEN

                 ADD 1 TO PSN
                 MOVE 0 TO TMP
                 MOVE 0 TO CHRS
                 MOVE 0 TO REPS
                 INSPECT INPUT-LINE(PSN:10) TALLYING TMP FOR
                    CHARACTERS BEFORE INITIAL 'x'
                 INSPECT INPUT-LINE(PSN + TMP + 1:10) TALLYING REPS FOR
                    CHARACTERS BEFORE INITIAL ')'

                 ADD REPS TO TMP GIVING LEN
                 MOVE INPUT-LINE(PSN:TMP) TO CHRS
                 MOVE INPUT-LINE(PSN + TMP + 1:REPS) TO REPS
                 
                 COMPUTE
                    PSN=PSN + LEN + 1
                 END-COMPUTE

                 ADD 1 TO PSN GIVING M-PSN
                 PERFORM UNTIL M-PSN > PSN + CHRS

                    MOVE CH-MUL(M-PSN) TO M-TMP
                    COMPUTE M-TMP=M-TMP * REPS END-COMPUTE
                    MOVE M-TMP TO CH-MUL(M-PSN)

                    ADD 1 TO M-PSN
                 END-PERFORM

              ELSE
                 MOVE CH-MUL(PSN) TO M-TMP
                 ADD M-TMP TO RET
                 MOVE M-TMP TO CH-MUL(PSN)
              END-IF
           END-PERFORM.

      
       END PROGRAM AOC-2016-D9P2.
