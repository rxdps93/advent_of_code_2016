000100 IDENTIFICATION DIVISION. 
000200 PROGRAM-ID. AOC-2016-D9P2.
000300
000400 ENVIRONMENT DIVISION. 
000500 INPUT-OUTPUT SECTION. 
000600 FILE-CONTROL. 
000700     SELECT INPUTFILE ASSIGN TO 'INPUT'
000800     ORGANIZATION IS LINE SEQUENTIAL.
000900
001000 DATA DIVISION. 
001100 FILE SECTION. 
001200 FD INPUTFILE IS EXTERNAL 
001300     RECORD IS VARYING IN SIZE
001400     DATA RECORD IS INPUT-LINE.
001500 01 INPUT-LINE   PIC X(99999).
001600
001700 WORKING-STORAGE SECTION. 
001800 01  LOOP        PIC 9       VALUE 1.
001900 01  RET         PIC 9(11)   VALUE 0.
002000 01  PSN         PIC 9(11)   VALUE 1.
002100 01  M-PSN       PIC 9(11)   VALUE 1.
002200 01  TMP         PIC 9(11).
002300 01  LEN         PIC 9(11).
002400 01  CHRS        PIC 9(11).
002500 01  REPS        PIC 9(11).
002600 01  IN-LEN      PIC 9(11).
002700 01  MULT.
002800     05 CH-MUL   OCCURS 1 TO 99999 TIMES DEPENDING ON IN-LEN.
002900        10 M-VAL PIC 9(11) VALUE 1.
003000 01  M-TMP       PIC 9(11).
003100
003200 PROCEDURE DIVISION.
003300 MAIN.
003400     OPEN INPUT INPUTFILE.
003500     PERFORM UNTIL LOOP = 0
003600        READ INPUTFILE NEXT RECORD INTO INPUT-LINE
003700        AT END
003800           MOVE 0 TO LOOP
003900        NOT AT END
004000           MOVE 0 TO RET
004100           INITIALIZE MULT REPLACING NUMERIC DATA BY 1
004200           PERFORM DECRYPT
004300           DISPLAY RET
004400        END-READ
004500     END-PERFORM
004600     CLOSE INPUTFILE
004700     GOBACK.
004800
004900 DECRYPT.
005000     MOVE LENGTH OF FUNCTION TRIM(INPUT-LINE) TO IN-LEN
005100     PERFORM VARYING PSN FROM 1 BY 1 UNTIL PSN > IN-LEN
005200*    UPDATE WEIGHTS IF (
005300        IF INPUT-LINE(PSN:1) = '(' THEN
005400
005500           ADD 1 TO PSN
005600           MOVE 0 TO TMP
005700           MOVE 0 TO CHRS
005800           MOVE 0 TO REPS
005900           INSPECT INPUT-LINE(PSN:10) TALLYING TMP FOR
006000              CHARACTERS BEFORE INITIAL 'x'
006100           INSPECT INPUT-LINE(PSN + TMP + 1:10) TALLYING REPS FOR
006200              CHARACTERS BEFORE INITIAL ')'
006300
006400           ADD REPS TO TMP GIVING LEN
006500           MOVE INPUT-LINE(PSN:TMP) TO CHRS
006600           MOVE INPUT-LINE(PSN + TMP + 1:REPS) TO REPS
006700           
006800           COMPUTE
006900              PSN=PSN + LEN + 1
007000           END-COMPUTE
007100
007200           ADD 1 TO PSN GIVING M-PSN
007300           PERFORM UNTIL M-PSN > PSN + CHRS
007400
007500              MOVE CH-MUL(M-PSN) TO M-TMP
007600              COMPUTE M-TMP=M-TMP * REPS END-COMPUTE
007700              MOVE M-TMP TO CH-MUL(M-PSN)
007800
007900              ADD 1 TO M-PSN
008000           END-PERFORM
008100
008200        ELSE
008300           MOVE CH-MUL(PSN) TO M-TMP
008400           ADD M-TMP TO RET
008500           MOVE M-TMP TO CH-MUL(PSN)
008600        END-IF
008700     END-PERFORM.
008800
008900
009000 END PROGRAM AOC-2016-D9P2.
