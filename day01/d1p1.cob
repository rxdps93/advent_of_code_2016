000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. AOC-2016-D1P1.
000300
000400 ENVIRONMENT DIVISION.
000500 INPUT-OUTPUT SECTION.
000600 FILE-CONTROL.
000700     SELECT INPUTFILE ASSIGN TO 'input.txt'
000800     ORGANIZATION IS LINE SEQUENTIAL.
000900
001000 DATA DIVISION.
001100 FILE SECTION.
001200 FD INPUTFILE IS EXTERNAL
001300     RECORD IS VARYING IN SIZE
001400     DATA RECORD IS INPUT-LINE.
001500 01 INPUT-LINE  PIC A(999).
001600
001700 WORKING-STORAGE SECTION.
001800 01 INDX        PIC 999    VALUE 001.
001900 01 OFFSET      PIC 9.
002000 01 DIR         PIC A.
002100 01 DIST        PIC 999.
002200 01 LOOP        PIC 9      VALUE 1.
002300
002400 01 X           PIC S999   VALUE +000.
002500 01 Y           PIC S999   VALUE +000.
002600 01 TOTAL-DIST  PIC 999.
002700 01 FACING      PIC 9      VALUE 1.
002800
002900 PROCEDURE DIVISION.
003000 MAIN.
003100     OPEN INPUT INPUTFILE.
003200     READ INPUTFILE.
003300     CLOSE INPUTFILE.
003400
003500     PERFORM UNTIL LOOP = 0
003600             PERFORM PARSE-INPUT
003700     END-PERFORM.
003800
003900     MOVE FUNCTION ABS(X) TO X.
004000     MOVE FUNCTION ABS(Y) TO Y.
004100     ADD X TO Y GIVING TOTAL-DIST.
004200     DISPLAY TOTAL-DIST.
004300
004400 PARSE-INPUT.
004500     MOVE INPUT-LINE(INDX:1) TO DIR.
004600     IF DIR IS NOT EQUAL TO "R" AND DIR IS NOT EQUAL TO "L" THEN
004700        MOVE 0 TO LOOP
004800     END-IF.
004900
005000     ADD 1 TO INDX.
005100     MOVE 0 TO OFFSET.
005200     PERFORM UNTIL INPUT-LINE(INDX + OFFSET:1) = "," OR
005300        INPUT-LINE(INDX + OFFSET:1) = " "
005400             ADD 1 TO OFFSET
005500     END-PERFORM.
005600
005700     MOVE INPUT-LINE(INDX:OFFSET) TO DIST.
005800     ADD OFFSET TO INDX.
005900     ADD 2 TO INDX.
006000
006100     IF DIR = "R" THEN
006200        ADD 1 TO FACING
006300     ELSE
006400        SUBTRACT 1 FROM FACING
006500     END-IF.
006600
006700     IF FACING = 5 THEN
006800        MOVE 1 TO FACING
006900     ELSE
007000        IF FACING = 0 THEN
007100           MOVE 4 TO FACING
007200        END-IF.
007300
007400     PERFORM DIST TIMES
007500             EVALUATE FACING
007600             WHEN 1
007700                  ADD 1 TO Y
007800             WHEN 2
007900                  ADD 1 TO X
008000             WHEN 3
008100                  SUBTRACT 1 FROM Y
008200             WHEN 4
008300                  SUBTRACT 1 FROM X
008400             END-EVALUATE
008500     END-PERFORM.
008600
008700 END PROGRAM AOC-2016-D1P1.
