000100 IDENTIFICATION DIVISION. 
000200 PROGRAM-ID. AOC-2016-D7P2.
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
001500 01 INPUT-LINE   PIC X(999).
001600
001700 WORKING-STORAGE SECTION.
001800 01 LOOP   PIC 9    VALUE 1.
001900 01 LOOP2  PIC 9    VALUE 0.
002000 01 LEN    PIC 999  VALUE 0.
002100 01 PSN    PIC 999  VALUE 0.
002200 01 TMP    PIC 999  VALUE 0.
002300 01 HNET   PIC 9    VALUE 0.
002400 01 CNT    PIC 999  VALUE 0.
002500 01 A-CNT  PIC 99   VALUE 1.
002600 01 B-CNT  PIC 99   VALUE 1.
002700 01 SSL.
002800     05 TYPES OCCURS 99 TIMES.
002900        10 ABA    PIC AA.
003000        10 BAB    PIC AA.
003100
003200 PROCEDURE DIVISION.
003300 MAIN.
003400     OPEN INPUT INPUTFILE.
003500     PERFORM UNTIL LOOP = 0
003600        READ INPUTFILE NEXT RECORD INTO INPUT-LINE
003700        AT END
003800           MOVE 0 TO LOOP
003900        NOT AT END
004000           PERFORM PARSE-LINE
004100        END-READ
004200     END-PERFORM
004300     CLOSE INPUTFILE.
004400     DISPLAY CNT
004500     GOBACK.
004600
004700 PARSE-LINE.
004800     MOVE 1 TO A-CNT
004900     MOVE 1 TO B-CNT
005000     MOVE 0 TO LEN
005100     INSPECT INPUT-LINE TALLYING LEN FOR CHARACTERS BEFORE SPACE
005200     SUBTRACT 2 FROM LEN
005300     PERFORM VARYING PSN FROM 1 BY 1 UNTIL PSN > LEN
005400        IF INPUT-LINE(PSN:1) IS EQUAL TO '[' THEN
005500           MOVE 1 TO HNET
005600        ELSE IF INPUT-LINE(PSN:1) IS EQUAL TO ']' THEN
005700           MOVE 0 TO HNET
005800        ELSE
005900*          compare char 1 && 3
006000*          compare char 1 && 2
006100           IF INPUT-LINE(PSN:1) = INPUT-LINE(PSN + 2:1) AND
006200              INPUT-LINE(PSN:1) NOT = INPUT-LINE(PSN + 1:1) THEN
006300           
006400*          check if a hypernet
006500              IF HNET = 0 THEN
006600                 MOVE INPUT-LINE(PSN:2) TO ABA(A-CNT)
006700                 ADD 1 TO A-CNT
006800              ELSE IF HNET = 1 THEN
006900                 MOVE INPUT-LINE(PSN + 1:2) TO BAB(B-CNT)
007000                 ADD 1 TO B-CNT
007100              END-IF
007200           END-IF
007300        END-IF
007400     END-PERFORM
007500     
007600     MOVE 0 TO LOOP2
007700     IF A-CNT > 1 AND B-CNT > 1 THEN
007800        PERFORM VARYING PSN FROM 1 BY 1 UNTIL PSN = A-CNT
007900           OR LOOP2 = 1
008000           PERFORM VARYING TMP FROM 1 BY 1 UNTIL TMP = B-CNT
008100              IF ABA(PSN) = BAB(TMP) THEN
008200                 MOVE 1 TO LOOP2
008300              END-IF
008400           END-PERFORM
008500        END-PERFORM
008600     END-IF
008700
008800     ADD LOOP2 TO CNT.
008900
009000 END PROGRAM AOC-2016-D7P2.
