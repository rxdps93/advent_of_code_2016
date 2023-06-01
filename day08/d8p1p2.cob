000100 IDENTIFICATION DIVISION. 
000200 PROGRAM-ID. AOC-2016-D8P1P2.
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
001500 01 INPUT-LINE   PIC X(99).
001600
001700 WORKING-STORAGE SECTION. 
001800 01  LOOP        PIC 9    VALUE 1.
001900 01  LEN         PIC 99.
002000 01  X           PIC 99.
002100 01  Y           PIC 99.
002200 01  TMP         PIC 99.
002300 01  OFFSET      PIC 99.
002400 01  LCD OCCURS 6 TIMES.
002500     05 ROW OCCURS 50 TIMES.
002600        10 PIXEL PIC X VALUE '.'.
002700 01  TMP-ROW     PIC X OCCURS 50 TIMES.
002800 01  TMP-COL     PIC X OCCURS 6 TIMES.
002900 01  PX-ON       PIC 999 VALUE 000.
003000
003100 PROCEDURE DIVISION.
003200 MAIN.
003300     OPEN INPUT INPUTFILE.
003400     PERFORM UNTIL LOOP = 0
003500        READ INPUTFILE NEXT RECORD INTO INPUT-LINE
003600        AT END
003700           MOVE 0 TO LOOP
003800        NOT AT END
003900           PERFORM PARSE-LINE
004000        END-READ
004100     END-PERFORM
004200     CLOSE INPUTFILE
004300     PERFORM PRT-SC
004400     
004500     MOVE 0 TO PX-ON
004600     PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 6
004700        INSPECT LCD(Y) TALLYING PX-ON FOR ALL '#'
004800     END-PERFORM
004900     DISPLAY PX-ON
005000     GOBACK.
005100
005200 PARSE-LINE.
005300     IF INPUT-LINE(2:1) = 'e' THEN
005400        PERFORM RECT
005500     ELSE IF INPUT-LINE(8:1) = 'c' THEN
005600        PERFORM ROT-COL
005700     ELSE IF INPUT-LINE(8:1) = 'r' THEN
005800        PERFORM ROT-ROW
005900     END-IF.
006000
006100 RECT.
006200*    X to represent how far to draw horizontally (e.g. num cols)
006300*    Y to represent how far to draw vertically (e.g. num rows)
006400     MOVE 0 TO LEN
006500     MOVE 0 TO TMP
006600     INSPECT INPUT-LINE TALLYING LEN FOR CHARACTERS BEFORE 'x'
006700     INSPECT FUNCTION TRIM(INPUT-LINE) TALLYING TMP
006800        FOR CHARACTERS AFTER 'x'
006900
007000     MOVE INPUT-LINE(6:LEN - 5) TO X
007100     MOVE INPUT-LINE(LEN + 2:TMP) TO Y
007200
007300     PERFORM VARYING LEN FROM 1 BY 1 UNTIL LEN > Y
007400        PERFORM VARYING TMP FROM 1 BY 1 UNTIL TMP > X
007500           MOVE '#' TO ROW(LEN TMP)
007600        END-PERFORM
007700     END-PERFORM.
007800
007900 ROT-COL.
008000*    X to represent which column to move
008100*    Y to represent shift amount
008200     MOVE FUNCTION TRIM(INPUT-LINE(17:2)) TO X
008300     MOVE FUNCTION TRIM(INPUT-LINE(22:3)) TO Y
008400
008500     PERFORM VARYING TMP FROM 0 BY 1 UNTIL TMP > 5
008600        COMPUTE OFFSET=FUNCTION MOD(TMP + Y 6) END-COMPUTE 
008700*       DISPLAY '    'TMP' GOES TO 'OFFSET
008800        MOVE ROW(TMP + 1 X + 1) TO TMP-COL(OFFSET + 1)
008900     END-PERFORM
009000
009100     PERFORM VARYING TMP FROM 1 BY 1 UNTIL TMP > 6
009200        MOVE TMP-COL(TMP) TO ROW(TMP X + 1)
009300     END-PERFORM.
009400
009500 ROT-ROW.
009600*    X to represent shift amount
009700*    Y to represent which row to move
009800     MOVE FUNCTION TRIM(INPUT-LINE(14:2)) TO Y
009900     MOVE FUNCTION TRIM(INPUT-LINE(19:3)) TO X
010000
010100     PERFORM VARYING TMP FROM 0 BY 1 UNTIL TMP > 49
010200        COMPUTE OFFSET=FUNCTION MOD(TMP + X 50) END-COMPUTE 
010300        MOVE ROW(Y + 1 TMP + 1) TO TMP-ROW(OFFSET + 1)
010400     END-PERFORM
010500     
010600     PERFORM VARYING TMP FROM 1 BY 1 UNTIL TMP > 50
010700        MOVE TMP-ROW(TMP) TO ROW(Y + 1 TMP)
010800     END-PERFORM.
010900
011000 PRT-SC.
011100     PERFORM VARYING TMP FROM 1 BY 1 UNTIL TMP > 6
011200        DISPLAY LCD(TMP)
011300     END-PERFORM.
011400
011500 END PROGRAM AOC-2016-D8P1P2.
