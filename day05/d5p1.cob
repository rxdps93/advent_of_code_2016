       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2016-D5P1.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION PIPE-OPEN
           FUNCTION PIPE-READ
           FUNCTION PIPE-CLOSE
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MD5-TEMP ASSIGN TO 'TEMP'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION. 
       FD  MD5-TEMP
           DATA RECORD IS MD5-REC.
       01  MD5-REC PIC X(32).

       WORKING-STORAGE SECTION.
       01  PIPE-LINE   PIC X(32768).
       01  PIPE-OUT    PIC X(32768).

       01  READ-LENGTH PIC 9(5).

       01  PIPE-COMMAND PIC X(64).

       01  PIPE-RECORD.
           05 PIPE-POINTER   USAGE POINTER.
           05 PIPE-RETURN    USAGE BINARY-LONG.

       01  PIPE-RECORD-OUT.
           05 PIPE-READ-STATUS USAGE POINTER.
              88 PIPE-GONE   VALUE NULL.
           05 PIPE-WRITE-STATUS USAGE BINARY-LONG.
       01  PIPE-STATUS USAGE BINARY-LONG.

       01  LEN PIC 99 VALUE 00.
       01  EOF PIC X VALUE 'F'.
       
       01  PRE   PIC X(8) VALUE 'echo -n'.
       01  DOOR-ID PIC A(3) VALUE 'abc'.
       01  INDX PIC 9(8) VALUE 0.
       01  POST PIC X(16) VALUE ' | md5sum > TEMP'.
       01  HASH PIC X(32).
       01  PSWD PIC X(8).
       01  CURR PIC 9 VALUE 1.

       01 DATE-TIME-DATA.
           05 CURRENT-DATE.
              10 DATE-YEAR  PIC 9(04).
              10 DATE-MONTH PIC 9(02).
              10 DATE-DAY   PIC 9(02).
           05 CURRENT-TIME.
              10 TIME-HRS   PIC 9(02).
              10 TIME-MIN   PIC 9(02).
              10 TIME-SEC   PIC 9(02).
              10 TIME-MS    PIC 9(02).

       PROCEDURE DIVISION.
       MAIN.
           PERFORM DISP-DT.
           PERFORM GET-PASSWORD.
           PERFORM DISP-DT.
           DISPLAY PSWD.

           GOBACK.

       DISP-DT.
           MOVE FUNCTION CURRENT-DATE TO DATE-TIME-DATA.
           DISPLAY
              DATE-YEAR'-'DATE-MONTH'-'DATE-DAY'@'
              TIME-HRS':'TIME-MIN':'TIME-SEC'.'TIME-MS
           END-DISPLAY.

       GET-PASSWORD.
           PERFORM UNTIL CURR > 8
              PERFORM MD5
      *       DISPLAY INDX': 'HASH
              IF FUNCTION MOD(INDX, 10000) = 0 THEN
                 DISPLAY INDX
              END-IF 
              IF HASH(1:5) IS EQUAL TO 00000 THEN
                 DISPLAY INDX
                 DISPLAY '    'CURR': 'HASH(6:1)
                 MOVE HASH(6:1) TO PSWD(CURR:1)
                 ADD 1 TO CURR
              END-IF
              ADD 1 TO INDX
           END-PERFORM.

       MD5.
           MOVE 0 TO LEN
           INSPECT INDX TALLYING LEN FOR LEADING ZEROS.
           MOVE FUNCTION 
              CONCATENATE(PRE DOOR-ID INDX(LEN + 1:8 - LEN) POST) TO
              PIPE-COMMAND
           MOVE PIPE-OPEN(PIPE-COMMAND, "r") TO PIPE-RECORD
           IF PIPE-RETURN IS NOT EQUAL TO 255 THEN
              MOVE PIPE-READ(PIPE-RECORD, PIPE-LINE) TO PIPE-RECORD-OUT
              MOVE PIPE-CLOSE(PIPE-RECORD) TO PIPE-STATUS 
              IF PIPE-STATUS IS EQUAL TO ZERO THEN
                 UNSTRING PIPE-LINE DELIMITED BY X"0A" INTO PIPE-LINE
                    COUNT IN READ-LENGTH
                 END-UNSTRING
                 MOVE PIPE-LINE(1:READ-LENGTH) TO HASH
              ELSE
                 DISPLAY "OOPS:" PIPE-STATUS UPON SYSERR
              END-IF
           END-IF.
      *    MOVE 0 TO LEN
      *    INSPECT INDX TALLYING LEN FOR LEADING ZEROS.
      **    DISPLAY LEN.
      **    DISPLAY DOOR-ID, INDX(LEN + 1:32 - LEN).
      *    CALL 'SYSTEM' USING FUNCTION
      *       CONCATENATE(PRE DOOR-ID INDX(LEN + 1:8 - LEN) POST).
      *    OPEN INPUT MD5-TEMP.
      *    READ MD5-TEMP INTO HASH.
      *    CLOSE MD5-TEMP.
      *    CALL 'CBL_DELETE_FILE' USING CONTENT 'TEMP'.



           

       END PROGRAM AOC-2016-D5P1.
