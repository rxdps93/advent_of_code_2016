       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2016-D5P1.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION PIPE-OPEN
           FUNCTION PIPE-READ
           FUNCTION PIPE-CLOSE
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
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
       
       01  PRE   PIC X(8) VALUE 'echo -n '.
       01  DOOR-ID PIC A(8) VALUE 'uqwqemis'.
       01  INDX PIC 9(8) VALUE 0.
       01  POST PIC X(9) VALUE ' | md5sum'.
       01  HASH PIC X(32).
       01  PSWD PIC X(8) VALUE '________'.
       01  POS  PIC 9.
       01  CURR PIC 9 VALUE 1.

       PROCEDURE DIVISION.
       MAIN.
           PERFORM GET-PASSWORD.
           DISPLAY PSWD.

           GOBACK.

       GET-PASSWORD.
           PERFORM UNTIL CURR > 8
              PERFORM MD5
              IF FUNCTION MOD(INDX, 1000) = 0 THEN
                 DISPLAY INDX
              END-IF 
              IF HASH(1:5) IS EQUAL TO 00000 THEN
                 DISPLAY INDX
                 DISPLAY '    'CURR': 'HASH(6:1)
                 IF HASH(6:1) IS NUMERIC AND
                    HASH(6:1) IS LESS THAN OR EQUAL TO 7 AND
                    PSWD(HASH(6:1):1) IS NOT EQUAL TO '_' THEN
                    MOVE HASH(6:1) TO POS
                    SUBTRACT ONE FROM POS
                    MOVE HASH(7:1) TO PSWD(POS:1)
                    ADD 1 TO CURR
                 END-IF
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

       END PROGRAM AOC-2016-D5P1.
