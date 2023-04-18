       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2016-D5P1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MD5-TEMP ASSIGN TO 'TEMP'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION. 
       FD MD5-TEMP 
           DATA RECORD IS MD5-REC.
       01  MD5-REC PIC X(32).

       WORKING-STORAGE SECTION.
       01  LEN PIC 99 VALUE 00.
       01  EOF PIC X VALUE 'F'.
       01  PRE   PIC X(8) VALUE 'echo -n'.
       01  DOOR-ID PIC X(3) VALUE 'abc'.
       01  INDX PIC 9(32) VALUE 0.
       01  POST PIC X(16) VALUE ' | md5sum > TEMP'.
       01  HASH PIC X(32).
       01  PSWD PIC X(8).
       01  CURR PIC 9 VALUE 1.

       PROCEDURE DIVISION.
       MAIN.
           PERFORM GET-PASSWORD.
           DISPLAY PSWD.

           GOBACK.

       GET-PASSWORD.
           PERFORM UNTIL CURR > 8
              PERFORM MD5
              DISPLAY INDX': 'HASH
              IF HASH(1:5) IS EQUAL TO 00000 THEN
                 DISPLAY CURR': 'HASH(6:1)
                 MOVE HASH(6:1) TO PSWD(CURR:1)
                 ADD 1 TO CURR
              END-IF
              ADD 1 TO INDX
           END-PERFORM.

       MD5.
           MOVE 0 TO LEN
           INSPECT INDX TALLYING LEN FOR LEADING ZEROS.
      *    DISPLAY LEN.
      *    DISPLAY DOOR-ID, INDX(LEN + 1:32 - LEN).
           CALL 'SYSTEM' USING FUNCTION
              CONCATENATE(PRE DOOR-ID INDX(LEN + 1:32 - LEN) POST).
           OPEN INPUT MD5-TEMP.
           READ MD5-TEMP INTO HASH.
           CLOSE MD5-TEMP.
           CALL 'CBL_DELETE_FILE' USING CONTENT 'TEMP'.



           

       END PROGRAM AOC-2016-D5P1.
