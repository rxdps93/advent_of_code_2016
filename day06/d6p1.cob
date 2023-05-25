       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2016-D6P1.

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
       01 INPUT-LINE   PIC A(8).

       WORKING-STORAGE SECTION. 
       01  LOOP     PIC 9       VALUE 1.
       01  FREQ     PIC 99    VALUE 0.
       01  OFFSET   PIC 9(9).

       PROCEDURE DIVISION.
       MAIN.
           MOVE FUNCTION ORD('a') TO OFFSET.
           SUBTRACT 1 FROM OFFSET.
           OPEN INPUT INPUTFILE.
           PERFORM UNTIL LOOP = 0
              READ INPUTFILE NEXT RECORD INTO INPUT-LINE
              AT END
                 MOVE 0 TO LOOP
              NOT AT END
                 DISPLAY INPUT-LINE
                 SUBTRACT
                    OFFSET FROM
                    FUNCTION ORD(INPUT-LINE(1:1))
                    GIVING FREQ
                 DISPLAY '    'FREQ
                 ADD 1 TO FREQ
                 DISPLAY '    'FUNCTION CHAR(FREQ)
              END-READ
           END-PERFORM
           CLOSE INPUTFILE.

           GOBACK.

       END PROGRAM AOC-2016-D6P1.
