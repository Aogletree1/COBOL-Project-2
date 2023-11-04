       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PIZZA-INVENTORY-PROG.
       AUTHOR.     AUSTIN_OGLETREE.
      **********************************************************
      *  This program is designed to create a running inven-
      *  tory of pizza trucks for Rolling Pizza Trucks. 
      *  This program has been modified for Project 2.
      *  It does the above but with if statements and basic
      *  calculations to give total values of the products
      *  and inventory numbers.
         
      **********************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBMPC.
       OBJECT-COMPUTER.    IBMPC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PR2FA22-Inven  
               ASSIGN TO 'PR2FA22-Inven.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PIZZA-TRUCK-OUTPUT-FILE 
               ASSIGN TO 'PIZZA-TRUCK-OUTPUT.TXT'.
 
      *
       DATA DIVISION.
       FILE SECTION.


       FD  PR2FA22-Inven
           RECORD CONTAINS 40 CHARACTERS.
       01  PIZZA-TRUCK-REC.
           05  TRUCK-ID-IN        PIC X(5).
           05  EMPLOYEE-ID-IN     PIC X(4).
           05  EMPLOYEE-NAME-IN   PIC X(20).
           05  ITEM-ID-IN         PIC A(2).
           05  NUM-IN-STOCK-IN    PIC 9(3).
           05  PURCHASE-IN        PIC 99V99.
           05  SELLING-IN         PIC 99V99.
      *
       FD    PIZZA-TRUCK-OUTPUT-FILE
             RECORD CONTAINS 80 CHARACTERS.

       01    PIZZA-OUTPUT-REC            PIC X(80).
      *********
       WORKING-STORAGE SECTION.
       01    WS-WORK-AREAS.
             05    ARE-THERE-MORE-RECORDS    PIC X(3) VALUE 'YES'.
             05    CAL1                      PIC 999V99 VALUE 0  .
             05    NUM-IN-STOCK-TOTAL        PIC 9999            .
             05    PURCHASE-PRICE-TOTAL      PIC 99V99           .
             05    TOTAL-INVEN-COST          PIC 9999V99         .
       
       
       

      *************************OUTPUT AREA*****************************
       01 REPORT-HEADER.
          05 FILLER            PIC X(2)             .
          05 H1-DATE            PIC 9999/99/99       .

          05 FILLER            PIC X(23) VALUE SPACES.
          05 COMPANY-NAME      PIC X(13) VALUE 'ROLLING PIZZA'.

          05 FILLER            PIC X(19) VALUE SPACES.
          05 INITIALS          PIC X(3) VALUE 'AHO'.
      *
       01 REPORT-HEADER-2.
          05 FILLER          PIC X(33) VALUE SPACES.
          05 REPORT-LINE     PIC X(16) VALUE 'INVENTORY REPORT'.
          

       01 REPORT-HEADER-3.
          05 FILLER          PIC X(5) VALUE SPACES        .
          05 TRUCK           PIC X(5) VALUE 'TRUCK'       .

          05 FILLER          PIC X(11) VALUE SPACES       .
          05 ITEM            PIC X(4) VALUE 'ITEM'        .

          05 FILLER          PIC X(10) VALUE SPACES       .
          05 NUM-IN          PIC X(6) VALUE 'NUM IN'      .

          05 FILLER          PIC X(5) VALUE SPACES        .
          05 PURCHASE        PIC X(8) VALUE 'PURCHASE'    .

          05 FILLER          PIC X(5) VALUE SPACES        .
          05 TOTAL-INVEN     PIC X(11) VALUE 'TOTAL INVEN'.
          
       01 REPORT-HEADER-4.
          05 FILLER          PIC X(6) VALUE SPACES  .
          05 ID1              PIC X(2) VALUE 'ID'   .

          05 FILLER          PIC X(13) VALUE SPACES .
          05 NAME            PIC X(4) VALUE 'NAME'  .

          05 FILLER          PIC X(10) VALUE SPACES .
          05 STOCK           PIC X(5) VALUE 'STOCK' .

          05 FILLER          PIC X(7) VALUE SPACES  .
          05 PRICE           PIC X(5) VALUE 'PRICE' .

          05 FILLER          PIC X(10) VALUE SPACES .
          05 COST            PIC X(4) VALUE 'COST'  .
          

       01 DETAIL-LINE.
          05 FILLER          PIC X(5) VALUE SPACES   .
          05 TRUCK-ID-OUT     PIC X(5)               .
          
          05 FILLER           PIC X(5) VALUE SPACES  .
          05 ITEM-NAME-OUT      PIC X(15)            .
          
          05 FILLER           PIC X(7) VALUE SPACES  .
          05 NUM-IN-STOCK-OUT PIC ZZ9                .
          
          05 FILLER           PIC X(7) VALUE SPACES  .
          05 DOLLSIGN         PIC X    VALUE '$'     .
          05 FILLER2          PIC X    VALUE SPACES  .
          05 PURCHASE-PRICE-OUT     PIC Z9.99        .
          
          05 FILLER           PIC X(10) VALUE SPACES .
          05 DOLLSIGN         PIC X     VALUE '$'    .
          05 INVENTORY-OUT    PIC ZZ9.99             .

       01 TOTAL-LINE.
          05 FILLER          PIC  X(25) VALUE SPACES   .
          05 TOTALS          PIC  X(7)  VALUE 'TOTALS:'.
   
          05 FILLER          PIC  X(4)  VALUE SPACES   .
          05 TOTAL-IN-STOCK-OUT  PIC  ZZZ9             .
        
          05 FILLER          PIC  X(6)  VALUE SPACES   .
          05 DOLLAR1         PIC  X(1)  VALUE '$'      .
          05 PRICE-OUT       PIC  ZZ9.99               .

          05 FILLER          PIC  X(7)  VALUE SPACES   .
          05 DOLLAR2         PIC  X(1)  VALUE '$'      .
          05 INVEN-COST-OUT  PIC  Z,ZZ9.99             .
 
       PROCEDURE DIVISION.

       100-MAIN-MODULE.

           PERFORM 125-HOUSEKEEPING
           PERFORM 150-READ-RECORDS
           PERFORM 225-TOTAL-LINE-WRITE
           PERFORM 250-CLOSE-ROUTINE

           .
      *
       125-HOUSEKEEPING.

           OPEN    INPUT     PR2FA22-Inven
           OPEN    OUTPUT    PIZZA-TRUCK-OUTPUT-FILE
           
           ACCEPT H1-DATE FROM DATE YYYYMMDD
          


           MOVE REPORT-HEADER    TO PIZZA-OUTPUT-REC
           WRITE PIZZA-OUTPUT-REC
                   AFTER ADVANCING 2 LINE

           MOVE REPORT-HEADER-2  TO PIZZA-OUTPUT-REC
           WRITE PIZZA-OUTPUT-REC
                   AFTER ADVANCING 2 LINE

           MOVE REPORT-HEADER-3      TO PIZZA-OUTPUT-REC
           WRITE PIZZA-OUTPUT-REC
                   AFTER ADVANCING 3 LINE

           MOVE REPORT-HEADER-4          TO PIZZA-OUTPUT-REC
           WRITE PIZZA-OUTPUT-REC
                   AFTER ADVANCING 1 LINE
            
                  .
      *
       150-READ-RECORDS.

             PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO'
                  READ PR2FA22-Inven 
                      AT END
                          MOVE 'NO' TO ARE-THERE-MORE-RECORDS
                      NOT AT END
                          PERFORM 200-PROCESS-RTN
                  END-READ
              END-PERFORM

              
           .
      *
       200-PROCESS-RTN.

      * THIS FIRST IF CHECKS FOR THE CORRECT INPUT.

              IF TRUCK-ID-IN EQUAL 'P100A' OR
                 'P200G' OR 'P300H'
                 MOVE TRUCK-ID-IN TO TRUCK-ID-OUT
              ELSE
                 MOVE 'ERROR' TO TRUCK-ID-OUT
                 END-IF
              
      * THESE IF STATEMENTS CHECKS AND THEN MOVES
      * THE CORRECT PIZZA TYPES TO THE RIGHT PLACES

              IF ITEM-ID-IN EQUALS 'CH'
                MOVE 'CHEESE PIZZA' TO ITEM-NAME-OUT
                END-IF

              IF ITEM-ID-IN EQUALS 'PP'
                MOVE 'PEPPERONI PIZZA' TO ITEM-NAME-OUT
                END-IF

              IF ITEM-ID-IN EQUALS 'SA'
                MOVE 'SAUSAGE PIZZA' TO ITEM-NAME-OUT
                END-IF

              IF ITEM-ID-IN EQUALS 'SU'
                MOVE 'SUPREME PIZZA' TO ITEM-NAME-OUT
                END-IF

              IF ITEM-ID-IN EQUALS 'PR'
                MOVE 'PRETZEL' TO ITEM-NAME-OUT
                END-IF

      *THESE IF STATEMENTS MAKE SURE ON NUMERIC DATA IS READ  

              IF NUM-IN-STOCK-IN IS NUMERIC
                 ADD NUM-IN-STOCK-IN TO NUM-IN-STOCK-TOTAL
                 ELSE MOVE '0' TO NUM-IN-STOCK-IN
                 END-IF

              IF PURCHASE-IN IS NUMERIC
                 ADD PURCHASE-IN TO PURCHASE-PRICE-TOTAL
                 ELSE MOVE '0' TO PURCHASE-IN
                 END-IF

              MOVE NUM-IN-STOCK-IN    TO NUM-IN-STOCK-OUT
              MOVE PURCHASE-IN        TO PURCHASE-PRICE-OUT

      * SIMPLE CALCULATION FOR OUTPUT, GIVES TOTAL SALE OF STOCK              

              COMPUTE CAL1 = NUM-IN-STOCK-IN * PURCHASE-IN


              MOVE CAL1       TO INVENTORY-OUT
              ADD  CAL1       TO TOTAL-INVEN-COST
 
              

              MOVE DETAIL-LINE TO PIZZA-OUTPUT-REC
              
                    
              WRITE PIZZA-OUTPUT-REC 
                     AFTER ADVANCING 1 LINE


           .

       
       225-TOTAL-LINE-WRITE.
           
      * WRITES THE TOTAL LINE, PUT OUTSIDE THE LOOP

           MOVE NUM-IN-STOCK-TOTAL TO TOTAL-IN-STOCK-OUT
           MOVE PURCHASE-PRICE-TOTAL TO PRICE-OUT
           MOVE TOTAL-INVEN-COST TO INVEN-COST-OUT

           MOVE TOTAL-LINE TO PIZZA-OUTPUT-REC
           WRITE PIZZA-OUTPUT-REC 
                 AFTER ADVANCING 2 LINE

           .


       250-CLOSE-ROUTINE.


              CLOSE    PR2FA22-Inven
                       PIZZA-TRUCK-OUTPUT-FILE

              STOP RUN
           .


