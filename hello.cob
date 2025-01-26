      * A minimal COBOL program to display "Hello, World!"
      * COBOL source format: 
      * Columns 1-6: Sequence number area (optional)
      * Column 7: Indicator area (* for comments)
      * Columns 8-11: Area A (divisions, sections, paragraphs)
      * Columns 12-72: Area B (statements, clauses)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       
       PROCEDURE DIVISION.
           DISPLAY "Hello, World!".
           STOP RUN.
