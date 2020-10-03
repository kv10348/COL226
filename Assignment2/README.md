
In Assignment 2: you only need to provide the lex specifications, and so do not need to concern yourself with the meaning and implementation of the operations and how they are used.
You need to write clear specifications of the OCaml functions which you implement, and document your code.
Tokens for OCamllex would be, but not limited to:
Float constants (with optional sign, no redundant initial zeroes before the decimal point or unnecessary trailing zeroes after the decimal point)
Parenthesis — ( and )
Brackets — [ and ]
Comma — ,
Colon —  :
Indices I — [ i , j ]
Ranges R — ( I : I )
Unary operators: SUM, AVG, MIN, MAX, COUNT, etc. (see below)
Binary operators: ADD (addition), SUBT (subtraction), MULT (multiplication), DIV (division)
Assignment operator  :=
Formula termination  ; (semicolon) 
The function operations can be one of the following:



Type 1 (unary operations on ranges of cells):

COUNT
ROWCOUNT
COLCOUNT
SUM
ROWSUM
COLSUM
AVG
ROWAVG
COLAVG
MIN
ROWMIN
COLMIN
MAX
ROWMAX
COLMAX
Type 2 (binary operations on ranges of cells):

ADD
SUBT
MULT
DIV
