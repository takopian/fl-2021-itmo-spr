grammar Expr;

start : e EOF;

e : <assoc=right> e op=POW e # Pow
  | e op=(PLUS | MINUS) e    # Sum
  | NUM                      # Num
  ;

PLUS : '+';

MINUS : '-';

POW : '^';

NUM : '0' | ('1' .. '9') ('0' .. '9')*;

WS : [ \t\r\n]+ -> skip ;

