grammar Expr;

start : e EOF;

e : f (PLUS f)*;
f : NUM;

PLUS : '+';

NUM : '0' | ('1' .. '9') ('0' .. '9')*;

WS : [ \t\r\n]+ -> skip ;

