grammar Arboretum;

finalExpression     : expression EOF ;

expression          : '(' expression ')'                        #parenthesisExp
                    | expression (AND|OR|EQ) expression         #boolExp
                    | expression (PLUS|MINUS|TIMES) expression  #twoSidedConnNumExp
                    | bool
                    | int
                    | double
                    ;

fragment LETTER     : [a-zA-Z] ;
fragment DIGIT      : [0-9] ;

TIMES               : '*' ;
PLUS                : '+' ;
MINUS               : '-' ;

AND                 : '&&' ;
OR                  : '||' ;
EQ                  : '==' ;

BOOL                : True
                    | False
                    ;

VAR                 : LETTER+ ;

INT                 : DIGIT+ ;
DOUBLE              : DIGIT+ ('.' DIGIT+)? ;




WHITESPACE          : ' ' -> skip;