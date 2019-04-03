grammar Arboretum;

/************************************************************************************
 * Parser Rules
 ************************************************************************************
 */

finalExpression     : expression ;

expression          : '(' expression ')'                        #parenthesisExp
                    | expression (AND|OR|EQ) expression         #twoSidedConnBoolExp
                    | expression (PLUS|MINUS|TIMES) expression  #twoSidedConnNumExp
                    | BOOL                                      #boolExp
                    | INT                                       #intExp
                    | DOUBLE                                    #doubleExp
                    ;

/************************************************************************************
 * Lexer Rules
 ************************************************************************************
 */

fragment LETTER     : [a-zA-Z] ;
fragment DIGIT      : [0-9] ;

TIMES               : '*' ;
PLUS                : '+' ;
MINUS               : '-' ;

AND                 : '&&' ;
OR                  : '||' ;
EQ                  : '==' ;

BOOL                : 'True'
                    | 'False'
                    ;

VAR                 : LETTER+ ;

INT                 : DIGIT+ ;
DOUBLE              : DIGIT+ ('.' DIGIT+)? ;




WHITESPACE          : ' ' -> skip;