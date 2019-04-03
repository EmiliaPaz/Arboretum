// Generated from Arboretum.g4 by ANTLR 4.7.2
// jshint ignore: start
var antlr4 = require('antlr4/index');



var serializedATN = ["\u0003\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964",
    "\u0002\u000f^\b\u0001\u0004\u0002\t\u0002\u0004\u0003\t\u0003\u0004",
    "\u0004\t\u0004\u0004\u0005\t\u0005\u0004\u0006\t\u0006\u0004\u0007\t",
    "\u0007\u0004\b\t\b\u0004\t\t\t\u0004\n\t\n\u0004\u000b\t\u000b\u0004",
    "\f\t\f\u0004\r\t\r\u0004\u000e\t\u000e\u0004\u000f\t\u000f\u0004\u0010",
    "\t\u0010\u0003\u0002\u0003\u0002\u0003\u0003\u0003\u0003\u0003\u0004",
    "\u0003\u0004\u0003\u0005\u0003\u0005\u0003\u0006\u0003\u0006\u0003\u0007",
    "\u0003\u0007\u0003\b\u0003\b\u0003\t\u0003\t\u0003\t\u0003\n\u0003\n",
    "\u0003\n\u0003\u000b\u0003\u000b\u0003\u000b\u0003\f\u0003\f\u0003\f",
    "\u0003\f\u0003\f\u0003\f\u0003\f\u0003\f\u0003\f\u0005\fB\n\f\u0003",
    "\r\u0006\rE\n\r\r\r\u000e\rF\u0003\u000e\u0006\u000eJ\n\u000e\r\u000e",
    "\u000e\u000eK\u0003\u000f\u0006\u000fO\n\u000f\r\u000f\u000e\u000fP",
    "\u0003\u000f\u0003\u000f\u0006\u000fU\n\u000f\r\u000f\u000e\u000fV\u0005",
    "\u000fY\n\u000f\u0003\u0010\u0003\u0010\u0003\u0010\u0003\u0010\u0002",
    "\u0002\u0011\u0003\u0003\u0005\u0004\u0007\u0002\t\u0002\u000b\u0005",
    "\r\u0006\u000f\u0007\u0011\b\u0013\t\u0015\n\u0017\u000b\u0019\f\u001b",
    "\r\u001d\u000e\u001f\u000f\u0003\u0002\u0004\u0004\u0002C\\c|\u0003",
    "\u00022;\u0002a\u0002\u0003\u0003\u0002\u0002\u0002\u0002\u0005\u0003",
    "\u0002\u0002\u0002\u0002\u000b\u0003\u0002\u0002\u0002\u0002\r\u0003",
    "\u0002\u0002\u0002\u0002\u000f\u0003\u0002\u0002\u0002\u0002\u0011\u0003",
    "\u0002\u0002\u0002\u0002\u0013\u0003\u0002\u0002\u0002\u0002\u0015\u0003",
    "\u0002\u0002\u0002\u0002\u0017\u0003\u0002\u0002\u0002\u0002\u0019\u0003",
    "\u0002\u0002\u0002\u0002\u001b\u0003\u0002\u0002\u0002\u0002\u001d\u0003",
    "\u0002\u0002\u0002\u0002\u001f\u0003\u0002\u0002\u0002\u0003!\u0003",
    "\u0002\u0002\u0002\u0005#\u0003\u0002\u0002\u0002\u0007%\u0003\u0002",
    "\u0002\u0002\t\'\u0003\u0002\u0002\u0002\u000b)\u0003\u0002\u0002\u0002",
    "\r+\u0003\u0002\u0002\u0002\u000f-\u0003\u0002\u0002\u0002\u0011/\u0003",
    "\u0002\u0002\u0002\u00132\u0003\u0002\u0002\u0002\u00155\u0003\u0002",
    "\u0002\u0002\u0017A\u0003\u0002\u0002\u0002\u0019D\u0003\u0002\u0002",
    "\u0002\u001bI\u0003\u0002\u0002\u0002\u001dN\u0003\u0002\u0002\u0002",
    "\u001fZ\u0003\u0002\u0002\u0002!\"\u0007*\u0002\u0002\"\u0004\u0003",
    "\u0002\u0002\u0002#$\u0007+\u0002\u0002$\u0006\u0003\u0002\u0002\u0002",
    "%&\t\u0002\u0002\u0002&\b\u0003\u0002\u0002\u0002\'(\t\u0003\u0002\u0002",
    "(\n\u0003\u0002\u0002\u0002)*\u0007,\u0002\u0002*\f\u0003\u0002\u0002",
    "\u0002+,\u0007-\u0002\u0002,\u000e\u0003\u0002\u0002\u0002-.\u0007/",
    "\u0002\u0002.\u0010\u0003\u0002\u0002\u0002/0\u0007(\u0002\u000201\u0007",
    "(\u0002\u00021\u0012\u0003\u0002\u0002\u000223\u0007~\u0002\u000234",
    "\u0007~\u0002\u00024\u0014\u0003\u0002\u0002\u000256\u0007?\u0002\u0002",
    "67\u0007?\u0002\u00027\u0016\u0003\u0002\u0002\u000289\u0007V\u0002",
    "\u00029:\u0007t\u0002\u0002:;\u0007w\u0002\u0002;B\u0007g\u0002\u0002",
    "<=\u0007H\u0002\u0002=>\u0007c\u0002\u0002>?\u0007n\u0002\u0002?@\u0007",
    "u\u0002\u0002@B\u0007g\u0002\u0002A8\u0003\u0002\u0002\u0002A<\u0003",
    "\u0002\u0002\u0002B\u0018\u0003\u0002\u0002\u0002CE\u0005\u0007\u0004",
    "\u0002DC\u0003\u0002\u0002\u0002EF\u0003\u0002\u0002\u0002FD\u0003\u0002",
    "\u0002\u0002FG\u0003\u0002\u0002\u0002G\u001a\u0003\u0002\u0002\u0002",
    "HJ\u0005\t\u0005\u0002IH\u0003\u0002\u0002\u0002JK\u0003\u0002\u0002",
    "\u0002KI\u0003\u0002\u0002\u0002KL\u0003\u0002\u0002\u0002L\u001c\u0003",
    "\u0002\u0002\u0002MO\u0005\t\u0005\u0002NM\u0003\u0002\u0002\u0002O",
    "P\u0003\u0002\u0002\u0002PN\u0003\u0002\u0002\u0002PQ\u0003\u0002\u0002",
    "\u0002QX\u0003\u0002\u0002\u0002RT\u00070\u0002\u0002SU\u0005\t\u0005",
    "\u0002TS\u0003\u0002\u0002\u0002UV\u0003\u0002\u0002\u0002VT\u0003\u0002",
    "\u0002\u0002VW\u0003\u0002\u0002\u0002WY\u0003\u0002\u0002\u0002XR\u0003",
    "\u0002\u0002\u0002XY\u0003\u0002\u0002\u0002Y\u001e\u0003\u0002\u0002",
    "\u0002Z[\u0007\"\u0002\u0002[\\\u0003\u0002\u0002\u0002\\]\b\u0010\u0002",
    "\u0002] \u0003\u0002\u0002\u0002\t\u0002AFKPVX\u0003\b\u0002\u0002"].join("");


var atn = new antlr4.atn.ATNDeserializer().deserialize(serializedATN);

var decisionsToDFA = atn.decisionToState.map( function(ds, index) { return new antlr4.dfa.DFA(ds, index); });

function ArboretumLexer(input) {
	antlr4.Lexer.call(this, input);
    this._interp = new antlr4.atn.LexerATNSimulator(this, atn, decisionsToDFA, new antlr4.PredictionContextCache());
    return this;
}

ArboretumLexer.prototype = Object.create(antlr4.Lexer.prototype);
ArboretumLexer.prototype.constructor = ArboretumLexer;

Object.defineProperty(ArboretumLexer.prototype, "atn", {
        get : function() {
                return atn;
        }
});

ArboretumLexer.EOF = antlr4.Token.EOF;
ArboretumLexer.T__0 = 1;
ArboretumLexer.T__1 = 2;
ArboretumLexer.TIMES = 3;
ArboretumLexer.PLUS = 4;
ArboretumLexer.MINUS = 5;
ArboretumLexer.AND = 6;
ArboretumLexer.OR = 7;
ArboretumLexer.EQ = 8;
ArboretumLexer.BOOL = 9;
ArboretumLexer.VAR = 10;
ArboretumLexer.INT = 11;
ArboretumLexer.DOUBLE = 12;
ArboretumLexer.WHITESPACE = 13;

ArboretumLexer.prototype.channelNames = [ "DEFAULT_TOKEN_CHANNEL", "HIDDEN" ];

ArboretumLexer.prototype.modeNames = [ "DEFAULT_MODE" ];

ArboretumLexer.prototype.literalNames = [ null, "'('", "')'", "'*'", "'+'", 
                                          "'-'", "'&&'", "'||'", "'=='", 
                                          null, null, null, null, "' '" ];

ArboretumLexer.prototype.symbolicNames = [ null, null, null, "TIMES", "PLUS", 
                                           "MINUS", "AND", "OR", "EQ", "BOOL", 
                                           "VAR", "INT", "DOUBLE", "WHITESPACE" ];

ArboretumLexer.prototype.ruleNames = [ "T__0", "T__1", "LETTER", "DIGIT", 
                                       "TIMES", "PLUS", "MINUS", "AND", 
                                       "OR", "EQ", "BOOL", "VAR", "INT", 
                                       "DOUBLE", "WHITESPACE" ];

ArboretumLexer.prototype.grammarFileName = "Arboretum.g4";



exports.ArboretumLexer = ArboretumLexer;

