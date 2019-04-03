// Generated from Arboretum.g4 by ANTLR 4.7.2
// jshint ignore: start
var antlr4 = require('antlr4/index');



var serializedATN = ["\u0003\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964",
    "\u0002\rH\b\u0001\u0004\u0002\t\u0002\u0004\u0003\t\u0003\u0004\u0004",
    "\t\u0004\u0004\u0005\t\u0005\u0004\u0006\t\u0006\u0004\u0007\t\u0007",
    "\u0004\b\t\b\u0004\t\t\t\u0004\n\t\n\u0004\u000b\t\u000b\u0004\f\t\f",
    "\u0004\r\t\r\u0004\u000e\t\u000e\u0003\u0002\u0003\u0002\u0003\u0003",
    "\u0003\u0003\u0003\u0004\u0003\u0004\u0003\u0005\u0003\u0005\u0003\u0006",
    "\u0003\u0006\u0003\u0007\u0003\u0007\u0003\b\u0003\b\u0003\t\u0003\t",
    "\u0003\n\u0003\n\u0003\u000b\u0003\u000b\u0003\u000b\u0003\f\u0006\f",
    "4\n\f\r\f\u000e\f5\u0003\r\u0006\r9\n\r\r\r\u000e\r:\u0003\r\u0003\r",
    "\u0006\r?\n\r\r\r\u000e\r@\u0005\rC\n\r\u0003\u000e\u0003\u000e\u0003",
    "\u000e\u0003\u000e\u0002\u0002\u000f\u0003\u0003\u0005\u0004\u0007\u0005",
    "\t\u0002\u000b\u0002\r\u0006\u000f\u0007\u0011\b\u0013\t\u0015\n\u0017",
    "\u000b\u0019\f\u001b\r\u0003\u0002\u0004\u0004\u0002C\\c|\u0003\u0002",
    "2;\u0002I\u0002\u0003\u0003\u0002\u0002\u0002\u0002\u0005\u0003\u0002",
    "\u0002\u0002\u0002\u0007\u0003\u0002\u0002\u0002\u0002\r\u0003\u0002",
    "\u0002\u0002\u0002\u000f\u0003\u0002\u0002\u0002\u0002\u0011\u0003\u0002",
    "\u0002\u0002\u0002\u0013\u0003\u0002\u0002\u0002\u0002\u0015\u0003\u0002",
    "\u0002\u0002\u0002\u0017\u0003\u0002\u0002\u0002\u0002\u0019\u0003\u0002",
    "\u0002\u0002\u0002\u001b\u0003\u0002\u0002\u0002\u0003\u001d\u0003\u0002",
    "\u0002\u0002\u0005\u001f\u0003\u0002\u0002\u0002\u0007!\u0003\u0002",
    "\u0002\u0002\t#\u0003\u0002\u0002\u0002\u000b%\u0003\u0002\u0002\u0002",
    "\r\'\u0003\u0002\u0002\u0002\u000f)\u0003\u0002\u0002\u0002\u0011+\u0003",
    "\u0002\u0002\u0002\u0013-\u0003\u0002\u0002\u0002\u0015/\u0003\u0002",
    "\u0002\u0002\u00173\u0003\u0002\u0002\u0002\u00198\u0003\u0002\u0002",
    "\u0002\u001bD\u0003\u0002\u0002\u0002\u001d\u001e\u0007*\u0002\u0002",
    "\u001e\u0004\u0003\u0002\u0002\u0002\u001f \u0007+\u0002\u0002 \u0006",
    "\u0003\u0002\u0002\u0002!\"\u0007`\u0002\u0002\"\b\u0003\u0002\u0002",
    "\u0002#$\t\u0002\u0002\u0002$\n\u0003\u0002\u0002\u0002%&\t\u0003\u0002",
    "\u0002&\f\u0003\u0002\u0002\u0002\'(\u0007,\u0002\u0002(\u000e\u0003",
    "\u0002\u0002\u0002)*\u00071\u0002\u0002*\u0010\u0003\u0002\u0002\u0002",
    "+,\u0007-\u0002\u0002,\u0012\u0003\u0002\u0002\u0002-.\u0007/\u0002",
    "\u0002.\u0014\u0003\u0002\u0002\u0002/0\u0005\t\u0005\u000201\u0005",
    "\u000b\u0006\u00021\u0016\u0003\u0002\u0002\u000224\u0005\t\u0005\u0002",
    "32\u0003\u0002\u0002\u000245\u0003\u0002\u0002\u000253\u0003\u0002\u0002",
    "\u000256\u0003\u0002\u0002\u00026\u0018\u0003\u0002\u0002\u000279\u0005",
    "\u000b\u0006\u000287\u0003\u0002\u0002\u00029:\u0003\u0002\u0002\u0002",
    ":8\u0003\u0002\u0002\u0002:;\u0003\u0002\u0002\u0002;B\u0003\u0002\u0002",
    "\u0002<>\u00070\u0002\u0002=?\u0005\u000b\u0006\u0002>=\u0003\u0002",
    "\u0002\u0002?@\u0003\u0002\u0002\u0002@>\u0003\u0002\u0002\u0002@A\u0003",
    "\u0002\u0002\u0002AC\u0003\u0002\u0002\u0002B<\u0003\u0002\u0002\u0002",
    "BC\u0003\u0002\u0002\u0002C\u001a\u0003\u0002\u0002\u0002DE\u0007\"",
    "\u0002\u0002EF\u0003\u0002\u0002\u0002FG\b\u000e\u0002\u0002G\u001c",
    "\u0003\u0002\u0002\u0002\u0007\u00025:@B\u0003\b\u0002\u0002"].join("");


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
ArboretumLexer.T__2 = 3;
ArboretumLexer.ASTERISK = 4;
ArboretumLexer.SLASH = 5;
ArboretumLexer.PLUS = 6;
ArboretumLexer.MINUS = 7;
ArboretumLexer.ID = 8;
ArboretumLexer.NAME = 9;
ArboretumLexer.NUMBER = 10;
ArboretumLexer.WHITESPACE = 11;

ArboretumLexer.prototype.channelNames = [ "DEFAULT_TOKEN_CHANNEL", "HIDDEN" ];

ArboretumLexer.prototype.modeNames = [ "DEFAULT_MODE" ];

ArboretumLexer.prototype.literalNames = [ null, "'('", "')'", "'^'", "'*'", 
                                          "'/'", "'+'", "'-'", null, null, 
                                          null, "' '" ];

ArboretumLexer.prototype.symbolicNames = [ null, null, null, null, "ASTERISK", 
                                           "SLASH", "PLUS", "MINUS", "ID", 
                                           "NAME", "NUMBER", "WHITESPACE" ];

ArboretumLexer.prototype.ruleNames = [ "T__0", "T__1", "T__2", "LETTER", 
                                       "DIGIT", "ASTERISK", "SLASH", "PLUS", 
                                       "MINUS", "ID", "NAME", "NUMBER", 
                                       "WHITESPACE" ];

ArboretumLexer.prototype.grammarFileName = "Arboretum.g4";



exports.ArboretumLexer = ArboretumLexer;

