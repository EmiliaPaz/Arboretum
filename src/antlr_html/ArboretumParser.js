// Generated from Arboretum.g4 by ANTLR 4.7.2
// jshint ignore: start
var antlr4 = require('antlr4/index');
var ArboretumListener = require('./ArboretumListener').ArboretumListener;
var grammarFileName = "Arboretum.g4";


var serializedATN = ["\u0003\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964",
    "\u0003\u000f\u001f\u0004\u0002\t\u0002\u0004\u0003\t\u0003\u0003\u0002",
    "\u0003\u0002\u0003\u0002\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0005\u0003\u0012\n",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0007\u0003\u001a\n\u0003\f\u0003\u000e\u0003\u001d\u000b\u0003",
    "\u0003\u0003\u0002\u0003\u0004\u0004\u0002\u0004\u0002\u0004\u0003\u0002",
    "\b\n\u0003\u0002\u0005\u0007\u0002!\u0002\u0006\u0003\u0002\u0002\u0002",
    "\u0004\u0011\u0003\u0002\u0002\u0002\u0006\u0007\u0005\u0004\u0003\u0002",
    "\u0007\b\u0007\u0002\u0002\u0003\b\u0003\u0003\u0002\u0002\u0002\t\n",
    "\b\u0003\u0001\u0002\n\u000b\u0007\u0003\u0002\u0002\u000b\f\u0005\u0004",
    "\u0003\u0002\f\r\u0007\u0004\u0002\u0002\r\u0012\u0003\u0002\u0002\u0002",
    "\u000e\u0012\u0007\u000b\u0002\u0002\u000f\u0012\u0007\r\u0002\u0002",
    "\u0010\u0012\u0007\u000e\u0002\u0002\u0011\t\u0003\u0002\u0002\u0002",
    "\u0011\u000e\u0003\u0002\u0002\u0002\u0011\u000f\u0003\u0002\u0002\u0002",
    "\u0011\u0010\u0003\u0002\u0002\u0002\u0012\u001b\u0003\u0002\u0002\u0002",
    "\u0013\u0014\f\u0007\u0002\u0002\u0014\u0015\t\u0002\u0002\u0002\u0015",
    "\u001a\u0005\u0004\u0003\b\u0016\u0017\f\u0006\u0002\u0002\u0017\u0018",
    "\t\u0003\u0002\u0002\u0018\u001a\u0005\u0004\u0003\u0007\u0019\u0013",
    "\u0003\u0002\u0002\u0002\u0019\u0016\u0003\u0002\u0002\u0002\u001a\u001d",
    "\u0003\u0002\u0002\u0002\u001b\u0019\u0003\u0002\u0002\u0002\u001b\u001c",
    "\u0003\u0002\u0002\u0002\u001c\u0005\u0003\u0002\u0002\u0002\u001d\u001b",
    "\u0003\u0002\u0002\u0002\u0005\u0011\u0019\u001b"].join("");


var atn = new antlr4.atn.ATNDeserializer().deserialize(serializedATN);

var decisionsToDFA = atn.decisionToState.map( function(ds, index) { return new antlr4.dfa.DFA(ds, index); });

var sharedContextCache = new antlr4.PredictionContextCache();

var literalNames = [ null, "'('", "')'", "'*'", "'+'", "'-'", "'&&'", "'||'", 
                     "'=='", null, null, null, null, "' '" ];

var symbolicNames = [ null, null, null, "TIMES", "PLUS", "MINUS", "AND", 
                      "OR", "EQ", "BOOL", "VAR", "INT", "DOUBLE", "WHITESPACE" ];

var ruleNames =  [ "finalExpression", "expression" ];

function ArboretumParser (input) {
	antlr4.Parser.call(this, input);
    this._interp = new antlr4.atn.ParserATNSimulator(this, atn, decisionsToDFA, sharedContextCache);
    this.ruleNames = ruleNames;
    this.literalNames = literalNames;
    this.symbolicNames = symbolicNames;
    return this;
}

ArboretumParser.prototype = Object.create(antlr4.Parser.prototype);
ArboretumParser.prototype.constructor = ArboretumParser;

Object.defineProperty(ArboretumParser.prototype, "atn", {
	get : function() {
		return atn;
	}
});

ArboretumParser.EOF = antlr4.Token.EOF;
ArboretumParser.T__0 = 1;
ArboretumParser.T__1 = 2;
ArboretumParser.TIMES = 3;
ArboretumParser.PLUS = 4;
ArboretumParser.MINUS = 5;
ArboretumParser.AND = 6;
ArboretumParser.OR = 7;
ArboretumParser.EQ = 8;
ArboretumParser.BOOL = 9;
ArboretumParser.VAR = 10;
ArboretumParser.INT = 11;
ArboretumParser.DOUBLE = 12;
ArboretumParser.WHITESPACE = 13;

ArboretumParser.RULE_finalExpression = 0;
ArboretumParser.RULE_expression = 1;


function FinalExpressionContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = ArboretumParser.RULE_finalExpression;
    return this;
}

FinalExpressionContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
FinalExpressionContext.prototype.constructor = FinalExpressionContext;

FinalExpressionContext.prototype.expression = function() {
    return this.getTypedRuleContext(ExpressionContext,0);
};

FinalExpressionContext.prototype.EOF = function() {
    return this.getToken(ArboretumParser.EOF, 0);
};

FinalExpressionContext.prototype.enterRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.enterFinalExpression(this);
	}
};

FinalExpressionContext.prototype.exitRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.exitFinalExpression(this);
	}
};




ArboretumParser.FinalExpressionContext = FinalExpressionContext;

ArboretumParser.prototype.finalExpression = function() {

    var localctx = new FinalExpressionContext(this, this._ctx, this.state);
    this.enterRule(localctx, 0, ArboretumParser.RULE_finalExpression);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 4;
        this.expression(0);
        this.state = 5;
        this.match(ArboretumParser.EOF);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function ExpressionContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = ArboretumParser.RULE_expression;
    return this;
}

ExpressionContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ExpressionContext.prototype.constructor = ExpressionContext;


 
ExpressionContext.prototype.copyFrom = function(ctx) {
    antlr4.ParserRuleContext.prototype.copyFrom.call(this, ctx);
};

function TwoSidedConnNumExpContext(parser, ctx) {
	ExpressionContext.call(this, parser);
    ExpressionContext.prototype.copyFrom.call(this, ctx);
    return this;
}

TwoSidedConnNumExpContext.prototype = Object.create(ExpressionContext.prototype);
TwoSidedConnNumExpContext.prototype.constructor = TwoSidedConnNumExpContext;

ArboretumParser.TwoSidedConnNumExpContext = TwoSidedConnNumExpContext;

TwoSidedConnNumExpContext.prototype.expression = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExpressionContext);
    } else {
        return this.getTypedRuleContext(ExpressionContext,i);
    }
};

TwoSidedConnNumExpContext.prototype.PLUS = function() {
    return this.getToken(ArboretumParser.PLUS, 0);
};

TwoSidedConnNumExpContext.prototype.MINUS = function() {
    return this.getToken(ArboretumParser.MINUS, 0);
};

TwoSidedConnNumExpContext.prototype.TIMES = function() {
    return this.getToken(ArboretumParser.TIMES, 0);
};
TwoSidedConnNumExpContext.prototype.enterRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.enterTwoSidedConnNumExp(this);
	}
};

TwoSidedConnNumExpContext.prototype.exitRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.exitTwoSidedConnNumExp(this);
	}
};


function ParenthesisExpContext(parser, ctx) {
	ExpressionContext.call(this, parser);
    ExpressionContext.prototype.copyFrom.call(this, ctx);
    return this;
}

ParenthesisExpContext.prototype = Object.create(ExpressionContext.prototype);
ParenthesisExpContext.prototype.constructor = ParenthesisExpContext;

ArboretumParser.ParenthesisExpContext = ParenthesisExpContext;

ParenthesisExpContext.prototype.expression = function() {
    return this.getTypedRuleContext(ExpressionContext,0);
};
ParenthesisExpContext.prototype.enterRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.enterParenthesisExp(this);
	}
};

ParenthesisExpContext.prototype.exitRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.exitParenthesisExp(this);
	}
};


function BoolExpContext(parser, ctx) {
	ExpressionContext.call(this, parser);
    ExpressionContext.prototype.copyFrom.call(this, ctx);
    return this;
}

BoolExpContext.prototype = Object.create(ExpressionContext.prototype);
BoolExpContext.prototype.constructor = BoolExpContext;

ArboretumParser.BoolExpContext = BoolExpContext;

BoolExpContext.prototype.BOOL = function() {
    return this.getToken(ArboretumParser.BOOL, 0);
};
BoolExpContext.prototype.enterRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.enterBoolExp(this);
	}
};

BoolExpContext.prototype.exitRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.exitBoolExp(this);
	}
};


function TwoSidedConnBoolExpContext(parser, ctx) {
	ExpressionContext.call(this, parser);
    ExpressionContext.prototype.copyFrom.call(this, ctx);
    return this;
}

TwoSidedConnBoolExpContext.prototype = Object.create(ExpressionContext.prototype);
TwoSidedConnBoolExpContext.prototype.constructor = TwoSidedConnBoolExpContext;

ArboretumParser.TwoSidedConnBoolExpContext = TwoSidedConnBoolExpContext;

TwoSidedConnBoolExpContext.prototype.expression = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExpressionContext);
    } else {
        return this.getTypedRuleContext(ExpressionContext,i);
    }
};

TwoSidedConnBoolExpContext.prototype.AND = function() {
    return this.getToken(ArboretumParser.AND, 0);
};

TwoSidedConnBoolExpContext.prototype.OR = function() {
    return this.getToken(ArboretumParser.OR, 0);
};

TwoSidedConnBoolExpContext.prototype.EQ = function() {
    return this.getToken(ArboretumParser.EQ, 0);
};
TwoSidedConnBoolExpContext.prototype.enterRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.enterTwoSidedConnBoolExp(this);
	}
};

TwoSidedConnBoolExpContext.prototype.exitRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.exitTwoSidedConnBoolExp(this);
	}
};


function IntExpContext(parser, ctx) {
	ExpressionContext.call(this, parser);
    ExpressionContext.prototype.copyFrom.call(this, ctx);
    return this;
}

IntExpContext.prototype = Object.create(ExpressionContext.prototype);
IntExpContext.prototype.constructor = IntExpContext;

ArboretumParser.IntExpContext = IntExpContext;

IntExpContext.prototype.INT = function() {
    return this.getToken(ArboretumParser.INT, 0);
};
IntExpContext.prototype.enterRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.enterIntExp(this);
	}
};

IntExpContext.prototype.exitRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.exitIntExp(this);
	}
};


function DoubleExpContext(parser, ctx) {
	ExpressionContext.call(this, parser);
    ExpressionContext.prototype.copyFrom.call(this, ctx);
    return this;
}

DoubleExpContext.prototype = Object.create(ExpressionContext.prototype);
DoubleExpContext.prototype.constructor = DoubleExpContext;

ArboretumParser.DoubleExpContext = DoubleExpContext;

DoubleExpContext.prototype.DOUBLE = function() {
    return this.getToken(ArboretumParser.DOUBLE, 0);
};
DoubleExpContext.prototype.enterRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.enterDoubleExp(this);
	}
};

DoubleExpContext.prototype.exitRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.exitDoubleExp(this);
	}
};



ArboretumParser.prototype.expression = function(_p) {
	if(_p===undefined) {
	    _p = 0;
	}
    var _parentctx = this._ctx;
    var _parentState = this.state;
    var localctx = new ExpressionContext(this, this._ctx, _parentState);
    var _prevctx = localctx;
    var _startState = 2;
    this.enterRecursionRule(localctx, 2, ArboretumParser.RULE_expression, _p);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 15;
        this._errHandler.sync(this);
        switch(this._input.LA(1)) {
        case ArboretumParser.T__0:
            localctx = new ParenthesisExpContext(this, localctx);
            this._ctx = localctx;
            _prevctx = localctx;

            this.state = 8;
            this.match(ArboretumParser.T__0);
            this.state = 9;
            this.expression(0);
            this.state = 10;
            this.match(ArboretumParser.T__1);
            break;
        case ArboretumParser.BOOL:
            localctx = new BoolExpContext(this, localctx);
            this._ctx = localctx;
            _prevctx = localctx;
            this.state = 12;
            this.match(ArboretumParser.BOOL);
            break;
        case ArboretumParser.INT:
            localctx = new IntExpContext(this, localctx);
            this._ctx = localctx;
            _prevctx = localctx;
            this.state = 13;
            this.match(ArboretumParser.INT);
            break;
        case ArboretumParser.DOUBLE:
            localctx = new DoubleExpContext(this, localctx);
            this._ctx = localctx;
            _prevctx = localctx;
            this.state = 14;
            this.match(ArboretumParser.DOUBLE);
            break;
        default:
            throw new antlr4.error.NoViableAltException(this);
        }
        this._ctx.stop = this._input.LT(-1);
        this.state = 25;
        this._errHandler.sync(this);
        var _alt = this._interp.adaptivePredict(this._input,2,this._ctx)
        while(_alt!=2 && _alt!=antlr4.atn.ATN.INVALID_ALT_NUMBER) {
            if(_alt===1) {
                if(this._parseListeners!==null) {
                    this.triggerExitRuleEvent();
                }
                _prevctx = localctx;
                this.state = 23;
                this._errHandler.sync(this);
                var la_ = this._interp.adaptivePredict(this._input,1,this._ctx);
                switch(la_) {
                case 1:
                    localctx = new TwoSidedConnBoolExpContext(this, new ExpressionContext(this, _parentctx, _parentState));
                    this.pushNewRecursionContext(localctx, _startState, ArboretumParser.RULE_expression);
                    this.state = 17;
                    if (!( this.precpred(this._ctx, 5))) {
                        throw new antlr4.error.FailedPredicateException(this, "this.precpred(this._ctx, 5)");
                    }
                    this.state = 18;
                    _la = this._input.LA(1);
                    if(!((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << ArboretumParser.AND) | (1 << ArboretumParser.OR) | (1 << ArboretumParser.EQ))) !== 0))) {
                    this._errHandler.recoverInline(this);
                    }
                    else {
                    	this._errHandler.reportMatch(this);
                        this.consume();
                    }
                    this.state = 19;
                    this.expression(6);
                    break;

                case 2:
                    localctx = new TwoSidedConnNumExpContext(this, new ExpressionContext(this, _parentctx, _parentState));
                    this.pushNewRecursionContext(localctx, _startState, ArboretumParser.RULE_expression);
                    this.state = 20;
                    if (!( this.precpred(this._ctx, 4))) {
                        throw new antlr4.error.FailedPredicateException(this, "this.precpred(this._ctx, 4)");
                    }
                    this.state = 21;
                    _la = this._input.LA(1);
                    if(!((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << ArboretumParser.TIMES) | (1 << ArboretumParser.PLUS) | (1 << ArboretumParser.MINUS))) !== 0))) {
                    this._errHandler.recoverInline(this);
                    }
                    else {
                    	this._errHandler.reportMatch(this);
                        this.consume();
                    }
                    this.state = 22;
                    this.expression(5);
                    break;

                } 
            }
            this.state = 27;
            this._errHandler.sync(this);
            _alt = this._interp.adaptivePredict(this._input,2,this._ctx);
        }

    } catch( error) {
        if(error instanceof antlr4.error.RecognitionException) {
	        localctx.exception = error;
	        this._errHandler.reportError(this, error);
	        this._errHandler.recover(this, error);
	    } else {
	    	throw error;
	    }
    } finally {
        this.unrollRecursionContexts(_parentctx)
    }
    return localctx;
};


ArboretumParser.prototype.sempred = function(localctx, ruleIndex, predIndex) {
	switch(ruleIndex) {
	case 1:
			return this.expression_sempred(localctx, predIndex);
    default:
        throw "No predicate with index:" + ruleIndex;
   }
};

ArboretumParser.prototype.expression_sempred = function(localctx, predIndex) {
	switch(predIndex) {
		case 0:
			return this.precpred(this._ctx, 5);
		case 1:
			return this.precpred(this._ctx, 4);
		default:
			throw "No predicate with index:" + predIndex;
	}
};


exports.ArboretumParser = ArboretumParser;
