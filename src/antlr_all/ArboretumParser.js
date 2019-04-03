// Generated from Arboretum.g4 by ANTLR 4.7.2
// jshint ignore: start
var antlr4 = require('antlr4/index');
var ArboretumListener = require('./ArboretumListener').ArboretumListener;
var grammarFileName = "Arboretum.g4";


var serializedATN = ["\u0003\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964",
    "\u0003\r!\u0004\u0002\t\u0002\u0003\u0002\u0003\u0002\u0003\u0002\u0003",
    "\u0002\u0003\u0002\u0003\u0002\u0003\u0002\u0003\u0002\u0003\u0002\u0003",
    "\u0002\u0003\u0002\u0003\u0002\u0005\u0002\u0011\n\u0002\u0003\u0002",
    "\u0003\u0002\u0003\u0002\u0003\u0002\u0003\u0002\u0003\u0002\u0003\u0002",
    "\u0003\u0002\u0003\u0002\u0007\u0002\u001c\n\u0002\f\u0002\u000e\u0002",
    "\u001f\u000b\u0002\u0003\u0002\u0002\u0003\u0002\u0003\u0002\u0002\u0004",
    "\u0003\u0002\u0006\u0007\u0003\u0002\b\t\u0002%\u0002\u0010\u0003\u0002",
    "\u0002\u0002\u0004\u0005\b\u0002\u0001\u0002\u0005\u0006\u0007\u0003",
    "\u0002\u0002\u0006\u0007\u0005\u0002\u0002\u0002\u0007\b\u0007\u0004",
    "\u0002\u0002\b\u0011\u0003\u0002\u0002\u0002\t\n\u0007\u000b\u0002\u0002",
    "\n\u000b\u0007\u0003\u0002\u0002\u000b\f\u0005\u0002\u0002\u0002\f\r",
    "\u0007\u0004\u0002\u0002\r\u0011\u0003\u0002\u0002\u0002\u000e\u0011",
    "\u0007\f\u0002\u0002\u000f\u0011\u0007\n\u0002\u0002\u0010\u0004\u0003",
    "\u0002\u0002\u0002\u0010\t\u0003\u0002\u0002\u0002\u0010\u000e\u0003",
    "\u0002\u0002\u0002\u0010\u000f\u0003\u0002\u0002\u0002\u0011\u001d\u0003",
    "\u0002\u0002\u0002\u0012\u0013\f\b\u0002\u0002\u0013\u0014\t\u0002\u0002",
    "\u0002\u0014\u001c\u0005\u0002\u0002\t\u0015\u0016\f\u0007\u0002\u0002",
    "\u0016\u0017\t\u0003\u0002\u0002\u0017\u001c\u0005\u0002\u0002\b\u0018",
    "\u0019\f\u0006\u0002\u0002\u0019\u001a\u0007\u0005\u0002\u0002\u001a",
    "\u001c\u0005\u0002\u0002\u0006\u001b\u0012\u0003\u0002\u0002\u0002\u001b",
    "\u0015\u0003\u0002\u0002\u0002\u001b\u0018\u0003\u0002\u0002\u0002\u001c",
    "\u001f\u0003\u0002\u0002\u0002\u001d\u001b\u0003\u0002\u0002\u0002\u001d",
    "\u001e\u0003\u0002\u0002\u0002\u001e\u0003\u0003\u0002\u0002\u0002\u001f",
    "\u001d\u0003\u0002\u0002\u0002\u0005\u0010\u001b\u001d"].join("");


var atn = new antlr4.atn.ATNDeserializer().deserialize(serializedATN);

var decisionsToDFA = atn.decisionToState.map( function(ds, index) { return new antlr4.dfa.DFA(ds, index); });

var sharedContextCache = new antlr4.PredictionContextCache();

var literalNames = [ null, "'('", "')'", "'^'", "'*'", "'/'", "'+'", "'-'", 
                     null, null, null, "' '" ];

var symbolicNames = [ null, null, null, null, "ASTERISK", "SLASH", "PLUS", 
                      "MINUS", "ID", "NAME", "NUMBER", "WHITESPACE" ];

var ruleNames =  [ "expression" ];

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
ArboretumParser.T__2 = 3;
ArboretumParser.ASTERISK = 4;
ArboretumParser.SLASH = 5;
ArboretumParser.PLUS = 6;
ArboretumParser.MINUS = 7;
ArboretumParser.ID = 8;
ArboretumParser.NAME = 9;
ArboretumParser.NUMBER = 10;
ArboretumParser.WHITESPACE = 11;

ArboretumParser.RULE_expression = 0;


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

function NumericAtomExpContext(parser, ctx) {
	ExpressionContext.call(this, parser);
    ExpressionContext.prototype.copyFrom.call(this, ctx);
    return this;
}

NumericAtomExpContext.prototype = Object.create(ExpressionContext.prototype);
NumericAtomExpContext.prototype.constructor = NumericAtomExpContext;

ArboretumParser.NumericAtomExpContext = NumericAtomExpContext;

NumericAtomExpContext.prototype.NUMBER = function() {
    return this.getToken(ArboretumParser.NUMBER, 0);
};
NumericAtomExpContext.prototype.enterRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.enterNumericAtomExp(this);
	}
};

NumericAtomExpContext.prototype.exitRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.exitNumericAtomExp(this);
	}
};


function PowerExpContext(parser, ctx) {
	ExpressionContext.call(this, parser);
    ExpressionContext.prototype.copyFrom.call(this, ctx);
    return this;
}

PowerExpContext.prototype = Object.create(ExpressionContext.prototype);
PowerExpContext.prototype.constructor = PowerExpContext;

ArboretumParser.PowerExpContext = PowerExpContext;

PowerExpContext.prototype.expression = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExpressionContext);
    } else {
        return this.getTypedRuleContext(ExpressionContext,i);
    }
};
PowerExpContext.prototype.enterRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.enterPowerExp(this);
	}
};

PowerExpContext.prototype.exitRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.exitPowerExp(this);
	}
};


function MulDivExpContext(parser, ctx) {
	ExpressionContext.call(this, parser);
    ExpressionContext.prototype.copyFrom.call(this, ctx);
    return this;
}

MulDivExpContext.prototype = Object.create(ExpressionContext.prototype);
MulDivExpContext.prototype.constructor = MulDivExpContext;

ArboretumParser.MulDivExpContext = MulDivExpContext;

MulDivExpContext.prototype.expression = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExpressionContext);
    } else {
        return this.getTypedRuleContext(ExpressionContext,i);
    }
};

MulDivExpContext.prototype.ASTERISK = function() {
    return this.getToken(ArboretumParser.ASTERISK, 0);
};

MulDivExpContext.prototype.SLASH = function() {
    return this.getToken(ArboretumParser.SLASH, 0);
};
MulDivExpContext.prototype.enterRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.enterMulDivExp(this);
	}
};

MulDivExpContext.prototype.exitRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.exitMulDivExp(this);
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


function IdAtomExpContext(parser, ctx) {
	ExpressionContext.call(this, parser);
    ExpressionContext.prototype.copyFrom.call(this, ctx);
    return this;
}

IdAtomExpContext.prototype = Object.create(ExpressionContext.prototype);
IdAtomExpContext.prototype.constructor = IdAtomExpContext;

ArboretumParser.IdAtomExpContext = IdAtomExpContext;

IdAtomExpContext.prototype.ID = function() {
    return this.getToken(ArboretumParser.ID, 0);
};
IdAtomExpContext.prototype.enterRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.enterIdAtomExp(this);
	}
};

IdAtomExpContext.prototype.exitRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.exitIdAtomExp(this);
	}
};


function AddSubExpContext(parser, ctx) {
	ExpressionContext.call(this, parser);
    ExpressionContext.prototype.copyFrom.call(this, ctx);
    return this;
}

AddSubExpContext.prototype = Object.create(ExpressionContext.prototype);
AddSubExpContext.prototype.constructor = AddSubExpContext;

ArboretumParser.AddSubExpContext = AddSubExpContext;

AddSubExpContext.prototype.expression = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExpressionContext);
    } else {
        return this.getTypedRuleContext(ExpressionContext,i);
    }
};

AddSubExpContext.prototype.PLUS = function() {
    return this.getToken(ArboretumParser.PLUS, 0);
};

AddSubExpContext.prototype.MINUS = function() {
    return this.getToken(ArboretumParser.MINUS, 0);
};
AddSubExpContext.prototype.enterRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.enterAddSubExp(this);
	}
};

AddSubExpContext.prototype.exitRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.exitAddSubExp(this);
	}
};


function FunctionExpContext(parser, ctx) {
	ExpressionContext.call(this, parser);
    ExpressionContext.prototype.copyFrom.call(this, ctx);
    return this;
}

FunctionExpContext.prototype = Object.create(ExpressionContext.prototype);
FunctionExpContext.prototype.constructor = FunctionExpContext;

ArboretumParser.FunctionExpContext = FunctionExpContext;

FunctionExpContext.prototype.NAME = function() {
    return this.getToken(ArboretumParser.NAME, 0);
};

FunctionExpContext.prototype.expression = function() {
    return this.getTypedRuleContext(ExpressionContext,0);
};
FunctionExpContext.prototype.enterRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.enterFunctionExp(this);
	}
};

FunctionExpContext.prototype.exitRule = function(listener) {
    if(listener instanceof ArboretumListener ) {
        listener.exitFunctionExp(this);
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
    var _startState = 0;
    this.enterRecursionRule(localctx, 0, ArboretumParser.RULE_expression, _p);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 14;
        this._errHandler.sync(this);
        switch(this._input.LA(1)) {
        case ArboretumParser.T__0:
            localctx = new ParenthesisExpContext(this, localctx);
            this._ctx = localctx;
            _prevctx = localctx;

            this.state = 3;
            this.match(ArboretumParser.T__0);
            this.state = 4;
            this.expression(0);
            this.state = 5;
            this.match(ArboretumParser.T__1);
            break;
        case ArboretumParser.NAME:
            localctx = new FunctionExpContext(this, localctx);
            this._ctx = localctx;
            _prevctx = localctx;
            this.state = 7;
            this.match(ArboretumParser.NAME);
            this.state = 8;
            this.match(ArboretumParser.T__0);
            this.state = 9;
            this.expression(0);
            this.state = 10;
            this.match(ArboretumParser.T__1);
            break;
        case ArboretumParser.NUMBER:
            localctx = new NumericAtomExpContext(this, localctx);
            this._ctx = localctx;
            _prevctx = localctx;
            this.state = 12;
            this.match(ArboretumParser.NUMBER);
            break;
        case ArboretumParser.ID:
            localctx = new IdAtomExpContext(this, localctx);
            this._ctx = localctx;
            _prevctx = localctx;
            this.state = 13;
            this.match(ArboretumParser.ID);
            break;
        default:
            throw new antlr4.error.NoViableAltException(this);
        }
        this._ctx.stop = this._input.LT(-1);
        this.state = 27;
        this._errHandler.sync(this);
        var _alt = this._interp.adaptivePredict(this._input,2,this._ctx)
        while(_alt!=2 && _alt!=antlr4.atn.ATN.INVALID_ALT_NUMBER) {
            if(_alt===1) {
                if(this._parseListeners!==null) {
                    this.triggerExitRuleEvent();
                }
                _prevctx = localctx;
                this.state = 25;
                this._errHandler.sync(this);
                var la_ = this._interp.adaptivePredict(this._input,1,this._ctx);
                switch(la_) {
                case 1:
                    localctx = new MulDivExpContext(this, new ExpressionContext(this, _parentctx, _parentState));
                    this.pushNewRecursionContext(localctx, _startState, ArboretumParser.RULE_expression);
                    this.state = 16;
                    if (!( this.precpred(this._ctx, 6))) {
                        throw new antlr4.error.FailedPredicateException(this, "this.precpred(this._ctx, 6)");
                    }
                    this.state = 17;
                    _la = this._input.LA(1);
                    if(!(_la===ArboretumParser.ASTERISK || _la===ArboretumParser.SLASH)) {
                    this._errHandler.recoverInline(this);
                    }
                    else {
                    	this._errHandler.reportMatch(this);
                        this.consume();
                    }
                    this.state = 18;
                    this.expression(7);
                    break;

                case 2:
                    localctx = new AddSubExpContext(this, new ExpressionContext(this, _parentctx, _parentState));
                    this.pushNewRecursionContext(localctx, _startState, ArboretumParser.RULE_expression);
                    this.state = 19;
                    if (!( this.precpred(this._ctx, 5))) {
                        throw new antlr4.error.FailedPredicateException(this, "this.precpred(this._ctx, 5)");
                    }
                    this.state = 20;
                    _la = this._input.LA(1);
                    if(!(_la===ArboretumParser.PLUS || _la===ArboretumParser.MINUS)) {
                    this._errHandler.recoverInline(this);
                    }
                    else {
                    	this._errHandler.reportMatch(this);
                        this.consume();
                    }
                    this.state = 21;
                    this.expression(6);
                    break;

                case 3:
                    localctx = new PowerExpContext(this, new ExpressionContext(this, _parentctx, _parentState));
                    this.pushNewRecursionContext(localctx, _startState, ArboretumParser.RULE_expression);
                    this.state = 22;
                    if (!( this.precpred(this._ctx, 4))) {
                        throw new antlr4.error.FailedPredicateException(this, "this.precpred(this._ctx, 4)");
                    }
                    this.state = 23;
                    this.match(ArboretumParser.T__2);
                    this.state = 24;
                    this.expression(4);
                    break;

                } 
            }
            this.state = 29;
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
	case 0:
			return this.expression_sempred(localctx, predIndex);
    default:
        throw "No predicate with index:" + ruleIndex;
   }
};

ArboretumParser.prototype.expression_sempred = function(localctx, predIndex) {
	switch(predIndex) {
		case 0:
			return this.precpred(this._ctx, 6);
		case 1:
			return this.precpred(this._ctx, 5);
		case 2:
			return this.precpred(this._ctx, 4);
		default:
			throw "No predicate with index:" + predIndex;
	}
};


exports.ArboretumParser = ArboretumParser;
