// Generated from Arboretum.g4 by ANTLR 4.7.2
// jshint ignore: start
var antlr4 = require('antlr4/index');

// This class defines a complete listener for a parse tree produced by ArboretumParser.
function ArboretumListener() {
	antlr4.tree.ParseTreeListener.call(this);
	return this;
}

ArboretumListener.prototype = Object.create(antlr4.tree.ParseTreeListener.prototype);
ArboretumListener.prototype.constructor = ArboretumListener;

// Enter a parse tree produced by ArboretumParser#finalExpression.
ArboretumListener.prototype.enterFinalExpression = function(ctx) {
};

// Exit a parse tree produced by ArboretumParser#finalExpression.
ArboretumListener.prototype.exitFinalExpression = function(ctx) {
};


// Enter a parse tree produced by ArboretumParser#twoSidedConnNumExp.
ArboretumListener.prototype.enterTwoSidedConnNumExp = function(ctx) {
};

// Exit a parse tree produced by ArboretumParser#twoSidedConnNumExp.
ArboretumListener.prototype.exitTwoSidedConnNumExp = function(ctx) {
};


// Enter a parse tree produced by ArboretumParser#parenthesisExp.
ArboretumListener.prototype.enterParenthesisExp = function(ctx) {
};

// Exit a parse tree produced by ArboretumParser#parenthesisExp.
ArboretumListener.prototype.exitParenthesisExp = function(ctx) {
};


// Enter a parse tree produced by ArboretumParser#boolExp.
ArboretumListener.prototype.enterBoolExp = function(ctx) {
};

// Exit a parse tree produced by ArboretumParser#boolExp.
ArboretumListener.prototype.exitBoolExp = function(ctx) {
};


// Enter a parse tree produced by ArboretumParser#twoSidedConnBoolExp.
ArboretumListener.prototype.enterTwoSidedConnBoolExp = function(ctx) {
};

// Exit a parse tree produced by ArboretumParser#twoSidedConnBoolExp.
ArboretumListener.prototype.exitTwoSidedConnBoolExp = function(ctx) {
};


// Enter a parse tree produced by ArboretumParser#intExp.
ArboretumListener.prototype.enterIntExp = function(ctx) {
};

// Exit a parse tree produced by ArboretumParser#intExp.
ArboretumListener.prototype.exitIntExp = function(ctx) {
};


// Enter a parse tree produced by ArboretumParser#doubleExp.
ArboretumListener.prototype.enterDoubleExp = function(ctx) {
};

// Exit a parse tree produced by ArboretumParser#doubleExp.
ArboretumListener.prototype.exitDoubleExp = function(ctx) {
};



exports.ArboretumListener = ArboretumListener;