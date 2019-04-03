const antlr4 = require('antlr4/index');
const ArboretumLexer = require('./ArboretumLexer');
const ArboretumParser = require('./ArboretumParser');
var ArboretumListener = require('./ArboretumListener').ArboretumListener;

HtmlArboretumListener = function(res) {
    this.Res = res;    
    ArboretumListener.call(this); // inherit default listener
    return this;
};

// inherit default listener
HtmlArboretumListener.prototype = Object.create(ArboretumListener.prototype);
HtmlArboretumListener.prototype.constructor = HtmlArboretumListener;

// override default listener behavior

exports.HtmlArboretumListener = HtmlArboretumListener;

HtmlArboretumListener.prototype.enterFinalExpression = function(ctx) {   
    this.Res.write('<span style="color:green">');         
};

HtmlArboretumListener.prototype.exitFinalExpression = function(ctx) {      
    this.Res.write(ctx.getText());
    this.Res.write("</span> ");
}; 

ArboretumListener.prototype.enterParenthesisExp = function(ctx) {
    this.Res.write('<span style="color:blue">');  
};

HtmlArboretumListener.prototype.exitParenthesisExp = function(ctx) {      
    this.Res.write(ctx.getText());
    this.Res.write("</span> ");
}; 

ArboretumListener.prototype.enterTwoSidedConnNumExp = function(ctx) {
    this.Res.write('<span style="color:red">');        
};

// Exit a parse tree produced by ArboretumParser#twoSidedConnNumExp.
ArboretumListener.prototype.exitTwoSidedConnNumExp = function(ctx) {
    this.Res.write(ctx.getText());
    this.Res.write("</span> ");
};


