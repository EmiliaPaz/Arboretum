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

HtmlArboretumListener.prototype.enterParenthesisExp = function(ctx) {          
    this.Res.write("<strong>");    
};
HtmlArboretumListener.prototype.enterParenthesisExp = function(ctx) {      
    this.Res.write(ctx.getText());
    this.Res.write("</strong> ");
}; 