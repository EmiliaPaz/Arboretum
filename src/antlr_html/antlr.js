const antlr4 = require('antlr4/index');
const ArboretumLexer = require('./ArboretumLexer');
const ArboretumParser = require('./ArboretumParser');
const HtmlArboretumListener = require('./HtmlArboretumListener').HtmlArboretumListener;

// http.createServer((req, res) => {
   
//    res.writeHead(200, {
//        'Content-Type': 'text/html',        
//    });

//    res.write('<html><head><meta charset="UTF-8"/> </head><body>');
   
//    var input = "(3+5)-7";
//    var chars = new antlr4.InputStream(input);
//    var lexer = new ArboretumLexer.ArboretumLexer(chars);
//    var tokens  = new antlr4.CommonTokenStream(lexer);
//    var parser = new ArboretumParser.ArboretumParser(tokens);
//    parser.buildParseTrees = true;   
//    var tree = parser.finalExpression();   
//    var htmlArboretum = new HtmlArboretumListener(res);

//    antlr4.tree.ParseTreeWalker.DEFAULT.walk(htmlArboretum, tree);
   
//    res.write('</body></html>');
//    res.end();

// }).listen(1337);


var app = Elm.Main.init({
    node: document.getElementById('elm')
});  // nah


/*
Methods we declared as 'port' in Elm will be available on app.ports that
will get invoked/called when we send a cmd message out via Elm.
*/
app.ports.toJS.subscribe(function(msg) {
    var input = "(3+5)-7";
   var chars = new InputStream(input);
   var lexer = new _ArboretumLexer(chars);
   var tokens  = new CommonTokenStream(lexer);
   var parser = new _ArboretumParser(tokens);
   parser.buildParseTrees = true;   
   var tree = parser.finalExpression();   
   
   class Visitor {
    visitChildren(ctx) {
      if (!ctx) {
        return;
      }
  
      if (ctx.children) {
        return ctx.children.map(child => {
          if (child.children && child.children.length != 0) {
            return child.accept(this);
          } else {
            return child.getText();
          }
        });
      }
    }
  }
  
   tree.accept(new Visitor());


   app.ports.toElm.send("message from js");        // Sending to the Elm sub

});
