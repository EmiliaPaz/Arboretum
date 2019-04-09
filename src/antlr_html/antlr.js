const http = require('http');
const antlr4 = require('antlr4/index');
const ArboretumLexer = require('./ArboretumLexer');
const ArboretumParser = require('./ArboretumParser');
const HtmlArboretumListener = require('./HtmlArboretumListener').HtmlArboretumListener;



http.createServer((req, res) => {
   
   res.writeHead(200, {
       'Content-Type': 'text/html',        
   });

   res.write('<html><head><meta charset="UTF-8"/> </head><body>');
   
   var input = "(3+5)-7";
   var chars = new antlr4.InputStream(input);
   var lexer = new ArboretumLexer.ArboretumLexer(chars);
   var tokens  = new antlr4.CommonTokenStream(lexer);
   var parser = new ArboretumParser.ArboretumParser(tokens);
   parser.buildParseTrees = true;   
   var tree = parser.finalExpression();   
   var htmlArboretum = new HtmlArboretumListener(res);

   antlr4.tree.ParseTreeWalker.DEFAULT.walk(htmlArboretum, tree);
   
   res.write('</body></html>');
   res.end();

}).listen(1337);