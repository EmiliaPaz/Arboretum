"use strict"

toAst = require("parser/visitor").toAst

var app = Elm.Main.init({
    node: document.getElementById('elm')
});  


/*
Methods we declared as 'port' in Elm will be available on app.ports that
will get invoked/called when we send a cmd message out via Elm.
*/
app.ports.parseText.subscribe(function(text) {
    const ast = toAST(text)
    console.log(ast)
});