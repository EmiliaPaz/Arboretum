"use strict"

/*
 * this file is the seam along which the language interpreter (written in Elm)
 * and the parser (written in JavaScript/Chevrotain) communicate
 */

const toAst = require("./parser/visitor").toAst

var app = Elm.Main.init({
    node: document.getElementById('elm')
});  


/*
Methods we declared as 'port' in Elm will be available on app.ports that
will get invoked/called when we send a cmd message out via Elm.
*/
app.ports.parseLines.subscribe(function(lines) {
    try {
        const asts = lines.map(l => toAst(l))
        app.ports.gotAst.send(asts)
    }
    catch(error) {

    }
});