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
    const asts = []
    for(const l of lines) {
        // wrap each parsing in try/catch because if one line fails we still
        // want to attempt the others 
        try {
            const a = toAst(l)
            asts.push(a)
        }
        catch(error) {
            console.log(error)
        }

    }
    if(asts.length > 0) {
        app.ports.gotAst.send(asts)
    }
});