var app = Elm.Main.init({
    node: document.getElementById('elm')
});  // nah


/*
Methods we declared as 'port' in Elm will be available on app.ports that
will get invoked/called when we send a cmd message out via Elm.
*/
app.ports.toJS.subscribe(function(msg) {
    console.log(msg);                               // Word from Elm
    app.ports.toElm.send("message from js");        // Sending to the Elm sub
});