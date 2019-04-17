var app = Elm.Main.init({
    node: document.getElementById('elm')
});  


/*
Methods we declared as 'port' in Elm will be available on app.ports that
will get invoked/called when we send a cmd message out via Elm.
*/
app.ports.parseText.subscribe(function(text) {
    console.log(text);                               // Word from Elm
    //1app.ports.toElm.send("message from js");        // Sending to the Elm sub
});