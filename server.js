// required dependencies
require('coffee-script');
var http = require('http');
var express = require('express');
var sharejs = require('share');
var nopt = require('nopt');
// var zmq = require('zmq');
var WebSocket = require('ws');
var osc = require('osc');

// global variables
var stderr = process.stderr;

// parse command-line options
var knownOpts = {
    "password" : [String, null],
    "ws-port" : [Number, null],
    "osc-receive-port" : [Number, null],
    "osc-send-port" : [Number, null],
    "help": Boolean
};

var shortHands = {
    "p" : ["--password"],
    "w" : ["--ws-port"],
    "o" : ["--osc-receive-port"],
    "s" : ["--osc-send-port"]
};

var parsed = nopt(knownOpts,shortHands,process.argv,2);

if(parsed['help']!=null) {
    stderr.write("extramuros server.js usage:\n");
    stderr.write(" --help (-h)               this help message\n");
    stderr.write(" --password [word] (-p)    password to authenticate messages to server (required)\n");
    stderr.write(" --ws-port (-w) [number]   TCP port for WebSocket connections to browsers and clients (default: 8000)\n");
    stderr.write(" --osc-receive-port (-o) [number]  UDP port on which to receive OSC messages (default: none)\n");
    stderr.write(" --osc-send-port (-s) [number]  UDP port on which to send OSC messages (default: none)\n");
    process.exit(1);
}

if(process.argv.length<3) {
    stderr.write("extramuros: use --help to display available options\n");
}

var wsPort = parsed['ws-port'];
if(wsPort==null) wsPort = 8000;
var oscReceivePort = parsed['osc-receive-port'];
var oscSendPort = parsed['osc-send-port'];
var password = parsed['password'];
if(password == null) {
    stderr.write("Error: --password option is not optional!\n");
    process.exit(1);
}

var httpServer = http.createServer();
var expressServer = express();
expressServer.use(express.static(__dirname));
httpServer.on('request',expressServer);

// var stdin = process.openStdin();
// stdin.addListener("data", function (d) { pub.send(d); });

var options = {
  db: {type: 'none'},
  browserChannel: {cors: '*'},
  auth: function(client, action) {
    // This auth handler rejects any ops bound for docs starting with 'readonly'.
    if (action.name === 'submit op' && action.docName.match(/^readonly/)) {
      action.reject();
    } else {
      action.accept();
    }
  }
};

var wss = new WebSocket.Server({server: httpServer});
wss.broadcast = function(data) {
  //stderr.write("extramuros: broadcast to " + wss.clients.size + " clients\n");
  for (let i of wss.clients) i.send(data);
};

var udp;

if(oscReceivePort != null) {
    udp = new osc.UDPPort( {
        localAddress: "0.0.0.0",
        localPort: oscReceivePort
    });
    stderr.write("extramuros: listening for OSC on UDP port " + oscReceivePort.toString()+"\n");

    udp.on("message", function (oscMsg) {
        console.log("An OSC message just arrived!", oscMsg);
    });

    if(oscSendPort != null) {
        stderr.write("extramuros: send osc messages on UDP port " + oscSendPort.toString() + "\n");
    }
}

wss.on('connection',function(ws) {
    // route incoming OSC back to browsers
    var udpListener = function(m) {
        var n = {
            'type': 'osc',
            'address': m.address,
            'args': m.args
        };
        try {
            ws.send(JSON.stringify(n));
        }
        catch(e) { stderr.write("warning: exception in WebSocket send\n"); }
    };

    if(udp!=null) udp.addListener("message",udpListener);
    ws.on("message",function(m) {
    var n = JSON.parse(m);

    if (n.request === "sendInput") {
        try {wss.broadcast(JSON.stringify({'type': "input", 'name': n.bufferName, 'position': n.caret }));}
        catch(e) { stderr.write("warning: exception in WebSocket send\n"); }
    }
    if (n.request === "triggerEditorOSC") {
        try {wss.broadcast(JSON.stringify({'type': "osc", 'address': "/extramuros/editor/" + n.bufferName.replace("edit", ""), 'arg': [1] }));}
        catch(e) { stderr.write("warning: exception in WebSocket send\n"); }
    }
	if(n.request === "eval") {
        sendOSCTriggerMessage(n.bufferName);
        evaluateBuffer(n.bufferName);
	}
    if(n.request === "evalCode") {
        sendOSCTriggerMessage(n.bufferName);
        evaluateCode(n.bufferName, n.code);
    }
	if(n.request === "evalJS") {
        evaluateJavaScriptGlobally(n.code);
	}
	if(n.request === "oscFromClient") {
		forwardOscFromClient(n.address,n.args);
	}
	if(n.request === "feedback") {
		forwardFeedbackFromClient(n.text);
	}
    });
    ws.on("close",function() {
	console.log("");
	if(udp!=null)udp.removeListener("message",udpListener);
	});
});

if(udp!=null)udp.open();

function evaluateCode(name, code) {
    var t = code.replace(/--.*\n/g, "\n").replace(/\t/g, "").replace(/\n\]/g, "]");;
    var n = { type: 'eval', code: t };
    try { wss.broadcast(JSON.stringify(n)); }
    catch (e) { stderr.write("warning: exception in WebSocket broadcast\n"); }
    console.log(JSON.stringify(n));
}

function sendOSCTriggerMessage(name) {
    if(oscSendPort != null) {
        let msg = {
            address: "/extramuros/editor/" + name.replace("edit", ""),
            args: [
                {
                    type: "i",
                    value: "1"
                }
            ]
        };

        udp.send(msg, "127.0.0.1", oscSendPort);

        console.log("An OSC message just send!", msg);
    }
}

function evaluateBuffer(name) {
  sharejs.client.open(name,'text','http://127.0.0.1:' + wsPort + '/channel', function (err,doc) {
    //var t = doc.getText().replace(/--.*\n/g, "").replace(/\r?\n|\r/g, "").replace(/\t/g, "");
    var t = doc.getText().replace(/--.*\n/g, "\n").replace(/\t/g, "").replace(/\n\]/g, "]");
    var n = { type: 'eval', code: t };
    try { wss.broadcast(JSON.stringify(n)); }
    catch (e) { stderr.write("warning: exception in WebSocket broadcast\n"); }
    console.log(JSON.stringify(n));
    // pub.send(doc.getText());
  });
}

function evaluateJavaScriptGlobally(code) {
    var n = { 'type': 'js', 'code': code };
    try { wss.broadcast(JSON.stringify(n)); }
    catch(e) { stderr.write("warning: exception in WebSocket broadcast\n"); }
}

function forwardOscFromClient(address,args) {
    var n = { 'type': 'osc', 'address': address, 'args': args };
    try { wss.broadcast(JSON.stringify(n)); }
    catch(e) { stderr.write("warning: exception in WebSocket broadcast\n"); }
}

function forwardFeedbackFromClient(text) {
    var n = { 'type': 'feedback', 'text': text };
    try { wss.broadcast(JSON.stringify(n)); }
    catch(e) { stderr.write("warning: exception in WebSocket broadcast\n"); }
}

var shareserver = sharejs.server.attach(expressServer, options);

expressServer.get('/?', function(req, res) {
  res.writeHead(302, {location: '/index.html'});
  res.end();
});
httpServer.listen(wsPort);
console.log("extramuros server, listening on TCP port " + wsPort + " (http/WebSockets)");

process.title = 'extramuros';
process.on('SIGINT',function() { pub.close(); });
