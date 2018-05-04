var bodyparser = require('body-parser');
var express = require('express');
var fs = require('fs');
var http = require('http');
var path = require('path');
var ratelimiter = require('express-rate-limit');
var syncexec = require('sync-exec');
var temp = require('temp');

var memorystore = require('./memory-store');

var app = express();

// automatically track and cleanup temporary files
temp.track();

// Rise4fun indicates that their requests are GZIP encoded, but they are not actually encoded.
// To prevent the bodyparser to automatically decode the non-encoded requests, we remove the encoding entry from the header.
app.use(function (req, res, next) {
	delete req.headers["content-encoding"];
	next();
});

// enable json and set the assets directory
app.use(express.static('public'));
app.use(bodyparser.json());

var store = new memorystore();

// to prevent denial-of-service attacks, construct a rate limit for each incoming verification request
app.use(new ratelimiter({
  max: 1, // limit each 1 requests per windowMs (per IP)
  delayMs: 0, // disable delaying - full speed until the max limit is reached
	keyGenerator: function (req) {
		return 'all';
	},
	message: JSON.stringify({
		Version: '1.0',
		Outputs: [{ MimeType: 'text/plain', Value: 'The verification server is currently too busy (try again in 20 seconds)...' }]
	}),
	store: store
}));

// removes the block on incoming requests in 1000ms after calling 'unblock'
unblock = function () {
	setTimeout(store.resetAll, 1000);
}

// handles '/run' requests made by Rise4fun by executing VerCors on the received program and sending back a result message.
handle_run_vercors = function (req, res, options) {
	// is the content actually parsed with the built-in JSON parser?
	if (req.body == undefined) {
		res.status(400).send('Error: expecting JSON content');
		unblock();
		return;
	}
	
	// does the parsed JSON object has the right format?
	if (req.body.Version == undefined || req.body.Source == undefined) {
		res.status(400).send('Error: incorrect JSON content');
		unblock();
		return;
	}
	
	console.log('INFO - connection accepted');

	// create a temporary file for the received program
	temp.open({ prefix: 'vercors-rise4fun', suffix: options.suffix }, function (err, info) {
		if (err) {
			res.status(400).send('Error: could not create a temporary file');
			console.log(err);
			unblock();
			return;
		}
		
		// close the file descriptor (directly writing with 'fs.write(info.fd, ...)' seems to be buggy.)
		fs.close(info.fd, function (err) {
			if (err) {
				res.status(400).send('Error: could not close the temporary file');
				console.log(err);
				unblock();
				return;
			}
			
			// write the received code to the temp file
			fs.writeFile(info.path, req.body.Source, function (err) {
				if (err) {
					res.status(400).send('Error: could not write to temporary file');
					console.log(err);
					unblock();
					return;
				}
				
				try {
					// execute vercors on the received program (with silicon)
					var toolpath = path.join(__dirname, '../unix/bin/vct --silicon');
					var tooloutput = syncexec(toolpath + ' ' + info.path, 40 * 1000); // timeout: 30 seconds
					var output = tooloutput.stdout;
				
					if (output == '') {
						output = 'Timeout!';
					}
									
					// render the output message as JSON
					res.setHeader('Content-Type', 'application/json');
					res.json({
						Version: "1.0",
						Outputs: [{ MimeType: "text/plain", Value: output }]
					});
				}
				catch (ex) {
					res.setHeader('Content-Type', 'application/json');
					res.json({
						Version: "1.0",
						Outputs: [{ MimeType: "text/plain", Value: 'Timeout!' }]
					});
				}
				finally {
					unblock();
				}
			});
		});
	});
}

app.post('/java/run', function (req, res) {
	handle_run_vercors(req, res, { suffix: '.java' });
});

app.post('/pvl/run', function (req, res) {
	handle_run_vercors(req, res, { suffix: '.pvl' });
});

app.post('/c/run', function (req, res) {
	handle_run_vercors(req, res, { suffix: '.c' });
});

app.post('*', function (req, res) {
	res.setHeader('Content-Type', 'application/json');
	res.json({
		Version: "1.0",
		Outputs: [{ MimeType: "text/plain", Value: 'The online interface currently does not support handling of this language...' }]
	});
	store.resetAll();
});

app.all('*', function (req, res) {
  res.send('Hi there! This is the online interface to VerCors.');
	store.resetAll();
});

app.listen(8080, function () {
	console.log('The VerCors online interface is active and listening on port 8080')
});
