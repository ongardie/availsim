"use strict";

var fs = require('fs');
var exec = require('child_process').exec;


var plotter = exec('R --no-save --interactive', {
  env: process.env,
});
var plotter_cb_queue = [];
plotter.stdin.write('repl = TRUE; source("plots.R")\n');
plotter.stdout.on('data', function (data) {
  if (data.match('--DONE--') != null)
    plotter_cb_queue.shift(0)();
  //console.log('stdout: ' + data);
});

plotter.stderr.on('data', function (data) {
  console.log('stderr: ' + data);
});

var submitPlot = function (dir, cb) {
  plotter_cb_queue.push(cb);
  plotter.stdin.write(dir + '\n');
}



$(function() {

//$('.btn-group').button();
/*$('#logs-othert').toggle(function() {
  console.log('toggle');
  //$('.btn-group').button('reset')
});
*/

var fill = function(name, div, opts) {
  for (var i in opts) {
    var active = i == 0 ? 'active' : '';
    div.append('<label class="btn btn-primary mh50 ' + active + '">' +
      '<input type="radio" name="' + name + '" value="' + opts[i] + '"> ' + opts[i] +
    '</label>');
  }
  div.append('<label class="btn btn-primary mh50">' +
      '<input type="radio" name="' + name + '" value="__other__">' +
      '<input type="text" name="' + name + '-other" class="form-control" placeholder="' + name + '">' +
    '</label>');
};

fill('logs', $('#logs'), [
  'same',
  'diff',
]);

fill('timing', $('#timing'), [
  'LAN',
  'WAN',
  'Delay1pct',
  'Down',
  'P1',
  'P2',
  'L1',
  'Deian',
  'BadRecv',
]);

fill('algorithm', $('#algorithm'), [
  'submission',
  'hesitant',
  'nograntnobump',
]);

fill('terms', $('#terms'), [
  'same',
  'diff',
]);


var crypto = require('crypto');

var form = $('#form');
var args = $('#args', form);

form.change(function() {
  setTimeout(function() {
      form.submit();
  }, 0);
});

$('#pin').click(function() {
  $('#pinned').prepend($('#graph').clone().removeProp('id'));
});

var getArg = function(name) {
    var v = $('.active [name=' + name + ']', form).val();
    if (v == '__other__')
      v = $('[name=' + name + '-other]', form).val();
    return ' --' + name + '=' + v + ' ';
}

form.submit(function() {
    var effargs = (args.val() +
                   getArg('logs') +
                   getArg('terms') +
                   getArg('timing') +
                   getArg('algorithm'));
    console.log(effargs);

    var sha1sum = crypto.createHash('sha1');
    sha1sum.update(effargs);

    // nonce so that browser doesn't cache old graph
    // (defeats point of update(effargs), though)
    var nonce = crypto.randomBytes(8).toString();
    sha1sum.update(nonce);

    var dir = 'explore/' + sha1sum.digest('hex');
    try {
      fs.mkdirSync(dir);
    } catch (e) {
      if (e.code != "EEXIST")
        throw e;
    }

    console.log('Running ./main ' + effargs);
    var sim = exec('../../main ' + effargs, {
      cwd: dir,
      env: process.env,
    });

    sim.on('error', function (e) {
      console.log(e);
    });

    sim.stdout.on('data', function (data) {
      //console.log('stdout: ' + data);
    });

    sim.stderr.on('data', function (data) {
      console.log('stderr: ' + data);
    });

    sim.on('exit', function (code) {
      console.log('sim exited with code ' + code);
      if (code != 0)
        return;

      console.log('Running R');
      /*
      var plot = exec('Rscript ../../plots.R', {
        cwd: dir,
        env: process.env,
      });

      plot.stdout.on('data', function (data) {
        console.log('stdout: ' + data);
      });

      plot.stderr.on('data', function (data) {
        //console.log('stderr: ' + data);
      });

      plot.on('close', function (code) {
        console.log('plot exited with code ' + code);
        $('#graph').prop('src', dir + '/Rplots.svg');
      });
      */

      submitPlot(dir, function() {
        console.log('R done');
        $('#graph').prop('src', dir + '/Rplots.svg');
      });
    });

    return false;
});

form.submit();

});
