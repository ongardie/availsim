"use strict";

var fs = require('fs');
var exec = require('child_process').exec;
var csv = require('csv');

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

var submitPlot = function (dir, expr, cb) {
  plotter_cb_queue.push(cb);
  plotter.stdin.write('setwd("' + dir + '"); ' + expr + ';\n');
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
  'diff-eqid',
  'diff-oldstale',
  'diff-oldminstale',
  'diff-136stale',
  '1old2old3new4new5both6old',
  '1both2new3new4new5new6new',
  '1old2both3old4new5new6old7new',
]);

fill('timing', $('#timing'), [
  'LAN',
  'RAMCloud',
  'WAN',
  'Delay1pct',
  'Down',
  'P1',
  'P1-RAMCloud',
  'P2',
  'P2-RAMCloud',
  'L1',
  'Deian',
  'BadRecv',
]);

fill('algorithm', $('#algorithm'), [
  'submission',
  'hesitant2',
  'phesitant',
  'nograntnobump',
  'stalelognobump',
  'zookeeper',
  'zookeeper2',
  'lease',
]);

fill('terms', $('#terms'), [
  'same',
  'diff',
]);

fill('cluster', $('#cluster'), [
  '3',
  '5',
  '7',
  '9',
  '5-2+2',
  '1-5to2-6:1old2old3new4new5both6old',
  '1-5to2-6:1both2new3new4new5new6new',
  '1-5to3-7:1old2both3old4both5both6old7both',
  '1-5to3-7:1old2both3old4new5new6old7new',
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

var zeropad = function(minlen, s) {
  while (s.length < minlen)
    s = "0" + s;
  return s;
}

form.submit(function() {
    var trace = $('#trace').val();
    var heartbeats = $('#heartbeats').val();
    var effargs = (args.val() +
                   getArg('logs') +
                   getArg('terms') +
                   getArg('timing') +
                   getArg('algorithm') +
                   getArg('cluster') +
                   ('--trace ' + trace + ' ') +
                   ('--heartbeats ' + heartbeats + ' '));
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

      $('#runs').html('');
      csv()
        .from.path(dir + '/samples.csv', {
          columns: true
        })
        .to.array(function (data) {
          console.log("finished reading samples.csv");
          data.forEach(function (row) {
            if (row.run >= trace)
              return;
            var link = $('<a></a>')
              .prop('href', '')
              .append((row.election_time / 1e3).toFixed(0))
              .click(function() {
                console.log('run: ' + dir + ' ' + zeropad(6, row.run.toString()));
                fs.readFile(dir + '/trace' + zeropad(6, row.run.toString()) + '.html',
                            {encoding: 'utf8', flag: 'r'},
                            function (err, data) {
                  if (err)
                    console.log(err);
                  $('#fulltrace').html('' + data);
                });
                submitPlot(dir, 'timeline(' + row.run + ')', function() {
                  console.log('R done');
                  $('#timeline').prop('src', dir + '/timeline' + zeropad(6, row.run.toString()) + '.svg');
                });
                return false;
              });
            $('#runs').append(link).append(' ');
          });
        })
        .transform(function(row){
          row.election_time = parseInt(row.election_time);
          row.run = parseInt(row.run);
          return row;
        });

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

      submitPlot(dir, 'cdf()', function() {
        console.log('R done');
        $('#graph').prop('src', dir + '/Rplots.svg');
      });
    });

    return false;
});

form.submit();

});
