import { Elm } from './elm/Main.elm';
import CodeMirror from 'codemirror';
import { dia, shapes } from 'jointjs';
import 'codemirror/mode/python/python';
import 'codemirror/lib/codemirror.css';
import 'code-mirror-themes/themes/monokai.css';

// const app = Elm.Main.init({
//   node: document.getElementById('elm')
// });

const graph = new dia.Graph;

var paper = new dia.Paper({
        el: document.getElementById('elm'),
        model: graph,
        width: "100%",
        height: "100%",
        snapLinks: { radius: 15 }
});


var connect = function(source, sourcePort, target, targetPort) {

    var link = new shapes.devs.Link({
        source: {
            id: source.id,
            port: sourcePort
        },
        target: {
            id: target.id,
            port: targetPort
        }
    });

    link.addTo(graph).reparent();
}

const i1 = new shapes.devs.Model({
  postition: { x: 150, y: 150 },
  size: { width: 200, height: 50 },
  outPorts: ['val'],
});
console.log(i1)
i1.attr({ label: { text: "6" } });

const printer = new shapes.devs.Model({
  position: { x: 350, y: 150 },
  size: { width: 200, height: 50 },
  inPorts: ['0'],
});


graph.addCells([i1, printer]);
connect(i1, 'val', printer, '0');

graph.on('change:source change:target', function(link) {
  var sourcePort = link.get('source').port;
  var sourceId = link.get('source').id;
  var targetPort = link.get('target').port;
  var targetId = link.get('target').id;

  var m = [
          'The port <b>' + sourcePort,
          '</b> of element with ID <b>' + sourceId,
          '</b> is connected to port <b>' + targetPort,
          '</b> of elemnt with ID <b>' + targetId + '</b>'
  ].join('');
  console.log(m)
});
console.log(graph.toJSON())

CodeMirror(document.getElementById('setup'), {
  value: '# setup', mode: 'python', theme: 'monokai'
}).on('keyup', function(e) {
  app.ports.setupReceiver.send(e.doc.getValue());
});

CodeMirror(document.getElementById('teardown'), {
  value: '# teardown', mode: 'python', theme: 'monokai'
}).on('keyup', function(e) {
  app.ports.teardownReceiver.send(e.doc.getValue());
});
