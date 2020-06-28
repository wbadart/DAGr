import { Elm } from './elm/Main.elm';
import CodeMirror from 'codemirror';
import { dia, shapes } from 'jointjs';
import 'codemirror/mode/python/python';
import 'codemirror/lib/codemirror.css';
import 'code-mirror-themes/themes/monokai.css';

import generateName from './names.js';


const app = Elm.Main.init({
  node: document.getElementById('elm')
});
const graph = new dia.Graph;
const paper = new dia.Paper({
  el: document.getElementById('paper'),
  model: graph,
  width: "100%",
  height: "100%",
  snapLinks: { radius: 15 }
});


// ==========
// Setup and teardown editors
// ==========


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


// ==========
// Elm events
// ==========


app.ports.compiledProgram.subscribe(function(prog) {
  console.log('RESULT:');
  console.log(prog);
  alert(prog);
});


app.ports.newNode.subscribe(function(expr) {
  const node = new shapes.devs.Model({
    id: generateName(),
    position: { x: 40, y: 20 },
    size: { width: 200, height: 50 },
    inPorts: [ 'in' ],
    outPorts: [ 'out' ],
    attrs: { text: { text: expr } },
  });
  graph.addCell(node);
  update_graph()
});

function update_graph() {
  const msg = {
    nodes: graph.getElements().reduce(
      (acc, e) => {
        acc[e.attributes.id] = e.attributes.attrs.text.text;
        return acc;
      }, {}),
    edges: graph.getLinks().map(
      e => ({
        src: e.attributes.source.id,
        dst: e.attributes.target.id,
      })
    ),
  };
  console.log('Sending message to Elm:', msg);
  app.ports.graphReceiver.send(msg);
}


// ==========
// JointJS configuration
// ==========
//


graph.on('change', function(e) {
  if (e.attributes.type !== "link" || !e.attributes.target.id) return
  update_graph()
});

graph.on('remove', function(e) {
  update_graph()
});
