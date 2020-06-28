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
let current_node;


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
    inPorts: [ 'in_1' ],
    outPorts: [ 'out' ],
    attrs: { text: { text: expr } },
  });
  graph.addCell(node);
  console.log('Added node:', node)
  update_graph()
});

paper.on('cell:pointerdblclick', (cellView, evt, x, y) => {
  console.log(cellView, evt, x, y);
  if (current_node) {
    const [current_elem, current_obj] = current_node;
    current_elem.classList.remove('active');
  }
  evt.target.classList.add('active');
  current_node = [evt.target, cellView.model];
  console.log('cur', current_node);
});

function update_graph() {
  const msg = {
    nodes: graph.getElements().reduce(
      (acc, e) => {
        acc[e.attributes.id] = e.attributes.attrs.text.text;
        return acc;
      }, {}),
    edges: graph.getLinks().map(e => {
      if (e.attributes.target.port === "out") return false;
      return {
        src: e.attributes.source.id,
        dst: e.attributes.target.id,
        order: JSON.parse(e.attributes.target.port.split('_')[1])
      }
    }).filter(x => x),
  };
  console.log('Sending message to Elm:', msg);
  app.ports.graphReceiver.send(msg);
}


// ==========
// JointJS configuration
// ==========

graph.on('change', function(e) {
  if (e.attributes.type === "link" && e.attributes.target.id)
    update_graph();
});

graph.on('remove', function(e) {
  update_graph();
});

document.getElementById('port_adder').onclick = () => {
  if (!current_node) return;
  const [elem, obj] = current_node;
  const port_num = obj.attributes.ports.items.length;
  obj.addInPort(`in_${port_num}`);
};
