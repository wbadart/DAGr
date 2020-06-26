import { Elm } from './elm/Main.elm';
import CodeMirror from 'codemirror';
import 'codemirror/mode/python/python';
import 'codemirror/lib/codemirror.css';
import 'code-mirror-themes/themes/monokai.css';

Elm.Main.init({
  node: document.getElementById('elm')
});

CodeMirror(document.getElementById('setup'), {
  value: '# setup', mode: 'python', theme: 'monokai'
});
CodeMirror(document.getElementById('teardown'), {
  value: '# teardown', mode: 'python', theme: 'monokai'
});
