<script>
	import { onMount } from 'svelte';
  import { writable } from 'svelte/store';
  import CodeMirror from 'codemirror';
  import 'codemirror/lib/codemirror.css';
  import 'code-mirror-themes/themes/monokai.css';
  import 'codemirror/mode/python/python';
  const program = {
    nodes: {
    },
    edges: {
    }
  };
  const editors = {setup: undefined, teardown: undefined},
        setup = writable(""),
        teardown = writable("");
  let setup_elem, teardown_elem,
      canvas, ctx;
  onMount(() => {
    editors.setup = CodeMirror(setup_elem, {value: "# setup", mode: "python", theme: "monokai"});
    editors.teardown = CodeMirror(teardown_elem, {value: "# teardown", mode: "python", theme: "monokai"});
    ctx = canvas.getContext("2d");
  });

  const code_edit = source => e => {
    const val = editors[source].doc.cm.getDoc().getValue();
    (source === "setup" ? setup : teardown).set(val);
  };
</script>

<aside>
  <div bind:this={setup_elem} on:keyup={code_edit("setup")}></div>
  <div bind:this={teardown_elem} on:keyup={code_edit("teardown")}></div>
</aside>

<canvas bind:this={canvas}></canvas>

<style lang="sass">
  aside
    height: 100%
    border-right: 1px solid black
    width: 320px
    & div:nth-child(1)
      padding-bottom: 1em
  canvas
    flex-grow: 2
</style>
