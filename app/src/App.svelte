<script>
  export let name;
  const program = {
    nodes: {
      i1: "6"
    },
    edges: {
    }
  };
  const userAction = async () => {
    const response = await fetch('http://localhost:3000/', {
      method: 'POST',
      body: JSON.stringify(program),
      headers: { 'Content-Type': 'application/json' }
    });
    console.log(await response.text());
  }
</script>

<main>
  <h1 on:mousedown="{userAction}">Hello {name}!</h1>
  <p>Visit the <a href="https://svelte.dev/tutorial">Svelte tutorial</a> to learn how to build Svelte apps.</p>
</main>

<style>
  main {
    text-align: center;
    padding: 1em;
    max-width: 240px;
    margin: 0 auto;
  }

  h1 {
    color: #ff3e00;
    text-transform: uppercase;
    font-size: 4em;
    font-weight: 100;
  }

  @media (min-width: 640px) {
    main {
      max-width: none;
    }
  }
</style>
