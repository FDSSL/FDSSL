<template>
  <textarea id="editor" placeholder="Write an FDSSL shader here, and click 'Build' below to see what it does!">{{ prog }}</textarea>
</template>

<script>
export default {
  name: 'ShaderInput',
  props: {
    prog: String
  }
}
</script>

<style scoped>
#editor {
  resize: none;
  width: 480px;
  border: 1px solid #ccc;
  border-radius: 4px;
}

@media(max-width: 1100px) {
  #editor {
    width: 580px;
    max-width: 600px;
  }
}

@media(max-width: 650px) {
  #editor {
    width: 95%;
    margin: 4px auto;
  }
}
</style>
