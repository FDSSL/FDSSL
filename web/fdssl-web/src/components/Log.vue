<template>
  <div>
    <ul class="logs" id="array-rendering">
      <li class="log-entry" v-for="item in logs" :key="item.msg">
        <span :class="item.type">{{ item.msg }}</span>
      </li>
    </ul>
  </div>
</template>

<script>
export default {
  name: 'Log',
  props: ['logs']
}
</script>

<style scoped>
.log-entry {
  border-width: 1px;
  border-style: none none solid none;
  border-color: #999;
}

.log-entry .log {
  color: #333;
}

.log-entry .error {
  color: #b00;
}

</style>
