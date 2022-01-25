<template>
  <button @click="$emit('requestBuild')" :disabled="isDisabled">{{ value }}</button>
</template>

<script>
export default {
  name: 'Build',
  data: () => {
    return {}
  },

  // parent can signal whether we are building, overriding this components's state
  // TODO setup 2-way signaling so the parent can signal back that a build is done
  props: ['building'],

  // emit an event on build request, will be handled by parent
  emits: ['requestBuild'],

  computed: {
    // button enabled state is dynamically computed based on whether we are (or are not building)
    isDisabled: function () {
      return this.building
    },

    value: function() {
      if(this.building) {
        return "...";
      } else {
        return "Build >";
      }
    }
  },

  methods: {
  }
}
</script>

<style scoped>

</style>
