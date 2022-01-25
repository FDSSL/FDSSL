<template>
  <div>
    <h1 style='text-align:center'>FDSSL Web Editor</h1>
  </div>
  <div class="flex-contain">
    <div class="half min500">
      <Editor class="h450 comp marg pad" :shader="shader" />
      <div class="h50 marg">
        <Build class="build comp" :building="building" @request-build="handleBuildRequest"/>
      </div>
    </div>
    <div class="half min500 comp marg pad" comp>
      <OpenGLDisplay :isOpenGLErrored="isOpenGLErrored" :vertShader="vertShader" :fragShader="fragShader" @on-gl-error="handleOpenGLError" />
    </div>
  </div>
  <Log class="h150 scroll marg comp pad" :logs="logs" />
  <!--<img alt="Vue logo" src="./assets/logo.png">-->
  <!--<HelloWorld msg="Welcome to Your Vue.js App"/>-->
  <div style='text-align:center;color:#ccc'>
    <span>• • •<br/>Created by Benjamin F. Wilson &amp; Cody Holiday - Copyright &copy; 2022</span>
  </div>
</template>

<script>
import Build from './components/Build.vue'
import Editor from './components/Editor.vue'
import Log from './components/Log.vue'
import OpenGLDisplay from './components/OpenGLDisplay.vue'

export default {
  name: 'App',
  components: {
    Build,
    Editor,
    Log,
    OpenGLDisplay
  },
  methods: {
    handleBuildRequest() {
      if(this.building) {
        // ignore repeat build requests
      }
      this.building = true;

      // compile the shader...
      this.log("compiling shaders...");
      let shaderResult = this.compile(this.shader);

      // update the computed shaders
      if(shaderResult != null && shaderResult.code == 200) {
        // shaders compiled OK
        this.vertShader = shaderResult.vert;
        this.fragShader = shaderResult.frag;
        this.log("vertex & frag shaders built successfully");

      } else {
        //
        this.error("unable to compile frag & vertex shaders");

      }

      // no longer building
      this.building = false;

    },

    // handle any opengl errors that are returned from the display
    handleOpenGLError(err) {
      // report the error
      this.isOpenGLErrored = true;
      this.error("[OpenGL ES] "+err);
    },

    log(msg) {
      this.logs.push({msg: msg, type: "log"});
    },

    error(msg) {
      this.logs.push({msg: msg, type: "error"});
    },

    compile(fdsslShader) {
      // compile an FDSSL shader...
      // do something for a moment...probably can use promises after this
      // TODO in this compile phase is where we pass an FDSSL shader to be compiled to vertex & fragment shaders
      return {
        code: 200, // passing code
        vert: this.vertShader,
        frag: this.fragShader
      };
    }
  },
  data: () => {
    return {
      building: false,
      isOpenGLErrored: false,
      vertShader: `
      precision highp float;
      precision highp int;

      uniform mat4 uProjectionMatrix;
      uniform mat4 uModelViewMatrix;

      attribute vec4 aVertPos;

      varying vec2 vXY;

      void main() {
        vXY = vec2(aVertPos.x, aVertPos.y);
        gl_Position = uProjectionMatrix * uModelViewMatrix  * aVertPos;
      }`,
      fragShader: `precision highp float;
      precision highp int;

      uniform mat4 uProjectionMatrix;
      uniform mat4 uModelViewMatrix;
      uniform float	uTime;

      varying vec2 vXY;

      void main() {
        float x = vXY.x + uTime * 3.3;
        float y = vXY.y + uTime * 3.7;

        float r = cos(x);
        float g = sin(y);
        float b = r + g;

        gl_FragColor = vec4(r,g,b,1.0);
      }`,
      shader: "",
      logs: []
    }
  }
}
</script>

<style>
#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  /*text-align: center;*/
  color: #000;
  margin-top: 60px;
  /*background: #ccc;*/
  max-width: 1000px;
  margin: 8px auto;
}

.flex-contain {
  display: flex;
}

.half {
  flex: 1 1 auto;
  width: 500px;
}

.min500 {
  min-height: 500px;
}

.h100 {
  height: 100px;
}

.h150 {
  height: 150px;
}

.h450 {
  height: 450px;
}

.h50 {
  height: 50px;
}

.h30 {
  height: 30px;
}

.scroll {
  overflow-y: scroll;
}

.pad {
  padding: 4px;
}

.marg {
  margin: 4px;
}

.comp {
  background: #ccc;
  border-radius: 4px;
}

.build {
  background: #fff;
  border: 1px solid #000;
  flex: 1 1 auto;
  width: 100%;
  height: 100%;
  display: block;
  transition: 0.3s;
}

.build:hover {
  cursor: pointer;
  color: #fff;
  background: #aca;
}

.build:active {
  background: #6c6;
  border-color: #aca;
}

@media(max-width: 1100px) {
  .flex-contain {
    display: block;
    max-width: 600px;
    margin: 0 auto;
  }

  .build {
    width: 590px;
    max-width: 590px;
  }

  .half {
    flex: none;
    display: block;
    max-width: 600px;
    width: auto;
  }
}

@media(max-width: 650px) {
  .build {
    width: 200px;
    margin: 0 auto;
  }
}
</style>
