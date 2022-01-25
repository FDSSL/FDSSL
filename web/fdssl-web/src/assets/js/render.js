// Test for FDSSL
let timer1 = Math.random() * 10000.0;
const seed = Math.random() * 10000.0;
let vertexCount = 0;
let rotate = false;
let vsSource = `
precision highp float;
precision highp int;

uniform mat4 uProjectionMatrix;
uniform mat4 uModelViewMatrix;

attribute vec4 aVertPos;

varying vec2 vXY;

void main() {
  vXY = vec2(aVertPos.x, aVertPos.y);
  gl_Position = uProjectionMatrix * uModelViewMatrix  * aVertPos;
}`;

let fsSource = `
precision highp float;
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
}
`;

// function update() {
//   rotate = document.getElementById("rotatebutton").checked;
//
//   let vs = document.getElementById("vertexShader").value;
//   let fs = document.getElementById("fragmentShader").value;
//
//   // clear errors
//   document.getElementById("errors").value = "";
//
//   if(vs != "") {
//     vsSource = vs;
//   }
//
//   if(fs != "") {
//     fsSource = fs;
//   }
//
//   main();
// }

// window.onload = () => {
//   update();
//   document.getElementById("rotatebutton").addEventListener("click", (e) => {
//     update();
//   });
//
//   document.getElementById("updateShaders").addEventListener("click", (e) => {
//     update();
//   });
// }

//
// Start here
//
function main() {
  // TODO turn this loop back on as needed
  if(rotate) {
    requestAnimationFrame(main);
  }
  const canvas = document.querySelector('#canvas-1');
  const gl = canvas.getContext('webgl2'); // or webgl

  // If we don't have a GL context, give up now

  if (!gl) {
    alert('Unable to initialize WebGL. Your browser or machine may not support it.');
    return;
  }

  // Initialize a shader program; this is where all the lighting
  // for the vertices and so forth is established.
  const shaderProgram = initShaderProgram(gl, vsSource, fsSource);

  // Collect all the info needed to use the shader program.
  // Look up which attribute our shader program is using
  // for aVertexPosition and look up uniform locations.
  const programInfo = {
    program: shaderProgram,
    attribLocations: {
      vertexPosition: gl.getAttribLocation(shaderProgram, 'aVertPos')
    },
    uniformLocations: {
      projectionMatrix: gl.getUniformLocation(shaderProgram, 'uProjectionMatrix'),
      modelViewMatrix: gl.getUniformLocation(shaderProgram, 'uModelViewMatrix'),

      slowTime: gl.getUniformLocation(shaderProgram, 'uSlowTime'),
      seed: gl.getUniformLocation(shaderProgram, 'uSeed'),
      time: gl.getUniformLocation(shaderProgram, 'uTime'),
      lightX: gl.getUniformLocation(shaderProgram, 'LightX'),
      lightY: gl.getUniformLocation(shaderProgram, 'LightX'),
      lightZ: gl.getUniformLocation(shaderProgram, 'LightX'),

      //ambient: gl.getUniformLocation(shaderProgram, 'uAmbient'),
      //diffuse: gl.getUniformLocation(shaderProgram, 'uDiffuse'),
      //specular: gl.getUniformLocation(shaderProgram, 'uSpecular'),
      //specularColor: gl.getUniformLocation(shaderProgram, 'SpecularColor'),
      //cloudColor: gl.getUniformLocation(shaderProgram, 'uCloudColor'),
      //shininess: gl.getUniformLocation(shaderProgram, 'Shininess'),
      //volumeStart: gl.getUniformLocation(shaderProgram, 'uVolumeStart'),
      //volumeDimens: gl.getUniformLocation(shaderProgram, 'uVolumeDimens'),
      //cloudDensity: gl.getUniformLocation(shaderProgram, 'uCloudDensity'),
      //rayCastStepSize: gl.getUniformLocation(shaderProgram, 'uRayCastStepSize'),
      //ignoreBounds: gl.getUniformLocation(shaderProgram, 'uIgnoreBounds'),

      //octaves: gl.getUniformLocation(shaderProgram, 'uOctaves')
    },
  };

  // Here's where we call the routine that builds all the
  // objects we'll be drawing.
  const buffers = initBuffers(gl);

  // Draw the scene
  drawScene(gl, programInfo, buffers);
}

//
// initBuffers
//
// Initialize the buffers we'll need. For this demo, we just
// have one object -- a simple two-dimensional square.
//
function initBuffers(gl) {

  // Create a buffer for the square's positions.

  const positionBuffer = gl.createBuffer();

  // Select the positionBuffer as the one to apply buffer
  // operations to from here out.

  gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);

  // cube positions
  const positions = [
    -1.0, 1.0, 1.0,     // Front-top-left
    1.0, 1.0, 1.0,      // Front-top-right
    -1.0, -1.0, 1.0,    // Front-bottom-left
    1.0, -1.0, 1.0,     // Front-bottom-right
    1.0, -1.0, -1.0,    // Back-bottom-right
    1.0, 1.0, 1.0,      // Front-top-right
    1.0, 1.0, -1.0,     // Back-top-right
    -1.0, 1.0, 1.0,     // Front-top-left
    -1.0, 1.0, -1.0,    // Back-top-left
    -1.0, -1.0, 1.0,    // Front-bottom-left
    -1.0, -1.0, -1.0,   // Back-bottom-left
    1.0, -1.0, -1.0,    // Back-bottom-right
    -1.0, 1.0, -1.0,    // Back-top-left
    1.0, 1.0, -1.0      // Back-top-right
  ];

  vertexCount = positions.length / 3;

  // Now pass the list of positions into WebGL to build the
  // shape. We do this by creating a Float32Array from the
  // JavaScript array, then use it to fill the current buffer.

  gl.bufferData(gl.ARRAY_BUFFER,
                new Float32Array(positions),
                gl.STATIC_DRAW);

  return {
    position: positionBuffer
  };
}

//
// Draw the scene.
//
function drawScene(gl, programInfo, buffers) {
  gl.clearColor(0.0, 0.0, 0.0, 0.0);  // Clear to black, fully opaque
  gl.clearDepth(1.0);                 // Clear everything
  gl.enable(gl.DEPTH_TEST);           // Enable depth testing
  gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
  gl.enable(gl.BLEND);
  gl.depthFunc(gl.LEQUAL);            // Near things obscure far things

  // Clear the canvas before we start drawing on it.

  gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

  // Create a perspective matrix, a special matrix that is
  // used to simulate the distortion of perspective in a camera.
  // Our field of view is 45 degrees, with a width/height
  // ratio that matches the display size of the canvas
  // and we only want to see objects between 0.1 units
  // and 100 units away from the camera.

  const fieldOfView = 45 * Math.PI / 180;   // in radians
  const aspect = gl.canvas.clientWidth / gl.canvas.clientHeight;
  const zNear = 0.1;
  const zFar = 100.0;
  const projectionMatrix = mat4.create();

  // note: glmatrix.js always has the first argument
  // as the destination to receive the result.
  mat4.perspective(projectionMatrix,
                   fieldOfView,
                   aspect,
                   zNear,
                   zFar);

  // Set the drawing position to the "identity" point, which is
  // the center of the scene.
  const modelViewMatrix = mat4.create();

  //mat4.rotate(modelViewMatrix, modelViewMatrix, [1.0, 0.0, 0.0]);

  // Now move the drawing position a bit to where we want to
  // start drawing the square.
  /**/
  mat4.translate(modelViewMatrix,     // destination matrix
                 modelViewMatrix,     // matrix to translate
                 [0.0, 0.0, -6.0]);  // amount to translate
                 // -0.0, 0.0, -6.0
                 /**/

  mat4.rotate(modelViewMatrix,modelViewMatrix, -45.0, [1.0,0.0,0.0]);
  //mat4.rotate(modelViewMatrix,modelViewMatrix, -45.0 [0.0,1.0,0.0]);
  mat4.rotate(modelViewMatrix,modelViewMatrix, timer1 * 1.0, [0.0,0.0,1.0]);

  // Tell WebGL how to pull out the positions from the position
  // buffer into the vertexPosition attribute.
  {
    const numComponents = 3;
    const type = gl.FLOAT;
    const normalize = false;
    const stride = 0;
    const offset = 0;
    gl.bindBuffer(gl.ARRAY_BUFFER, buffers.position);
    gl.vertexAttribPointer(
        programInfo.attribLocations.vertexPosition,
        numComponents,
        type,
        normalize,
        stride,
        offset);
    gl.enableVertexAttribArray(programInfo.attribLocations.vertexPosition);
  }

  // Tell WebGL to use our program when drawing
  gl.useProgram(programInfo.program);

  // Set the shader uniforms
  gl.uniformMatrix4fv(
      programInfo.uniformLocations.projectionMatrix,
      false,
      projectionMatrix);

  gl.uniformMatrix4fv(
      programInfo.uniformLocations.modelViewMatrix,
      false,
      modelViewMatrix);

  // additional uniforms
  gl.uniform1f(programInfo.uniformLocations.seed, seed);
  gl.uniform1f(programInfo.uniformLocations.slowTime, timer1 * 5.0);
  gl.uniform1f(programInfo.uniformLocations.time, timer1 * 2.5);
  gl.uniform1f(programInfo.uniformLocations.lightX, 10.0); // 10.0 * Math.sin(timer1 * 5.0)
  gl.uniform1f(programInfo.uniformLocations.lightY, 10.0); // 10.0 * Math.cos(timer1 * 5.0)
  gl.uniform1f(programInfo.uniformLocations.lightZ, 10.0); // 0.0

  //gl.uniform1f(programInfo.uniformLocations.ambient, 0.1);
  //gl.uniform1f(programInfo.uniformLocations.diffuse, 0.6);
  //gl.uniform1f(programInfo.uniformLocations.specular, 1.0);
  //gl.uniform3f(programInfo.uniformLocations.specularColor, 1.0, 1.0, 1.0);
  //gl.uniform3f(programInfo.uniformLocations.cloudColor, 1.0, 0.0, 0.0);
  //gl.uniform1f(programInfo.uniformLocations.shininess, 6.0);
  //gl.uniform3f(programInfo.uniformLocations.volumeStart, -1.0, -1.0, -1.0);
  //gl.uniform3f(programInfo.uniformLocations.volumeDimens, 2.0, 2.0, 2.0);
  //gl.uniform1f(programInfo.uniformLocations.cloudDensity, 0.222); // 0.222
  //gl.uniform1f(programInfo.uniformLocations.rayCastStepSize, 0.025); // 0.025
  //gl.uniform1f(programInfo.uniformLocations.ignoreBounds, false);

  timer1+=0.005;

  {
    const offset = 0;
    const vertexCount2 = vertexCount;
    gl.drawArrays(gl.TRIANGLE_STRIP, offset, vertexCount2);
    //gl.drawArrays(gl.TRIANGLE_FAN, offset, heartCount);
  }
}

//
// Initialize a shader program, so WebGL knows how to draw our data
//
function initShaderProgram(gl, vsSource, fsSource) {
  const vertexShader = loadShader("vertex", gl, gl.VERTEX_SHADER, vsSource);
  const fragmentShader = loadShader("fragment", gl, gl.FRAGMENT_SHADER, fsSource);

  // Create the shader program

  const shaderProgram = gl.createProgram();
  gl.attachShader(shaderProgram, vertexShader);
  gl.attachShader(shaderProgram, fragmentShader);
  gl.linkProgram(shaderProgram);

  // If creating the shader program failed, alert

  if (!gl.getProgramParameter(shaderProgram, gl.LINK_STATUS)) {
    alert('Unable to initialize the shader program: ' + gl.getProgramInfoLog(shaderProgram));
    return null;
  }

  return shaderProgram;
}

//
// creates a shader of the given type, uploads the source and
// compiles it.
//
function loadShader(name, gl, type, source) {
  const shader = gl.createShader(type);

  // Send the source to the shader object

  gl.shaderSource(shader, source);

  // Compile the shader program

  gl.compileShader(shader);

  // See if it compiled successfully

  if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
    //alert("You have " + name + " shader errors in the console");
    //console.info('An error occurred compiling the ' + name + ' shader:\n' + gl.getShaderInfoLog(shader));
    document.getElementById("errors").value = "You have " + name + " shader errors in the console\n\n" + 'An error occurred compiling the ' + name + ' shader:\n' + gl.getShaderInfoLog(shader);
    gl.deleteShader(shader);
    // stop updating...
    rotate = false;
    return null;
  }

  return shader;
}
