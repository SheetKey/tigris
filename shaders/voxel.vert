#version 330 core

layout (location = 0) in vec4 aPos;
layout (location = 1) in vec3 blockOffsetIn;

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;
uniform mat4 rmat;
uniform float rgbaIn[16384];

out VERT_OUT {
  vec3 blockOffset;
  float rgba[16384];
} vert_out;

void main() {
  // gl_Position = proj * view * model * rmat * aPos;
  gl_Position = aPos;
  vert_out.blockOffset = blockOffsetIn;
  vert_out.rgba = rgbaIn;
}