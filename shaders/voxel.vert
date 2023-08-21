#version 330 core

layout (location = 0) in vec4 aPos;
layout (location = 1) in vec3 blockOffsetIn;
layout (location = 2) in float rgbaIn[16384];

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;
uniform mat4 rmat;

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