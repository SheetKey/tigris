#version 330 core

layout (location = 0) in vec3 aPos;

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;
uniform mat4 rmat;
// uniform for pos offset of tree

void main() {
  // gl_Position = proj * view * model * rmat * vec4(aPos, 1);
  gl_Position = vec4(aPos, 1);
}