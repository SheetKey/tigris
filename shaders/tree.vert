#version 330 core

layout (location = 0) in vec3 aPos;

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;
uniform mat4 rmat;

void main() {
  gl_Position = proj * view * model * rmat * vec4(aPos, 1);
}