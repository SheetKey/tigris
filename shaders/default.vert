#version 330 core

layout (location = 0) in vec4 aPos;
layout (location = 1) in vec2 aTexCoord;

out vec2 TexCoord;

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;
uniform mat4 rmat;


void main() {
  gl_Position = proj * view * model * rmat * aPos;
  TexCoord = aTexCoord;
}