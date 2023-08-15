#version 330 core

in vec2 TexCoord;

out vec4 FragColor;

uniform sampler2D Texture;

void main() {
  vec4 texColor = texture(Texture, TexCoord);
  if(texColor.a < 0.1)
    discard;
  FragColor = texColor;
}