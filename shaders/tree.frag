#version 330 core

in vec3 FragPos;
in vec3 Normal;

out vec4 FragColor;

void main() {
  // ambient
  float ambientStrength = 0.1;
  vec3 lightColor = vec3(1.0, 1.0, 1.0);
  vec3 ambient = ambientStrength * lightColor;

  // diffuse
  vec3 norm = normalize(Normal);
  vec3 lightPos = vec3(0.0, 500.0, 0.0);
  vec3 lightDir = normalize(lightPos - FragPos);
  float diff = max(dot(norm, lightDir), 0.0);
  vec3 diffuse = diff * lightColor;

  vec3 objectColor = vec3(0.35f, 0.2f, 0.0f);
  vec3 result = (ambient + diffuse) * objectColor;
  FragColor = vec4(result, 1.0);
}