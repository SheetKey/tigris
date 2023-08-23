#version 330 core

layout (points) in;

in VERT_OUT {
  vec3 blockOffset;
  float rgba[16384];
} gs_in[];

layout (triangle_strip, max_vertices = 256) out;
out vec4 fColor;

vec3 vecIdxToVox(const int i) {
  int r = i / 64;
  int d = int(mod(float(i), 64));  
  int p = d * 64 + r;
  int z = p / 256;
  int xy = int(mod(float(p), 256));
  int y = xy / 16;
  int x = int(mod(float(xy), 16));
  return vec3(ivec3(x, y, z));
}

void main() {
  vec4 offset = gl_in[0].gl_Position;
  vec4 fblockOffset = vec4(gs_in[0].blockOffset, 1);

  for(int i=0;i<4096;i++){
    fColor = vec4(gs_in[0].rgba[i], 
                  gs_in[0].rgba[i+1], 
                  gs_in[0].rgba[i+2], 
                  gs_in[0].rgba[i+3]);
    if(fColor.a < 0.1){
      continue;
    }
    vec3 pos = vecIdxToVox(i);
    vec4 fpos = vec4(pos, 1);
    vec4 basePos = offset + fblockOffset + fpos;

    gl_Position = basePos + vec4(0.f, 1.f, 1.f, 1.f);
    EmitVertex();
    gl_Position = basePos + vec4(1.f, 1.f, 1.f, 1.f);
    EmitVertex();
    gl_Position = basePos + vec4(0.f, 0.f, 1.f, 1.f);
    EmitVertex();
    gl_Position = basePos + vec4(1.f, 0.f, 1.f, 1.f);
    EmitVertex();
    gl_Position = basePos + vec4(1.f, 0.f, 0.f, 1.f);
    EmitVertex();
    gl_Position = basePos + vec4(1.f, 1.f, 1.f, 1.f);
    EmitVertex();
    gl_Position = basePos + vec4(1.f, 1.f, 0.f, 1.f);
    EmitVertex();
    gl_Position = basePos + vec4(0.f, 1.f, 1.f, 1.f);
    EmitVertex();
    gl_Position = basePos + vec4(0.f, 1.f, 0.f, 1.f);
    EmitVertex();
    gl_Position = basePos + vec4(0.f, 0.f, 1.f, 1.f);
    EmitVertex();
    gl_Position = basePos + vec4(0.f, 0.f, 0.f, 1.f);
    EmitVertex();
    gl_Position = basePos + vec4(1.f, 0.f, 0.f, 1.f);
    EmitVertex();
    gl_Position = basePos + vec4(0.f, 1.f, 0.f, 1.f);
    EmitVertex();
    gl_Position = basePos + vec4(1.f, 1.f, 0.f, 1.f);
    EmitVertex();
    EndPrimitive();
  }
}
