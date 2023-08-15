#version 330 core

layout (points) in;

in VERT_OUT {
  ivec3 blockOffset;
  uint rgba[16384];
} gs_in[];

layout (triangles, max_vertices = 57344) out;
out vec4 fColor;

void main() {
  const vec4 offset = gl_in[0].gl_Position;
  const vec4 fblockOffset = vec4(gs_in[0].blockOffset, 1);

  for(int i=0;i<4096;i++){
    fColor = vec4(float(gs_in[0].rgba[i]), 
                  float(gs_in[0].rgba[i+1]), 
                  float(gs_in[0].rgba[i+2]), 
                  float(gs_in[0].rgba[i+3]));
    if(fColor.a < 0.1){
      continue;
    }
    ivec3 pos = vecIdxToVox(i);
    vec4 fpos = vec4(pos, 1);
    vec4 basePos = offset + fblockOffset + fpos;

    gl_Position = basePos + vec4(0.f, 1.f, 1.f, 1.f);
    EmitVertex();
    glPosition = basePos + vec4(1.f, 1.f, 1.f, 1.f);
    EmitVertex();
    glPosition = basePos + vec4(0.f, 0.f, 1.f, 1.f);
    EmitVertex();
    glPosition = basePos + vec4(1.f, 0.f, 1.f, 1.f);
    EmitVertex();
    glPosition = basePos + vec4(1.f, 0.f, 0.f, 1.f);
    EmitVertex();
    glPosition = basePos + vec4(1.f, 1.f, 1.f, 1.f);
    EmitVertex();
    glPosition = basePos + vec4(1.f, 1.f, 0.f, 1.f);
    EmitVertex();
    glPosition = basePos + vec4(0.f, 1.f, 1.f, 1.f);
    EmitVertex();
    glPosition = basePos + vec4(0.f, 1.f, 0.f, 1.f);
    EmitVertex();
    glPosition = basePos + vec4(0.f, 0.f, 1.f, 1.f);
    EmitVertex();
    glPosition = basePos + vec4(0.f, 0.f, 0.f, 1.f);
    EmitVertex();
    glPosition = basePos + vec4(1.f, 0.f, 0.f, 1.f);
    EmitVertex();
    glPosition = basePos + vec4(0.f, 1.f, 0.f, 1.f);
    EmitVertex();
    glPosition = basePos + vec4(1.f, 1.f, 0.f, 1.f);
    EmitVertex();
    EndPrimitive();
  }
}

ivec3 vecIdxToVox(const int i) {
  r = i / 64;
  d = mod(i, 64);  
  p = d * 64 + r;
  z = p / 256;
  xy = mod(p, 256);
  y = xy / 16;
  x = mod(xy, 16);
  return ivec3(x, y, z);
}