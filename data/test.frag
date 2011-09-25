uniform sampler2D tex;
const float epsilon = 1e-6;

vec3 RGBtoHSV(vec3 color)
{
  /* hue, saturation and value are all in the range [0,1> here, as opposed to their
     normal ranges of: hue: [0,360>, sat: [0, 100] and value: [0, 256> */
  int sortindex[3];
  sortindex[0]=0;
  sortindex[1]=1;
  sortindex[2]=2;
  float rgbArr[3];
  rgbArr[0]=color.r;
  rgbArr[1]=color.g;
  rgbArr[2]=color.b;

  float hue, saturation, value, diff;
  float minCol, maxCol;
  int minIndex, maxIndex;

  if(color.g < color.r)
    sortindex[0] = sortindex[1];
  if(color.b < color.g)
    sortindex[1] = sortindex[2];
  if(color.r < color.b)
    sortindex[2] =sortindex[0];

  minIndex = sortindex[0];
  maxIndex = sortindex[2];
  minCol = rgbArr[minIndex];
  maxCol = rgbArr[maxIndex];

  diff = maxCol - minCol;

  /* Hue */
  if( diff < epsilon){
    hue = 0.0;
  }
  else if(maxIndex == 0){
    hue = ((1.0/6.0) * ( (color.g - color.b) / diff )) + 1.0;
    hue = fract(hue);
  }
  else if(maxIndex == 1){
    hue = ((1.0/6.0) * ( (color.b - color.r) / diff )) + (1.0/3.0);
  }
  else if(maxIndex == 2){
    hue = ((1.0/6.0) * ( (color.r - color.g) / diff )) + (2.0/3.0);
  }

  /* Saturation */
  if (maxCol < epsilon)
    saturation = 0.0;
  else
    saturation = (maxCol - minCol) / maxCol;

  /* Value */
  value = maxCol;

  return vec3(hue, saturation, value);
}

int modulus(float x,float y) {
  return int(x - y * floor(x/y));
}

vec3 HSVtoRGB(vec3 color)
{
  float f,p,q,t, hueRound;
  int hueIndex;
  float hue, saturation, value;
  vec3 result;

  /* just for clarity */
  hue = color.r;
  saturation = color.g;
  value = color.b;
  float v = value;

  hueRound = floor(hue * 6.0);
  hueIndex = modulus(hueRound,6);
  f = (hue * 6.0) - hueRound;
  p = value * (1.0 - saturation);
  q = value * (1.0 - f*saturation);
  t = value * (1.0 - (1.0 - f)*saturation);

  if (hueIndex == 0) {
      result = vec3(v,t,p);
  }
  else if (hueIndex == 1) {
    result = vec3(q,v,p);
  }
  else if (hueIndex == 2) {
    result = vec3(p,v,t);
  }
  else if (hueIndex == 3) {
    result = vec3(p,q,v);
  }
  else if (hueIndex == 4) {
    result = vec3(t,p,v);
  }
  else if (hueIndex == 5) {
    result = vec3(v,p,q);
  }
  return result;
}
/* float LinearToGamma(float value, float gamma, float maxval) { return max*pow(value/maxval, 1/gamma); } */
/* float GammaToLinear(float value, float gamma, float maxval) { return max*pow(value/maxval, gamma); } */
void main()
{
  vec4 texel = texture2D(tex,gl_TexCoord[0].st);
  vec4 srcColor = texel;
  vec3 hsvColor = texel.rgb;
  vec3 rgbColor = texel.rgb;
  hsvColor = RGBtoHSV(srcColor.rgb);
  rgbColor = HSVtoRGB(hsvColor);
  gl_FragColor = vec4(rgbColor.r, rgbColor.g, rgbColor.b, srcColor.a);

}
