#version 330 core

uniform vec3 Ambient;
uniform vec3 LightColor;
uniform float Shininess;
uniform float Strength;

in vec4 Color;
in vec3 Normal;

in vec3 LightDirection;
in vec3 HalfVector;
in float Attenuation;

out vec4 FragColor;

void main()
{
    vec3 Ambient = vec3(0.75,0.75,0.75);
    vec3 LightColor = vec3(0.5,0.5,0.5);

    float Shininess = 0.1;
    float Strength = 0.5;
// LightDirection, HalfVector, and Attenuation are interpolated
// now, from vertex shader calculations
float diffuse = max(0.0, dot(Normal, LightDirection));
float specular = max(0.0, dot(Normal, HalfVector));
if (diffuse == 0.0)
specular = 0.0;
else
specular = pow(specular, Shininess) * Strength;
vec3 scatteredLight = Ambient + LightColor * diffuse * Attenuation;
vec3 reflectedLight = LightColor * specular * Attenuation;
vec3 rgb = min(Color.rgb * scatteredLight + reflectedLight,
vec3(1.0));
FragColor = vec4(rgb, Color.a);
}