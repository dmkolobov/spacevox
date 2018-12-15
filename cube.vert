
#version 430 core

uniform mat4 view_matrix;
uniform mat4 projection_matrix;

layout (location = 0) in vec4 position;
layout (location = 1) in vec3 norm;
layout (location = 2) in mat4 voxel_matrix;

out vec4 Color;
out vec3 Normal;

out vec3 LightDirection;
out vec3 HalfVector;
out float Attenuation;

void main(void)
{
    float ConstantAttenuation = 0.5;
    float LinearAttenuation = 0.1;
    float QuadraticAttenuation = 0.01;

    vec3 LightPosition = vec3(2.0, 2.0, 2.0);
    vec3 eye_direction  = mat3(view_matrix) * vec3(0.0, 0.0, 0.0);

    LightDirection = LightPosition - vec3(position);
    float lightDistance = length(LightDirection);
    LightDirection = LightDirection / lightDistance;

    Attenuation = 1.0 / (ConstantAttenuation +
                         LinearAttenuation * lightDistance +
                         QuadraticAttenuation * lightDistance * lightDistance);
    
    HalfVector = normalize(LightDirection + eye_direction);

    Color = vec4(0.5, 0.5, 0.5, 1.0);

    Normal = (norm);

    float foo = 1.0; 
    int bar = 2;

    float car = foo * bar;

    gl_Position = projection_matrix * view_matrix * (voxel_matrix * position);
}
