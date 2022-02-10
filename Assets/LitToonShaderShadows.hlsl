void LitToonShading_float(in float3 Normal, in float RampSmoothness, in float3 ClipSpacePosition, in float3 WorldPosition, in float4 RampTint,
                          in float RampOffset, out float3 RampOutput, out float3 Direction)
{
    #ifdef SHADERGRAPH_PREVIEW
        RampOutput = float3(0.5, 0.5, 0.5);
        Direction = float3(0.5, 0.5, 0.5);
    #else
        #if SHADOWS_SCREEN
            half4 shadowCoord = ComputeScreenPos(ClipSpacePosition);
        #else
            half4 shadowCoord = TransformWorldToShadowCoord(WorldPosition);
    #endif
    
    #if _MAIN_LIGHT_SHADOWS_CASCADE || _MAIN_LIGHT_SHADOWS
        Light light = GetMainLight(shadowCoord);
    #else
        Light light = GetMainLight();
    #endif
    
    half d = dot(Normal, light.direction) * 0.5 + 0.5; // math here to normalize into UV space
    half ramp = smoothstep(RampOffset, RampOffset + RampSmoothness, d);
    
    ramp *= light.shadowAttenuation;
    
    RampOutput = light.color * (ramp + RampTint);
    Direction = light.direction;
    
    #endif
}