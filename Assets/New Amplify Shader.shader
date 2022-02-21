// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "Handdrawn"
{
	Properties
	{
		_RockLight("Rock Light", Color) = (0.6039216,0.682353,0.5803922,1)
		_MossLight("Moss Light", Color) = (0.6039216,0.682353,0.5803922,1)
		_RockDark("Rock Dark", Color) = (0.4862745,0.6039216,0.6156863,1)
		_MossDark("Moss Dark", Color) = (0.372549,0.5019608,0.4980392,1)
		_MossEdgeTHickness("Moss Edge THickness", Float) = 0.12
		_MultiplyAmount("Multiply Amount", Float) = 2.24
		_MossTranslate2("Moss Translate 2", Vector) = (0,0,0,0)
		_MossScale2("Moss Scale 2", Vector) = (6,6,6,0)
		_MossScale("Moss Scale", Vector) = (1.19,1,1,0)
		_MossTranslate("Moss Translate", Vector) = (0,0,0,0)
		_MatchingNoiseScale2("Matching Noise Scale 2", Vector) = (7,7,7,0)
		_MatchingTranslate("Matching Translate", Vector) = (0,0,0,0)
		_EdgeColor("Edge Color", Color) = (0.1490196,0.2509804,0.2627451,1)
		_MatchingNoiseScale("Matching Noise Scale", Vector) = (1,1,1,0)
		_Displacement("Displacement", Float) = 0.2
		_OutlineThickness("Outline Thickness", Float) = 0.01
		[HideInInspector] __dirty( "", Int ) = 1
	}

	SubShader
	{
		Tags{ }
		Cull Front
		CGPROGRAM
		#pragma target 3.0
		#pragma surface outlineSurf Outline nofog  keepalpha noshadow noambient novertexlights nolightmap nodynlightmap nodirlightmap nometa noforwardadd vertex:outlineVertexDataFunc 
		
		void outlineVertexDataFunc( inout appdata_full v, out Input o )
		{
			UNITY_INITIALIZE_OUTPUT( Input, o );
			float3 ase_worldPos = mul( unity_ObjectToWorld, v.vertex );
			float simplePerlin3D194 = snoise( ase_worldPos );
			simplePerlin3D194 = simplePerlin3D194*0.5 + 0.5;
			float simplePerlin3D193 = snoise( ase_worldPos*1.2 );
			simplePerlin3D193 = simplePerlin3D193*0.5 + 0.5;
			float simplePerlin3D192 = snoise( ase_worldPos*1.4 );
			simplePerlin3D192 = simplePerlin3D192*0.5 + 0.5;
			float3 appendResult195 = (float3(simplePerlin3D194 , simplePerlin3D193 , simplePerlin3D192));
			float3 ase_vertexNormal = v.normal.xyz;
			float3 outlineVar = ( ( _Displacement * appendResult195 ) + ( ase_vertexNormal * _OutlineThickness ) );
			v.vertex.xyz += outlineVar;
		}
		inline half4 LightingOutline( SurfaceOutput s, half3 lightDir, half atten ) { return half4 ( 0,0,0, s.Alpha); }
		void outlineSurf( Input i, inout SurfaceOutput o )
		{
			o.Emission = _EdgeColor.rgb;
		}
		ENDCG
		

		Tags{ "RenderType" = "Opaque"  "Queue" = "Geometry+0" "IsEmissive" = "true"  }
		Cull Back
		CGINCLUDE
		#include "UnityPBSLighting.cginc"
		#include "UnityCG.cginc"
		#include "Lighting.cginc"
		#pragma target 3.0
		struct Input
		{
			float3 worldPos;
			float3 worldNormal;
		};

		struct SurfaceOutputCustomLightingCustom
		{
			half3 Albedo;
			half3 Normal;
			half3 Emission;
			half Metallic;
			half Smoothness;
			half Occlusion;
			half Alpha;
			Input SurfInput;
			UnityGIInput GIData;
		};

		uniform float _Displacement;
		uniform float4 _RockLight;
		uniform float4 _MossLight;
		uniform float3 _MossTranslate2;
		uniform float3 _MossScale2;
		uniform float3 _MossScale;
		uniform float3 _MossTranslate;
		uniform float _MultiplyAmount;
		uniform float4 _RockDark;
		uniform float4 _MossDark;
		uniform float _MossEdgeTHickness;
		uniform float3 _MatchingNoiseScale2;
		uniform float3 _MatchingTranslate;
		uniform float3 _MatchingNoiseScale;
		uniform float4 _EdgeColor;
		uniform float _OutlineThickness;


		float3 mod3D289( float3 x ) { return x - floor( x / 289.0 ) * 289.0; }

		float4 mod3D289( float4 x ) { return x - floor( x / 289.0 ) * 289.0; }

		float4 permute( float4 x ) { return mod3D289( ( x * 34.0 + 1.0 ) * x ); }

		float4 taylorInvSqrt( float4 r ) { return 1.79284291400159 - r * 0.85373472095314; }

		float snoise( float3 v )
		{
			const float2 C = float2( 1.0 / 6.0, 1.0 / 3.0 );
			float3 i = floor( v + dot( v, C.yyy ) );
			float3 x0 = v - i + dot( i, C.xxx );
			float3 g = step( x0.yzx, x0.xyz );
			float3 l = 1.0 - g;
			float3 i1 = min( g.xyz, l.zxy );
			float3 i2 = max( g.xyz, l.zxy );
			float3 x1 = x0 - i1 + C.xxx;
			float3 x2 = x0 - i2 + C.yyy;
			float3 x3 = x0 - 0.5;
			i = mod3D289( i);
			float4 p = permute( permute( permute( i.z + float4( 0.0, i1.z, i2.z, 1.0 ) ) + i.y + float4( 0.0, i1.y, i2.y, 1.0 ) ) + i.x + float4( 0.0, i1.x, i2.x, 1.0 ) );
			float4 j = p - 49.0 * floor( p / 49.0 );  // mod(p,7*7)
			float4 x_ = floor( j / 7.0 );
			float4 y_ = floor( j - 7.0 * x_ );  // mod(j,N)
			float4 x = ( x_ * 2.0 + 0.5 ) / 7.0 - 1.0;
			float4 y = ( y_ * 2.0 + 0.5 ) / 7.0 - 1.0;
			float4 h = 1.0 - abs( x ) - abs( y );
			float4 b0 = float4( x.xy, y.xy );
			float4 b1 = float4( x.zw, y.zw );
			float4 s0 = floor( b0 ) * 2.0 + 1.0;
			float4 s1 = floor( b1 ) * 2.0 + 1.0;
			float4 sh = -step( h, 0.0 );
			float4 a0 = b0.xzyw + s0.xzyw * sh.xxyy;
			float4 a1 = b1.xzyw + s1.xzyw * sh.zzww;
			float3 g0 = float3( a0.xy, h.x );
			float3 g1 = float3( a0.zw, h.y );
			float3 g2 = float3( a1.xy, h.z );
			float3 g3 = float3( a1.zw, h.w );
			float4 norm = taylorInvSqrt( float4( dot( g0, g0 ), dot( g1, g1 ), dot( g2, g2 ), dot( g3, g3 ) ) );
			g0 *= norm.x;
			g1 *= norm.y;
			g2 *= norm.z;
			g3 *= norm.w;
			float4 m = max( 0.6 - float4( dot( x0, x0 ), dot( x1, x1 ), dot( x2, x2 ), dot( x3, x3 ) ), 0.0 );
			m = m* m;
			m = m* m;
			float4 px = float4( dot( x0, g0 ), dot( x1, g1 ), dot( x2, g2 ), dot( x3, g3 ) );
			return 42.0 * dot( m, px);
		}


		float4 CalculateContrast( float contrastValue, float4 colorTarget )
		{
			float t = 0.5 * ( 1.0 - contrastValue );
			return mul( float4x4( contrastValue,0,0,t, 0,contrastValue,0,t, 0,0,contrastValue,t, 0,0,0,1 ), colorTarget );
		}

		void vertexDataFunc( inout appdata_full v, out Input o )
		{
			UNITY_INITIALIZE_OUTPUT( Input, o );
			float3 ase_worldPos = mul( unity_ObjectToWorld, v.vertex );
			float simplePerlin3D184 = snoise( ase_worldPos );
			simplePerlin3D184 = simplePerlin3D184*0.5 + 0.5;
			float simplePerlin3D185 = snoise( ase_worldPos*1.2 );
			simplePerlin3D185 = simplePerlin3D185*0.5 + 0.5;
			float simplePerlin3D186 = snoise( ase_worldPos*1.4 );
			simplePerlin3D186 = simplePerlin3D186*0.5 + 0.5;
			float3 appendResult187 = (float3(simplePerlin3D184 , simplePerlin3D185 , simplePerlin3D186));
			v.vertex.xyz += ( ( appendResult187 * _Displacement ) + 0 );
			v.vertex.w = 1;
		}

		inline half4 LightingStandardCustomLighting( inout SurfaceOutputCustomLightingCustom s, half3 viewDir, UnityGI gi )
		{
			UnityGIInput data = s.GIData;
			Input i = s.SurfInput;
			half4 c = 0;
			c.rgb = 0;
			c.a = 1;
			return c;
		}

		inline void LightingStandardCustomLighting_GI( inout SurfaceOutputCustomLightingCustom s, UnityGIInput data, inout UnityGI gi )
		{
			s.GIData = data;
		}

		void surf( Input i , inout SurfaceOutputCustomLightingCustom o )
		{
			o.SurfInput = i;
			float3 ase_worldPos = i.worldPos;
			float simplePerlin3D54 = snoise( ( _MossTranslate2 + ( _MossScale2 * ase_worldPos ) ) );
			float simplePerlin3D55 = snoise( ( ( ase_worldPos * _MossScale ) + _MossTranslate ) );
			float3 objToWorld49 = mul( unity_ObjectToWorld, float4( float3( 0,0,0 ), 1 ) ).xyz;
			float clampResult43 = clamp( ( ase_worldPos.y + -objToWorld49.y + 0.3 ) , 0.0 , 1.0 );
			float temp_output_39_0 = ( ( ( simplePerlin3D54 * 0.59 ) + simplePerlin3D55 ) * _MultiplyAmount * clampResult43 );
			float temp_output_28_0 = saturate( round( temp_output_39_0 ) );
			float4 lerpResult25 = lerp( _RockLight , _MossLight , temp_output_28_0);
			float4 lerpResult29 = lerp( _RockDark , _MossDark , temp_output_28_0);
			float3 ase_worldNormal = i.worldNormal;
			float3 ase_normWorldNormal = normalize( ase_worldNormal );
			#if defined(LIGHTMAP_ON) && UNITY_VERSION < 560 //aseld
			float3 ase_worldlightDir = 0;
			#else //aseld
			float3 ase_worldlightDir = Unity_SafeNormalize( UnityWorldSpaceLightDir( ase_worldPos ) );
			#endif //aseld
			float dotResult2 = dot( ase_normWorldNormal , ase_worldlightDir );
			float temp_output_7_0 = ( 1 * dotResult2 );
			float lerpResult8 = lerp( 1.0 , 0.0 , ( temp_output_7_0 + ( 1.0 - dotResult2 ) ));
			float4 lerpResult24 = lerp( lerpResult25 , lerpResult29 , saturate( ( lerpResult8 / 0.0 ) ));
			float3 temp_output_86_0 = ( ase_worldPos + _MatchingTranslate );
			float simplePerlin3D83 = snoise( ( _MatchingNoiseScale2 * temp_output_86_0 ) );
			float4 temp_cast_0 = (( simplePerlin3D83 + -0.26 )).xxxx;
			float simplePerlin3D91 = snoise( ( temp_output_86_0 * _MatchingNoiseScale ) );
			float4 temp_cast_1 = (( simplePerlin3D91 + -0.37 )).xxxx;
			float4 temp_output_79_0 = ( CalculateContrast(1.89,temp_cast_0) + CalculateContrast(1.71,temp_cast_1) );
			float4 blendOpSrc74 = saturate( ( 1.0 - CalculateContrast(0.32,temp_output_79_0) ) );
			float4 blendOpDest74 = CalculateContrast(0.1,(float4( 0,0,0,0 ) + (float4( 0,0,0,0 ) - float4( 0,0,0,1 )) * (float4( 1,1,1,1 ) - float4( 0,0,0,0 )) / (float4( 1,1,1,1 ) - float4( 0,0,0,1 ))));
			float4 lerpBlendMode74 = lerp(blendOpDest74,( 1.0 - ( 1.0 - blendOpSrc74 ) * ( 1.0 - blendOpDest74 ) ),0.0);
			float4 temp_output_95_0 = ( lerpResult24 * ( 1.0 - ( -temp_output_28_0 + saturate( round( ( temp_output_39_0 + ( _MossEdgeTHickness * clampResult43 ) ) ) ) ) ) * saturate( ( ( saturate( lerpBlendMode74 )).r + 0.0 ) ) * ( saturate( ( temp_output_7_0 / 0.0 ) ) + 0.0 ) );
			o.Emission = ( temp_output_95_0 + ( saturate( ( 1.0 - ( temp_output_95_0 * 20.0 ) ) ) * _EdgeColor ) ).rgb;
		}

		ENDCG
		CGPROGRAM
		#pragma surface surf StandardCustomLighting keepalpha fullforwardshadows vertex:vertexDataFunc 

		ENDCG
		Pass
		{
			Name "ShadowCaster"
			Tags{ "LightMode" = "ShadowCaster" }
			ZWrite On
			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			#pragma target 3.0
			#pragma multi_compile_shadowcaster
			#pragma multi_compile UNITY_PASS_SHADOWCASTER
			#pragma skip_variants FOG_LINEAR FOG_EXP FOG_EXP2
			#include "HLSLSupport.cginc"
			#if ( SHADER_API_D3D11 || SHADER_API_GLCORE || SHADER_API_GLES || SHADER_API_GLES3 || SHADER_API_METAL || SHADER_API_VULKAN )
				#define CAN_SKIP_VPOS
			#endif
			#include "UnityCG.cginc"
			#include "Lighting.cginc"
			#include "UnityPBSLighting.cginc"
			struct v2f
			{
				V2F_SHADOW_CASTER;
				float3 worldPos : TEXCOORD1;
				float3 worldNormal : TEXCOORD2;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};
			v2f vert( appdata_full v )
			{
				v2f o;
				UNITY_SETUP_INSTANCE_ID( v );
				UNITY_INITIALIZE_OUTPUT( v2f, o );
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );
				UNITY_TRANSFER_INSTANCE_ID( v, o );
				Input customInputData;
				vertexDataFunc( v, customInputData );
				float3 worldPos = mul( unity_ObjectToWorld, v.vertex ).xyz;
				half3 worldNormal = UnityObjectToWorldNormal( v.normal );
				o.worldNormal = worldNormal;
				o.worldPos = worldPos;
				TRANSFER_SHADOW_CASTER_NORMALOFFSET( o )
				return o;
			}
			half4 frag( v2f IN
			#if !defined( CAN_SKIP_VPOS )
			, UNITY_VPOS_TYPE vpos : VPOS
			#endif
			) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				Input surfIN;
				UNITY_INITIALIZE_OUTPUT( Input, surfIN );
				float3 worldPos = IN.worldPos;
				half3 worldViewDir = normalize( UnityWorldSpaceViewDir( worldPos ) );
				surfIN.worldPos = worldPos;
				surfIN.worldNormal = IN.worldNormal;
				SurfaceOutputCustomLightingCustom o;
				UNITY_INITIALIZE_OUTPUT( SurfaceOutputCustomLightingCustom, o )
				surf( surfIN, o );
				#if defined( CAN_SKIP_VPOS )
				float2 vpos = IN.pos;
				#endif
				SHADOW_CASTER_FRAGMENT( IN )
			}
			ENDCG
		}
	}
	Fallback "Diffuse"
	CustomEditor "ASEMaterialInspector"
}
/*ASEBEGIN
Version=18900
1920;0;1920;1059;3266.708;1256.037;1;True;False
Node;AmplifyShaderEditor.Vector3Node;88;-3127.861,-1110.388;Inherit;False;Property;_MatchingTranslate;Matching Translate;13;0;Create;True;0;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.WorldPosInputsNode;87;-3127.861,-1303.388;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleAddOpNode;86;-2764.861,-1201.388;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.Vector3Node;90;-2812.861,-1044.388;Inherit;False;Property;_MatchingNoiseScale;Matching Noise Scale;15;0;Create;True;0;0;0;False;0;False;1,1,1;1,1,1;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.Vector3Node;85;-2941.861,-1462.388;Inherit;False;Property;_MatchingNoiseScale2;Matching Noise Scale 2;11;0;Create;True;0;0;0;False;0;False;7,7,7;7,7,7;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.Vector3Node;61;-4082.624,-294.9728;Inherit;False;Property;_MossScale2;Moss Scale 2;7;0;Create;True;0;0;0;False;0;False;6,6,6;6,6,6;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.WorldPosInputsNode;63;-4134.833,4.558289;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;89;-2589.861,-1080.388;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;84;-2679.852,-1425.364;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;83;-2512.185,-1441.128;Inherit;True;Simplex3D;False;False;2;0;FLOAT3;0,0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;91;-2386.861,-1134.388;Inherit;True;Simplex3D;False;False;2;0;FLOAT3;0,0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;62;-3856.507,-179.6328;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.Vector3Node;59;-3969.679,-513.4788;Inherit;False;Property;_MossTranslate2;Moss Translate 2;6;0;Create;True;0;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.Vector3Node;65;-4124.989,231.6962;Inherit;False;Property;_MossScale;Moss Scale;8;0;Create;True;0;0;0;False;0;False;1.19,1,1;1.19,1,1;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;64;-3857.989,82.69623;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.TransformPositionNode;49;-3614.882,674.653;Inherit;False;Object;World;False;Fast;True;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.Vector3Node;67;-3910.989,388.6962;Inherit;False;Property;_MossTranslate;Moss Translate;9;0;Create;True;0;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleAddOpNode;92;-2138.861,-1142.388;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;-0.37;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;82;-2230.635,-1405.276;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;-0.26;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;58;-3752.875,-365.7086;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleContrastOpNode;93;-2015.861,-1082.388;Inherit;True;2;1;COLOR;0,0,0,0;False;0;FLOAT;1.71;False;1;COLOR;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;54;-3571.005,-376.6132;Inherit;True;Simplex3D;False;False;2;0;FLOAT3;0,0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleContrastOpNode;81;-2068.491,-1376.329;Inherit;True;2;1;COLOR;0,0,0,0;False;0;FLOAT;1.89;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;47;-3350.882,773.653;Inherit;False;Constant;_Float1;Float 1;5;0;Create;True;0;0;0;False;0;False;0.3;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.NegateNode;48;-3356.882,663.653;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;56;-3678.23,144.3995;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.WorldPosInputsNode;50;-3586.123,496.7531;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.WorldSpaceLightDirHlpNode;1;-3381.508,1657.674;Inherit;False;True;1;0;FLOAT;0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.WorldNormalVector;3;-3354.286,1507.243;Inherit;False;True;1;0;FLOAT3;0,0,1;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleAddOpNode;44;-3152,640;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;53;-3218.236,-301.7303;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.59;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;79;-1761.813,-1303.62;Inherit;True;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;55;-3549.234,97.24649;Inherit;True;Simplex3D;False;False;2;0;FLOAT3;0,0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleContrastOpNode;179;-1545.354,-1290.403;Inherit;False;2;1;COLOR;0,0,0,0;False;0;FLOAT;0.32;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleAddOpNode;52;-3060.016,14.40552;Inherit;True;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DotProductOpNode;2;-3082.286,1507.243;Inherit;True;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;42;-3025.882,476.653;Inherit;False;Property;_MossEdgeTHickness;Moss Edge THickness;4;0;Create;True;0;0;0;False;0;False;0.12;0.23;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;43;-3003.882,595.653;Inherit;True;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.LightAttenuation;6;-3107.07,861.4109;Inherit;False;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;51;-3055.981,258.5985;Inherit;False;Property;_MultiplyAmount;Multiply Amount;5;0;Create;True;0;0;0;False;0;False;2.24;2.24;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;41;-2765.882,515.653;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;78;-1200.813,-1434.62;Inherit;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;7;-2829.286,851.2426;Inherit;True;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;39;-2835.882,140.653;Inherit;True;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;72;-2314.383,-804.6397;Inherit;True;5;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,1;False;2;FLOAT4;1,1,1,1;False;3;FLOAT4;0,0,0,0;False;4;FLOAT4;1,1,1,1;False;1;FLOAT4;0
Node;AmplifyShaderEditor.OneMinusNode;4;-2797.286,1075.243;Inherit;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;38;-2573.882,388.653;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;77;-1118.813,-1084.62;Inherit;False;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleContrastOpNode;73;-2007.383,-809.6397;Inherit;True;2;1;COLOR;0,0,0,0;False;0;FLOAT;0.1;False;1;COLOR;0
Node;AmplifyShaderEditor.RoundOpNode;40;-2585.882,162.653;Inherit;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;5;-2336.299,1001.571;Inherit;True;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;8;-1999.299,990.5706;Inherit;False;3;0;FLOAT;1;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.BlendOpsNode;74;-1705.116,-819.93;Inherit;True;Screen;True;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;10;-1971.991,1152.965;Inherit;False;Constant;_Float0;Float 0;0;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;28;-2222.469,64.76097;Inherit;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;176;-2352.372,1338.832;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RoundOpNode;36;-2455.907,395.3445;Inherit;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;30;-1785.694,76.14667;Inherit;False;Property;_RockDark;Rock Dark;2;0;Create;True;0;0;0;False;0;False;0.4862745,0.6039216,0.6156863,1;0.8389958,0.8962264,0.1986917,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleDivideOpNode;20;-1658.329,1266.654;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.NegateNode;32;-2094.492,275.3001;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;31;-1786.173,262.5367;Inherit;False;Property;_MossDark;Moss Dark;3;0;Create;True;0;0;0;False;0;False;0.372549,0.5019608,0.4980392,1;0.372549,0.5019608,0.4980392,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleDivideOpNode;9;-1693.299,1031.571;Inherit;True;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;35;-2285.797,402.8367;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;26;-1807.521,-346.39;Inherit;False;Property;_RockLight;Rock Light;0;0;Create;True;0;0;0;False;0;False;0.6039216,0.682353,0.5803922,1;0.6039216,0.682353,0.5803922,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ColorNode;27;-1808,-160;Inherit;False;Property;_MossLight;Moss Light;1;0;Create;True;0;0;0;False;0;False;0.6039216,0.682353,0.5803922,1;0.683245,0.7264151,0.5516643,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.BreakToComponentsNode;75;-1441.116,-735.93;Inherit;False;COLOR;1;0;COLOR;0,0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.SaturateNode;11;-1454.299,935.5706;Inherit;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;76;-1322.382,-740.6397;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;29;-1505.694,230.1467;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SaturateNode;22;-1454.329,1312.654;Inherit;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;33;-2103.492,407.9122;Inherit;True;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;25;-1527.521,-192.39;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.OneMinusNode;34;-1897.76,432.6857;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;24;-1234.521,-54.39003;Inherit;True;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.WorldPosInputsNode;191;304,2112;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleAddOpNode;23;-1254.329,1311.654;Inherit;True;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;94;-1210.382,-740.6397;Inherit;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;192;608,2256;Inherit;True;Simplex3D;True;False;2;0;FLOAT3;0,0,0;False;1;FLOAT;1.4;False;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;193;608,2032;Inherit;True;Simplex3D;True;False;2;0;FLOAT3;0,0,0;False;1;FLOAT;1.2;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;95;-566.2029,151.5952;Inherit;False;4;4;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;115;-368,848;Inherit;False;Constant;_Float2;Float 2;15;0;Create;True;0;0;0;False;0;False;20;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;194;608,1808;Inherit;True;Simplex3D;True;False;2;0;FLOAT3;0,0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.NormalVertexDataNode;201;1109.002,2405.256;Inherit;False;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;114;-196,734;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.DynamicAppendNode;195;896,2032;Inherit;True;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;189;928,1568;Inherit;False;Property;_Displacement;Displacement;18;0;Create;True;0;0;0;False;0;False;0.2;0.2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.WorldPosInputsNode;183;304.5055,1403.47;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RangedFloatNode;202;1211.715,2665.583;Inherit;False;Property;_OutlineThickness;Outline Thickness;19;0;Create;True;0;0;0;False;0;False;0.01;0.01;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;185;608,1328;Inherit;True;Simplex3D;True;False;2;0;FLOAT3;0,0,0;False;1;FLOAT;1.2;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;119;-52.78558,1123.642;Inherit;False;Property;_EdgeColor;Edge Color;14;0;Create;True;0;0;0;False;0;False;0.1490196,0.2509804,0.2627451,1;0.1941703,0.2169811,0.1893467,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.OneMinusNode;116;-48,736;Inherit;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;186;608,1552;Inherit;True;Simplex3D;True;False;2;0;FLOAT3;0,0,0;False;1;FLOAT;1.4;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;200;1355.16,2483.177;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;196;1152,2048;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;184;608,1104;Inherit;True;Simplex3D;True;False;2;0;FLOAT3;0,0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;117;156.3809,705.3085;Inherit;False;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.DynamicAppendNode;187;896,1328;Inherit;True;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.WireNode;199;186.2819,1734.013;Inherit;False;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleAddOpNode;197;1399.432,1980.233;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.OutlineNode;198;1434.85,1813.766;Inherit;False;2;True;None;0;0;Front;3;0;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;188;1154.217,1344.354;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;118;284.8538,903.8575;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RoundOpNode;80;-1516.814,-1521.084;Inherit;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;168;-3602.009,1209.756;Inherit;False;Property;_RampSmoothness;Ramp Smoothness;16;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;190;1439.833,1432.965;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleAddOpNode;113;307.5095,293.6365;Inherit;True;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SmoothstepOpNode;164;-3177.177,1027.925;Inherit;True;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;167;-3292.009,1112.756;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;69;-2984.325,-648.7936;Inherit;False;Property;_TriplanarSampler;Triplanar Sampler;10;0;Create;True;0;0;0;False;0;False;0.7;0.7;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;71;-3023.117,-875.93;Inherit;True;Property;_Texture0;Texture 0;12;0;Create;True;0;0;0;False;0;False;099ca5f9aeec7924d89ef3be794f9540;69436384d471c614dabc12398a763263;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.RangedFloatNode;182;-2781.329,-547.1101;Inherit;False;Constant;_Float5;Float 5;19;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TriplanarNode;68;-2746.04,-807.9922;Inherit;True;Spherical;Object;False;Top Texture 0;_TopTexture0;white;0;None;Mid Texture 0;_MidTexture0;white;-1;None;Bot Texture 0;_BotTexture0;white;3;None;Triplanar Sampler;Tangent;10;0;SAMPLER2D;;False;5;FLOAT;1;False;1;SAMPLER2D;;False;6;FLOAT;0;False;2;SAMPLER2D;;False;7;FLOAT;0;False;9;FLOAT3;0,0,0;False;8;FLOAT;1;False;3;FLOAT2;1,1;False;4;FLOAT;1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;165;-3565.009,982.7557;Inherit;False;Property;_RampOffset;Ramp Offset;17;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.StandardSurfaceOutputNode;178;1733.866,872.1019;Float;False;True;-1;2;ASEMaterialInspector;0;0;CustomLighting;Handdrawn;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;Back;0;False;-1;0;False;-1;False;0;False;-1;0;False;-1;False;0;Opaque;0.5;True;True;0;False;Opaque;;Geometry;All;14;all;True;True;True;True;0;False;-1;False;0;False;-1;255;False;-1;255;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;False;2;15;10;25;False;0.5;True;0;0;False;-1;0;False;-1;0;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;0;0,0,0,0;VertexOffset;True;False;Cylindrical;False;Relative;0;;-1;-1;-1;-1;0;False;0;0;False;-1;-1;0;False;-1;0;0;0;False;0.1;False;-1;0;False;-1;False;15;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT;0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT;0;False;9;FLOAT;0;False;10;FLOAT;0;False;13;FLOAT3;0,0,0;False;11;FLOAT3;0,0,0;False;12;FLOAT3;0,0,0;False;14;FLOAT4;0,0,0,0;False;15;FLOAT3;0,0,0;False;0
WireConnection;86;0;87;0
WireConnection;86;1;88;0
WireConnection;89;0;86;0
WireConnection;89;1;90;0
WireConnection;84;0;85;0
WireConnection;84;1;86;0
WireConnection;83;0;84;0
WireConnection;91;0;89;0
WireConnection;62;0;61;0
WireConnection;62;1;63;0
WireConnection;64;0;63;0
WireConnection;64;1;65;0
WireConnection;92;0;91;0
WireConnection;82;0;83;0
WireConnection;58;0;59;0
WireConnection;58;1;62;0
WireConnection;93;1;92;0
WireConnection;54;0;58;0
WireConnection;81;1;82;0
WireConnection;48;0;49;2
WireConnection;56;0;64;0
WireConnection;56;1;67;0
WireConnection;44;0;50;2
WireConnection;44;1;48;0
WireConnection;44;2;47;0
WireConnection;53;0;54;0
WireConnection;79;0;81;0
WireConnection;79;1;93;0
WireConnection;55;0;56;0
WireConnection;179;1;79;0
WireConnection;52;0;53;0
WireConnection;52;1;55;0
WireConnection;2;0;3;0
WireConnection;2;1;1;0
WireConnection;43;0;44;0
WireConnection;41;0;42;0
WireConnection;41;1;43;0
WireConnection;78;0;179;0
WireConnection;7;0;6;0
WireConnection;7;1;2;0
WireConnection;39;0;52;0
WireConnection;39;1;51;0
WireConnection;39;2;43;0
WireConnection;4;0;2;0
WireConnection;38;0;39;0
WireConnection;38;1;41;0
WireConnection;77;0;78;0
WireConnection;73;1;72;0
WireConnection;40;0;39;0
WireConnection;5;0;7;0
WireConnection;5;1;4;0
WireConnection;8;2;5;0
WireConnection;74;0;77;0
WireConnection;74;1;73;0
WireConnection;28;0;40;0
WireConnection;176;0;7;0
WireConnection;36;0;38;0
WireConnection;20;0;176;0
WireConnection;20;1;10;0
WireConnection;32;0;28;0
WireConnection;9;0;8;0
WireConnection;9;1;10;0
WireConnection;35;0;36;0
WireConnection;75;0;74;0
WireConnection;11;0;9;0
WireConnection;76;0;75;0
WireConnection;29;0;30;0
WireConnection;29;1;31;0
WireConnection;29;2;28;0
WireConnection;22;0;20;0
WireConnection;33;0;32;0
WireConnection;33;1;35;0
WireConnection;25;0;26;0
WireConnection;25;1;27;0
WireConnection;25;2;28;0
WireConnection;34;0;33;0
WireConnection;24;0;25;0
WireConnection;24;1;29;0
WireConnection;24;2;11;0
WireConnection;23;0;22;0
WireConnection;94;0;76;0
WireConnection;192;0;191;0
WireConnection;193;0;191;0
WireConnection;95;0;24;0
WireConnection;95;1;34;0
WireConnection;95;2;94;0
WireConnection;95;3;23;0
WireConnection;194;0;191;0
WireConnection;114;0;95;0
WireConnection;114;1;115;0
WireConnection;195;0;194;0
WireConnection;195;1;193;0
WireConnection;195;2;192;0
WireConnection;185;0;183;0
WireConnection;116;0;114;0
WireConnection;186;0;183;0
WireConnection;200;0;201;0
WireConnection;200;1;202;0
WireConnection;196;0;189;0
WireConnection;196;1;195;0
WireConnection;184;0;183;0
WireConnection;117;0;116;0
WireConnection;187;0;184;0
WireConnection;187;1;185;0
WireConnection;187;2;186;0
WireConnection;199;0;119;0
WireConnection;197;0;196;0
WireConnection;197;1;200;0
WireConnection;198;0;199;0
WireConnection;198;1;197;0
WireConnection;188;0;187;0
WireConnection;188;1;189;0
WireConnection;118;0;117;0
WireConnection;118;1;119;0
WireConnection;80;0;79;0
WireConnection;190;0;188;0
WireConnection;190;1;198;0
WireConnection;113;0;95;0
WireConnection;113;1;118;0
WireConnection;164;0;2;0
WireConnection;164;1;165;0
WireConnection;164;2;167;0
WireConnection;167;0;165;0
WireConnection;167;1;168;0
WireConnection;68;0;71;0
WireConnection;68;3;69;0
WireConnection;68;4;182;0
WireConnection;178;2;113;0
WireConnection;178;11;190;0
ASEEND*/
//CHKSM=F834CD12BFED401D21C05BE2131686AB9B85D04A