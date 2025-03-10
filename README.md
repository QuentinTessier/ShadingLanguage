This project is a dive into compilation in a C like language and not Haskell.
I intend to make a simple shading language compile to SPIRV, the goal would be to support vertex and fragment shaders.

This is what I intend the language to look like:
```

pub const MyGlobalVariable : i32 = 1;

pub fn add : (a: i32, b; i32) -> i32 {
    return a + b;
}

// currently missing feature in parser
// type names are not defined yet either using "vectorX" for now

attributes CustomVertexInput {
    position: @<location(0), binding(0)> vector3,
    normal: @<location(1), binding(0)> vector3,
    texCoords: @<location(2), binding(0)> vector2,
};

attributes CustomVertexOutput {
    position: vector3, // infer location and binding
    normal: vector3,
    texCoords: vector2,
};

// ------------------------------------------------------------ //

pub entry @vertex main : (input: CustomVertexInput, sampler: @binding(1) Sampler2D) -> CustomVertexOutput {
    var out: CustomVertexOutput = {};

    return out;
}

```