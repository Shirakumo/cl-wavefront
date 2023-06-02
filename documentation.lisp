#|
 This file is a part of cl-wavefront
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.wavefront)

;; objects
(docs:define-docs
  (type named-object
    "Superclass for all objects which may carry a name.

See NAME")
  
  (function name
    "Accesses the string name of the object

See NAMED-OBJECT")
  
  (type object
    "Representation of an \"object\".

An object encompasses one or more groups.

See EXTRACT-MESHES
See NAMED-OBJECT
See GROUPS")
  
  (function groups
    "Accesses the hash table of groups.

Each group can be accessed via its name.

See OBJECT
See CONTEXT
See GROUP")
  
  (type texture-map
    "Representation of a texture map.

See FILE
See BLEND-U
See BLEND-V
See BOOST
See ORIGIN
See SCALE
See TURBULENCE
See RESOLUTION
See CLAMP
See MULTIPLIER
See BUMP-CHANNEL
See SPECULAR-TYPE")
  
  (function file
    "Accesses the relative pathname of the texture map.

See TEXTURE-MAP")
  
  (function blend-u
    "Accesses whether the texture should blend U coordinates.

See TEXTURE-MAP")
  
  (function blend-v
    "Accesses whether the texture should blend V coordinates.

See TEXTURE-MAP")
  
  (function boost
    "Accesses the boosting factor for mipmap sharpness.

See TEXTURE-MAP")
  
  (function origin
    "Accesses the texture origin offset.

This is a vector of three elements [U V W]

See TEXTURE-MAP")
  
  (function scale
    "Accesses the texture scale factor.

This is a vector of three elements [U V W]

See TEXTURE-MAP")
  
  (function turbulence
    "Accesses the texture turbulence factor.

This is a vector of three elements [U V W]

See TEXTURE-MAP")
  
  (function resolution
    "Accesses the texture's resolution.

This is a single value for square textures.

See TEXTURE-MAP")
  
  (function clamp
    "Accesses whether the texture should repeat or be clamped to its edges.

See TEXTURE-MAP")
  
  (function multiplier
    "Accesses the multiplier factor for the bump map.

See TEXTURE-MAP")
  
  (function bump-channel
    "Accesses the channel to use in the bump map texture.

See TEXTURE-MAP")
  
  (function texture-type
    "Accesses the type of the texture.

This can be sphere, cube_top, cube_bottom...

See TEXTURE-MAP")
  
  (type material
    "Representation of a material defined in a Wavefront MTL file.

See NAMED-OBJECT
See ILLUMINATION-MODEL
See AMBIENT-FACTOR
See DIFFUSE-FACTOR
See SPECULAR-FACTOR
See SPECULAR-EXPONENT
See TRANSMISSION-FACTOR
See TRANSMISSION-FILTER
See REFRACTIVE-INDEX
See ROUGHNESS-FACTOR
See METALLIC-FACTOR
See SHEEN-FACTOR
See EMISSIVE-FACTOR
See AMBIENT-MAP
See DIFFUSE-MAP
See SPECULAR-MAP
See TRANSMISSION-MAP
See ROUGHNESS-MAP
See METALLIC-MAP
See SHEEN-MAP
See EMISSIVE-MAP
See BUMP-MAP
See DISPLACEMENT-MAP
See STENCIL-MAP
See NORMAL-MAP
See ROUGH-METAL-OCCLUSION-MAP")
  
  (function illumination-model
    "Accesses the intended illumination model of the material.

0. Color on and Ambient off
1. Color on and Ambient on
2. Highlight on
3. Reflection on and Ray trace on
4. Transparency: Glass on, Reflection: Ray trace on
5. Reflection: Fresnel on and Ray trace on
6. Transparency: Refraction on, Reflection: Fresnel off and Ray trace on
7. Transparency: Refraction on, Reflection: Fresnel on and Ray trace on
8. Reflection on and Ray trace off
9. Transparency: Glass on, Reflection: Ray trace off
10. Casts shadows onto invisible surfaces

See METAL")
  
  (function ambient-factor
    "Accesses the ambient color factor.

This is a vector of three elements [ R G B ]

See METAL")
  
  (function diffuse-factor
    "Accesses the diffuse color factor.

This is a vector of three elements [ R G B ]

See METAL")
  
  (function specular-factor
    "Accesses the specular color factor.

This is a vector of three elements [ R G B ]

See METAL")
  
  (function specular-exponent
    "Accesses the specular exponent.

See METAL")
  
  (function transmission-factor
    "Accesses the transmission factor.

0.0 means opaque
1.0 means transparent

See METAL")
  
  (function transmission-filter
    "Accesses the transmission color filter.

This is a vector of three elements [ R G B ]

See METAL")
  
  (function refractive-index
    "Accesses the refractive index if transmissive.

See METAL")
  
  (function roughness-factor
    "Accesses the roughness factor.

See METAL")
  
  (function metallic-factor
    "Accesses the metallic/metalness factor.

See METAL")
  
  (function sheen-factor
    "Accesses the sheen factor.

See METAL")
  
  (function emissive-factor
    "Accesses the emissive factor.

This is a vector of three elements [ R G B ]

See METAL")
  
  (function ambient-map
    "Accesses the ambient texture map.

See MATERIAL
See TEXTURE-MAP")
  
  (function diffuse-map
    "Accesses the diffuse texture map.

See MATERIAL
See TEXTURE-MAP")
  
  (function specular-map
    "Accesses the specular texture map.

See MATERIAL
See TEXTURE-MAP")
  
  (function transmission-map
    "Accesses the transmission texture map.

See MATERIAL
See TEXTURE-MAP")
  
  (function roughness-map
    "Accesses the roughness texture map.

See MATERIAL
See TEXTURE-MAP")
  
  (function metallic-map
    "Accesses the metallic texture map.

See MATERIAL
See TEXTURE-MAP")
  
  (function sheen-map
    "Accesses the sheen texture map.

See MATERIAL
See TEXTURE-MAP")
  
  (function emissive-map
    "Accesses the emissive texture map.

See MATERIAL
See TEXTURE-MAP")
  
  (function bump-map
    "Accesses the bump texture map.

See MATERIAL
See TEXTURE-MAP")
  
  (function displacement-map
    "Accesses the displacement texture map.

See MATERIAL
See TEXTURE-MAP")
  
  (function stencil-map
    "Accesses the stencil decal texture map.

See MATERIAL
See TEXTURE-MAP")
  
  (function normal-map
    "Accesses the normal texture map.

See MATERIAL
See TEXTURE-MAP")
  
  (function rough-metal-occlusion-map
    "Accesses the combined roughness/metal/occlusion texture map.

See MATERIAL
See TEXTURE-MAP")
  
  (type face
    "Representation of a face on a polygonal mesh.

All three arrays of VERTICES, UVS, NORMALS must be of the same length
and each contain indices into their respective arrays in the CONTEXT
of the FACE, modulo the size of each entry in the respective array.

Meaning for vertices, you should multiply the index by 4 to get the
index of the first coordinate of the vertex in the context's vertices
array, and for UVs and normals you should multiply the index by 3.

See VERTICES
See UVS
See NORMALS
See MATERIAL")
  
  (function vertices
    "Accesses the vertices of the object

For a FACE this is a vector of indices.
Fro a CONTEXT this is a vector of single floats.

See FACE
See CONTEXT")
  
  (function uvs
    "Accesses the UV coordinates of the object

For a FACE this is a vector of indices.
Fro a CONTEXT this is a vector of single floats.

See FACE
See CONTEXT")
  
  (function normals
    "Accesses the normals of the object

For a FACE this is a vector of indices.
Fro a CONTEXT this is a vector of single floats.

See FACE
See CONTEXT")
  
  (type group
    "Representation of a group of faces.

See EXTRACT-MESHES
See NAMED-OBJECT
See FACES
See LOD")
  
  (function faces
    "Accesses the vector of faces belonging to the group.

See GROUP
See FACE")
  
  (function lod
    "Accesses the level of detail the group should be visible at.

See GROUP")
  
  (type context
    "Representation of all the data in a collection of OBJ and MTL files.

See EXTRACT-MESHES
See VERTICES
See UVS
See NORMALS
See GROUPS
See MATERIALS
See OBJECTS
See PARSE")
  
  (function materials
    "Accesses the materials table in the context.

Each material is indexed by its name.

See CONTEXT
See MATERIAL")
  
  (function objects
    "Accesses the objects table in the context.

Each object is indexed by its name.
There is always at least one object.

See CONTEXT
See OBJECT")
  
  (type mesh
    "Representation of packed vertex data.

See VERTEX-DATA
See FACE-DATA
See ATTRIBUTES
See FACES-TO-MESH
See EXTRACT-MESHES")
  
  (function vertex-data
    "Accesses the vector of packed vertices.

This is a vector of SINGLE-FLOATs, with the per-vertex data tightly
packed for easy upload to the GPU. The attributes stored per vertex is
described in the ATTRIBUTES list. The number of vertices per face is
stored in the FACE-LENGTH of the mesh. The material to use for every
face in the mesh is in MATERIAL.

See MESH
See ATTRIBUTES
See FACE-LENGTH")
  
  (function face-data
    "Accesses the vector of packed face indices.

This is a vector of (UNSIGNED-BYTE 32)s describing the vertex indices
that form the faces.

See MESH")
  
  (function attributes
    "Accesses the list of attributes stored in the mesh's vertex data.

The order is significant. May contain the following:

 :POSITION  --- Three single-floats for the world-space position of
                the vertex.
 :NORMAL    --- Three single-floats for the normal vector of the
                vertex.
 :UV        --- Two single-floats for the U and V coordinates in the
                textures.

See MESH")

  (function face-length
    "Accesses the number of vertices per face in the mesh.

See MESH")
  
  (function shared-faces
    "Splits the given faces array into sets of face arrays that share the same properties.

Specifically they all must share the same material, face length, and
attributes.")
  
  (function faces-to-mesh
    "Constructs a MESH instance from the given FACES array.

The consequences are undefined if the faces in the mesh vary in the
number of faces they address, the number of properties they store, or
the material they reference.

See SHARED-FACES
See EXTRACT-MESHES")
  
  (function extract-meshes
    "Extract a set of distinct meshes.

If no THING is given, all meshes present in the context are
extracted. Otherwise, you may pass a vector of FACEs, a GROUP, or an
OBJECT to extract meshes from.

As much as possible, names will be assigned to the resulting meshes.

See CONTEXT
See FACE
See GROUP
See OBJECT
See SHARED-FACES
See FACES-TO-MESH"))

;; parser
(docs:define-docs
  (function parse
    "Parse an OBJ or MTL file from a file, stream, or string representation.

Returns a CONTEXT that contains the parsed data.
May parse recursively if other files are referenced by the file
contents.

If you pass in a CONTEXT, the contents will be added to the passed
context instance.

A warning is signalled for any unrecognised directives.
An error is signalled for any malformed directives.

See CONTEXT"))
