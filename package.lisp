#|
 This file is a part of cl-wavefront
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.wavefront
  (:use #:cl)
  (:export
   #:named-object
   #:name
   #:object
   #:groups
   #:texture-map
   #:file
   #:blend-u
   #:blend-v
   #:boost
   #:origin
   #:scale
   #:turbulence
   #:resolution
   #:clamp
   #:multiplier
   #:bump-channel
   #:texture-type
   #:material
   #:illumination-model
   #:ambient-factor
   #:diffuse-factor
   #:specular-factor
   #:specular-exponent
   #:transmission-factor
   #:transmission-filter
   #:refractive-index
   #:roughness-factor
   #:metallic-factor
   #:sheen-factor
   #:emissive-factor
   #:ambient-map
   #:diffuse-map
   #:specular-map
   #:transmission-map
   #:roughness-map
   #:metallic-map
   #:sheen-map
   #:emissive-map
   #:bump-map
   #:displacement-map
   #:stencil-map
   #:normal-map
   #:rough-metal-occlusion-map
   #:face
   #:vertices
   #:uvs
   #:normals
   #:material
   #:group
   #:faces
   #:lod
   #:context
   #:vertices
   #:uvs
   #:normals
   #:groups
   #:materials
   #:objects
   #:mesh
   #:vertex-data
   #:index-data
   #:material
   #:attributes
   #:face-length
   #:shared-faces
   #:faces-to-mesh
   #:extract-meshes)
  (:export
   #:parse))
