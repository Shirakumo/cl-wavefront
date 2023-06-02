#|
 This file is a part of cl-wavefront
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.wavefront)

(defclass named-object ()
  ((name :initarg :name :initform NIL :accessor name)))

(defmethod print-object ((object named-object) stream)
  (print-unreadable-object (object stream :type T :identity (null (name object)))
    (when (name object) (princ (name object) stream))))

(defclass object (named-object)
  ((groups :initarg :group :initform (make-array 0 :element-type T :adjustable T :fill-pointer T) :accessor groups)))

(defclass texture-map ()
  ((file :initarg :file :initform NIL :accessor file)
   (blend-u :initarg :blend-u :initform T :accessor blend-u)
   (blend-v :initarg :blend-v :initform T :accessor blend-v)
   (boost :initarg :boost :initform NIL :accessor boost)
   (origin :initarg :origin :initform #(0.0 0.0 0.0) :accessor origin)
   (scale :initarg :scale :initform #(1.0 1.0 1.0) :accessor scale)
   (turbulence :initarg :turbulence :initform #(0.0 0.0 0.0) :accessor turbulence)
   (resolution :initarg :resolution :initform NIL :accessor resolution)
   (clamp :initarg :clamp :initform NIL :accessor clamp)
   (multiplier :initarg :multiplier :initform 1.0 :accessor multiplier)
   (bump-channel :initarg :bump-channel :initform 1 :accessor bump-channel)
   (specular-type :initarg :specular-type :initform NIL :accessor specular-type)))

(defmethod print-object ((object texture-map) stream)
  (print-unreadable-object (object stream :type T)
    (princ (file object) stream)))

(defclass material (named-object)
  ((illumination-model :initarg :illumination-model :initform 2 :accessor illumination-model)
   (ambient-factor :initarg :ambient-factor :initform #(0.0 0.0 0.0) :accessor ambient-factor)
   (diffuse-factor :initarg :diffuse-factor :initform #(0.0 0.0 0.0) :accessor diffuse-factor)
   (specular-factor :initarg :specular-factor :initform #(0.0 0.0 0.0) :accessor specular-factor)
   (specular-exponent :initarg :specular-exponent :initform 1.0 :accessor specular-exponent)
   (transmission-factor :initarg :transmission-factor :initform 0.0 :accessor transmission-factor)
   (transmission-filter :initarg :transmission-filter :initform #(0.0 0.0 0.0) :accessor transmission-filter)
   (refractive-index :initarg :refractive-index :initform 1.0 :accessor refractive-index)
   (roughness-factor :initarg :roughness-factor :initform 1.0 :accessor roughness-factor)
   (metallic-factor :initarg :metallic-factor :initform 1.0 :accessor metallic-factor)
   (sheen-factor :initarg :sheen-factor :initform 1.0 :accessor sheen-factor)
   (emissive-factor :initarg :emissive-factor :initform #(0.0 0.0 0.0) :accessor emissive-factor)
   (ambient-map :initarg :ambient-map :initform NIL :accessor ambient-map)
   (diffuse-map :initarg :diffuse-map :initform NIL :accessor diffuse-map)
   (specular-map :initarg :specular-map :initform NIL :accessor specular-map)
   (transmission-map :initarg :transmission-map :initform NIL :accessor transmission-map)
   (roughness-map :initarg :roughness-map :initform NIL :accessor roughness-map)
   (metallic-map :initarg :metallic-map :initform NIL :accessor metallic-map)
   (sheen-map :initarg :sheen-map :initform NIL :accessor sheen-map)
   (emissive-map :initarg :emissive-map :initform NIL :accessor emissive-map)
   (bump-map :initarg :bump-map :initform NIL :accessor bump-map)
   (displacement-map :initarg :displacement-map :initform NIL :accessor displacement-map)
   (stencil-map :initarg :stencil-map :initform NIL :accessor stencil-map)
   (normal-map :initarg :normal-map :initform NIL :accessor normal-map)
   (rough-metal-occlusion-map :initarg :rough-metal-occlusion-map :initform NIL :accessor rough-metal-occlusion-map)))

(defclass face ()
  ((vertices :initarg :vertices :accessor vertices)
   (uvs :initarg :uvs :accessor uvs)
   (normals :initarg :normals :accessor normals)
   (material :initarg :material :accessor material)))

(defmethod print-object ((object face) stream)
  (print-unreadable-object (object stream :type T)
    (format stream "~a" (coerce (vertices object) 'list))))

(defclass group (named-object)
  ((faces :initform (make-array 0 :element-type T :adjustable T :fill-pointer T) :accessor faces)
   (lod :initform 0 :accessor lod)))

(defclass context ()
  ((vertices :initform (make-array 0 :element-type 'single-float :adjustable T :fill-pointer T) :accessor vertices)
   (uvs :initform (make-array 0 :element-type 'single-float :adjustable T :fill-pointer T) :accessor uvs)
   (normals :initform (make-array 0 :element-type 'single-float :adjustable T :fill-pointer T) :accessor normals)
   (groups :initform (make-hash-table :test 'equal) :accessor groups)
   (materials :initform (make-hash-table :test 'equal) :accessor materials)
   (objects :initform (make-hash-table :test 'equal) :accessor objects)
   (object :initform (make-instance 'object) :accessor object)
   (current :initform NIL :accessor current)
   (material :initform NIL :accessor material)))

(defmethod print-object ((object context) stream)
  (print-unreadable-object (object stream :type T)
    (format stream "~a vert~:p~[~:;~:* ~a uv~:p~]~[~:;~:* ~a normal~:p~]~[~:;~:* ~a group~:p~]~[~:;~:* ~a material~:p~]~[~:;~:* ~a object~:p~]"
            (length (vertices object))
            (length (uvs object))
            (length (normals object))
            (hash-table-count (groups object))
            (hash-table-count (materials object))
            (hash-table-count (objects object)))))

(defclass mesh (named-object)
  ((vertex-data :initarg :vertex-data :accessor vertex-data)
   (index-data :initarg :index-data :accessor index-data)
   (material :initarg :material :initform NIL :accessor material)
   (attributes :initarg :attributes :initform () :accessor attributes)
   (face-length :initarg :face-length :initform NIL :accessor face-length)))

(defun shared-faces (faces)
  (let ((table (make-hash-table :test 'eq)))
    (loop for face across faces
          for array = (or (gethash (material face) table)
                          (setf (gethash (material face) table) (make-array 0 :adjustable T :fill-pointer T)))
          do (vector-push-extend face array))
    (loop for v being the hash-values of table
          collect v)))

(defun faces-to-mesh (context faces)
  (let* ((prototype (aref faces 0))
         (face-length (length (vertices prototype)))
         (vertex-data (make-array 0 :element-type 'single-float :adjustable T :fill-pointer T))
         (index-data (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T))
         (attributes '()))
    (when (< 0 (length (vertices prototype)))
      (push :position attributes))
    (when (< 0 (length (normals prototype)))
      (push :normal attributes))
    (when (< 0 (length (uvs prototype)))
      (push :uv attributes))
    (setf attributes (reverse attributes))
    (let ((size-per-element (+ (if (member :position attributes) 3 0)
                               (if (member :uv attributes) 2 0)
                               (if (member :normal attributes) 3 0)))
          (index-cache (make-hash-table :test 'equal)))
      (flet ((copy (source start count target)
               (loop for i from start below (+ start count)
                     do (vector-push-extend (aref source i) target))))
        (loop for face across faces
              do (loop for i from 0 below face-length
                       for index = (loop for attribute in attributes
                                         collect (aref (ecase attribute
                                                         (:position (vertices face))
                                                         (:normal (normals face))
                                                         (:uv (uvs face)))
                                                       i))
                       for idx = (gethash index index-cache)
                       do (unless idx
                            (setf idx (truncate (length vertex-data) size-per-element))
                            (setf (gethash index index-cache) idx)
                            (loop for attribute in attributes
                                  for i in index
                                  do (case attribute
                                       (:position
                                        (copy (vertices context) (* i 4) 3 vertex-data))
                                       (:normal
                                        (copy (normals context) (* i 3) 3 vertex-data))
                                       (:uv
                                        (copy (uvs context) (* i 3) 2 vertex-data)))))
                          (vector-push-extend idx index-data))))
      (make-instance 'mesh
                     :vertex-data vertex-data
                     :index-data index-data
                     :material (material prototype)
                     :attributes attributes
                     :face-length face-length))))

(defun extract-meshes (context &optional thing)
  (etypecase thing
    (vector
     (loop for faces in (shared-faces thing)
           collect (faces-to-mesh context faces)))
    (group
     (let ((meshes (extract-meshes context (faces thing))))
       (loop for mesh in meshes
             for i from 0
             do (setf (name meshes) (format NIL "~a-~d" (name thing) i)))
       meshes))
    (object
     (let ((faces (make-array 0 :adjustable T :fill-pointer T)))
       (loop for group across (groups thing)
             do (loop for face across (faces group)
                      do (vector-push-extend face faces)))
       (let ((meshes (extract-meshes context faces)))
         (loop for mesh in meshes
               for i from 0
               do (setf (name meshes) (format NIL "~a-~d" (name thing) i)))
         meshes)))
    (null
     (let ((faces (make-array 0 :adjustable T :fill-pointer T)))
       (loop for object being the hash-values of (objects context)
             do (loop for group across (groups object)
                      do (loop for face across (faces group)
                               do (vector-push-extend face faces))))
       (extract-meshes context faces)))))
