(in-package #:org.shirakumo.fraf.wavefront)

(defvar *line-number*)

(defun skip-line (in)
  (loop for char = (read-char in NIL NIL)
        while char
        until (or (char= char #\Linefeed)
                  (and (char= char #\Return)
                       (not (char= char #\Linefeed (peek-char NIL in))))))
  (incf *line-number*))

(defun %read-line (in)
  (with-output-to-string (out)
    (loop for char = (read-char in NIL NIL)
          while char
          do (case char
               ((#\Linefeed #\Return)
                (incf *line-number*)
                (return))
               (#\\
                (let ((char (read-char in)))
                  (case char
                    (#\Linefeed)
                    (#\Return ; Special case handling for CRLF skipping
                     (when (char= (peek-char NIL in) #\Linefeed)
                       (read-char in)))
                    (T
                     (write-char char out)))))
               (#\#
                (skip-line in)
                (return))
               (T
                (write-char char out))))))

(defun read-wavefront-line (in)
  (when (peek-char T in NIL NIL)
    (string-trim '(#\Space #\Tab) (%read-line in))))

(defgeneric %parse (context command line))

(defmethod %parse ((context context) command line)
  (warn "Unknown command ~a on line ~d:~%  ~a" command *line-number* line))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun compile-arglist-regex (args)
    (with-output-to-string (out)
      (format out "\\s*")
      (loop with optional = NIL
            for cons on args
            do (case (car cons)
                 (&optional
                  (setf optional T)
                  (format out "(:?"))
                 (&rest
                  (format out "\\s*(.*)\\s*")
                  (loop-finish))
                 (T
                  (format out "([^ ]+)~@[\\s*~]" (cdr cons))))
            finally (when optional (format out ")?"))))))

(defmacro define-parser (command args &body body)
  (let ((regex (compile-arglist-regex args)))
    `(defmethod %parse ((context context) (command (eql ,command)) line)
       (cl-ppcre:register-groups-bind ,(remove '&optional (remove '&rest args)) (,regex line :start ,(1+ (length (string command))))
         ,@(when (member '&rest args)
             `((setf ,(car (last args)) (cl-ppcre:split "\\s+" ,(car (last args))))))
         ,@body))))

(defun push-floats (array &rest float-ishs)
  (dolist (string float-ishs array)
    (let ((float (parse-float:parse-float string :type (array-element-type array))))
      (vector-push-extend float array))))

(defun parse-floats (&rest float-ishs)
  (let ((array (make-array (length float-ishs) :element-type 'single-float)))
    (map-into array #'parse-float:parse-float float-ishs)))

(defun resolve-index (index-ish array current-index)
  (let ((index (parse-integer index-ish)))
    (cond ((< index 0)
           (setf index (+ index current-index)))
          ((= index 0)
           (error "0-based vertex indices are not allowed."))
          (T
           (setf index (- index 1))))
    (vector-push-extend index array)))

(define-parser :v (x y z &optional w)
  (push-floats (vertices context) x y z (or w "1.0")))

(define-parser :vn (x y z)
  (push-floats (normals context) x y z))

(define-parser :vt (u v &optional w)
  (push-floats (uvs context) u v (or w "0.0")))

(define-parser :f (&rest verts)
  (let* ((vertices (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T))
         (uvs (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T))
         (normals (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T)))
    (loop for v in verts
          for (vertex uv normal) = (cl-ppcre:split "/" v)
          do (resolve-index vertex vertices (floor (length (vertices context)) 4))
             (when (and uv (string/= "" uv))
               (resolve-index uv uvs (floor (length (uvs context)) 3)))
             (when (and normal (string/= "" normal))
               (resolve-index normal normals (floor (length (normals context)) 3))))
    (unless (typep (current context) 'group)
      (let ((group (make-instance 'group :name NIL)))
        (setf (gethash NIL (groups (object context))) group)
        (setf (gethash NIL (groups context)) group)
        (setf (current context) group)))
    (vector-push-extend (make-instance 'face :vertices vertices :uvs uvs :normals normals
                                             :material (material context))
                        (faces (current context)))))

(define-parser :g (name)
  (let ((group (make-instance 'group :name name)))
    (setf (gethash name (groups (object context))) group)
    (setf (gethash name (groups context)) group)
    (setf (current context) group)))

(define-parser :o (name)
  ;; If the null object contains anything, register it.
  (when (and (null (name (object context))) (< 0 (hash-table-count (groups (object context)))))
    (setf (gethash NIL (objects context)) (object context)))
  (let ((object (make-instance 'object :name name)))
    (setf (gethash name (objects context)) object)
    (setf (object context) object)))

(define-parser :s (&rest args)
  ;; ignore for now
  )

(define-parser :usemtl (material)
  (setf (material context) material))

(define-parser :newmtl (name)
  (let ((material (make-instance 'material :name name)))
    (setf (gethash name (materials context)) material)
    (setf (current context) material)))

(define-parser :lod (level)
  (setf (lod (current context)) (parse-integer level)))

(define-parser :mtllib (file)
  (parse (merge-pathnames file) context))

(defmacro define-property-parser (command args accessor)
  `(define-parser ,command ,args
     (setf (,accessor (current context)) ,(if (rest args)
                                              `(parse-floats ,@args)
                                              `(parse-float:parse-float ,@args)))))

(define-property-parser :ka (r g b) ambient-factor)
(define-property-parser :kd (r g b) diffuse-factor)
(define-property-parser :ks (r g b) specular-factor)
(define-parser :d (v)
  (setf (transmission-factor (current context)) (- 1.0 (parse-float:parse-float v))))
(define-property-parser :tr (v) transmission-factor)
(define-property-parser :tf (r g b) transmission-filter)
(define-property-parser :ni (v) refractive-index)
(define-property-parser :ns (v) specular-exponent)
(define-property-parser :pr (r g b) roughness-factor)
(define-property-parser :pm (r g b) metallic-factor)
(define-property-parser :ps (r g b) sheen-factor)
(define-property-parser :ke (r g b) emissive-factor)
(define-parser :illum (v)
  (setf (illumination-model (current context)) (parse-integer v)))

(defmacro define-map-parser (command accessor)
  `(define-parser ,command (&rest map-args)
     (setf (,accessor (current context)) (parse-map map-args))))

(defun parse-map (args)
  (let ((map (make-instance 'texture-map))
        (args args))
    (flet ((parse-many ()
             (cl:map '(vector single-float) #'parse-float:parse-float
                  (loop repeat 3
                        until (or (null args) (string= "-" (car args) :end2 1))
                        collect (pop args)))))
      (loop for arg = (pop args)
            while arg
            do (cond ((string= arg "-blendu")
                      (setf (blend-u map) (string= "on" (pop args))))
                     ((string= arg "-blendv")
                      (setf (blend-v map) (string= "on" (pop args))))
                     ((string= arg "-boost")
                      (setf (boost map) (parse-float:parse-float (pop args))))
                     ((string= arg "-o")
                      (setf (origin map) (parse-many)))
                     ((string= arg "-s")
                      (setf (scale map) (parse-many)))
                     ((string= arg "-t")
                      (setf (turbulence map) (parse-many)))
                     ((string= arg "-texres")
                      (setf (resolution map) (parse-integer (pop args))))
                     ((string= arg "-clamp")
                      (setf (clamp map) (string= "on" (pop args))))
                     ((string= arg "-bm")
                      (setf (multiplier map) (parse-float:parse-float (pop args))))
                     ((string= arg "-imfchan")
                      (setf (bump-channel map) (intern (string-upcase (pop args)) "KEYWORD")))
                     ((string= arg "-type")
                      (setf (texture-type map) (pop args)))
                     ((string= arg "-" :end1 1)
                      (warn "Unrecognized texture map option: ~a" arg)
                      (pop args))
                     (T
                      (setf (file map) (merge-pathnames arg)))))
      map)))

(define-map-parser :map_ka ambient-map)
(define-map-parser :map_kd diffuse-map)
(define-map-parser :map_ks specular-map)
(define-map-parser :map_tr transmission-map)
(define-map-parser :map_bump bump-map)
(define-map-parser :bump bump-map)
(define-map-parser :disp displacement-map)
(define-map-parser :decal stencil-map)
(define-map-parser :map_pr roughness-map)
(define-map-parser :map_pm metallic-map)
(define-map-parser :map_ps sheen-map)
(define-map-parser :map_ke emissive-map)
(define-map-parser :map_rma rough-metal-occlusion-map)
(define-map-parser :map_orm rough-metal-occlusion-map)
(define-map-parser :norm normal-map)

(defun parse (source &optional context)
  (etypecase source
    (string
     (with-input-from-string (source source)
       (parse source context)))
    (pathname
     (let ((*default-pathname-defaults* source))
       (with-open-file (source source :direction :input)
         (parse source context))))
    (stream
     (unless context (setf context (make-instance 'context)))
     (loop with *line-number* = 0
           for line = (read-wavefront-line source)
           while line
           do (when (string/= "" line)
                (let* ((command-name (subseq line 0 (or (position #\Space line) (length line))))
                       (command (or (find-symbol (string-upcase command-name) "KEYWORD")
                                    command-name)))
                  (%parse context command line))))
     ;; Ensure virtual groups and objects
     (when (and (null (name (object context))) (< 0 (hash-table-count (groups (object context)))))
       (setf (gethash NIL (objects context)) (object context)))
     ;; Resolve materials
     (loop for group being the hash-values of (groups context)
           do (loop for face across (faces group)
                    for material = (gethash (material face) (materials context))
                    do (when (material face)
                         (unless material
                           (warn "Unknown material referenced with name ~s" (material face)))
                         (setf (material face) material))))
     context)))
