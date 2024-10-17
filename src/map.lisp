(in-package #:ecs-tutorial-2)


(ecs:defcomponent map)

(ecs:defcomponent map-tile
  (obstacle nil :type boolean))

(ecs:defcomponent map-tile-prefab
  (gid 0 :type fixnum :index map-tile-prefab :unique t))

(defun tile->spec (tile bitmap map-entity)
  (let ((width (tiled:tile-width tile))
        (height (tiled:tile-height tile)))
    (copy-list
     `((:parent :entity ,map-entity)
       (:image :bitmap ,(al:create-sub-bitmap bitmap
                                              (tiled:tile-pixel-x tile)
                                              (tiled:tile-pixel-y tile)
                                              width height))
       (:map-tile-prefab :gid ,(tiled:tile-gid tile))
       (:size :width ,(float width)
              :height ,(float height))))))

(defun properties->spec (properties)
  (when properties
    (loop :for component :being :the :hash-key
          :using (hash-value slots) :of properties
          :when (typep slots 'hash-table)
          :collect (list* (make-keyword (string-upcase component))
                          (loop :for name :being :the :hash-key
                                :using (hash-value value) :of slots
                                :nconcing (list
                                           (make-keyword (string-upcase name))
                                           value))))))

(defun load-tile-prefab (tile bitmap map-entity &optional extra-specs)
  (unless (ecs:entity-valid-p
           (map-tile-prefab (tiled:tile-gid tile) :missing-error-p nil))
    (let* ((internal-tile-spec (tile->spec tile bitmap map-entity))
           (properties (when (typep tile 'tiled:properties-mixin)
                         (tiled:properties tile)))
           (external-tile-spec (properties->spec properties))
           sequence frames
           (animation-spec
             (when (typep tile 'tiled:animated-tile)
               (setf sequence (make-keyword
                               (string-upcase (gethash "sequence" properties)))
                     frames (tiled:tile-frames tile))
               (animation->spec (first frames) sequence)))
           (tile-spec (ecs:spec-adjoin
                       (nconc internal-tile-spec external-tile-spec
                              animation-spec extra-specs)
                       '(:map-tile)))
           (tile (ecs:make-object tile-spec)))
      (dolist (frame (rest frames))
        (load-tile-prefab (tiled:frame-tile frame) bitmap map-entity
                          (animation->spec frame sequence)))
      tile)))

(defun load-tile (entity tile x y)
  (let ((prefab (map-tile-prefab (tiled:tile-gid tile))))
    (ecs:copy-entity prefab
                     :destination entity
                     :except '(:map-tile-prefab :animation-frame))
    (when (has-animation-frame-p prefab)
      (instantiate-animation entity prefab))
    (make-position entity :x (float x)
                          :y (float y))))

(defun load-map (filename)
  (let ((map (tiled:load-map filename))
        (map-entity (ecs:make-object '((:map)))))
    (dolist (tileset (tiled:map-tilesets map))
      (let ((bitmap (load-bitmap
                     (tiled:image-source (tiled:tileset-image tileset)))))
        (dolist (tile (tiled:tileset-tiles tileset))
          (load-tile-prefab tile bitmap map-entity))))
    (dolist (layer (tiled:map-layers map))
      (typecase layer
        (tiled:tile-layer
         (dolist (cell (tiled:layer-cells layer))
           (load-tile (ecs:make-entity) (tiled:cell-tile cell)
                      (tiled:cell-x cell) (tiled:cell-y cell))))
        (tiled:object-layer
         (dolist (object (tiled:object-group-objects layer))
           (let ((object-entity (ecs:make-object
                                 (properties->spec (tiled:properties object)))))
             (typecase object
               (tiled:tile-object
                 (load-tile object-entity (tiled:object-tile object)
                            (tiled:object-x object)
                            (- (tiled:object-y object)
                               (tiled:object-height object))))))))))
    map-entity))
