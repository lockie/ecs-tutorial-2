(in-package #:ecs-tutorial-2)


(ecs:defcomponent (image :finalize (lambda (entity &key bitmap)
                                     (when (has-map-tile-prefab-p entity)
                                       (al:destroy-bitmap bitmap))))
  (bitmap (cffi:null-pointer) :type cffi:foreign-pointer))

(ecs:defcomponent animation-frame
  (sequence :|| :type keyword :index sequence-frames)
  (duration 0.0 :type single-float))

(ecs:defcomponent animation-state
  (sequence :|| :type keyword)
  (frame 0 :type fixnum)
  (duration 0.0 :type single-float)
  (elapsed 0.0 :type single-float))

(ecs:defsystem render-images
  (:components-ro (position image)
   :initially (al:hold-bitmap-drawing t)
   :finally (al:hold-bitmap-drawing nil))
  (al:draw-bitmap image-bitmap position-x position-y 0))

(ecs:defsystem update-animations
  (:components-rw (animation-state image)
   :arguments ((dt single-float)))
  (incf animation-state-elapsed dt)
  (when (> animation-state-elapsed animation-state-duration)
    (let+ (((&values nframes rest-time) (floor animation-state-elapsed
                                               animation-state-duration))
           (sequence-frames (sequence-frames animation-state-sequence))
           ((&values &ign nframe) (truncate (+ animation-state-frame
                                               (the fixnum nframes))
                                            (length sequence-frames)))
           (frame (nth nframe sequence-frames)))
      (setf animation-state-elapsed rest-time
            animation-state-frame nframe
            animation-state-duration (animation-frame-duration frame)
            image-bitmap (image-bitmap frame)))))

(defun load-bitmap (filename)
  (al:ensure-loaded #'al:load-bitmap (namestring filename)))

(defun animation->spec (frame sequence)
  (copy-list
   `((:animation-frame :sequence ,sequence
                       :duration ,(* 0.001 (tiled:frame-duration frame))))))

(defun instantiate-animation (entity prefab)
  (let ((duration (animation-frame-duration prefab)))
    (make-animation-state entity
                          :sequence (animation-frame-sequence prefab)
                          :duration duration
                          :elapsed (random duration))))

(defun change-animation-sequence (entity new-sequence)
  (unless (eq (animation-state-sequence entity) new-sequence)
    (let ((first-frame (first (sequence-frames new-sequence :count 1))))
      (with-animation-state () entity
        (setf sequence new-sequence
              frame 0
              duration (animation-frame-duration first-frame)
              elapsed 0.0
              (image-bitmap entity) (image-bitmap first-frame))))))
