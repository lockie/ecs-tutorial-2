(in-package #:ecs-tutorial-2)


(ecs:defcomponent character
  (speed 0.0 :type single-float)
  (target-x single-float-nan :type single-float)
  (target-y single-float-nan :type single-float))

(ecs:defcomponent player
  (player 1 :type bit :index player-entity :unique t))

(ecs:defcomponent enemy
  (vision-range 0.0 :type single-float)
  (attack-range 0.0 :type single-float))

(ecs:defcomponent path-point
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (traveller -1 :type ecs:entity :index path-points))

(ecs:defcomponent path
  (destination-x 0.0 :type single-float)
  (destination-y 0.0 :type single-float))

(ecs:defsystem move-characters
  (:components-rw (position character)
   :components-ro (size)
   :when (null (active-narratives t))
   :arguments ((dt single-float)))
  (when (or (float-nan-p character-target-x)
            (float-nan-p character-target-y))
    (setf character-target-x position-x
          character-target-y position-y))
  (if (and (approx-equal position-x character-target-x)
           (approx-equal position-y character-target-y))
      (change-animation-sequence
       entity
       (if (has-player-p entity) :orc-idle :demon-idle))
      (let* ((angle (atan (- character-target-y position-y)
                          (- character-target-x position-x)))
             (dx (* character-speed dt (cos angle)))
             (dy (* character-speed dt (sin angle)))
             (new-x (+ position-x dx))
             (new-y (+ position-y dy)))
        (setf position-x new-x
              position-y new-y)
        (change-animation-sequence
         entity
         (if (has-player-p entity) :orc-run :demon-run)))))

(declaim (inline tile-start)
         (ftype (function (single-float single-float single-float single-float)
                          (values single-float single-float))
                tile-start))
(defun tile-start (x y width height)
  (values (* width (the fixnum (floor x width)))
          (* height (the fixnum (floor y height)))))

(declaim (ftype (function (single-float single-float) boolean) tile-obstacle-p))
(defun tile-obstacle-p (x y)
  (loop :for entity :of-type ecs:entity :in (tiles (tile-hash x y))
        :thereis (and (has-map-tile-p entity)
                      (map-tile-obstacle entity))))

(declaim (inline obstaclep))
(defun obstaclep (x y size-width size-height)
  (multiple-value-call #'tile-obstacle-p
    (tile-start x y size-width size-height)))

(ecs:defsystem control-player
  (:components-ro (player position size)
   :components-rw (character)
   :after (move-characters)
   :when (null (active-narratives t)))
  (al:with-current-keyboard-state keyboard-state
    (let ((dx 0) (dy 0))
      (when (al:key-down keyboard-state :W) (setf dy -1))
      (when (al:key-down keyboard-state :A) (setf dx -1))
      (when (al:key-down keyboard-state :S) (setf dy +1))
      (when (al:key-down keyboard-state :D) (setf dx +1))

      (setf
       character-target-x
       (clamp (+ position-x dx) size-width (- +window-width+ size-width))
       character-target-y
       (clamp (+ position-y dy) size-height (- +window-height+ size-height)))

       (when (or (and (or (minusp dx) (minusp dy))
                      (obstaclep character-target-x
                                 character-target-y
                                 size-width size-height))
                 (and (or (minusp dx) (plusp dy))
                      (obstaclep character-target-x
                                 (+ character-target-y size-height)
                                 size-width size-height))
                 (and (or (plusp dx) (minusp dy))
                      (obstaclep (+ character-target-x size-width)
                                 character-target-y
                                size-width size-height))
                 (and (or (plusp dx) (plusp dy))
                      (obstaclep (+ character-target-x size-width)
                                (+ character-target-y size-height)
                                size-width size-height)))
         (setf character-target-x position-x
               character-target-y position-y)))))

(ecs:defsystem handle-enemies
  (:components-ro (position size enemy)
   :components-rw (character)
   :with ((player-x player-y) := (with-position () (player-entity 1)
                                   (values x y))))
  (when (and (approx-equal position-x player-x enemy-vision-range)
             (approx-equal position-y player-y enemy-vision-range)
             (not (and (has-path-p entity)
                       (approx-equal
                        (path-destination-x entity) player-x size-width)
                       (approx-equal
                        (path-destination-y entity) player-y size-height))))
    (find-path position-x position-y player-x player-y
               :entity entity :tile-width size-width :tile-height size-height))
  (when (and (approx-equal position-x player-x enemy-attack-range)
             (approx-equal position-y player-y enemy-attack-range))
    (setf *should-quit* t)
    (al:show-native-message-box (cffi:null-pointer)
                                "ECS Tutorial 2" "" "You died"
                                (cffi:null-pointer) :warn)))

(ecs:defsystem follow-path
  (:components-ro (enemy path position)
   :components-rw (character))
  "Follows path previously calculated by A* algorithm."
  (if-let (first-point (first (path-points entity :count 1)))
    (with-path-point (point-x point-y) first-point
      (if (and (approx-equal position-x point-x)
               (approx-equal position-y point-y))
          (ecs:delete-entity first-point)
          (setf character-target-x point-x
                character-target-y point-y)))
    (delete-path entity)))

(a*:define-path-finder find-path (entity tile-width tile-height)
    (:variables ((world-width  (floor +window-width+  tile-width))
                 (world-height (floor +window-height+ tile-height)))
     :world-size (* world-width world-height)
     :indexer (a*:make-row-major-indexer
               world-width
               :node-width tile-width :node-height tile-height)
     :goal-reached-p (lambda (x1 y1 x2 y2)
                       (and (= (floor x1 tile-width)  (floor x2 tile-width))
                            (= (floor y1 tile-height) (floor y2 tile-height))))
     :neighbour-enumerator (lambda (x y f)
                             (let+ (((&values sx sy)
                                     (tile-start x y tile-width tile-height)))
                               (funcall (a*:make-8-directions-enumerator
                                         :node-width tile-width
                                         :node-height tile-height
                                         :max-x +window-width+
                                         :max-y +window-height+)
                                        sx sy f)))
     :exact-cost (lambda (x1 y1 x2 y2)
                   (if (or (obstaclep x2 y2 tile-width tile-height)
                           (and (/= x1 x2)
                                (/= y1 y2)
                                (or (obstaclep x1 y2 tile-width tile-height)
                                    (obstaclep x2 y1 tile-width tile-height))))
                       most-positive-single-float
                       0.0))
     :heuristic-cost (a*:make-octile-distance-heuristic)
     :path-initiator (lambda (length)
                       (declare (ignorable length))
                       (when (has-path-p entity)
                         (dolist (point (path-points entity))
                           (ecs:delete-entity point)))
                       (assign-path entity :destination-x goal-x
                                           :destination-y goal-y))
     :path-processor (lambda (x y)
                       (ecs:make-object
                        `((:path-point :x ,x :y ,y :traveller ,entity)
                          (:parent :entity ,entity)))))
  (declare (type (single-float 1.0 100.0) tile-width tile-height)
           (type (integer 1 100) world-width world-height)))
