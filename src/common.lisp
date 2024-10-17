(in-package #:ecs-tutorial-2)


(define-constant +window-width+ 1280)
(define-constant +window-height+ 800)

(ecs:defcomponent parent
  (entity -1 :type ecs:entity :index children))

(ecs:hook-up ecs:*entity-deleting-hook*
             (lambda (entity)
               (dolist (child (children entity))
                 (ecs:delete-entity child))))

(declaim (inline tile-hash)
         (ftype (function ((single-float 0.0 #.(float (1- (ash 1 28))))
                           (single-float 0.0 #.(float (1- (ash 1 28)))))
                          fixnum)
                tile-hash))
(defun tile-hash (x y)
  (let ((x* (truncate x))
        (y* (truncate y)))
    (logior (ash x* 32) y*)))

(ecs:defcomponent position
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (tile-hash (tile-hash x y) :type fixnum :index tiles))

(ecs:defcomponent size
  (width  0.0 :type single-float)
  (height 0.0 :type single-float))

(declaim
 (inline approx-equal)
 (ftype (function (single-float single-float &optional single-float) boolean)
        approx-equal))
(defun approx-equal (a b &optional (epsilon 0.5))
  (< (abs (- a b)) epsilon))

(defvar *should-quit*)
