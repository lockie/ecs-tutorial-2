(in-package #:ecs-tutorial-2)


(defvar *window-background*)
(defvar *button-normal-background*)
(defvar *button-hover-background*)
(defvar *button-active-background*)

(ui:defwindow narrative (&key text)
    (:x (truncate +window-width+ 4) :y (truncate +window-height+ 4)
     :w (truncate +window-width+ 2) :h (truncate +window-height+ 2)
     :styles ((:item-9slice :window-fixed-background *window-background*)
              (:item-9slice :button-normal *button-normal-background*)
              (:item-9slice :button-hover *button-hover-background*)
              (:item-9slice :button-active *button-active-background*)
              (:color :text-color :r 0 :g 0 :b 0)
              (:color :button-text-normal :r 0 :g 0 :b 0)
              (:color :button-text-hover :r 0 :g 0 :b 0)
              (:color :button-text-active :r 0 :g 0 :b 0)))
  (ui:layout-space (:height (truncate +window-height+ 4) :format :dynamic)
    (ui:layout-space-push :x 0.05 :y 0.15 :w 0.9 :h 1.2)
    (ui:label-wrap text)
    (ui:layout-space-push :x 0.5 :y 1.4 :w 0.4 :h 0.35)
    (ui:button-label "Ok"
      t)))

(ecs:defcomponent narrative
  (text "" :type string)
  (shown nil :type boolean)
  (active nil :type boolean :index active-narratives))

(ecs:defcomponent win)

(defconstant +interact-distance-factor+ 1.2)

(ecs:defsystem show-narrative
  (:components-ro (position)
   :components-rw (narrative)
   :arguments ((ui-context cffi:foreign-pointer))
   :with ((player-x player-y dx dy) :=
          (let ((player (player-entity 1)))
            (values
             (position-x player)
             (position-y player)
             (* +interact-distance-factor+ (size-width player))
             (* +interact-distance-factor+ (size-height player))))))
  (al:with-current-keyboard-state keyboard-state
    (when (and (approx-equal position-x player-x dx)
               (approx-equal position-y player-y dy)
               (or narrative-active
                   (not narrative-shown)
                   (al:key-down keyboard-state :E)))
      (setf narrative-shown t
            narrative-active t)
      (when (or (narrative ui-context :text narrative-text)
                (al:key-down keyboard-state :escape)
                (al:key-down keyboard-state :space)
                (al:key-down keyboard-state :enter))
        (setf narrative-active nil)
        (when (has-win-p entity)
          (setf *should-quit* t))))))

(defun load-ui ()
  (flet ((load-ui-image (name)
           (al:ensure-loaded #'nk:allegro-create-image name)))
    (setf *window-background*        (load-ui-image "panel-031.png")
          *button-normal-background* (load-ui-image "panel-001.png")
          *button-active-background* (load-ui-image "panel-000.png")
          *button-hover-background*  (load-ui-image "panel-0011.png"))))

(defun unload-ui ()
  (nk:allegro-del-image *window-background*)
  (nk:allegro-del-image *button-normal-background*)
  (nk:allegro-del-image *button-active-background*)
  (nk:allegro-del-image *button-hover-background*))
