(in-package #:ecs-tutorial-2)


(define-constant +repl-update-interval+ 0.3d0)

(defvar *resources-path*
  (asdf:system-relative-pathname :ecs-tutorial-2 #P"Resources/"))

(deploy:define-hook (:boot set-resources-path) ()
  (setf *resources-path*
        (merge-pathnames #P"Resources/"
                         (uiop:pathname-parent-directory-pathname
                          (deploy:runtime-directory)))))

(define-constant +font-path+ "inconsolata.ttf" :test #'string=)
(define-constant +font-size+ 24)
(define-constant +ui-font-path+ "alegreya-sc.ttf" :test #'string=)
(define-constant +ui-font-size+ 28)

(define-constant +config-path+ "../config.cfg" :test #'string=)

(defun init ()
  (ecs:bind-storage)
  (load-ui)
  (load-map "level1.tmx")
  (trivial-garbage:gc :full t))

(declaim (type fixnum *fps*))
(defvar *fps* 0)

(declaim (ftype (function (double-float cffi:foreign-pointer)) update))
(defun update (dt ui-context)
  (unless (zerop dt)
    (setf *fps* (round 1 dt)))
  (ecs:run-systems :dt (float dt 0.0) :ui-context ui-context))

(defvar *font*)

(defun render ()
  (nk:allegro-render)
  (al:draw-text *font* (al:map-rgba 255 255 255 0) 0 0 0
                (format nil "~d FPS" *fps*)))

(cffi:defcallback %main :int ((argc :int) (argv :pointer))
  (declare (ignore argc argv))
  (handler-bind
      ((error #'(lambda (e) (unless *debugger-hook*
                         (al:show-native-message-box
                          (cffi:null-pointer) "Hey guys"
                          "We got a big error here :("
                          (with-output-to-string (s)
                            (uiop:print-condition-backtrace e :stream s))
                          (cffi:null-pointer) :error)))))
    (uiop:chdir (setf *default-pathname-defaults* *resources-path*))
    (al:set-config-value (al:get-system-config) "trace" "level" "info")
    (al:set-app-name "ecs-tutorial-2")
    (unless (al:init)
      (error "Initializing liballegro failed"))
    (let ((config (al:load-config-file +config-path+)))
      (unless (cffi:null-pointer-p config)
        (al:merge-config-into (al:get-system-config) config)))
    (unless (al:init-primitives-addon)
      (error "Initializing primitives addon failed"))
    (unless (al:init-image-addon)
      (error "Initializing image addon failed"))
    (unless (al:init-font-addon)
      (error "Initializing liballegro font addon failed"))
    (unless (al:init-ttf-addon)
      (error "Initializing liballegro TTF addon failed"))
    (al:set-new-display-option :vsync 2 :require)
    (let ((display (al:create-display +window-width+ +window-height+))
          (event-queue (al:create-event-queue)))
      (when (cffi:null-pointer-p display)
        (error "Initializing display failed"))
      (al:inhibit-screensaver t)
      (al:set-window-title display "ECS Tutorial 2")
      (al:register-event-source event-queue
                                (al:get-display-event-source display))
      (al:install-keyboard)
      (al:register-event-source event-queue
                                (al:get-keyboard-event-source))
      (al:install-mouse)
      (al:register-event-source event-queue
                                (al:get-mouse-event-source))
      (unwind-protect
           (cffi:with-foreign-object (event '(:union al:event))
             (init)
             (#+darwin trivial-main-thread:call-in-main-thread #-darwin funcall
              #'livesupport:setup-lisp-repl)
             (loop
               :named main-game-loop
               :with *font* := (al:ensure-loaded #'al:load-ttf-font
                                                 +font-path+
                                                 (- +font-size+) 0)
               :with ui-font := (al:ensure-loaded
                                 #'nk:allegro-font-create-from-file
                                 +ui-font-path+ (- +ui-font-size+) 0)
               :with ui-context := (nk:allegro-init ui-font display
                                                    +window-width+
                                                    +window-height+)
               :with ticks :of-type double-float := (al:get-time)
               :with last-repl-update :of-type double-float := ticks
               :with dt :of-type double-float := 0d0
               :with *should-quit* := nil
               :while (loop
                        :named event-loop
                        :initially (nk:input-begin ui-context)
                        :while (al:get-next-event event-queue event)
                        :for type := (cffi:foreign-slot-value
                                      event '(:union al:event) 'al::type)
                        :do (nk:allegro-handle-event event)
                        :always (not (eq type :display-close))
                        :never *should-quit*
                        :finally (nk:input-end ui-context))
               :do (let ((new-ticks (al:get-time)))
                     (setf dt (- new-ticks ticks)
                           ticks new-ticks))
                   (when (> (- ticks last-repl-update)
                            +repl-update-interval+)
                     (livesupport:update-repl-link)
                     (setf last-repl-update ticks))
                   (al:clear-to-color (al:map-rgb 0 0 0))
                   (livesupport:continuable
                     (update dt ui-context)
                     (render))
                   (al:flip-display)
               :finally (unload-ui)
                        (nk:allegro-shutdown)
                        (nk:allegro-font-del ui-font)
                        (al:destroy-font *font*)))
        (al:inhibit-screensaver nil)
        (al:destroy-event-queue event-queue)
        (al:destroy-display display)
        (al:stop-samples)
        (al:uninstall-system)
        (al:shutdown-ttf-addon)
        (al:shutdown-font-addon)
        (al:shutdown-image-addon))))
  0)

(defun main ()
  (#+darwin trivial-main-thread:with-body-in-main-thread #-darwin progn nil
    (float-features:with-float-traps-masked
        (:divide-by-zero :invalid :inexact :overflow :underflow)
      (al:run-main 0 (cffi:null-pointer) (cffi:callback %main)))))

