(defsystem "ecs-tutorial-2"
  :version "0.0.1"
  :author "Andrew Kravchuk"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-astar
               #:cl-fast-ecs
               #:cl-liballegro
               #:cl-liballegro-nuklear
               #:cl-liballegro-nuklear/declarative
               #:cl-tiled
               #:let-plus
               #:livesupport)
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "common")
                 (:file "animation")
                 (:file "map")
                 (:file "narrative")
                 (:file "character")
                 (:file "main"))))
  :description "cl-fast-ecs framework tutorial."
  :defsystem-depends-on (#:deploy)
  :build-operation "deploy-op"
  :build-pathname #P"ecs-tutorial-2"
  :entry-point "ecs-tutorial-2:main")
