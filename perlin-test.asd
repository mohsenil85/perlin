#|
  This file is a part of perlin project.
|#

(in-package :cl-user)
(defpackage perlin-test-asd
  (:use :cl :asdf))
(in-package :perlin-test-asd)

(defsystem perlin-test
  :author ""
  :license ""
  :depends-on (:perlin
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "perlin"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
