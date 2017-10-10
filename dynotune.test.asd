#|
  This file is a part of dynotune project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage dynotune.test-asd
  (:use :cl :asdf))
(in-package :dynotune.test-asd)


(defsystem dynotune.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of dynotune"
  :license "LLGPL"
  :depends-on (:dynotune
               :fiveam
               :named-readtables)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval
 (read-from-string
  "(let ((res (5am:run :dynotune)))
     (explain! res)
     (every #'fiveam::TEST-PASSED-P res))"))
))
