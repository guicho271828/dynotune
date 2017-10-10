#|
  This file is a part of dynotune project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Automated parameter tuner for CL

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage dynotune-asd
  (:use :cl :asdf))
(in-package :dynotune-asd)


(defsystem dynotune
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:iterate :alexandria :trivia)
  :components ((:module "src"
                :components
                ((:file "package"))))
  :description "Automated parameter tuner for CL"
  :in-order-to ((test-op (test-op :dynotune.test))))
