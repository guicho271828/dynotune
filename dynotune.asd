;;;; Autogenerated ASD file for system "DYNOTUNE"
;;;; In order to regenerate it, run update-asdf
;;;; from shell (see https://github.com/phoe-krk/asd-generator)
;;;; For those who do not have update-asdf,
;;;; run `ros install asd-generator` (if you have roswell installed)
;;;; There are also an interface available from lisp:
;;;; (asd-generator:regen &key im-sure)
(defsystem dynotune
 :version "0.1"
 :author "Masataro Asai"
 :mailto "guicho2.71828@gmail.com"
 :license "LLGPL"
 :depends-on (:iterate
              :alexandria
              :trivia
              :introspect-environment
              :type-r)
 :pathname "src/"
 :components ((:file "package")
              (:file "generator")
              (:file "optimizers/differential-evolution")
              (:file "optimizers/grid-search")
              (:file "optimizers/hill-climbing")
              (:file "optimizers/particle-swarm")
              (:file "optimizers/random-search")
              (:file "optimizers/simulated-annealing")
              (:file "optimizers/tabu"))
 :description "Automated parameter tuner for CL"
 :in-order-to ((test-op (test-op :dynotune.test))))
