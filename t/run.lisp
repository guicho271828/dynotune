
(in-package :dynotune.test)
(in-suite :dynotune)
(named-readtables:in-readtable coefficient-helper)

(test dynotune
  (dolist (f '(rastrigin
               ackley
               sphere
               rosenbrock
               beale
               goldstein-price
               booth
               bukin-n6
               matyas
               levi
               three-hump-camel
               easom
               cross-in-tray
               eggholder
               holder-table
               mccormick
               schaffer-n2
               schaffer-n4
               styblinski-tang))
    (dolist (optimizer (list (random-search 100)))
      (tune fn optimizer ranges))))

