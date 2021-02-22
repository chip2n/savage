(uiop:define-package #:savage
  (:use #:cl #:arrow-macros)
  (:export #:*svg*
           #:*width*
           #:*height*
           #:svg
           #:compile-svg
           #:inset
           #:rect
           #:circle
           #:line
           #:text
           #:group))
