;; This file illustrates the proxy class C++ interface generated
;; by SWIG.

(setf *print-case* :downcase) ; legible.
; :invert actually turns on case-sensitivity, even though it says invert and not preserve.
; for details, see http://www.cliki.net/Case%20sensitivity
(setf (readtable-case *readtable*) :invert)

; we need cffi for swig<->common lisp to function, make sure it is installed.
; Then we load it here:
(require 'cffi)
(shadowing-import 'cffi:defcallback)
(use-package :cffi)




(:cd "~/dj/swig/Examples/cffi/simple")

;; make, if not already
(run-program "make" '() :output t)

(open-shared-library "./example.so")

(compile-file "example.lisp")
(load "example.lx64fsl")

(compile-file "example-clos.lisp")
(load "example-clos.lx64fsl")



;; ----- Object creation -----

(format t "Creating some objects:")
(setf circle (make-instance 'Circle 10))
(format t "    Created circle")
(setf square (make-instance 'Square 10))
(format t "    Created square")

;; ----- Access a static member -----

(format t "A total of %d shapes were created" (Shape_nshapes))

;; ----- Member data access -----

;; Set the location of the object

(setf (slot-value circle 'x) 20)
(setf (slot-value circle 'y) 30

(setf (slot-value square 'x) -10)
(setf (slot-value square 'y) 5)

(format t "Here is their current position:")
(format t "    Circle = (%f, %f)", circle$x,circle$y)
(format t "    Square = (%f, %f)", square$x,square$y)

;; ----- Call some methods -----

(format t "Here are some properties of the shapes:~%")

(apply (lambda (o) (format t "       area = ~A perimeter = ~A~%" (o$area)  (o$perimeter))) '(circle, square))

(format t "Guess I'll clean up now")
(circle.delete)
(square.delete)

(format t "~A shapes remain~%" (Shape_nshapes))
(format t "Goodbye~%")

