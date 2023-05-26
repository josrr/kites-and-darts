;;;; objects.lisp

(in-package #:rhombs)

(defclass tile/2 ()
  (;;(hard-segments :initarg :hard-segments :initform nil :accessor hard-segments)
   (points :initarg :points :initform nil :accessor points)))

(defclass kite/2 (tile/2) ())

(defclass dart/2 (tile/2) ())

(defun distance (p1 p2)
  (let ((base (- (point-x p2) (point-x p1)))
        (height (- (point-y p2) (point-y p1))))
    (values (sqrt (+ (expt base 2) (expt height 2)))
            base
            height)))

(defun to-radians (degree)
  (/ (* degree 2 pi) 360))

(defparameter *phi* (/ (1+ (sqrt 5d0)) 2))
(defparameter *phi1* (/ *phi* (1+ *phi*)))
(defparameter *144* (to-radians 144))

(defun make-dart/2 (b1 b2)
  (multiple-value-bind (a+b base height) (distance b1 b2)
   (let* ((a (* *phi1* a+b))
          (theta (atan height base))
          (angle (+ *144* theta))
          (b3 (make-point (+ (point-x b2) (* a (cos angle)))
                          (+ (point-y b2) (* a (sin angle))))))
     (make-instance 'dart/2
                    :points (list b1 b2 b3)))))
