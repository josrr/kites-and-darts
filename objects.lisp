;;;; objects.lisp

(in-package #:rhombs)

(defclass tile/2 ()
  (;;(hard-segments :initarg :hard-segments :initform nil :accessor hard-segments)
   (points :initarg :points :initform nil :accessor points)))

(defclass kite/2 (tile/2) ())
(defclass left-kite/2 (kite/2) ())
(defclass right-kite/2 (kite/2) ())

(defclass dart/2 (tile/2) ())
(defclass left-dart/2 (dart/2) ())
(defclass right-dart/2 (dart/2) ())

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
(defparameter *36* (to-radians 36))
(defparameter *108* (to-radians 108))
(defparameter *72* (to-radians 72))

(defun make-dart/2 (b1 b2 &optional (handedness :left))
  (multiple-value-bind (a+b base height) (distance b1 b2)
    (let ((a (* *phi1* a+b))
          (theta (atan height base)))
      (case handedness
        (:left (let* ((angle (+ *144* theta))
                      (b3 (make-point (+ (point-x b2) (* a (cos angle)))
                                      (+ (point-y b2) (* a (sin angle))))))
                 (make-instance 'left-dart/2
                                :points (list b1 b2 b3))))
        (:right (let* ((angle (- theta *36*))
                       (b3 (make-point (+ (point-x b1) (* a (cos angle)))
                                       (+ (point-y b1) (* a (sin angle))))))
                  (make-instance 'right-dart/2
                                 :points (list b1 b2 b3))))))))

(defun make-kite/2 (b1 b2  &optional (handedness :left))
  (multiple-value-bind (a base height) (distance b1 b2)
    (let ((b (/ a *phi*))
          (theta (atan height base)))
      (case handedness
        (:left (let* ((angle (- theta *108*))
                      (b3 (make-point (+ (point-x b2) (* b (cos angle)))
                                      (+ (point-y b2) (* b (sin angle))))))
                 (make-instance 'left-kite/2
                                :points (list b1 b2 b3))))
        (:right (let* ((angle (+ *108* theta))
                       (b3 (make-point (+ (point-x b2) (* b (cos angle)))
                                       (+ (point-y b2) (* b (sin angle))))))
                  (make-instance 'right-kite/2
                                 :points (list b1 b2 b3))))))))

(defparameter *right-dart-color* (make-rgb-color 1 1 (/ #x99 255)))
(defparameter *left-dart-color* (make-rgb-color 1 1 (/ #xd9 255)))
(defparameter *right-kite-color* (make-rgb-color 1 (/ #x8c 255) (/ #x8c 255)))
(defparameter *left-kite-color* (make-rgb-color 1 (/ #xcc 255) (/ #xcc 255)))

(defmethod draw ((obj left-dart/2) pane)
  (draw-polygon pane (points obj) :ink *left-dart-color*)
  (draw-line pane (elt (points obj) 0) (elt (points obj) 1)
             :ink +black+ :line-thickness 4)
  (draw-line pane (elt (points obj) 1) (elt (points obj) 2)
             :ink +black+ :line-thickness 4))

(defmethod draw ((obj right-dart/2) pane)
  (draw-polygon pane (points obj) :ink *right-dart-color*)
  (draw-line pane (elt (points obj) 0) (elt (points obj) 1)
             :ink +black+ :line-thickness 4)
  (draw-line pane (elt (points obj) 1) (elt (points obj) 2)
             :ink +black+ :line-thickness 4))

(defmethod draw ((obj left-kite/2) pane)
  (draw-polygon pane (points obj) :ink *left-kite-color*)
  (draw-line pane (elt (points obj) 0) (elt (points obj) 1)
             :ink +black+ :line-thickness 4)
  (draw-line pane (elt (points obj) 1) (elt (points obj) 2)
             :ink +black+ :line-thickness 4))

(defmethod draw ((obj right-kite/2) pane)
  (draw-polygon pane (points obj) :ink *right-kite-color*)
  (draw-line pane (elt (points obj) 0) (elt (points obj) 1)
             :ink +black+ :line-thickness 4)
  (draw-line pane (elt (points obj) 1) (elt (points obj) 2)
             :ink +black+ :line-thickness 4))

(defmethod p2-step ((obj left-dart/2))
  (let* ((d (make-dart/2 (elt (points obj) 1) (elt (points obj) 2) :left))
         (k (make-kite/2 (elt (points obj) 0) (elt (points d) 2) :right)))
    (list k d)))

(defmethod p2-step ((obj right-dart/2))
  (let* ((d (make-dart/2 (elt (points obj) 1) (elt (points obj) 2) :right))
         (k (make-kite/2 (elt (points obj) 0) (elt (points d) 2) :left)))
    (list k d)))

(defmethod p2-step ((obj right-kite/2))
  (let* ((k1 (make-kite/2 (elt (points obj) 1) (elt (points obj) 2) :right))
         (d (make-dart/2 (elt (points obj) 0) (elt (points k1) 2) :right))
         (k2 (make-kite/2 (elt (points obj) 1) (elt (points d) 2) :left)))
    (list k1 d k2)))

(defmethod p2-step ((obj left-kite/2))
  (let* ((k1 (make-kite/2 (elt (points obj) 1) (elt (points obj) 2) :left))
         (d (make-dart/2 (elt (points obj) 0) (elt (points k1) 2) :left))
         (k2 (make-kite/2 (elt (points obj) 1) (elt (points d) 2) :right)))
    (list k1 d k2)))
