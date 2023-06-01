;;;; objects.lisp

(in-package #:kites-and-darts)

(defclass tile/2 (standard-polygon) ())

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
            (atan height base))))

(defun to-radians (degree)
  (/ (* degree 2 pi) 360))

(defparameter *phi* (/ (1+ (sqrt 5d0)) 2))
(defparameter *phi1* (/ *phi* (1+ *phi*)))
(defparameter *144* (to-radians 144))
(defparameter *108* (to-radians 108))

(defun make-dart/2 (b1 b2 &optional (handedness :left))
  (multiple-value-bind (a+b theta) (distance b1 b2)
    (let ((a (* *phi1* a+b))
          (angle))
      (make-instance (case handedness
                       (:left
                        (setf angle (+ theta *144*))
                        'left-dart/2)
                       (:right
                        (setf angle (- theta *144*))
                        'right-dart/2))
                     #|:polygon (clim:make-polygon (list b1 b2
                     (make-point (+ (point-x b2) (* a (cos angle)))
                     (+ (point-y b2) (* a (sin angle))))))|#
                     :points (list b1 b2
                                   (make-point (+ (point-x b2) (* a (cos angle)))
                                               (+ (point-y b2) (* a (sin angle)))))))))

(defun make-kite/2 (b1 b2  &optional (handedness :left))
  (multiple-value-bind (a theta) (distance b1 b2)
    (let ((b (/ a *phi*))
          (angle))
      (make-instance (case handedness
                       (:left
                        (setf angle (- theta *108*))
                        'left-kite/2)
                       (:right
                        (setf angle (+ theta *108*))
                        'right-kite/2))
                     #|:polygon (clim:make-polygon (list b1 b2
                     (make-point (+ (point-x b2) (* b (cos angle)))
                     (+ (point-y b2) (* b (sin angle))))))|#
                     :points (list b1 b2
                                   (make-point (+ (point-x b2) (* b (cos angle)))
                                               (+ (point-y b2) (* b (sin angle)))))))))

(defmethod draw ((obj tile/2) pane)
  (draw-design pane obj)
  (let ((points (polygon-points obj)))
    (draw-line pane (elt points 0) (elt points 1)
               :ink +black+ :line-thickness 1)
    (draw-line pane (elt points 1) (elt points 2)
               :ink +black+ :line-thickness 1)))

(defparameter *right-dart-color* (make-rgb-color 1 1 (/ #x99 255)))
(defparameter *left-dart-color* (make-rgb-color 1 1 (/ #xd9 255)))
(defparameter *right-kite-color* (make-rgb-color 1 (/ #x8c 255) (/ #x8c 255)))
(defparameter *left-kite-color* (make-rgb-color 1 (/ #xcc 255) (/ #xcc 255)))

(defmethod draw :around ((obj left-dart/2) pane)
  (with-drawing-options (pane :ink *left-dart-color*)
    (call-next-method)))

(defmethod draw :around ((obj right-dart/2) pane)
  (with-drawing-options (pane :ink *right-dart-color*)
    (call-next-method)))

(defmethod draw :around ((obj left-kite/2) pane)
  (with-drawing-options (pane :ink *left-kite-color*)
    (call-next-method)))

(defmethod draw :around ((obj right-kite/2) pane)
  (with-drawing-options (pane :ink *right-kite-color*)
    (call-next-method)))

(defmethod p2-step :around ((obj tile/2) region)
  (if (region-intersects-region-p region obj)
      (call-next-method)
      nil))

(defmethod p2-step ((obj left-dart/2) region)
  (declare (ignore region))
  (let ((d (make-dart/2 (elt (polygon-points obj) 1)
                        (elt (polygon-points obj) 2)
                        :left)))
    (list d
          (make-kite/2 (elt (polygon-points obj) 0)
                       (elt (polygon-points d) 2)
                       :right))))

(defmethod p2-step ((obj right-dart/2) region)
  (declare (ignore region))
  (let ((d (make-dart/2 (elt (polygon-points obj) 1)
                        (elt (polygon-points obj) 2)
                        :right)))
    (list d
          (make-kite/2 (elt (polygon-points obj) 0)
                       (elt (polygon-points d) 2)
                       :left))))

(defmethod p2-step ((obj left-kite/2) region)
  (declare (ignore region))
  (let* ((k1 (make-kite/2 (elt (polygon-points obj) 1)
                          (elt (polygon-points obj) 2)
                          :left))
         (d (make-dart/2 (elt (polygon-points obj) 0)
                         (elt (polygon-points k1) 2)
                         :left)))
    (list k1 d
          (make-kite/2 (elt (polygon-points obj) 1)
                       (elt (polygon-points d) 2)
                       :right))))

(defmethod p2-step ((obj right-kite/2) region)
  (declare (ignore region))
  (let* ((k1 (make-kite/2 (elt (polygon-points obj) 1)
                          (elt (polygon-points obj) 2)
                          :right))
         (d (make-dart/2 (elt (polygon-points obj) 0)
                         (elt (polygon-points k1) 2)
                         :right)))
    (list k1 d
          (make-kite/2 (elt (polygon-points obj) 1)
                       (elt (polygon-points d) 2)
                       :left))))
