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

;;; Con polígonos

;;(defclass tile/2-poly (tile/2))

(defclass polytile-mixin ()
  ((z0 :initarg :z0 :initform 0 :reader polytile-z0)
   (z1 :initarg :z1 :initform 1 :reader polytile-z1)
   (z2 :initarg :z2 :initform nil :reader polytile-z2)))

(defclass left-polykite/2 (polytile-mixin left-kite/2) ())
(defclass right-polykite/2 (polytile-mixin right-kite/2) ())

(defclass left-polydart/2 (polytile-mixin left-dart/2) ())
(defclass right-polydart/2 (polytile-mixin right-dart/2) ())

(defparameter *p-phi* (poly 1 0 1 -1))
(defparameter *p-1/phi* (poly 0 0 1 -1))
(defparameter *p-t* (poly 0 1))
(defparameter *p-t2* (poly 0 0 1))
(defparameter *cis-pi/5* (cis (/ pi 5)))

(defun polytile-point (p)
  (declare (type (or polynomial integer) p))
  (let ((z (poly-eval p *cis-pi/5*)))
    (make-point (realpart z) (imagpart z))))

(defun make-polydart/2 (z0 z1 &optional (handedness :left))
  (let (z2)
   (make-instance (case handedness
                    (:left
                     (setf z2 (poly-simplify
                               (poly+ z0 (poly*poly (poly*poly (poly- z1 z0) *p-t*)
                                                    *p-1/phi*))))
                     'left-polydart/2)
                    (:right
                     (setf z2 (poly-simplify
                               (poly+ z1 (poly*poly (poly*poly (poly- z0 z1) *p-t*)
                                                    *p-1/phi*))))
                     'right-polydart/2))
                  :z0 z0
                  :z1 z1
                  :z2 z2
                  :points (list (polytile-point z0)
                                (polytile-point z1)
                                (polytile-point z2)))))

(defun make-polykite/2 (z0 z1 &optional (handedness :left))
  (let (z2)
   (make-instance (case handedness
                    (:left
                     (setf z2 (poly-simplify
                               (poly+ z1 (poly*poly (poly*poly (poly- z0 z1) *p-t2*)
                                                    *p-1/phi*))))
                     'left-polykite/2)
                    (:right
                     (setf z2 (poly-simplify
                               (poly+ z0 (poly*poly (poly- z1 z0) *p-t*))))
                     'right-polykite/2))
                  :z0 z0
                  :z1 z1
                  :z2 z2
                  :points (list (polytile-point z0)
                                (polytile-point z1)
                                (polytile-point z2)))))

(defmethod p2-step ((obj left-polydart/2) region)
  (declare (ignore region))
  (let ((d (make-polydart/2 (polytile-z1 obj)
                            (polytile-z2 obj)
                            :left)))
    (list d
          (make-polykite/2 (polytile-z0 obj)
                           (polytile-z2 d)
                           :right))))

(defmethod p2-step ((obj right-polydart/2) region)
  (declare (ignore region))
  (let ((d (make-polydart/2 (polytile-z1 obj)
                            (polytile-z2 obj)
                            :right)))
    (list d
          (make-polykite/2 (polytile-z0 obj)
                           (polytile-z2 d)
                           :left))))

(defmethod p2-step ((obj left-polykite/2) region)
  (declare (ignore region))
  (let* ((k1 (make-polykite/2 (polytile-z1 obj)
                              (polytile-z2 obj)
                              :left))
         (d (make-polydart/2 (polytile-z0 obj)
                             (polytile-z2 k1)
                             :left)))
    (list k1 d
          (make-polykite/2 (polytile-z1 obj)
                           (polytile-z2 d)
                           :right))))

(defmethod p2-step ((obj right-polykite/2) region)
  (declare (ignore region))
  (let* ((k1 (make-polykite/2 (polytile-z1 obj)
                              (polytile-z2 obj)
                              :right))
         (d (make-polydart/2 (polytile-z0 obj)
                             (polytile-z2 k1)
                             :right)))
    (list k1 d
          (make-polykite/2 (polytile-z1 obj)
                           (polytile-z2 d)
                           :left))))
