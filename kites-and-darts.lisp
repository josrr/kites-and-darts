;;;; kites-and-darts.lisp

(in-package #:kites-and-darts)

(defun canvas-center (canvas)
  (let ((region (bounding-rectangle canvas)))
    (values (/ (- (rectangle-max-x region) (rectangle-min-x region)) 2)
            (/ (- (rectangle-max-y region) (rectangle-min-y region)) 2)
            region)))

(defun draw-tiles (objects canvas)
  (multiple-value-bind (x y region) (canvas-center canvas)
    (with-drawing-options (canvas :clipping-region region)
      (with-drawing-options (canvas :transformation
                                    (compose-transformations
                                     (make-scaling-transformation* 3.8 3.8 x y)
                                     (make-translation-transformation 0 (- y 273/2))))
        (loop for obj in objects
              do (draw obj canvas))
        (draw-design canvas (kites-and-darts-clipping-region *application-frame*)
                     :filled nil :line-thickness 2 :ink +red+)))))

(defun display-canvas (frame canvas)
  (window-clear canvas)
  (draw-tiles (kites-and-darts-frame-objects frame) canvas))

(defun toggle-polynomial-operations (gadget new-value)
  (let ((frame (pane-frame gadget)))
    (with-accessors ((polyopsp kites-and-darts-polynomial-operations-p)) frame
      (setf polyopsp new-value)
      (execute-frame-command frame '(com-reset)))))

(define-application-frame kites-and-darts-frame ()
  ((objects :initarg :objects :accessor kites-and-darts-frame-objects)
   (clipping-region :initarg :clipping-region :accessor kites-and-darts-clipping-region)
   (polynomial-operations-p :initform nil :accessor kites-and-darts-polynomial-operations-p))
  (:panes (canvas (make-pane 'application-pane
                             :name 'canvas
                             :background +black+
                             :display-function #'display-canvas
                             :display-time :command-loop))
          (toggle-btn :toggle-button
                      :label "Toggle polynomial operations"
                      :value nil
                      :value-changed-callback #'toggle-polynomial-operations)
          (interactor :interactor))
  (:layouts (default
             (vertically (:min-height 1280 :max-height 1280 :height 1280
                          :min-width 1024 :max-width 1024 :width 1024)
               (4/5 canvas)
               (1/20 (labelling (:label "Options") toggle-btn))
               (3/20 interactor))))
  (:menu-bar t))

(define-kites-and-darts-frame-command (com-step :name "Step" :menu t) ()
  (with-accessors ((objects kites-and-darts-frame-objects)
                   (polyopsp kites-and-darts-polynomial-operations-p))
      *application-frame*
    (setf objects (if (null objects)
                      (list (if polyopsp
                                (make-polydart/2 0 1024 :left)
                                (make-dart/2 (make-point 0 0) (make-point 1024 0) :left)))
                      (loop with region = (kites-and-darts-clipping-region *application-frame*)
                            for obj in objects
                            append (p2-step obj region))))))

(define-kites-and-darts-frame-command (com-redraw :name "Redraw" :menu t) ()
  t)

(define-kites-and-darts-frame-command (com-reset :name "Reset" :menu t) ()
  (setf (kites-and-darts-frame-objects *application-frame*) nil))

(defmethod run-frame-top-level :before ((frame kites-and-darts-frame) &key &allow-other-keys)
  (multiple-value-bind (x y) (canvas-center (find-pane-named frame 'canvas))
    (declare (ignore y))
    (setf (kites-and-darts-clipping-region frame)
          (make-rectangle* (- x 137) 0 (+ x 137) 273))))

(defun start ()
  (find-application-frame 'kites-and-darts-frame
                          :objects nil))

;; (bt:make-thread #'start)
