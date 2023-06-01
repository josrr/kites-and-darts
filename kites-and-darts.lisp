;;;; kites-and-darts.lisp

(in-package #:kites-and-darts)

(defun canvas-center (canvas)
  (let ((region (bounding-rectangle canvas)))
    (values (/ (- (rectangle-max-x region) (rectangle-min-x region)) 2)
            (/ (- (rectangle-max-y region) (rectangle-min-y region)) 2))))

(defun draw-tiles (objects canvas)
  (multiple-value-bind (x y) (canvas-center (find-pane-named *application-frame*
                                                             'canvas))
    (with-drawing-options (canvas :clipping-region (make-rectangle* 0 0 1024 1024))
      (with-drawing-options (canvas :transformation
                                    (make-scaling-transformation* 3.8 3.8 x (- y 70)))
        (loop for obj in (reverse objects)
              do (draw obj canvas))
        (draw-rectangle* canvas (- x 137) (- y 186) (+ x 137) (+ y -186 273)
                         :filled nil :line-thickness 2 :ink +red+)))))

(defun display-canvas (frame canvas)
  (window-clear canvas)
  (draw-tiles (kites-and-darts-frame-objects frame) canvas))

(define-application-frame kites-and-darts-frame ()
  ((objects :initarg :objects :accessor kites-and-darts-frame-objects))
  (:panes (canvas (make-pane 'application-pane
                             :name 'canvas
                             :background +gray80+
                             :display-function #'display-canvas
                             :display-time :command-loop))
          (interactor :interactor))
  (:layouts (default
             (vertically (:min-height 1280 :max-height 1280 :height 1280
                          :min-width 1024 :max-width 1024 :width 1024)
               (3/4 canvas)
               (1/4 interactor))))
  (:menu-bar t))

(define-kites-and-darts-frame-command (com-step :name "Step" :menu t) ()
  (with-accessors ((objects kites-and-darts-frame-objects)) *application-frame*
    (multiple-value-bind (x y) (canvas-center (find-pane-named *application-frame*
                                                               'canvas))
      (setf objects (if (null objects)
                        (list (make-dart/2 (make-point 0 (- y 186))
                                           (make-point 1024 (- y 186)) :left))
                        (loop with region = (make-rectangle* (- x 138) (- y 187)
                                                             (+ x 138) (+ y -186 273))
                              for obj in objects
                              append (p2-step obj region)))))))

(define-kites-and-darts-frame-command (com-redraw :name "Redraw" :menu t) ()
  t)

(define-kites-and-darts-frame-command (com-reset :name "Reset" :menu t) ()
  (setf (kites-and-darts-frame-objects *application-frame*) nil))

(defun start ()
  (find-application-frame 'kites-and-darts-frame
                          :objects nil))

;; (bt:make-thread #'start)
