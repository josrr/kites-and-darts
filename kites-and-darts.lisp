;;;; kites-and-darts.lisp

(in-package #:kites-and-darts)

(defun draw-tiles (objects canvas)
  (window-clear canvas)
  (with-drawing-options (canvas :clipping-region (make-rectangle* 0 0 1024 1024))
    (with-drawing-options (canvas :transformation
                                  (compose-transformations (make-scaling-transformation* 5 5 512 512)
                                                           (make-reflection-transformation* 0 186
                                                                                            1024 186)))
      (loop for obj in (reverse objects)
            do (draw obj canvas)))))

(defun display-canvas (frame canvas)
  (window-clear canvas)
  (draw-tiles (kites-and-darts-frame-objects frame) canvas))

(define-application-frame kites-and-darts-frame ()
  ((objects :initarg :objects :accessor kites-and-darts-frame-objects))
  (:panes (canvas (make-pane 'application-pane
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
    (setf objects
          (loop for obj in objects
                append (p2-step obj)))))

(define-kites-and-darts-frame-command (com-reset :name "Reset" :menu t) ()
  (setf (kites-and-darts-frame-objects *application-frame*)
        (list (make-dart/2 (make-point 0 (+ -512 186))
                           (make-point 1024 (+ -512 186)) :left))))

(defun start ()
  (find-application-frame 'kites-and-darts-frame
                          :objects (list (make-dart/2 (make-point 0 (+ -512 186))
                                                      (make-point 1024 (+ -512 186)) :left))))

;; (bt:make-thread #'start)
