;;;; rhombs.lisp

(in-package #:rhombs)

(defun draw-tiles (objects canvas)
  (window-clear canvas)
  (with-drawing-options (canvas
                         ;;:transformation (make-rotation-transformation* pi 512 512)
                         :transformation (make-reflection-transformation* 0 512 1024 512))
    (loop for obj in (reverse objects)
          do (draw obj canvas))))

(defun display-canvas (frame canvas)
  (window-clear canvas)
  (draw-tiles (rhombs-frame-objects frame) canvas))

(define-application-frame rhombs-frame ()
  ((objects :initarg :objects :accessor rhombs-frame-objects))
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

(define-rhombs-frame-command (com-step :name "Step" :menu t) ()
  (with-accessors ((objects rhombs-frame-objects)) *application-frame*
    (setf objects
          (loop for obj in objects
                append (p2-step obj)))))

(define-rhombs-frame-command (com-reset :name "Reset" :menu t) ()
  (setf (rhombs-frame-objects *application-frame*)
        (list ;;(make-kite/2 (make-point 0 0) (make-point 1024 0) :right)
              (make-dart/2 (make-point 0 0) (make-point 1024 0) :left)
              )))

(defun start ()
  (find-application-frame 'rhombs-frame
                          :objects (list ;;(make-kite/2 (make-point 0 0) (make-point 1024 0))
                                         (make-dart/2 (make-point 0 0) (make-point 1024 0) :left)
                                         )))



;; (bt:make-thread #'start)

;;(defmethod run-frame-top-level :before ((frame rhombs-frame) &key &allow-other-keys) t)
;;(remove-method #'run-frame-top-level (find-method #'run-frame-top-level '(:before) (list (find-class 'rhombs-frame))))
