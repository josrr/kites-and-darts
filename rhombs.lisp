;;;; rhombs.lisp

(in-package #:rhombs)

(define-application-frame rhombs-frame ()
  ()
  (:panes (canvas (make-pane 'application-pane
                             :background +black+
                             ))
          (interactor :interactor))
  (:layouts (default
             (vertically ()
               (7/8 canvas)
               (1/8 interactor))))
  (:menu-bar t))

(defun start ()
  (find-application-frame 'rhombs-frame))

;; (bt:make-thread #'start)
