;;;; polynomials.lisp

(in-package #:kites-and-darts)

(deftype polynomial () 'simple-vector)
(proclaim '(inline degree coef poly make-poly))

(defun coef (p i)
  (svref p i))

(defun degree (p)
  (1- (length p)))

(defun poly (&rest coefs)
  (apply #'vector coefs))

(defun make-poly (degree)
  (make-array (+ degree 1) :initial-element 0))

(defsetf coef (p i) (val)
  `(setf (svref (the polynomial ,p) ,i) ,val))

(defun normalize-poly (p)
  "Alter a polynomial by dropping trailing zeros."
  (if (numberp p)
      p
      (let ((p-degree (position 0 p
                                :test (complement #'eql)
                                :from-end t)))
        (cond ((null p-degree) 0)
              ((<= p-degree 0) (normalize-poly (coef p 0)))
              ((< p-degree (degree p)) (delete 0 p :start p-degree))
              (t p)))))

(defun copy-poly (p)
  "Make a copy a polynomial."
  (copy-seq p))

(defun k+poly (k p)
  "Add a constant k to a polynomial p."
  (cond ((eql k 0) p)
        ((and (numberp k) (numberp p))
         (+ k p))
        (t (let ((r (copy-poly p)))
             (setf (coef r 0) (poly+poly (coef r 0) k))
             r))))

(defun poly+poly (p q)
  "Add a constant k to a polynomial p."
  (normalize-poly
   (cond
     ((numberp p) (k+poly p q))
     ((numberp q) (k+poly q p))
     (t (if (> (degree p) (degree q))
            (poly+poly q p)
            (loop with r = (copy-poly q)
                  for i from 0 to (degree p) do
                    (setf (coef r i) (poly+poly (coef r i) (coef p i)))
                  finally (return r)))))))

(defun k*poly (k p)
  "Multiply a polynomial p by a constant factor k."
  (cond
    ((eql k 0) 0)
    ((eql k 1) p)
    ((and (numberp k) (numberp p)) (* k p))
    (t (loop with r = (make-poly (degree p))
             for i from 0 to (degree p) do
               (setf (coef r i) (poly*poly k (coef p i)))
             finally (return r)))))

(defun poly*poly (p q)
  "Multiply two polynomials."
  (normalize-poly
   (cond
     ((numberp p) (k*poly p q))
     ((numberp q) (k*poly q p))
     (t (loop with r-degree = (+ (degree p) (degree q))
              with r = (make-poly r-degree)
              for i from 0 to (degree p)
              unless (eql (coef p i) 0) do
                (loop for j from 0 to (degree q) do
                  (setf (coef r (+ i j))
                        (poly+poly (coef r (+ i j))
                                   (poly*poly (coef p i)
                                              (coef q j)))))
              finally (return r))))))

(defun poly+ (&rest args)
  "Unary or binary polynomial addition."
  (ecase (length args)
    (1 (first args))
    (2 (poly+poly (first args) (second args)))))

(defun poly- (&rest args)
  "Unary or binary polynomial subtraction."
  (ecase (length args)
    (0 0)
    (1 (poly*poly -1 (first args)))
    (2 (poly+poly (first args) (poly*poly -1 (second args))))))

(defun poly-eval (poly argument)
  (declare (type (or polynomial integer) poly))
  (if (numberp poly)
      poly
      (loop for power from 0 to (degree poly)
            sum (* (coef poly power) (expt argument power)))))

(defparameter *p-t4* (poly -1 1 -1 1))

(defun poly-simplify (poly)
  (declare (type (or polynomial integer) poly) (optimize (speed 3)))
  (labels ((monomial (degree)
             (concatenate 'vector (make-array (- degree 4) :initial-element 0) #(1)))
           (gen-after (degree)
             (loop with p = 0
                   for d from 5 to degree do
                     (setf p (poly+poly p (poly*poly (monomial d)
                                                     (poly*poly (coef poly d) *p-t4*))))
                   finally (return p))))
    (if (or (numberp poly) (< (degree poly) 4))
        poly
        (poly-simplify (let* ((degree (degree poly))
                              (after (and (> degree 4) (gen-after degree)))
                              (sum (poly+poly (subseq poly 0 4)
                                              (poly*poly (coef poly 4) *p-t4*))))
                         (if after (poly+poly sum after) sum))))))
