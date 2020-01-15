;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file contains several experiments conducted with methods to
;; implement the Blum-Blum-Shub random number generator.
;; 
;; ---------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; 
;; Date: 2020-01-14
;; 
;; Sources:
;;   -> "https://en.wikipedia.org/wiki/Blum_Blum_Shub"
;;   -> "https://de.wikipedia.org/wiki/Blum-Blum-Shub-Generator"
;;   -> "https://github.com/OverStruck/blum-blum-shub-prbg/blob/master/bbs.cpp"
;;   -> "https://code.google.com/p/javarng/"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test 01: Simple sequence.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-blum-blum-shub-generator (&key p q seed)
  (let ((M (* p q)))
    (let ((x0 seed))
      #'(lambda ()
          (setf x0 (mod (* x0 x0) M))
          x0))))

;;; -------------------------------------------------------

(let ((random-generator (make-blum-blum-shub-generator :p 11 :q 19 :seed 3)))
  (loop repeat 7 do
    (print (funcall random-generator))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test 02: With "bitnum".                                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bitnum (bits)
  "Returns the number of 1-bits which comprimise the integer-encoded BITS."
  (let ((number-of-bits (integer-length bits)))
    (loop
      for  bit-index from 0 below number-of-bits
      when (logbitp bit-index bits)
      sum  1)))

;;; -------------------------------------------------------

;; Output is based upon the parity bit.
(defun make-blum-blum-shub-generator (&key p q seed)
  (let ((M (* p q)))
    (let ((x0 seed))
      #'(lambda ()
          (setf x0 (mod (* x0 x0) M))
          (list x0 (bitnum (mod x0 2)))))))

;;; -------------------------------------------------------

(let ((random-generator (make-blum-blum-shub-generator :p 7 :q 11 :seed 64)))
  (loop repeat 7 do
    (print (funcall random-generator))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test 03: With least significant bit.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Output is based upon the least significant bit.
(defun make-blum-blum-shub-generator (&key p q seed)
  (let ((M (* p q)))
    (let ((x0 seed))
      #'(lambda ()
          (setf x0 (mod (* x0 x0) M))
          (list x0 (if (logbitp 0 x0) 1 0))))))

;;; -------------------------------------------------------

(let ((random-generator (make-blum-blum-shub-generator :p 11 :q 19 :seed 3)))
  (loop repeat 7 do
    (print (funcall random-generator))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test 04: With fungible output bits generator.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun count-one-bits (bits)
  "Returns the number of bits in the integer-encoded BITS that equal 1."
  (declare (integer bits))
  (loop for   bit-index from 0 below (integer-length bits)
        count (logbitp bit-index bits)))

;;; -------------------------------------------------------

(defparameter +EVEN-PARITY-BIT+
  #'(lambda (x[n+1])
      (declare (integer x[n+1]))
      (mod (count-one-bits x[n+1]) 2))
  "Returns 0 if x[n+1] contains an even number of 1-bits, otherwise 0.")

;;; -------------------------------------------------------

(defparameter +ODD-PARITY-BIT+
  #'(lambda (x[n+1])
      (declare (integer x[n+1]))
      (if (evenp (count-one-bits x[n+1])) 1 0))
  "Returns 0 if x[n+1] contains an odd number of 1-bits, otherwise 1.")

;;; -------------------------------------------------------

(defparameter +LEAST-SIGNIFICANT-BIT+
  #'(lambda (x[n+1])
      (declare (integer x[n+1]))
      (if (logbitp 0 x[n+1]) 1 0))
  "Extracts the least significant bits from x[n+1].")

;;; -------------------------------------------------------

(declaim (ftype (function (integer) integer) output-generator))

(defun make-blum-blum-shub-generator (&key p q seed
                             (output-generator +LEAST-SIGNIFICANT-BIT+))
  "Creates and returns a nullary function which represents a simple
   Blum-Blum-Shub pseudorandom number generator with the parameters
   P, Q, and SEED, returning a list of two elements:
     (1) the bits selected from the generated number x[n+1] by providing
         it to the OUTPUT-GENERATOR and obtaining its result,
     (2) the generated number x[n+1] itself.
   ---
   While remaining unchecked, P and Q should be two large primes.
   SEED should be an integer that is co-prime to M = P * Q, and should
   not be one or zero.
   ---
   The actual output, a sequence of one or more bits, might be changed
   through the OUTPUT-GENERATOR, which constitutes a function accepting
   the generated element x[n+1] and returns the output bits, adhering
   to the signature
     lambda(x[n+1]) => integer-encoded-random-bits
   ---
   Examplary usage:
     (let ((random-generator (make-blum-blum-shub-generator :p 11 :q 19
                                                            :seed 3)))
       (loop repeat 7 do
         (print (funcall random-generator))))"
  (declare (integer p q seed))
  (let ((M (* p q)))
    (declare (integer M))
    (let ((x0 seed))
      (declare (integer x0))
      #'(lambda ()
          (setf x0 (mod (* x0 x0) M))
          (let ((output-bits (funcall output-generator x0)))
            (list x0 output-bits))))))

;;; -------------------------------------------------------

(let ((random-generator (make-blum-blum-shub-generator
                          :p 11 :q 19 :seed 3
                          :output-generator +LEAST-SIGNIFICANT-BIT+)))
  (loop repeat 7 do
    (print (funcall random-generator))))

;;; -------------------------------------------------------

(let ((random-generator (make-blum-blum-shub-generator
                          :p 11 :q 19 :seed 3
                          :output-generator +EVEN-PARITY-BIT+)))
  (loop repeat 7 do
    (print (funcall random-generator))))

;;; -------------------------------------------------------

(let ((random-generator (make-blum-blum-shub-generator
                          :p 11 :q 19 :seed 3
                          :output-generator +ODD-PARITY-BIT+)))
  (loop repeat 7 do
    (print (funcall random-generator))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test 05: Object-oriented design.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Blum-Blum-Shub-Random-Generator
  (:constructor make-blum-blum-shub-random-generator (&key p q (M (* p q)) seed (x0 seed))))
  (p    0 :type integer)
  (q    0 :type integer)
  (M    0 :type integer)
  (seed 0 :type integer)
  (x0   0 :type integer))

;;; -------------------------------------------------------

;; Output is based upon the least significant bit.
(defun get-next-random-number (blum-blum-shub-generator)
  (with-slots (x0 M) blum-blum-shub-generator
    (setf x0 (mod (* x0 x0) M))
    (let ((output (ldb (byte 1 0) x0)))
      (list output x0))))

;;; -------------------------------------------------------

(let ((random-generator (make-blum-blum-shub-random-generator :p 11 :q 19 :seed 3)))
  (loop repeat 7 do
    (print (get-next-random-number random-generator))))




