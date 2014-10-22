(in-package :cl-user)
(defpackage perlin
  (:use :cl)
  (:export #:perlin2d
           #:*seed*  
           #:perlin2d-grid  ))
(in-package :perlin)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defparameter *seed* 0)
(declaim (fixnum *seed*))

;; table of 256 randomly generated noise values between 0 and 255
(defparameter *noise-hash*
  #(208 34 231 213 32 248 233 56 161 78 24 140 71 48 140 254 245 255 247 247 40
    185 248 251 245 28 124 204 204 76 36 1 107 28 234 163 202 224 245 128 167 204
    9 92 217 54 239 174 173 102 193 189 190 121 100 108 167 44 43 77 180 204 8 81
    70 223 11 38 24 254 210 210 177 32 81 195 243 125 8 169 112 32 97 53 195 13
    203 9 47 104 125 117 114 124 165 203 181 235 193 206 70 180 174 0 167 181 41
    164 30 116 127 198 245 146 87 224 149 206 57 4 192 210 65 210 129 240 178 105
    228 108 245 148 140 40 35 195 38 58 65 207 215 253 65 85 208 76 62 3 237 55 89
    232 50 217 64 244 157 199 121 252 90 17 212 203 149 152 140 187 234 177 73 174
    193 100 192 143 97 53 145 135 19 103 13 90 135 151 199 91 239 247 33 39 145
    101 120 99 3 186 86 99 41 237 203 111 79 220 135 158 42 30 154 120 67 87 167
    135 176 183 191 253 115 184 21 233 58 129 233 142 39 128 211 118 137 139 255
    114 20 218 113 154 27 127 246 250 1 8 198 250 209 92 222 173 21 88 102 219))
(declaim ((simple-vector 256) *noise-hash*))

;; A hash that returns a unique value based on the *noise-hash* and
;; the *seed* value for each x and y.
(declaim (inline noise2))
(defun noise2 (x y)
  (declare (fixnum x y))
  (let ((tmp (svref *noise-hash* (mod (+ y *seed*) 256))))
    (declare (fixnum tmp))
    (svref *noise-hash* (mod (+ tmp x) 256))))

;; linear interpolation
(declaim (inline lin-inter))
(defun lin-inter (x y s)
  (declare (single-float x y s))
  (the single-float (+ x (* s (- (float y) (float x))))))

;; smoother interpolation
(declaim (inline smooth-inter))
(defun smooth-inter (x y s)
  (declare (single-float x y s))
  (the single-float (lin-inter x y (* s s (- 3 (* 2 s))))))

;; An "interpolated" noise based on the four integer points
;; surrounding the the desired 2d point.
(defun noise2d (x y)
  (declare (single-float x y))
  (multiple-value-bind (x0 x0frac) (truncate x)
    (declare (fixnum x0))
    (multiple-value-bind (y0 y0frac) (truncate y)
      (declare (fixnum y0))
      (let ((x1 (1+ x0))
            (y1 (1+ y0)))
        (let ((a (noise2 x0 y0))
              (b (noise2 x1 y0))
              (c (noise2 x0 y1))
              (d (noise2 x1 y1)))
          (declare (fixnum a b c d))
          (let ((low (smooth-inter (float a) (float b) x0frac))
                (high (smooth-inter (float c) (float d) x0frac)))
            (the single-float (smooth-inter low high y0frac))))))))

;; Repeatedly calls noise2d with attenuating amplitudes and
;; frequencies based on the depth.
(defun perlin2d (x y freq depth)
  (declare ((or single-float fixnum) x y freq)
           (fixnum depth))
  (let ((xn (float (* x freq)))
        (yn (float (* y freq)))
        (final 0.0)
        (amp 1.0)
        (div 0.0))
    (declare (single-float xn yn final amp div))
    (loop for d below depth do
         (let ((val (noise2d xn yn)))
           (declare (single-float val))
           (incf div (* 256 amp))
           (incf final (* amp val))
           (setf xn (* 2 xn)
                 yn (* 2 yn)
                 amp (/ amp 2))))
    (/ final div)))

(defun perlin2d-grid (width height freq depth)
  (declare (fixnum height width))
  (let ((res (make-array (list height width))))
    (loop for i below height do
         (loop for j below width do
              (setf (aref res i j)
                    (perlin2d j i freq depth))))
    res))

;; testing
;
;(defun float-tol= (f1 f2 tol)
;  (<= (abs (- f1 f2)) tol))
;
;(defun grid= (g1 g2)
;  (and (= (array-dimension g1 0) (array-dimension g2 0))
;       (= (array-dimension g1 1) (array-dimension g2 1))
;       (block test
;         (loop for i below (array-dimension g1 0) do
;              (loop for j below (array-dimension g1 1) do
;                   (if (not (float-tol= (aref g1 i j) (aref g2 i j) 1e-6))
;                       (return-from test nil))))
;         t)))
;
;(assert (grid= (perlin2d-grid 10 10 0.1 4)
;               #2A((0.1640625 0.1570625 0.16926248 0.17707917 0.19853958 0.20390625 0.21207291 0.24511458 0.2802354 0.2910625)
;                   (0.2760625 0.252159 0.24234432 0.24275613 0.26394892 0.2781271 0.25979844 0.2861497 0.32293785 0.3433058)
;                   (0.39608753 0.3766293 0.34910646 0.34282663 0.3544947 0.3345292 0.31827244 0.3632239 0.39757347 0.412352)
;                   (0.49072918 0.48198682 0.44704172 0.4316448 0.43432924 0.40695837 0.39906517 0.44262257 0.4631173 0.45987865)
;                   (0.55717295 0.5607182 0.5356763 0.51018393 0.49363282 0.48166665 0.49415284 0.530718 0.5306078 0.49285027)
;                   (0.6205729 0.5955104 0.6086042 0.59054166 0.55008334 0.5390625 0.5568917 0.5922146 0.5844021 0.52926874)
;                   (0.68532294 0.6567571 0.6645566 0.6434223 0.59820837 0.54338336 0.5626713 0.5870797 0.5960794 0.5874974)
;                   (0.6919229 0.70569813 0.7004099 0.6657957 0.6199037 0.6099167 0.59334207 0.60148424 0.5972078 0.58702916)
;                   (0.6965104 0.71042484 0.700246 0.67135227 0.6438424 0.6595375 0.6326418 0.6176882 0.6002578 0.58889407)
;                   (0.7032729 0.6750662 0.65730596 0.65157986 0.6757527 0.69942915 0.67761785 0.6222717 0.5984794 0.6083035))))
;
;; testing to do exactly what C version is doing
;;(time (let (v)
;;        (loop for i below 4000 do
;;             (loop for j below 4000 do
;;                  (setf v (perlin2d j i 0.1 4))))
;;        v))
