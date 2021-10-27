;; cl-h3.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(defpackage :clh3i
  (:nicknames)
  (:use #:cl #:j-utils #:alexandria)
  (:export)
  )
(in-package :clh3i)

(cffi:define-foreign-library h3-lib
    (:darwin (:or "libh3.dylib" "libh3"))
    (:unix (:or "libh3.so" "libh3.so.1" "libh3" "h3"))
    (t (:default "libh3")))
(cffi:use-foreign-library h3-lib)

(autowrap:c-include
 #+darwin"/usr/local/include/h3/h3api.h"
 #+linux"h3/h3api.h"
 :sysincludes (list #+linux"/usr/include/x86_64-linux-gnu/"
                    #+linux"/usr/include/x86_64-linux-gnu/c++/9/"
                    #+darwin"/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/")
 :spec-path '(cl-h3 specs)

 :trace-c2ffi t

 :exclude-definitions ("^va_list$" "Random" "Signal" "long-double"
                                   "^acos$" "^asin$" "^atan$" "^cos$" "^sin$" "^tan$"
                                   "^log$" "^exp$" "^acosh$" "^cosh$" "^asinh$" "^sinh$"
                                   "^tanh$" "^atanh$"  "^sqrt$" "^floor$" "^round$"
                                   "^time$" "^close$" "^open$" "^read$" "^write$"
                                   "^sleep$" "^truncate$" "^ceil$"
                                   "^abs$" "^abort$" "^random$" "^remove$" "^signal$")
 ;; :symbol-regex (("^GDAL_(.*)$" () "\\1")
 ;;                ("^Gdal(.*)$" () "\\1")
 ;;                ("^gdal(.*)$" () "\\1")
 ;;                ("^(OGR.*)$" () "\\1")
 ;;                ("^(Ogr.*)$" () "\\1")
 ;;                ("^(ogr.*)$" () "\\1"))
 )
(in-package :cl-h3)

(declaim (inline grid-distance cell-to-lat-lng cell-to-lat-lng-degrees
                 cells-to-directed-edge
                 rad2deg deg2rad))
(defun rad2deg (rad)
  (clh3i:rads-to-degs rad))

(defun deg2rad (deg)
  (clh3i:degs-to-rads deg))

(declaim (inline clh3i:grid-distance))
(defun grid-distance (a b)
  (autowrap:with-many-alloc ((dist 'clh3i:uint64-t))
    (clh3i:grid-distance a b dist)
    (cffi:mem-ref dist :uint64)))

(defun cell-to-lat-lng (cell)
  (autowrap:with-many-alloc ((geo 'clh3i:lat-lng))
    (clh3i:cell-to-lat-lng cell geo)
    (values (clh3i:lat-lng.lat geo)
            (clh3i:lat-lng.lng geo))))

(declaim (inline clh3i:cell-to-lat-lng clh3i:rads-to-degs))
(defun cell-to-lat-lng-degrees (cell)
  (autowrap:with-many-alloc ((geo 'clh3i:lat-lng))
    (clh3i:cell-to-lat-lng cell geo)
    (values (clh3i:rads-to-degs (clh3i:lat-lng.lat geo))
            (clh3i:rads-to-degs (clh3i:lat-lng.lng geo)))))

(declaim (inline clh3i:cells-to-directed-edge))
(defun cells-to-directed-edge (a b)
  (clh3i:cells-to-directed-edge a b))

(declaim (inline clh3i:directed-edge-to-boundary
                 clh3i:cell-boundary.num-verts
                 clh3i:cell-boundary.verts[]
                 clh3i:lat-lng.lat
                 clh3i:lat-lng.lng))
(defun directed-edge-to-boundary (edge)
  (autowrap:with-many-alloc ((cell-bound 'clh3i:cell-boundary))
    (clh3i:directed-edge-to-boundary edge cell-bound)
    (loop
      with num-verts = (clh3i:cell-boundary.num-verts cell-bound)
      for i below num-verts
      ;; TODO: Learn why this is (* 2 i)
      for ll = (clh3i:cell-boundary.verts[] cell-bound (* 2 i))
      collecting (cons (clh3i:lat-lng.lat ll)
                       (clh3i:lat-lng.lng ll)))))

(defun haversine-distance (th1 ph1 th2 ph2)
  "Find the great circle distance between two points on a sphere.
Parameters are latitude and longitude of the first and second points, in radians.
Returns the great-circle distance in kilometers."
  (let* ((earth-radius 6371.0088)
         (phn (- ph1 ph2))
         (dz (- (sin th1) (sin th2)))
         (dx (- (* (cos phn) (cos th1)) (cos th2)))
         (dy (* (sin phn) (cos th1))))
    (* 2 earth-radius
       (asin (/ (sqrt (+ (* dx dx)
                         (* dy dy)
                         (* dz dz)))
                2)))))


(defun max-grid-disk-size (k)
  (clh3i::max-grid-disk-size k))

(defun grid-disk (index k)
  (let ((max-neighbors (max-grid-disk-size k)))
    (autowrap:with-alloc (neighbors 'clh3i:h3index max-neighbors)
      (loop for i below max-neighbors do
        (setf (cffi:mem-ref neighbors :uint64 (* (cffi:foreign-type-size :uint64) i)) 0))
      (clh3i::grid-disk index k neighbors)
      (loop
        for i upto max-neighbors
        for neigh = (cffi:mem-ref neighbors :uint64 (* (cffi:foreign-type-size :uint64) i))
        when (not (zerop neigh))
          collect neigh))))
