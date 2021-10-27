;; package.lisp
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

(in-package :cl-user)
(defpackage :cl-h3.test
  (:use :cl
        :fiveam
        :alexandria
        :cl-h3))

(in-package :cl-h3.test)

(def-suite :cl-h3)
(in-suite :cl-h3)

(defun fnear (a b &optional (eps 0.0001))
  (< (abs (- b a)) eps))

(test distance-test
  (let ((cell1 #16r8f2830828052d25)
        (cell2 #16r8f283082a30e623))
  (is (= 2340
         (h3:grid-distance cell1
                              cell2)))
  (multiple-value-bind (lat1 lng1)
      (h3:cell-to-lat-lng-degrees cell1)
    (is (fnear 37.77523588 lat1))
    (is (fnear -122.419755 lng1))

    (multiple-value-bind (lat2 lng2)
        (h3:cell-to-lat-lng-degrees cell2)

      (is (fnear lat2 37.7899905289064))
      (is (fnear lng2 -122.40212077170318))
      (format t "~%lat1 ~a~% lng1 ~a~%" lat1 lng1)
      (format t "lat2 ~a~% lng2 ~a~%" lat2 lng2)
      (let ((hav-diff (h3:haversine-distance
                                (deg2rad lat1)
                                (deg2rad lng1)
                                (deg2rad lat2)
                                (deg2rad lng2))))
      (format t "havdiff ~a~%" hav-diff)
      (is (fnear
           2.256853
           hav-diff)
           ))))))

(test edge-test
  (let* ((origin      #16r8a2a1072b59ffff)
         (destination #16r8a2a1072b597fff)

         (edge (h3:cells-to-directed-edge origin destination))
         (pts (h3:directed-edge-to-boundary edge))
         (expected '((40.69005860095358d0 . -74.04415176176158d0)
                    (40.68990769452519d0 . -74.0450617923963d0))))
    (loop
      for pt in pts
      do
         (let ((lat-found (find (car pt) expected
                                :key (compose #'deg2rad #'car)
                                :test #'fnear))
               (lng-found (find (cdr pt) expected
                                :key (compose #'deg2rad #'cdr)
                                :test #'fnear)))
           (is (not (null lat-found)))
           (is (not (null lng-found)))))))

(test neighbors-tets
  (let* ((indexed #16R8a2a1072b59ffff)
         (k 2)
         (max-neighbors (h3:max-grid-disk-size k))
         (neighbors (h3:grid-disk indexed k)))
    (format t "Max neighbors size ~a: ~a~%" k max-neighbors)
    (format t "Real neighbors size ~a~%" (length neighbors))
    (format t "Neighbors to ~x:~%" indexed)
    (loop for neigh in neighbors do
      (format t "    ~x distance ~a~%" neigh (h3:grid-distance indexed neigh)))))
