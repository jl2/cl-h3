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

(defun fnear (a b &optional (eps 0.0001))
  (< (abs (- b a)) eps))

(def-suite :h3-old-tests)
(in-suite :h3-old-tests)

(test distance-test
  (let* ((cell1 #16r8f2830828052d25)
         (cell2 #16r8f283082a30e623)
         (ll1 (h3:cell-to-lat-lng-degrees cell1))
         (ll2 (h3:cell-to-lat-lng-degrees cell2))

         (lat1 (car ll1))
         (lng1 (cdr ll1))
         (lat2 (car ll2))
         (lng2 (cdr ll2))

         (grid-dist (h3:grid-distance cell1
                                      cell2))
         (hav-diff (h3:haversine-distance
                    (degs-to-rads lat1)
                    (degs-to-rads lng1)
                    (degs-to-rads lat2)
                    (degs-to-rads lng2))))

    (is (= 2340 grid-dist))

    (is (fnear 37.77523588d0 lat1))
    (is (fnear -122.419755d0 lng1))

    (is (fnear lat2 37.7899905289064d0))
    (is (fnear lng2 -122.40212077170318d0))
    (is (fnear 2.256853 hav-diff))
    ))

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
                                :key (compose #'degs-to-rads #'car)
                                :test #'fnear))
               (lng-found (find (cdr pt) expected
                                :key (compose #'degs-to-rads #'cdr)
                                :test #'fnear)))
           (is (not (null lat-found)))
           (is (not (null lng-found)))))))

(test neighbors-test
  (let* ((indexed #16R8a2a1072b59ffff)
         (k 2)
         (max-neighbors (h3:max-grid-disk-size k))
         (neighbors (h3:grid-disk indexed k))
         (expected '(#16r8A2A1072B59FFFF
                     #16r8A2A1072B597FFF
                     #16r8A2A1070C96FFFF
                     #16r8A2A1072B4B7FFF
                     #16r8A2A1072B4A7FFF
                     #16r8A2A1072B58FFFF
                     #16r8A2A1072B587FFF
                     #16r8A2A1072B5B7FFF
                     #16r8A2A1072A2CFFFF
                     #16r8A2A1070C967FFF
                     #16r8A2A1070C947FFF
                     #16r8A2A1070C94FFFF
                     #16r8A2A1072B497FFF
                     #16r8A2A1072B487FFF
                     #16r8A2A1072B4AFFFF
                     #16r8A2A1072B417FFF
                     #16r8A2A1072B437FFF
                     #16r8A2A1072B5AFFFF
                     #16r8A2A1072B5A7FFF)))
    (loop for neigh in neighbors do
      (setf expected (remove neigh expected)))
    (is (null expected))))

(test index-test
  (let* ((lat (h3:degs-to-rads 40.689167d0))
         (lng (h3:degs-to-rads -74.044444d0))
         (resolution 10)
         (indexed (h3:lat-lng-to-cell lat lng resolution))
         (boundary (h3:cell-to-boundary indexed))
         (ll (h3:cell-to-lat-lng indexed)))
    (is (fnear (car ll) lat))
    (is (fnear (cdr ll) lng))))
