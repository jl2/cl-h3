(in-package :cl-h3.test)


(def-suite :h3-basic)
(in-suite :h3-basic)

(test ll-to-int
  (let* ((lat (h3:degs-to-rads 37.7752702151959d0))
         (lng (h3:degs-to-rads -122.418307270836d0))
         (res 9)
         (cell-dec 617700169958293503)
         (cell-hex #16r8928308280fffff))
    (is (= (h3:lat-lng-to-cell lat lng res)
           cell-dec))
    (is (= (h3:lat-lng-to-cell lat lng res)
           cell-hex))))

;; (test edge-test
;;   (let* ((origin      #16r8a2a1072b59ffff)
;;          (destination #16r8a2a1072b597fff)

;;          (edge (h3:cells-to-directed-edge origin destination))
;;          (pts (h3:directed-edge-to-boundary edge))
;;          (expected '((40.69005860095358d0 . -74.04415176176158d0)
;;                     (40.68990769452519d0 . -74.0450617923963d0))))
;;     (loop
;;       for pt in pts
;;       do
;;          (let ((lat-found (find (car pt) expected
;;                                 :key (compose #'degs-to-rads #'car)
;;                                 :test #'fnear))
;;                (lng-found (find (cdr pt) expected
;;                                 :key (compose #'degs-to-rads #'cdr)
;;                                 :test #'fnear)))
;;            (is (not (null lat-found)))
;;            (is (not (null lng-found)))))))

;; (test neighbors-test
;;   (let* ((indexed #16R8a2a1072b59ffff)
;;          (k 2)
;;          (max-neighbors (h3:max-grid-disk-size k))
;;          (neighbors (h3:grid-disk indexed k))
;;          (expected '(#16r8A2A1072B59FFFF
;;                      #16r8A2A1072B597FFF
;;                      #16r8A2A1070C96FFFF
;;                      #16r8A2A1072B4B7FFF
;;                      #16r8A2A1072B4A7FFF
;;                      #16r8A2A1072B58FFFF
;;                      #16r8A2A1072B587FFF
;;                      #16r8A2A1072B5B7FFF
;;                      #16r8A2A1072A2CFFFF
;;                      #16r8A2A1070C967FFF
;;                      #16r8A2A1070C947FFF
;;                      #16r8A2A1070C94FFFF
;;                      #16r8A2A1072B497FFF
;;                      #16r8A2A1072B487FFF
;;                      #16r8A2A1072B4AFFFF
;;                      #16r8A2A1072B417FFF
;;                      #16r8A2A1072B437FFF
;;                      #16r8A2A1072B5AFFFF
;;                      #16r8A2A1072B5A7FFF)))
;;     (format t "Max neighbors size ~a: ~a~%" k max-neighbors)
;;     (format t "Real neighbors size ~a~%" (length neighbors))
;;     (format t "Neighbors to ~x:~%" indexed)
;;     (loop for neigh in neighbors do
;;       (setf expected (remove neigh expected))
;;       (format t "    ~x distance ~a~%" neigh (h3:grid-distance indexed neigh)))
;;     (is (null expected))))

;; (test index-test
;;   (let* ((lat (h3:degs-to-rads 40.689167d0))
;;          (lng (h3:degs-to-rads -74.044444d0))
;;          (resolution 10)
;;          (indexed (h3:lat-lng-to-cell lat lng resolution))
;;          (boundary (h3:cell-to-boundary indexed))
;;          (ll (h3:cell-to-lat-lng indexed)))
;;     (is (fnear (car ll) lat))
;;     (is (fnear (cdr ll) lng))
;;     (format t "~%cell: ~x~%boundary:" indexed)
;;     (loop
;;       for (lat . lng) in boundary
;;       do
;;          (format t "~a ~a~%" (h3:rads-to-degs lat) (h3:rads-to-degs lng)))
;;     (format t "Center ~a ~a~%" (h3:rads-to-degs (car ll)) (h3:rads-to-degs (cdr ll)))))


