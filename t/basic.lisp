(in-package :cl-h3.test)


(def-suite :h3-basic)
(in-suite :h3-basic)

(test test1
  (let* ((lat (h3:degs-to-rads 37.7752702151959d0))
         (lng (h3:degs-to-rads -122.418307270836d0))
         (res 9)
         (expected #16r8928308280fffff))
    (is (= (h3:lat-lng-to-cell lat lng res)
           expected))))

(test test-grid-disk
  (let ((expected (sort
                   (list #16r89283082873ffff
                         #16r89283082877ffff
                         #16r8928308283bffff
                         #16r89283082807ffff
                         #16r8928308280bffff
                         #16r8928308280fffff
                         #16r89283082803ffff)
                   #'<))
        (actual (sort
                 (h3:grid-disk #16r8928308280fffff 1)
                 #'<)))
    (is (= (length expected) (length actual)))
    (is (equal expected actual))))

(test compact
  (let* ((cell 617700169958293503)
         (children (h3:cell-to-children cell))
         (compacted (h3:compact-cells children)))
    (is (= (length compacted) 1))
    (is (= (first compacted) cell))))

(test test-get-faces
  (let* ((hex 577832942814887935)
         (expected '(2 3 7 8 12))
         (actual (sort (h3:get-icosahedron-faces hex) #'<)))
    (is (= (length expected) (length actual)))
    (is (equal expected actual)))

  (let* ((hex 579873636396040191)
         (expected '(13))
         (actual (sort (h3:get-icosahedron-faces hex) #'<)))
    (is (= (length expected) (length actual)))
    (is (equal expected actual)))

  (let* ((hex 579768083279773695)
         (expected '(15 16))
         (actual (sort (h3:get-icosahedron-faces hex) #'<)))
    (is (= (length expected) (length actual)))
    (is (equal expected actual))))
