(in-package :cl-h3.test)

(def-suite :h3-h3)
(in-suite :h3-h3)

(test is-valid
  (is (h3:is-valid-cell #16r85283473fffffff))
  (is (h3:is-valid-cell #16r850dab63fffffff))
  (is (h3:is-valid-cell #16r5004295803a88))
  (loop for res below 16 do
    (is (h3:is-valid-cell
         (h3:lat-lng-to-cell 37d0 -122d0 res)))))

(test geo-to-h3
  (is (=
       (h3:lat-lng-to-cell (h3:degs-to-rads 37.3615593d0)
                           (h3:degs-to-rads -122.0553238d0)
                           5)
       #16r85283473fffffff)))

(test get-resolution
  (loop
    for res below 16
    for cell = (h3:lat-lng-to-cell (h3:degs-to-rads 37.3615593d0)
                                   (h3:degs-to-rads -122.0553238d0)
                                   res)
    do
       (is (= (h3:get-resolution cell)
              res))))

(test silly-geo-to-h3
  (let* ((lat (h3:degs-to-rads 37.3615593d0))
         (lng (h3:degs-to-rads -122.0553238d0))
         (lat-plus-pi (+ pi lat))
         (lng-plus-2pi (+ pi pi lng))
         (expected-1 #16r85283473fffffff)
         (expected-2 #16r85ca2d53fffffff))
    (is (= (h3:lat-lng-to-cell lat lng 5)
           expected-1))
    (is (= (h3:lat-lng-to-cell lat-plus-pi lng-plus-2pi 5)
           expected-2))))

(test h3-to-geo
  (let ((lat (h3:degs-to-rads 37.34579337536848d0))
        (lng (h3:degs-to-rads -121.97637597255124d0))
        (res (h3:cell-to-lat-lng #16r85283473fffffff)))
    (is (fnear lat (h3:lat res)))
    (is (fnear lng (h3:lng res)))))

(test h3-to-geo-boundary
  (let ((actual (h3:cell-to-boundary #16r85283473fffffff))
        (expected (mapcar #'h3:d2r
                          '((37.271355866731895d0 . -121.91508032705622d0)
                            (37.353926450852256d0 . -121.86222328902491d0)
                            (37.42834118609435d0 . -121.9235499963016d0)
                            (37.42012867767778d0 . -122.0377349642703d0)
                            (37.33755608435298d0 . -122.09042892904395d0)
                            (37.26319797461824d0 . -122.02910130919d0)))))
    (is (= (length expected) (length actual)))
    (loop
      for act in actual
      for exp in expected
      do
         (is (fnear exp act)))))

(test k-ring-1
  (let* ((hex #16r8928308280fffff)
         (actual (sort (h3:grid-disk hex 1) #'<)))
    (is (= 7 (length actual)))
    (is (equal actual
               (sort (list #16r8928308280bffff
                 #16r89283082807ffff
                 #16r89283082877ffff
                 #16r8928308280fffff
                 #16r89283082803ffff
                 #16r89283082873ffff
                 #16r8928308283bffff)
                     #'<)))))

(test k-ring-2
  (let* ((hex #16r8928308280fffff)
         (actual (sort (h3:grid-disk hex 2) #'<)))
    (is (= (+ 1 6 12) (length actual)))
    (is (equal
         (sort (list
                #16r89283082813ffff
                #16r89283082817ffff
                #16r8928308281bffff
                #16r89283082863ffff
                #16r89283082823ffff
                #16r89283082873ffff
                #16r89283082877ffff
                #16r8928308280fffff
                #16r8928308287bffff
                #16r89283082833ffff
                #16r8928308282bffff
                #16r8928308283bffff
                #16r89283082857ffff
                #16r892830828abffff
                #16r89283082847ffff
                #16r89283082867ffff
                #16r89283082803ffff
                #16r89283082807ffff
                #16r8928308280bffff
                )
               #'<)
         actual))))

(test k-ring-pentagon
  (let* ((hex #16r821c07fffffffff)
         (actual (sort (h3:grid-disk hex 1) #'<)))
    (is (= (+ 1 5) (length actual)))
    (is (equal
         (sort (list
                #16r821c2ffffffffff
                #16r821c27fffffffff
                #16r821c07fffffffff
                #16r821c17fffffffff
                #16r821c1ffffffffff
                #16r821c37fffffffff
                )
               #'<)
         actual))))

(test k-ring-distance
  (let* ((hex #16r8928308280fffff)
         (actual (sort (h3:grid-disk-distances hex 1) #'< :key #'car)))
    (is (= (+ 1 6) (length actual)))
    (is (equal
         (sort (list
                '(#16r8928308280fffff . 0)
                '(#16r8928308280bffff . 1)
                '(#16r89283082807ffff . 1)
                '(#16r89283082877ffff . 1)
                '(#16r89283082803ffff . 1)
                '(#16r89283082873ffff . 1)
                '(#16r8928308283bffff . 1))
               #'<
               :key #'car)
         actual))))

(test polyfill
  (is (> (length (h3:polygon-to-cells
                  (mapcar #'h3:d2r
                          '(
                            (37.8133189999832380d0 . -122.4089866999972145d0)
                            (37.7866302000007224d0 . -122.3805436999997056d0)
                            (37.7198061999978478d0 . -122.3544736999993603d0)
                            (37.7076131999975672d0 . -122.5123436999983966d0)
                            (37.7835871999971715d0 . -122.5247187000021967d0)
                            (37.8151571999998453d0 . -122.4798767000009000d0)
                            ))
                  9))
         1000)))

(test polyfill-with-hole
      (is (> (length
              (h3:polygon-to-cells
               (mapcar #'h3:d2r
                       '((37.813318999983238 . -122.4089866999972145)
                         (37.7866302000007224 . -122.3805436999997056)
                         (37.7198061999978478 . -122.3544736999993603)
                         (37.7076131999975672 . -122.5123436999983966)
                         (37.7835871999971715 . -122.5247187000021967)
                         (37.8151571999998453 . -122.4798767000009008)))
               9
               (mapcar #'h3:d2r'((37.7869802 . -122.4471197)
                                 (37.7664102 . -122.4590777)
                                 (37.7710682 . -122.4137097)))))
             1000)))
              
