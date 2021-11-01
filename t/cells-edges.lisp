(in-package :cl-h3.test)

(def-suite :h3-cells-edges)
(in-suite :h3-cells-edges)

(test test1
  (let* ((lat (h3:degs-to-rads 37.7752702151959d0))
         (lng (h3:degs-to-rads -122.418307270836d0))
         (res 9)
         (expected #16r8928308280fffff))
    (is (= (h3:lat-lng-to-cell lat lng res)
           expected))))

(test test2
  (is (equal
       (h3:lat-lng (h3:degs-to-rads 37.77670234943567d0)
                   (h3:degs-to-rads -122.41845932318311d0))
       (h3:cell-to-lat-lng #16r8928308280fffff))))


(test test3
  (let ((expected '((37.775197782893386d0 . -122.41719971841658d0)
                    (37.77688044840226d0 . -122.41612835779264d0)
                    (37.778385004930925d0 . -122.4173879761762d0)
                    (37.77820687262238d0 . -122.41971895414807d0)
                    (37.77652420699321d0 . -122.42079024541877d0)
                    (37.775019673792606d0 . -122.4195306280734d0)))
        (actual (h3:cell-to-boundary #16r8928308280fffff)))
    (loop
      for a in expected
      for b in actual
      do
         (is (fnear (h3:lat a) (h3:rads-to-degs (h3:lat b))))
         (is (fnear (h3:lng a) (h3:rads-to-degs (h3:lng b)))))))

