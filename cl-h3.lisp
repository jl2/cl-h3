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


(in-package :cl-h3)

(setf (symbol-function 'are-neighbor-cells) (symbol-function 'clh3i::are-neighbor-cells))

(setf (symbol-function 'cell-area-m2) (symbol-function 'clh3i::cell-area-m2))
(setf (symbol-function 'cell-area-km2) (symbol-function 'clh3i::cell-area-km2))
(setf (symbol-function 'cell-area-rads2) (symbol-function 'clh3i::cell-area-rads2))

(defun cell-to-boundary (index)
  (autowrap:with-many-alloc ((cell-bound 'clh3i:cell-boundary))
    (clh3i:cell-to-boundary index cell-bound)
    (loop
      with num-verts = (clh3i:cell-boundary.num-verts cell-bound)
      for i below num-verts
      ;; TODO: Learn why this is (* 2 i)
      for ll = (clh3i:cell-boundary.verts[] cell-bound (* 2 i))
      collecting (cons (clh3i:lat-lng.lat ll)
                       (clh3i:lat-lng.lng ll)))))

(setf (symbol-function 'cell-to-center-child)
      (symbol-function 'clh3i::cell-to-center-child))

(defun cell-to-children (index res)
  (autowrap:with-many-alloc ((child-count 'clh3i::uint64-t))
    (clh3i::cell-to-children-size index res child-count)
    (let ((child-count (cffi:mem-ref child-count :uint64)))
      (autowrap:with-many-alloc ((children 'clh3i::uint64-t child-count))
        (clh3i:cell-to-children index res children)
        (loop
          for i below child-count
          collecting (cffi:mem-ref children :uint64 (* (cffi:foreign-type-size :uint64) i)))))))

(defun cell-to-lat-lng (cell)
  (autowrap:with-many-alloc ((geo 'clh3i:lat-lng))
    (clh3i:cell-to-lat-lng cell geo)
    (cons (clh3i:lat-lng.lat geo)
          (clh3i:lat-lng.lng geo))))

(defun cell-to-lat-lng-degrees (cell)
  (autowrap:with-many-alloc ((geo 'clh3i::lat-lng))
    (clh3i:cell-to-lat-lng cell geo)
    (cons (clh3i:rads-to-degs (clh3i:lat-lng.lat geo))
          (clh3i:rads-to-degs (clh3i:lat-lng.lng geo)))))

(defun cell-to-parent (cell parent-res)
  (autowrap:with-many-alloc ((parent 'clh3i::uint64-t))
    (clh3i:cell-to-parent cell parent-res parent)
    (cffi:mem-ref parent :uint64)))

(defun cell-to-vertex (cell vertex-num)
  (autowrap:with-many-alloc ((vertex 'clh3i::uint64-t))
    (clh3i:cell-to-vertex cell vertex-num vertex)
    (cffi:mem-ref vertex :uint64)))

(defun cell-to-vertexes (cell)
  (autowrap:with-many-alloc ((vertexes 'clh3i::uint64-t 6))
    (clh3i:cell-to-vertexes cell vertexes)
    (loop
      for i below 6
          collecting (cffi:mem-ref vertexes :uint64
                                   (* (cffi:foreign-type-size :uint64) i)))))

(setf (symbol-function 'cells-to-directed-edge) (symbol-function 'clh3i::cells-to-directed-edge))

(defun compact-cells (cells)
  (let ((count (length cells)))
    (autowrap:with-many-alloc ((in-set 'clh3i::int64-t count)
                               (out-set 'clh3i::int64-t count))
      (loop
        for i below count
        for idx in cells
        for offset = (* (cffi:foreign-type-size :uint64) i)
        do
           (setf (cffi:mem-ref in-set :uint64 offset) idx)
           (setf (cffi:mem-ref out-set :uint64 offset) 0))

      (clh3i:compact-cells in-set out-set count)

      (loop
        for i below count
        for offset = (* (cffi:foreign-type-size :uint64) i)
        for val = (cffi:mem-ref in-set :uint64 offset)
        when (not (zerop val))
          collect val))))

(setf (symbol-function 'degs-to-rads) (symbol-function 'clh3i::degs-to-rads))

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

(defun directed-edge-to-cells (edge)
  (autowrap:with-many-alloc ((origin-destination 'clh3i:uint64-t 2))
    (clh3i:directed-edge-to-cells edge origin-destination)
    (cons (cffi:mem-ref origin-destination :uint64 0)
          (cffi:mem-ref origin-destination :uint64 (cffi:foreign-type-size :uint64)))))


(defun distance-m (lat1 lng1 lat2 lng2)
  (autowrap:with-many-alloc ((a 'clh3i::lat-lng)
                             (b 'clh3i::lat-lng))
    (setf (clh3i:lat-lng.lat a) lat1)
    (setf (clh3i:lat-lng.lng a) lng1)
    (setf (clh3i:lat-lng.lat b) lat2)
    (setf (clh3i:lat-lng.lng b) lng2)
    (clh3i:distance-m a b)))
(defun distance-km (lat1 lng1 lat2 lng2)
  (autowrap:with-many-alloc ((a 'clh3i::lat-lng)
                             (b 'clh3i::lat-lng))
    (setf (clh3i:lat-lng.lat a) lat1)
    (setf (clh3i:lat-lng.lng a) lng1)
    (setf (clh3i:lat-lng.lat b) lat2)
    (setf (clh3i:lat-lng.lng b) lng2)
    (clh3i:distance-km a b)))
(defun distance-rads (lat1 lng1 lat2 lng2)
  (autowrap:with-many-alloc ((a 'clh3i::lat-lng)
                             (b 'clh3i::lat-lng))
    (setf (clh3i:lat-lng.lat a) lat1)
    (setf (clh3i:lat-lng.lng a) lng1)
    (setf (clh3i:lat-lng.lat b) lat2)
    (setf (clh3i:lat-lng.lng b) lng2)
    (clh3i:distance-km a b)))

(setf (symbol-function 'exact-edge-length-m) (symbol-function 'clh3i::exact-edge-length-m))
(setf (symbol-function 'exact-edge-length-km) (symbol-function 'clh3i::exact-edge-length-km))
(setf (symbol-function 'exact-edge-length-rads) (symbol-function 'clh3i::exact-edge-length-rads))

(defun experimental-h3-to-local-ij (origin pt2)
  (autowrap:with-many-alloc ((res 'clh3i::coord-ij))
    (clh3i::experimental-h3to-local-ij origin pt2 res)
    (cons (clh3i::coord-ij.i res)
          (clh3i::coord-ij.j res))))

(defun experimental-local-ij-to-h3 (origin ci cj)
  (autowrap:with-many-alloc ((cij 'clh3i::coord-ij)
                             (res 'clh3i::h3index))
    (setf (clh3i::coord-ij.i cij) ci)
    (setf (clh3i::coord-ij.j cij) cj)
    (clh3i::experimental-local-ij-to-h3 origin cij res)
    (cffi:mem-ref res :uint64 0)
    ))

(setf (symbol-function 'get-base-cell-number) (symbol-function 'clh3i::get-base-cell-number))
(setf (symbol-function 'get-directed-edge-destination) (symbol-function 'clh3i::get-directed-edge-destination))
(setf (symbol-function 'get-directed-edge-origin) (symbol-function 'clh3i::get-directed-edge-origin))

(setf (symbol-function 'get-hexagon-area-avg-km2) (symbol-function 'clh3i::get-hexagon-area-avg-km2))
(setf (symbol-function 'get-hexagon-area-avg-m2) (symbol-function 'clh3i::get-hexagon-area-avg-m2))
(setf (symbol-function 'get-hexagon-edge-length-avg-km) (symbol-function 'clh3i::get-hexagon-edge-length-avg-km))
(setf (symbol-function 'get-hexagon-edge-length-avg-m) (symbol-function 'clh3i::get-hexagon-edge-length-avg-m))


(defun get-icosahedron-faces (edge)
  (autowrap:with-many-alloc ((face-count 'clh3i::uint64-t))
    (setf (cffi:mem-ref face-count :uint64 0) 0)
    (clh3i::max-face-count edge face-count)
    (let ((max-face-count (cffi:mem-ref face-count :uint64)))
      (autowrap:with-many-alloc ((faces 'clh3i::uint64-t max-face-count))
        (clh3i::get-icosahedron-faces edge faces)
        (loop for i below max-face-count
              for offset = (* (cffi:foreign-type-size :uint64) i)
              when (>= (cffi:mem-ref faces :uint64 offset) 0)
                collect (cffi:mem-ref faces :uint64 offset))))))
                
(setf (symbol-function 'get-num-cells) (symbol-function 'clh3i::get-num-cells))

(defun get-pentagon-count ()
  (clh3i::pentagon-count))

(defun get-pentagons (res)
  (autowrap:with-many-alloc ((pentagons 'clh3i::h3index (clh3i::pentagon-count)))
    (clh3i::get-pentagons res pentagons)
    (loop for i below (clh3i::pentagon-count)
          for offset = (* (cffi:foreign-type-size :uint64) i)
          collect (cffi:mem-ref pentagons :uint64 offset))))

(defun get-res-0-cell-count ()
  (clh3i::res0cell-count))

(defun get-res-0-cells ()
  (autowrap:with-many-alloc ((cells 'clh3i::h3index (clh3i::res0cell-count)))
    (clh3i:get-res0cells cells)
    (loop for i below (clh3i::res0cell-count)
          for offset = (* (cffi:foreign-type-size :uint64) i)
          collect (cffi:mem-ref cells :uint64 offset))))

(setf (symbol-function 'get-resolution) (symbol-function 'clh3i::get-resolution))

(setf (symbol-function 'max-grid-disk-size) (symbol-function 'clh3i::max-grid-disk-size))

(defun grid-disk (index k)
  (let ((max-neighbors (max-grid-disk-size k)))
    (autowrap:with-alloc (neighbors 'clh3i:h3index max-neighbors)
      (loop for i below max-neighbors do
        (setf (cffi:mem-ref neighbors :uint64 (* (cffi:foreign-type-size :uint64) i)) 0))
      (clh3i::grid-disk index k neighbors)
      (loop
        for i below max-neighbors
        for neigh = (cffi:mem-ref neighbors :uint64 (* (cffi:foreign-type-size :uint64) i))
        when (not (zerop neigh))
          collect neigh))))

(defun grid-disk-distances (origin k)
  (let ((max-neighbors (max-grid-disk-size k)))
    (autowrap:with-many-alloc ((neighbors 'clh3i:h3index max-neighbors)
                               (distances :int max-neighbors))
      (loop
        for i below max-neighbors
        for offset64 = (* (cffi:foreign-type-size :uint64) i)
        for offset32 = (* (cffi:foreign-type-size :int) i)
        do
           (setf (cffi:mem-ref neighbors :uint64 offset64) 0)
           (setf (cffi:mem-ref distances :int offset32) 0))
      (clh3i::grid-disk-distances origin k neighbors distances)
      (loop
        for i below max-neighbors
        for offset64 = (* (cffi:foreign-type-size :uint64) i)
        for offset32 = (* (cffi:foreign-type-size :int) i)
        for neigh = (cffi:mem-ref neighbors :uint64 offset64)
        for dist =  (cffi:mem-ref distances :int offset32)
        when (not (zerop neigh))
          collect (cons neigh dist)))))

(defun grid-distance (a b)
  (autowrap:with-many-alloc ((dist 'clh3i:uint64-t))
    (clh3i:grid-distance a b dist)
    (cffi:mem-ref dist :uint64)))

(defun grid-path-cells-size (start end)
  (autowrap:with-alloc (max-cells 'clh3i:int64-t)
    (clh3i::grid-path-cells-size start end max-cells)
    (cffi::mem-ref max-cells :int64 0)))
  
(defun grid-path-cells (start end)
  (let ((max-path-size (grid-path-cells-size start end)))
    (autowrap:with-alloc (cells 'clh3i:h3index max-path-size)
      (loop
        for i below max-path-size
        for offset = (* (cffi:foreign-type-size :uint64) i)
        for cell = (cffi:mem-ref cells :uint64 offset)
        when (not (zerop cell))
          collect cell))))

(defun grid-ring-unsafe (index k)
  (let ((max-cells (if (zerop k) 1 (* 6 k))))
    (autowrap:with-alloc (cells 'clh3i:h3index max-cells)
      (loop for i below max-cells do
        (setf (cffi:mem-ref cells :uint64 (* (cffi:foreign-type-size :uint64) i)) 0))
      (clh3i::grid-disk index k cells)
      (loop
        for i below max-cells
        for cell = (cffi:mem-ref cells :uint64 (* (cffi:foreign-type-size :uint64) i))
        when (not (zerop cell))
          collect cell))))

(setf (symbol-function 'is-pentagon) (symbol-function 'clh3i::is-pentagon))
(setf (symbol-function 'is-valid-cell) (symbol-function 'clh3i::is-valid-cell))
(setf (symbol-function 'is-res-class-iii) (symbol-function 'clh3i::is-res-class-iii))
(setf (symbol-function 'is-valid-directed-edge) (symbol-function 'clh3i::is-valid-directed-edge))
(setf (symbol-function 'is-valid-vertex) (symbol-function 'clh3i::is-valid-vertex))


(defun lat-lng-to-cell (lat lng resolution)
  (autowrap:with-many-alloc ((geo 'clh3i::lat-lng)
                             (index 'clh3i::h3index))
    (setf (clh3i:lat-lng.lat geo) lat)
    (setf (clh3i:lat-lng.lng geo) lng)
    (clh3i:lat-lng-to-cell geo resolution index)
    (cffi:mem-ref index :uint64)))

(setf (symbol-function 'rads-to-degs) (symbol-function 'clh3i::rads-to-degs))


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
