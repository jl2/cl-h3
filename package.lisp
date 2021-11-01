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

(defpackage :cl-h3

  (:nicknames #:h3)

  (:use #:cl #:j-utils #:alexandria)
  (:export
   #:h3-to-string
   #:lat-lng
   #:lat
   #:lng
   #:are-neighbor-cells
   #:cell-area
   #:cell-to-boundary
   #:cell-to-center-child
   #:cell-to-children
   #:cell-to-lat-lng
   #:cell-to-lat-lng-degrees
   #:cell-to-parent
   #:cell-to-vertex
   #:cell-to-vertexes
   #:cells-to-directed-edge
   #:cells-to-directed-edge
   #:compact-cells
   #:degs-to-rads
   #:directed-edge-to-boundary
   #:directed-edge-to-cells
   #:distance-m
   #:distance-km
   #:distance-rads
   #:exact-edge-length
   #:experimental-h3-to-local-ij
   #:experimental-local-ij-to-h3
   #:get-base-cell-number
   #:get-directed-edge-destination
   #:get-directed-edge-origin
   #:get-hexagon-area-avg-m2
   #:get-hexagon-area-avg-km2
   #:get-hexagon-edge-length-avg-m
   #:get-hexagon-edge-length-avg-km
   #:get-icosahedron-faces
   #:get-num-cells
   #:get-pentagons
   #:get-pentagon-count
   #:get-res-0-cells
   #:get-res-0-cell-count
   #:get-resolution
   #:grid-disk
   #:grid-disk-distances
   #:grid-distance
   #:grid-path-cells
   #:grid-ring-unsafe
   #:h3-set-to-multi-polygon
   #:h3-to-string
   #:haversine-distance
   #:is-pentagon
   #:is-res-class-iii
   #:is-valid-cell
   #:is-valid-directed-edge
   #:is-valid-vertex
   #:lat-lng-to-cell
   #:max-grid-disk-size
   #:origin-to-directed-edges
   #:polygon-to-cells
   #:rads-to-degs
   #:string-to-h3
   #:uncompact-cells
   #:vertex-to-lat-lng
           ))
