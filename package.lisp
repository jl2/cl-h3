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
   #:lat-lng-to-cell
   #:cell-to-lat-lng
   #:cell-to-boundary
   #:grid-disk
   #:grid-disk-distances
   #:grid-ring-unsafe
   #:polygon-to-cells
   #:h3-set-to-multi-polygon
   #:degs-to-rads
   #:rads-to-degs
   #:distance
   #:get-hexagon-area-avg
   #:cell-area
   #:get-hexagon-edge-length-avg
   #:exact-edge-length
   #:get-num-cells
   #:get-res-0-cells
   #:get-pentagons
   #:get-resolution
   #:get-base-cell-number
   #:string-to-h3
   #:h3-to-string
   #:is-valid-cell
   #:cell-to-parent
   #:cell-to-children
   #:cell-to-center-child
   #:compact-cells
   #:uncompact-cells
   #:is-res-class-iii
   #:is-pentagon
   #:get-icosahedron-faces
   #:are-neighbor-cells
   #:cells-to-directed-edge
   #:is-valid-directed-edge
   #:get-directed-edge-origin
   #:get-directed-edge-destination
   #:directed-edge-to-cells
   #:origin-to-directed-edges
   #:directed-edge-to-boundary
   #:cell-to-vertex
   #:cell-to-vertexes
   #:vertex-to-lat-lng
   #:is-valid-vertex
   #:grid-distance
   #:grid-path-cells
   #:experimental-h3-to-local-ij
   #:experimental-local-ij-to-h3



   #:grid-distance
           #:cell-to-lat-lng
           #:cell-to-lat-lng-degrees
           #:cells-to-directed-edge
           #:directed-edge-to-boundary
           #:directed-edge-to-boundary-degrees
           #:rad2deg
           #:deg2rad
           #:haversine-distance
           #:grid-disk
           #:max-grid-disk-size
           ))
