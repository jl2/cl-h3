;; clh3i.lisp
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

;; The clh3i package contains the low level C wrapper library.
;; For now it's auto-generated with autowrap, but I'll look at a minimal CFFI
;; if autowrap becomes unwieldy or has too much overhead.

;; The cl-h3 (h3) package provides a Lisp-friendly interface.

(defpackage :clh3i
  (:nicknames)
  (:use #:cl #:j-utils #:alexandria)
  (:export ;; #:lat-lng
           ;; #:uint64-t
           ;; #:cell-boundary
           ;; #:hindex
           ;; #:directed-edge-to-boundary
           ;; #:cell-boundary.num-verts
           ;; #:cell-boundary.verts[]
           ;; #:lat-lng.lat
           ;; #:lat-lng.lng
           ))

(in-package :clh3i)

(cffi:define-foreign-library h3-lib
    (:darwin (:or "libh3.dylib" "libh3"))
    (:unix (:or "libh3.so" "libh3.so.1" "libh3" "h3"))
    (t (:default "libh3")))
(cffi:use-foreign-library h3-lib)

(autowrap:c-include
 #+darwin"h3/h3api.h"
 #+linux"h3/h3api.h"
 :sysincludes (list #+linux"/usr/include/x86_64-linux-gnu/"
                    #+linux"/usr/include/x86_64-linux-gnu/c++/9/"
                    #+darwin"/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/")
 :spec-path '(clh3i specs)
 :trace-c2ffi t
 :exclude-definitions
 ("^va_list$"
  "Random" "Signal" "long-double"
  "^acos$" "^asin$" "^atan$" "^cos$" "^sin$" "^tan$" "^div$" "^ldiv$" "^lldiv$"
  "^log$" "^exp$" "^acosh$" "^cosh$" "^asinh$" "^sinh$"
  "^tanh$" "^atanh$"  "^sqrt$" "^floor$" "^round$"
  "^time$" "^close$" "^open$" "^read$" "^write$"
  "^sleep$" "^truncate$" "^ceil$"
  "^abs$" "^abort$" "^random$" "^remove$" "^signal$"))

(declaim (inline
          are-neighbor-cells
          cell-area-km2
          cell-area-m2
          cell-area-rads2
          cell-boundary
          cell-boundary.num-verts
          cell-boundary.verts[]
          cell-to-boundary
          cell-to-center-child
          cell-to-children
          cell-to-children-size
          cell-to-lat-lng
          cell-to-parent
          cell-to-vertex
          cell-to-vertexes
          cells-to-directed-edge
          compact-cells
          coord-ij
          coord-ij.i
          coord-ij.j
          degs-to-rads
          destroy-linked-polygon
          directed-edge-to-boundary
          directed-edge-to-cells
          distance-km
          distance-m
          exact-edge-length-km
          exact-edge-length-m
          exact-edge-length-rads
          experimental-h3to-local-ij
          experimental-local-ij-to-h3
          geo-loop
          geo-loop-ptr
          geo-loop.num-verts
          geo-loop.verts
          geo-polygon
          geo-polygon.geoloop
          geo-polygon.num-holes
          get-base-cell-number
          get-directed-edge-destination
          get-directed-edge-origin
          get-hexagon-area-avg-km2
          get-hexagon-area-avg-m2
          get-hexagon-edge-length-avg-km
          get-hexagon-edge-length-avg-m
          get-icosahedron-faces
          get-num-cells
          get-pentagons
          get-res0cells
          get-resolution
          grid-disk
          grid-disk-distances
          grid-distance
          grid-path-cells-size
          h3index
          h3set-to-linked-geo
          int64-t
          is-pentagon
          is-res-class-iii
          is-valid-cell
          is-valid-directed-edge
          is-valid-vertex
          lat-lng
          lat-lng-ptr
          lat-lng-to-cell
          lat-lng.lat
          lat-lng.lng
          linked-geo-loop.first
          linked-geo-loop.next
          linked-geo-polygon
          linked-geo-polygon.first
          linked-geo-polygon.next
          linked-lat-lng.next
          linked-lat-lng.vertex
          make-geo-loop
          max-face-count
          max-grid-disk-size
          max-polygon-to-cells-size
          origin-to-directed-edges
          pentagon-count
          polygon-to-cells
          rads-to-degs
          res0cell-count
          string-to-h3
          uint64-t
          uncompact-cells
          vertex-to-lat-lng
          ))
