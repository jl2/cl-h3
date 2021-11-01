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
