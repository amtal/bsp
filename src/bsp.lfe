;;; Code automatically generated from bsp.nw literate program. Go read the PDF
;;; generated alongside, instead of this commentless soup. The following isn't
;;; meant for human consumption!
(include-lib "lfe_utils/include/all.lfe")
(defmodule bsp
  (using lists)
  (export     todo))

;; Vector manipulation:
; sanitize input
(defn vector? 
  [v] (when (is_tuple v))
    (lists:all (fun is_integer 1) (vec-contents v)))
  [_] 'false)

; get list of integers
(defn vec-contents [v] 
  (tl (tup->list v)))

(defn make-vec [tag contents]
  (list->tup (cons tag contents)))
; make first element last, shift rest to take its place
(defn vec-rot [v] 
  (in (list->tup (cons tag xs'))
   [xs' (++ most (list last))
    (last . most) xs
    (tag . xs) (tup->list v)]))

; same, but overwrite first element
(defn vec-rot-with [x v]
  (in (list->tup (cons tag xs')
   [xs' (++ (tl xs) (list x))
    (tag . xs) (tup->list v)])))

; first vector element ('x')
(defn 1st-el [v] (element 2 v))
(defn vec-size [v] (- (size v) 1))

; proper zero-based indexing
(defn vec-nth [n v] (element (+ n 2) v))

; overflow indexes that get too large
(defn vec-index [n v]
  (let (if (< 0 n') n' (+ size n'))
   [n' (rem n size)
    size (vec-size v)]))
(defn 2^n-vec? [v]
  (lists:all (fun 2^n? 1) (vec-contents v)))
; construct a null vector of the same type as another
(defn null-vec [v] 
  (: erlang make_tuple (vec-size v) 0 
                       (list (tuple 1 (element 1 v))))

(defn :+ (va vb) 
  (in (make-vec tag els)
   [tag (element 1 va)
    els (lists:zipwith (fun erlang + 2) 
                       (vec-contents va) 
                       (vec-contents vb))]))

;; General utilities:
(defn list->tup [l] (list_to_tuple l))
(defn tup->list [t] (tuple_to_list t))
; test for power of two
(defn 2^n?
  [n] (when (=< n #xff)) (2^n-byte? n)
  [n] (andalso (== 0 (band #xff n)) 
               (2^n? (bsr n 8))))

(defn 2^n-byte?
  [1]  'true [2]  'true [4]  'true [8]   'true
  [16] 'true [32] 'true [64] 'true [128] 'true
  [_] 'false)
    todo

;; BSP tree manipulation:
    todo
