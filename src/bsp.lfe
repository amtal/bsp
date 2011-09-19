;;; Code automatically generated from bsp.nw literate program. Go read the PDF
;;; generated alongside, instead of this commentless soup. The following isn't
;;; meant for human consumption!
(include-lib "lfe_utils/include/all.lfe")
(defmodule bsp
  (using lists)
  (export (new 2)
	  (is_tree 1) (size 1) (sparsity 1)
	  (map 2)
	  (factor 2)))

;; Vector manipulation:
; sanitize input
(defn vector? 
  [v] (when (is_tuple v))
    (lists:all (fun is_integer 1) (vec-contents v))
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
  (in (list->tup (cons tag xs'))
   [xs' (++ (tl xs) (list x))
    (tag . xs) (tup->list v)]))

; first vector element ('x')
(defn 1st-el [v] (element 2 v))
(defn vec-size [v] (- (tuple_size v) 1))

; proper zero-based indexing
(defn vec-nth [n v] (element (+ n 2) v))

; overflow indexes that get too large
(defn vec-index [n v]
  (in (if (< 0 n') n' (+ size n'))
   [n' (rem n size)
    size (vec-size v)]))
(defn 2^n-vec? [v]
  (lists:all (fun 2^n? 1) (vec-contents v)))
; construct a null vector of the same type as another
(defn null-vec [v] 
  (: erlang make_tuple (+ 1 (vec-size v)) 
                       0 
                       (list (tuple 1 (element 1 v)))))

(defn unit-hypercube? [v]
  (== v (: erlang make_tuple (+ 1 (vec-size v))
                             1
                             (list (tuple 1 (element 1 v))))))

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
(defmacro :bsp ([size data] `(tuple 'bsp ,size ,data)))
(defn factor [axes (:bsp size data)]
  (in (:bsp outer-size (factor inner-size axes data 'acc))
   [(tuple outer-size inner-size) (partition-vec axes size)
    _ (if (/= (vec-size axes) (vec-size size))
        (error (tuple 'vector_dim_mismatch axes size)))]))
; where
  (defn factor [inner-size axes data inner] 'todo)
  (defn partition-vec [_ _] 'todo)
    

;; BSP tree manipulation:
(defn new [size lup] 
  (if (2^n-vec? size)
    (:bsp size (build-new size (null-vec size) lup))
    (error 'badarg (list size lup))))
; where
  (defn build-new [size pos lup]
    (if (unit-hypercube? size)
      ; 1x1x1 sizes can be looked up
      (let [(volume (funcall lup pos))]
        (if (is_list volume)
          (error (tuple 'no_lists pos volume)))
        volume)
      ; larger ones must be split into two branches
      (in (if (minimal-identical-trees? left right)
            left ; merge
            (cons left right)) ; branch
       [left (build-new size' pos-left lup)
        right (build-new size' pos-right lup)
        ; cut the current axis in two
        size' (vec-rot-with (bsr (1st-el size) 1) size)
        ; position increased by an order of magnitude, with
        ; least significant bit indicating choice between
        ; left and right branches
        pos-left (vec-rot-with offset pos)
        pos-right (vec-rot-with (bor 1 offset) pos)
        offset (bsl (1st-el pos) 1)])))
  (defn minimal-identical-trees? [a b]
    (andalso (not (is_list a))
             (not (is_list b))
             (== a b)))
    
(defn is_tree 
  [(:bsp size _data)] (2^n-vec? size)
  [_] 'false)

(defn size [(:bsp size _data)] size)

(defn sparsity [(:bsp size data)] 
  (tuple (count-leaves data)
         (lists:foldl (fn [a b] (* a b)) 1 (vec-contents data))))
; where
  (defn count-leaves 
    [(cons l r)] (+ (count-leaves l) (count-leaves r))
    [_] 1)
(defn map [f (:bsp _ data)] (map-tree f data))
; where
  (defn map-tree
    [f (cons l r)]
      (in (if (minimal-identical-trees? l' r') 
            l' ; tree got simplified, merge
            (cons l' r'))
       [l' (map-tree f l) 
        r' (map-tree f r)])
    [f leaf] (funcall f leaf))
