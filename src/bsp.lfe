(include-lib "lfe_utils/include/all.lfe")
(defmodule bsp
  (export     todo))

; sanitize input
(defn vector? 
  [x] (when (is_tuple x))
    (-> (tl (tuple_to_list x))
        (lists:all (fun is_integer) <>))
  [_] 'false)


    todo

    todo
