(ns nomis-code-playing.ch-01-play
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :as M]
            [incanter.charts :refer [xy-plot add-points]] ; FIXME Get rid of refers
            [incanter.core :refer [view]] ; FIXME Get rid of refers
            [midje.sweet :refer :all]
            [clatrix.core :as cl]
            [clojure.string :as str]))

;;;; ___________________________________________________________________________

;;;; FIXME Things I am unclear about:

;;;; - Operations: duplication between eg `equals` & `M/=`" and `add` & `M/+`.
;;;;   But not identical. What are the two layers for? (What happene in each?)

;;;; - Is there an `M/xxxx` version of `mmul`?

;;;; - Doc for `matrix` says "2-dimensional matrix", but there is mention of
;;;;   n-dimensional in places (doc and code I think).

;;;; - Doc for `matrix` says: `matrix works` as a synonym for `array`, which
;;;;   creates n-dimensional arrays.
;;;;   Is an array an n-dimensional matrix?

;;;; - There are vectors too distinct from 1xN or Nx1 matrices.
;;;;   See https://github.com/mikera/core.matrix/wiki/Vectors-vs.-matrices

;;;; ___________________________________________________________________________

(defn matrix?-and-type-and-value [x]
  [(matrix? x)
   (type x)
   x])

;;;; ___________________________________________________________________________
;;;; A list of lists

(def my-lol '((0 1 2)
              (3 4 5)))

(fact "About `my-lol`"
  (-> my-lol
      matrix?-and-type-and-value)
  => [true
      clojure.lang.PersistentList
      my-lol])

;;;; ___________________________________________________________________________
;;;; A vector of vectors

(def my-vov [[0 1 2]
             [3 4 5]])

(fact "About `my-vov`"
  (-> my-vov
      matrix?-and-type-and-value)
  => [true
      clojure.lang.PersistentVector
      my-vov])

;;;; ___________________________________________________________________________
;;;; Matrix basics

(fact "About `matrix` and `matrix?`"

  (fact "Lists are regarded as matrices"
    (matrix? my-lol)
    => true)

  (fact "Vectors of vectors are regarded as matrices"
    (matrix? my-vov)
    => true)

  (fact "Vectors are not regarded as matrices" ; FIXME You haven't got much about vectors -- it's been a bit random
    (matrix? [1 2 3])
    => false)
  
  (fact "`matrix` turns lists into vectors"
    (-> (matrix my-lol)
        type)
    => clojure.lang.PersistentVector)

  (fact "`matrix?-and-type-and-value` returns the three things mentioned"
    (-> my-lol
        matrix?-and-type-and-value)
    => [true
        clojure.lang.PersistentList
        '((0 1 2)
          (3 4 5))])
  
  (fact "`matrix` Leaves vectors untouched"
    (= (-> my-vov          matrix?-and-type-and-value)
       (-> (matrix my-vov) matrix?-and-type-and-value))
    => true)

  (fact "`pm` pretty prints matrices"
    (with-out-str
      (pm (matrix my-vov)))
    => 
    "[[0 1 2]
 [3 4 5]]
"))

;;;; ___________________________________________________________________________
;;;; Can have other implementations (Clatrix here)

;;;; Note that matrices are treated as mutable objects by the Clatrix library.

;;;; Is Clatrix abandonware? No activity in 2.5 years.

(fact "`cl/matrix` converts to reals"
  (cl/matrix my-vov)
  => [[0.0 1.0 2.0]
      [3.0 4.0 5.0]])

(fact "specifying implementations"

  (fact "`:persistent-vector` is the default"
    (= (-> (matrix my-vov)                    matrix?-and-type-and-value)
       (-> (matrix :persistent-vector my-vov) matrix?-and-type-and-value)
       [true
        clojure.lang.PersistentVector
        [[0 1 2]
         [3 4 5]]])
    => true)

  (fact "`:clatrix` gives a Clatrix matrix"
    (= (-> (cl/matrix my-vov)       matrix?-and-type-and-value)
       (-> (matrix :clatrix my-vov) matrix?-and-type-and-value)
       [true
        clatrix.core.Matrix
        [[0 1 2]
         [3 4 5]]])
    => true))

(fact "`matrix?` and `cl/matrix?`"

  (fact "the obvious"
    
    (fact "`matrix?` recognises ordinary matrices (as you would expect)"
      (matrix? (matrix my-vov))
      => true)

    (fact "`cl-matrix?` recognises Clatrix matrices (as you would expect)"
      (cl/matrix? (cl/matrix my-vov))
      => true))

  (fact "more interesting"
    
    (fact "`matrix?` recognises Clatrix matrices"
      (matrix? (cl/matrix my-vov))
      => true)

    (fact "`cl/matrix?` does not recognise ordinary matrices"
      (cl/matrix? (matrix my-vov))
      => false)))

;;;; ___________________________________________________________________________
;;;; Be careful #1

(fact "`[0 1 2]` may seem to be OK as a matrix, but it isn't"

  (let [m (matrix [0 1 2])]
    
    (fact "Seems OK: `row-count`"      (row-count m)      => 3)
    (fact "Seems OK: `shape`"          (shape m)          => [3])
    (fact "Seems OK: `dimensionality`" (dimensionality m) => 1)

    (fact "Not OK: `matrix?`"          (matrix? m) => false)
    (fact "Not OK: `column-count`"     (column-count m)
      => (throws
          #"Number has zero dimensionality, cannot get count for dimension: 0"))))

;;;; ___________________________________________________________________________
;;;; Be careful #2

(fact "`[[[1]]]` may seem to be OK as a matrix, but it isn't"

  (let [m (matrix [[[1]]])]

    (fact "Seems OK: `row-count`"      (row-count m)      => 1) 
    (fact "Seems OK: `column-count`"   (column-count m)   => 1)
    (fact "Seems OK: `shape`"          (shape m)          => [1 1 1])
    (fact "Seems OK: `dimensionality`" (dimensionality m) => 3)
    
    (fact "Not OK: `matrix?`"          (matrix? m) => false)

    (fact "Clatrix is better at giving errors"
      (cl/matrix m)
      => (throws #"PersistentVector cannot be cast to java.lang.Number"))))

;;;; ___________________________________________________________________________
;;;; Clatrix shorthand

(fact "Clatrix has a shorthand notation for Nx1 matrices"
  (= (-> (cl/matrix [0 1 2])       matrix?-and-type-and-value)
     (-> (cl/matrix [[0] [1] [2]]) matrix?-and-type-and-value)
     [true
      clatrix.core.Matrix
      [0 1 2]])
  => true)

;;;; ___________________________________________________________________________
;;;; Clatrix fiddles with definition of equality

(fact "Clatrix fiddles with definiton of equality"

  (let [int-vov [[0 1 2]
                 [3 4 5]]
        real-vov [[0.0 1.0 2.0]
                  [3.0 4.0 5.0]]
        matrix-from-ints (cl/matrix int-vov)]

    (fact "Background info part 1"
      int-vov =not=> real-vov)

    (fact "Background info part 2"
      (= int-vov real-vov) => falsey)

    (fact "Using Midge equality testing, all as expected (given that Clatrix converts to reals"
      (fact "Reals: equal"    matrix-from-ints =>     real-vov)
      (fact "Ints: not equal" matrix-from-ints =not=> int-vov))

    (fact "Using Clojure equality testing"
      (fact "Reals: equal"          (= matrix-from-ints real-vov) => true)
      (fact "Ints: equality fiddle" (= matrix-from-ints int-vov)  => true))

    ;; It seems we can deduce two interesting things:
    ;; - Clatrix fiddles with equality (and normal Clojure sees the fiddling).
    ;; - Midje implements its own idea of equality.
    ))

;;;; ___________________________________________________________________________
;;;; Some matrices

(def my-matrix (matrix my-vov))

(def my-cl-matrix (cl/matrix my-vov))

;;;; ___________________________________________________________________________
;;;; Shape

(fact "About `shape`"
  (= (shape my-matrix)
     (shape my-cl-matrix)
     [2 3])
  => true)

;;;; ___________________________________________________________________________
;;;; Dimensionality

;;;; It seems that this is always 2. Ah, maybe that's just the implementations
;;;; you are looking at, and maybe the protocol allows other dimensionality.

(fact "About `dimensionality`"
  (= (dimensionality my-matrix)
     (dimensionality my-cl-matrix)
     2)
  =>
  true)

;;;; ___________________________________________________________________________
;;;; Counts

(fact "Use `row-count`, not `count`"

  (let [local-vov [[0 1 2]]]
    
    (fact "`count` and `row-count` are the same for ordinary matrices #1"
      (fact "`count`"     (count     my-matrix) => 2)
      (fact "`row-count`" (row-count my-matrix) => 2))
    
    (fact "`count` and `row-count` are the same for ordinary matrices #2"
      (fact "`count`"     (count     (matrix local-vov)) => 1)
      (fact "`row-count`" (row-count (matrix local-vov)) => 1))

    (fact "`count` and `row-count` can be the same for Clatrix matrices"
      (fact "`count`"     (count     my-cl-matrix) => 2)
      (fact "`row-count`" (row-count my-cl-matrix) => 2))

    (fact "`count` and `row-count` can be different for Clatrix matrices"
      (fact "`count`"     (count     (cl/matrix local-vov)) => 3)
      (fact "`row-count`" (row-count (cl/matrix local-vov)) => 1))))

(fact "`column-count`"

  (fact "Ordinary matrices"
    (column-count my-matrix)
    => 3)

  (fact "Clatrix"
    (column-count my-cl-matrix)
    => 3))

;;;; ___________________________________________________________________________
;;;; Getting and setting

;;;; FIXME What about accessors for ordinary matrices? Do they exist?
;;;;       Ah, you can use `get-in` I guess. But you want something in the
;;;;       matrix protocol, so you can switch implementations with ease.

(fact "About `cl/get`"
  (let [m (cl/matrix [[0 1 2]
                      [3 4 5]])]

    (fact "with two indices"
      (cl/get m 1 1) => 4.0)
    
    (fact "with one index, should be row-first but isn't this column-first?"
      (for [i (range 6)]
        (cl/get m i))
      => [0.0 3.0 1.0 4.0 2.0 5.0])))

(fact "About `cl/set`"

  (fact "with two indices"
    (let [m (cl/matrix [[0 1 2]
                        [3 4 5]])]
      (cl/set m 1 1 10))
    => (cl/matrix [[0 1  2]
                   [3 10 5]]))

  (fact "with one indices"
    (let [m (cl/matrix [[0 1 2]
                        [3 4 5]])]
      (cl/set m 1 10))
    => (cl/matrix [[0  1  2]
                   [10 4 5]])))

(fact "`cl/set` mutates"
  (let [m (cl/matrix [[0 1 2]
                      [3 4 5]])]
    (cl/set m 1 10)
    m)
  => (cl/matrix [[0  1  2]
                 [10 4 5]]))

;;;; ___________________________________________________________________________
;;;; `cl/map` and `cl/map-indexed`

(fact "`cl/map`"
  (let [m (cl/matrix [[0 1 2]
                      [3 4 5]])]
    (cl/map inc m))
  => (cl/matrix [[1 2 3]
                 [4 5 6]]))

(fact "`cl/map-indexed`"
  (let [m (cl/matrix [[  0 100 200]
                      [300 400 500]])]
    (cl/map-indexed (fn [i j x]
                      (+ x i j))
                    m))
  => (cl/matrix [[  0 101 202]
                 [301 402 503]]))

(fact "`cl/map` does not mutate (unlike `cl/set`)"
  (let [m (cl/matrix [[0 1 2]
                      [3 4 5]])]
    (cl/map inc m)
    m)
  => (cl/matrix [[0 1 2]
                 [3 4 5]]))

;;;; ___________________________________________________________________________
;;;; `square-mat`

(defn square-mat-FROM-BOOK ; :example-of-code-i-can-improve:
  "Creates a square matrix of size n x n whose elements are all e.
  Accepts an optional argument for the matrix implementation."
  [n e & {:keys [implementation]
          :or {implementation :persistent-vector}}]
  (let [repeater #(repeat n %)]
    (matrix implementation (-> e repeater repeater))))

(defn square-mat ; :sk-improved-version:
  "Creates a square matrix of size n x n whose elements are all e.
  Accepts an optional argument for the matrix implementation."
  [n e & {:keys [implementation]
          :or {implementation :persistent-vector}}]
  (->> e
       (repeat n)
       (repeat n)
       (matrix implementation)))

(fact "About `square-mat`"

  (fact "With `:implementation` unspecified"
    (square-mat 2 1)
    => (matrix [[1 1]
                [1 1]]))

  (fact "With `:implementation` specified"
    (square-mat 2 1 :implementation :clatrix)
    => (cl/matrix [[1 1]
                   [1 1]])))

;;;; ___________________________________________________________________________
;;;; `identity-matrix`

(fact "About `identity-matrix`"
  
  (fact "With implementation unspecified -- not it converts to reals"
    (-> (identity-matrix 2) matrix?-and-type-and-value)
    => [true
        clojure.lang.PersistentVector
        [[1.0 0.0]
         [0.0 1.0]]])
  
  (fact "With implementation specified"
    (-> (identity-matrix :clatrix 2) matrix?-and-type-and-value)
    => [true
        clatrix.core.Matrix
        (matrix [[1.0 0.0]
                 [0.0 1.0]])]))

(defn id-mat-FROM-BOOK ; :example-of-code-i-can-improve:
  "Creates an identity matrix of n x n size"
  [n]
  (let [init (square-mat n 0 :implementation :clatrix)
        identity-f (fn [i j n]
                     (if (= i j) 1 n))]
    (cl/map-indexed identity-f init)))

(defn id-mat ; :sk-improved-version:
  "Creates an identity matrix of n x n size"
  [n]
  (cl/map-indexed (fn [i j _] (if (= i j) 1 0))
                  (square-mat n 0 :implementation :clatrix)))

(fact "About `id-mat`"
  (= (id-mat 3)
     (id-mat-FROM-BOOK 3)
     [[1 0 0]
      [0 1 0]
      [0 0 1]])
  => true)

;;;; ___________________________________________________________________________
;;;; Random matrices

(defn rand-square-mat-FROM-BOOK ; :example-of-code-i-can-improve:
  "Generates a random matrix of size n x n"
  [n]
  (matrix
   (repeatedly n #(map rand-int (repeat n 100)))))

(defn rand-square-mat ; :sk-improved-version:
  "Generates a random matrix of size n x n"
  [n]
  (letfn [(row [] (repeatedly n #(rand-int 100)))]
    (-> (repeatedly n row)
        matrix)))

(defn rand-square-clmat
  "Generates a random clatrix matrix of size n x n"
  [n]
  (cl/map rand-int (square-mat n 100 :implementation :clatrix)))

;;;; eg
;;;;
;;;; (rand-square-mat 3)
;;;; => [[48 25 13]
;;;;     [76 24 33]
;;;;     [41 94 65]]
;;;;
;;;; (pm (rand-square-clmat 3))
;;;; [[ 8.000 69.000 92.000]
;;;;  [88.000 98.000 99.000]
;;;;  [80.000 72.000 15.000]]

;;;; There is also `cl/rnorm`, which produces random matrices using a normal
;;;; distribution.

;;;; ___________________________________________________________________________
;;;; `compute-matrix`

(defn id-computed-mat
  "Creates an identity matrix of size n x n using `compute-matrix`."
  [n]
  (compute-matrix [n n]
                  #(if (= %1 %2) 1 0)))

(defn rand-computed-mat
  "Creates an n x m matrix of random elements using `compute-matrix`."
  [n m]
  (compute-matrix [n m]
                  (fn [_ _] (rand-int 100))))

;;;; (id-computed-mat 3)
;;;; (rand-computed-mat 2 3)

;;;; ___________________________________________________________________________
;;;; `equals` & `M/==`

(fact "About `equals` & `M/=`"
  (let [a1 (matrix [[0 1 2] [3 4 5]])
        a2 (matrix [[0 1 2] [3 4 5]])
        a3 (matrix [[0 1 2] [3 4 5]])
        a4 (matrix [[0 1 2] [3 4 5]])
        b  (matrix [[0 0 0] [0 0 0]])]
    (fact "`equals` checks equality of one or two matrices, and, for two matrices, an epsilon can be supplied"
      (equals a1)       => true
      (equals a1 a2)    => true
      (equals a1 b)     => false
      (equals a1 b 100) => true)
    (fact "`M/==` checks equality of any number of matrices"
      (M/==)               => true
      (M/== a1)            => true
      (M/== a1 a2 a3 a4)   => true
      (M/== a1 a2 a3 a4 b) => false)))

;;;; ___________________________________________________________________________
;;;; `add` & `M/+`

(fact "`add` & `M/+` both add element-wise or broadcasting a scalar"
  (let [a     (matrix [[1 2 3]
                       [4 5 6]])
        b     (matrix [[10 10 10]
                       [10 10 10]])
        c     (matrix [[100 100 100]
                       [100 100 100]])
        a+b+c (matrix [[111 112 113]
                       [114 115 116]])
        n1    1000
        n2    2000
        a+n1  [[1001 1002 1003]
               [1004 1005 1006]]
        n1+n2 3000]
    (doseq [op ['add 'M/+]]
      (fact {:midje/description op}
        (let [op (resolve op)]
          (fact "matrices"               (op a b c) => a+b+c)
          (fact "broadcasting scalar #1" (op n1 a)  => a+n1)
          (fact "broadcasting scalar #2" (op a n1)  => a+n1)
          (fact "scalars"                (op n1 n2) => n1+n2))))))

;;;; ___________________________________________________________________________
;;;; `mul` & `M/*`

(fact "`mul` & `M/*` both multiply element-wise or broadcasting a scalar"
  (let [a     (matrix [[1 2 3]
                       [4 5 6]])
        b     (matrix [[-1 -2 -3]
                       [-4 -5 -6]])
        c     (matrix [[100 100 100]
                       [100 100 100]])
        a*b*c (matrix [[ -100  -400  -900]
                       [-1600 -2500 -3600]])
        n1    1000
        n2    2000
        a*n1  (matrix [[1000 2000 3000]
                       [4000 5000 6000]])
        n1*n2 2000000]
    (doseq [op ['mul 'M/*]]
      (fact {:midje/description op}
        (let [op (resolve op)]
          (fact "matrices"               (op a b c) => a*b*c)
          (fact "broadcasting scalar #1" (op a n1)  => a*n1)
          (fact "broadcasting scalar #2" (op n1 a)  => a*n1)
          (fact "scalars"                (op n1 n2) => n1*n2))))))

;;;; ___________________________________________________________________________
;;;; `mmul`

(fact "`mmul` multiplies"
  (let [a     (matrix [[1 2 3]
                       [4 5 6]])
        b     (matrix [[10 20]
                       [20 30]
                       [30 40]])
        c     (matrix [[10  0]
                       [ 0 10]])
        a*b*c (matrix [[1400.0 2000.0]
                       [3200.0 4700.0]])
        n1    1000
        n2    2000
        a*n1  (matrix [[1000 2000 3000]
                       [4000 5000 6000]])
        n1*n2 2000000]
    (fact "matrices"               (mmul a b c) => a*b*c)
    (fact "broadcasting scalar #1" (mmul a n1)  => a*n1)
    (fact "broadcasting scalar #2" (mmul n1 a)  => a*n1)
    (fact "scalars"                (mmul n1 n2) => n1*n2)))

;;;; ___________________________________________________________________________
;;;; `scale`

(fact "About `scale`"
  (let [m      (matrix [[1 2 3]
                        [4 5 6]])
        m*1000 (matrix [[1000 2000 3000]
                        [4000 5000 6000]])
        m*m*10 (matrix [[ 10  40  90]
                        [160 250 360]])]
    (fact "What the doc string says"
      (scale m 10 100) => m*1000)
    (fact "Also"
      (scale 1 m 10 m) => m*m*10)
    (fact "So you might expect that this is OK, but it isn't"
      (scale m 10 m) => throws)))

;;;; ___________________________________________________________________________
;;;; A performance comparison

(defn time-mat-mul
  "Measures the time for multiplication of two matrices A and B"
  [A B]
  (time (M/* A B)))

(defn core-matrix-mul-time []
  (let [A (rand-square-mat 100)
        B (rand-square-mat 100)]
    (time-mat-mul A B)))

(defn clatrix-mul-time []
  (let [A (rand-square-clmat 100)
        B (rand-square-clmat 100)]
    (time-mat-mul A B)))

;; (do (core-matrix-mul-time) nil)
;; "Elapsed time: 4.113151 msecs"

;; (do (clatrix-mul-time) nil)
;; "Elapsed time: 0.107426 msecs"

;;;; ___________________________________________________________________________
;;;; `transpose`

(fact "About `transpose`"
  (let [n  2
        v  [1 2 3]
        m  [[1 2 3]
            [4 5 6]]
        m' [[1 4]
            [2 5]
            [3 6]]
        m2  [[1 2 3]]
        m2' [[1]
             [2]
             [3]]]
    (fact "Leaves scalars unchanged" (transpose n) => n)
    (fact "Leaves vectors unchanged" (transpose v) => v) ; FIXME You haven't got much about vectors -- it's been a bit random
    (fact "Transposes matrices #1"   (transpose m) => m')
    (fact "Transposes matrices #2"   (transpose m2) => m2')))

;;;; ___________________________________________________________________________
;;;; `inverse`

(fact "About `inverse`"
  (fact "Non-singular"
    (let [m (cl/matrix [[1 2]
                        [3 4]])]
      (M/== (mmul m
                  (inverse m))
            (identity-matrix 2))
      => true))
  (fact "Singular"
    (let [m (cl/matrix [[2 2]
                        [1 1]])]
      (inverse m)
      => (throws #"singular"))))

;;;; ___________________________________________________________________________
;;;; `det`

(fact "About `det`"
  (let [m (cl/matrix [[1 2]
                      [3 4]])]
    (det m)
    => -2.0))

;;;; ___________________________________________________________________________
;;;; Example: Tichonov regularization -- Interpolating using matrices

(defn lmatrix-FROM-BOOK [n] ; :example-of-code-i-can-improve:
  (compute-matrix :clatrix [n (+ n 2)]
                  (fn [i j]
                    ({0 -1, 1 2, 2 -1}
                     (- j i)
                     0))))

(defn lmatrix [n] ; :sk-improved-version:
  (compute-matrix :clatrix
                  [n (+ n 2)]
                  (fn [i j]
                    (case (- j i)
                      0 -1
                      1  2
                      2 -1
                      0))))

(fact "About `lmatrix`"
  (lmatrix 4)
  => (matrix [[-1.0  2.0 -1.0  0.0  0.0  0.0]
              [ 0.0 -1.0  2.0 -1.0  0.0  0.0]
              [ 0.0  0.0 -1.0  2.0 -1.0  0.0]
              [ 0.0  0.0  0.0 -1.0  2.0 -1.0]]))

(defn problem
  "Return a map of the problem setup for a
  given matrix size, number of observed values
  and regularization parameter"
  [n n-observed lambda]
  (let [i (shuffle (range n))]
    {:L               (mmul (lmatrix n) lambda)
     :observed        (take n-observed i)
     :hidden          (drop n-observed i)
     :observed-values (matrix :clatrix
                              (repeatedly n-observed rand))}))

(defn solve
  "Return a map containing the approximated value
  y of each hidden point x"
  [{:keys [L observed hidden observed-values] :as problem}]
  (let [nc  (column-count L)
        nr  (row-count L)
        L1  (cl/get L (range nr) hidden)
        L2  (cl/get L (range nr) observed)
        l11 (mmul (transpose L1) L1)
        l12 (mmul (transpose L1) L2)]
    (assoc problem
           :hidden-values
           (mmul -1 (inverse l11) l12 observed-values))))

(defn plot-points
  "Plots sample points of a solution s"
  [s]
  (let [X (concat (:hidden s) (:observed s))
        Y (concat (:hidden-values s) (:observed-values s))]
    (view
     (add-points
      (xy-plot X Y) (:observed s) (:observed-values s)))))

(defn plot-rand-sample []
  (plot-points (solve (problem 150 10 30))))

;;;; (plot-rand-sample)
