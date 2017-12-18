(ns nomis-code-playing.ch-01-play
  (:require [clojure.core.matrix :refer :all]
            [midje.sweet :refer :all]
            [clatrix.core :as cl]
            [clojure.string :as str]))

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

  (fact "Vectors are regarded as matrices"
    (matrix? my-vov)
    => true)
  
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
