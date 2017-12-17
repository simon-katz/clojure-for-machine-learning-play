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
  =>[true
     clojure.lang.PersistentList
     my-lol])

;;;; ___________________________________________________________________________
;;;; A vector of vectors

(def my-vov [[0 1 2]
             [3 4 5]])

(fact "About `my-vov`"
  (-> my-vov
      matrix?-and-type-and-value)
  =>[true
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
    => truthy)

  (fact "`pm` pretty prints matrices"
    (with-out-str
      (pm (matrix my-vov)))
    => 
    "[[0 1 2]
 [3 4 5]]
"))

(fact "More about `matrix?`"

  (fact (matrix? [])
    => false)

  (fact (matrix? [[]])
    => true)

  (fact (matrix? [[[1 2 3]]])
    => false)

  (fact "WTF?"
    (matrix? (matrix [[[1 2 3]]]))
    => false))

;;;; ___________________________________________________________________________
;;;; Can have other implementations (clatrix here)

;; Note that matrices are treated as mutable objects by the clatrix library.

(fact "`cl/matrix` converts to reals"
  (cl/matrix my-vov)
  => [[0.0 1.0 2.0]
      [3.0 4.0 5.0]])

(fact "specifying implementations"

  (fact "`:persistent-vector` is the default"
    (= (-> (matrix my-vov)                    matrix?-and-type-and-value)
       (-> (matrix :persistent-vector my-vov) matrix?-and-type-and-value))
    => true)

  (fact "`:clatrix` gives a clatrix matrix"
    (= (-> (cl/matrix my-vov)       matrix?-and-type-and-value)
       (-> (matrix :clatrix my-vov) matrix?-and-type-and-value))
    => true))

(fact "`matrix?` and `cl/matrix?`"

  (fact "the obvious"
    
    (fact "`matrix?` recognises ordinary matrices (as you would expect)"
      (matrix? (matrix my-vov))
      => true)

    (fact "`cl-matrix?` recognises clatrix matrices (as you would expect)"
      (cl/matrix? (cl/matrix my-vov))
      => true))

  (fact "more interesting"
    
    (fact "`matrix?` recognises clatrix matrices"
      (matrix? (cl/matrix my-vov))
      => true)

    (fact "`cl-matrix?` does not recognise ordinary matrices"
      (cl/matrix? (matrix my-vov))
      => false)))

;;;; ___________________________________________________________________________
;;;; Dimensionality

(def some-matrices
  [(matrix [0 1])
   (matrix [[0 1]])
   ;; FIXME I suspect the following might be invalid.
   (matrix [[[0 1]]])
   (matrix [[[[[[[[[[[[0 1]]]]]]]]]]]])])

(def some-cl-matrices
  [(cl/matrix [0 1])
   (cl/matrix [[0 1]])])

(fact "Shorthand notation for Nx1 matrices"
  (= (cl/matrix [0 1])
     (cl/matrix [[0] [1]]))
  => truthy)

(fact "what dimension matrices are supported?"

  (fact (map dimensionality some-matrices)
    =>
    [1 2 3 12])

  (fact "clatrix dimensionality is different -- seems that only 2D is supported"

    (fact (map dimensionality some-cl-matrices)
      =>
      [2 2])

    (fact "Sheesh!"
      (= (dimensionality (first some-matrices))
         (dimensionality (first some-cl-matrices)))
      => falsey)

    (fact (cl/matrix [[[0 1]]])
      => (throws #"PersistentVector cannot be cast to java.lang.Number"))

    (fact (cl/matrix [[[[[[[[[[[[0 1]]]]]]]]]]]])
      => (throws #"PersistentVector cannot be cast to java.lang.Number"))))

;;;; ___________________________________________________________________________
;;;; Shape

(fact "shape"

  (map shape some-matrices)
  => [[2] [1 2] [1 1 2] [1 1 1 1 1 1 1 1 1 1 1 2]]

  (fact "clatrix shape is different -- seems that only 2D is supported"

    (fact (map shape some-cl-matrices)
      => [[2 1] [1 2]])

    (fact "Sheesh!"
      (= (shape (first some-matrices))
         (shape (first some-cl-matrices)))
      => falsey)))

;;;; ___________________________________________________________________________
;;;; Counts

(fact "counts"

  (fact "`count` and `row-count`"
    (map (juxt count row-count)
         some-matrices)
    => [[2 2]
        [1 1]
        [1 1]
        [1 1]])

  (fact "`column-count`"

    (fact (column-count (first some-matrices))
      => (throws #"Number has zero dimensionality, cannot get count for dimension: 0"))

    (fact 
      (map column-count
           (rest some-matrices))
      => [2 1 1]))

  (fact "clarix"

    (fact "`count` and `row-count`"
      (map (juxt count row-count)
           some-cl-matrices)
      => [[2 2]
          [2 1] ; huh?
          ])

    (fact "`column-count`"

      (fact 
        (map column-count some-cl-matrices)
        => [1 2]))))
