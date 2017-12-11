(ns nomis-code-playing.ch-01-play
  (:require [clojure.core.matrix :refer :all]
            [midje.sweet :refer :all]
            [clatrix.core :as cl]
            [clojure.string :as str]))

;;;; ___________________________________________________________________________
;;;; Basics

(fact (matrix [[0 1 2]
               [3 4 5]])
  => [[0 1 2]
      [3 4 5]])

(fact (matrix '((0 1 2)
                (3 4 5)))
  => [[0 1 2]
      [3 4 5]])

(fact (with-out-str
        (pm (matrix [[0 1 2]
                     [3 4 5]])))
  => 
  "[[0 1 2]
 [3 4 5]]
")

(fact "About `matrix?`"

  (fact (matrix? (matrix [[0 1 2]
                          [3 4 5]]))
    => true)

  (fact (matrix? [[0 1 2]
                  [3 4 5]])
    => true)

  (fact (matrix? [[]])
    => true)

  (fact (matrix? [])
    => false)

  (fact (matrix? [[[1 2 3]]])
    => false)

  (fact "WTF?"
    (matrix? (matrix [[[1 2 3]]]))
    => false))

;;;; ___________________________________________________________________________
;;;; Can have other implementations (clatrix here)

;; You should note that matrices are treated as mutable objects
;; by the clatrix library.

(fact "`cl/matrix` converts to reals"
  (cl/matrix [[0 1 2] [3 4 5]])
  => [[0.0 1.0 2.0]
      [3.0 4.0 5.0]])

(fact "specifying implementations"

  (fact
    (= (matrix :persistent-vector
               [[0 1 2]
                [3 4 5]])
       (matrix [[0 1 2]
                [3 4 5]]))
    => truthy)

  (fact
    (= (matrix [[0 1 2]
                [3 4 5]])
       (cl/matrix [[0 1 2]
                   [3 4 5]]))
    => falsey)

  (fact
    (= (matrix :clatrix [[0 1 2]
                         [3 4 5]])
       (cl/matrix [[0 1 2]
                   [3 4 5]]))
    => truthy))

(fact "`matrix?` and `cl/matrix`"
  (map (juxt matrix?
             cl/matrix?)
       [(matrix [[0 1 2]
                 [3 4 5]])
        (cl/matrix [[0 1 2]
                    [3 4 5]])])
  => [[true false]
      [true true]])

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
