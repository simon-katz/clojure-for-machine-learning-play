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
       (-> (matrix :persistent-vector my-vov) matrix?-and-type-and-value))
    => true)

  (fact "`:clatrix` gives a Clatrix matrix"
    (= (-> (cl/matrix my-vov)       matrix?-and-type-and-value)
       (-> (matrix :clatrix my-vov) matrix?-and-type-and-value))
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

    (fact "`cl-matrix?` does not recognise ordinary matrices"
      (cl/matrix? (matrix my-vov))
      => false)))

;;;; ___________________________________________________________________________
;;;; Some matrices

(def some-vovs
  [[0 1 2] ; Hah! This is not OK for ordinary matrices - but you didn't find out until you used `column-count`.
   [[0 1 2]]
   ;; FIXME Are the following OK?
   ;;       - I think OK for ordinary matrices (although the doc string for
   ;;         `matrix` says "2-dimensional", it also says "works as a synonym
   ;;         for `array`", which does n-dimensional).
   ;;       - Not OK for Clatrix.
   [[[0 1 2]]]
   [[[[[[[[[[[[0 1 2]]]]]]]]]]]]])

(def some-matrices
  (map matrix some-vovs))

(def some-cl-matrices
  (map cl/matrix
       (take 2 some-vovs)))

(fact "Clatrix is only 2D"
  (cl/matrix (nth some-vovs 2))
  => (throws #"PersistentVector cannot be cast to java.lang.Number")
  (cl/matrix (nth some-vovs 3))
  => (throws #"PersistentVector cannot be cast to java.lang.Number"))

(fact "Clatrix has a shorthand notation for Nx1 matrices"
  (= (-> (cl/matrix [0 1 2])       matrix?-and-type-and-value)
     (-> (cl/matrix [[0] [1] [2]]) matrix?-and-type-and-value))
  => true
  (fact "and that is different to ordinary matrices"
    (fact "Seems OK"
      (= (matrix [0 1 2])
         (matrix [[0] [1] [2]]))
      => false
      (fact "but it is not"
        (column-count (matrix [0 1 2]))
        => (throws #"Number has zero dimensionality, cannot get count for dimension: 0")))))

;;;; ___________________________________________________________________________
;;;; Shape

(fact "About `shape`"

  (fact "Shape of ordinary matrices is as you would expect"
    (map shape some-matrices)
    => [[3] [1 3] [1 1 3] [1 1 1 1 1 1 1 1 1 1 1 3]])

  (fact "Shape of Clatrix matrices is different"
    (fact "Only 2D is supported"
      (map shape some-cl-matrices)
      => [[3 1] [1 3]])
    (fact "In particular"
      (= (shape (first some-matrices))
         (shape (first some-cl-matrices)))
      => false)))

;;;; ___________________________________________________________________________
;;;; Dimensionality

(fact "About `dimensionality`"

  (fact "Dimensionality of ordinary matrices is as you would expect"
    (map dimensionality some-matrices)
    =>
    [1 2 3 12])

  (fact "Dimensionality of Clatrix matrices is different"
    (fact "Only 2D is supported"
      (map dimensionality some-cl-matrices)
      => [2 2])
    (fact "In particular"
      (= (dimensionality (first some-matrices))
         (dimensionality (first some-cl-matrices)))
      => false)))

;;;; ___________________________________________________________________________
;;;; Counts

(fact "Use `row-count`, not `count`"
  
  (fact "`count` and `row-count` are the same for ordinary matrices"
    (fact "`count`"     (map count     some-matrices) => [3 1 1 1])
    (fact "`row-count`" (map row-count some-matrices) => [3 1 1 1]))

  (fact "`count` and `row-count` are different for Clatrix matrices"
    (fact "`count`"     (map count     some-cl-matrices) => [3 3])
    (fact "`row-count`" (map row-count some-cl-matrices) => [3 1])))

(fact "counts"

  (fact "`column-count`"

    (fact "Ordinary matrices"

      (fact "The other ordinary matrices are OK"
        (map column-count
             (rest some-matrices))
        => [3 1 1]))

    (fact "Clarix"
      (map column-count some-cl-matrices)
      => [1 3])))
