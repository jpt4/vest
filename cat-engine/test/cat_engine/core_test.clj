(ns cat-engine.core-test
  (:require [clojure.test :refer :all]
            [cat-engine.core :refer :all]))

;;generate test 3D data point
(def tst-3dp (mk-point 'test 'catst (list 0.5 0.1 0.8)))

(deftest a-test
  (testing "make a point."
    (is (= '(test catst (0.5 0.1 0.8))
           tst-3dp))))
