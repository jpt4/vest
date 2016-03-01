(ns k-means.core-test
  (:require [clojure.test :refer :all]
            [k-means.core :refer :all]))

;(deftest a-test
 ; (testing "FIXME, I fail."
  ;  (is (= 0 1))))

(deftest pairs-of-values
  (let [args ["--server" "localhost"
              "--port" "8080"
              "--environment" "production"]]
    (is (= {:server "localhost"
            :port "8080"
            :environment "production"}
           (parse-args args)))))
