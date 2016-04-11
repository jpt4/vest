(ns cat-engine.core
  (:gen-class))

(defn mk-point [val cat dls] (list val cat dls))

(defn val [p] (nth p 0))
(defn cat [p] (nth p 1))
(defn dls [p] (nth p 2))
(defn n-co [p n] (nth (dls p) n))
(defn tag [td] (first td))
(defn tag [td] (last td))
