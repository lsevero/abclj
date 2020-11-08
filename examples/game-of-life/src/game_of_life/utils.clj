(ns game-of-life.utils
  "Namespace with utils to convert between CL and CLJ data structures.

  There is also helper functions to ease the mismatch between Java and CLJ."
  (:require [abclj.core :refer [cl->clj cl-cons cl-nil cons->vec]]
            [clojure.walk :refer [postwalk]]
            [game-of-life.life :as life-cl]))

(defn cl-universe->clj-universe [universe]
  (map (fn [row]
         (map cl->clj (cons->vec row)))
       (cons->vec universe)))

(defn clj-universe->cl-universe [universe]
  (cl-cons
   (postwalk #(if (seq? %)
                (conj (vec %) cl-nil)
                %)
             universe)))

(defn gen-universe
  "Return an universe in CLJ structure"
  [width height]
  (-> (life-cl/gen-rand-universe width height)
      cl-universe->clj-universe))

(defn update-universe
  "Return a new universe in CLJ structure."
  [universe]
  (->> universe
       clj-universe->cl-universe
       life-cl/update-universe
       cl-universe->clj-universe))

(defn find-pos [universe x y]
  (nth (nth universe x) y))
