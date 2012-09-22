(ns raman.core
  (:use [clojure.java.io])
  (:import [java.io File]))

(def src-dir "T:\\raman")

(defn list-all-files [dir extension]
  (let [entries (map #(java.io.File. (str (.getAbsolutePath dir) "\\" %)) (.list dir))
        files (filter #(and (.isFile %) (re-find extension (.getName %))) entries)]
    files))

(defn file-in-subdir [f subdir-name]
  (File. (str (.getParent f) "\\" subdir-name "\\" (.getName f))))

(defn read-cols [file sep]
  (with-open [in-rdr (reader file)]
    (let [lines      (line-seq in-rdr)
          line-count (count lines)
          split-lines (map #(clojure.string/split % sep) lines)
          xs (into-array Double/TYPE (map #(Double/parseDouble (first %)) split-lines))
          ys (into-array Double/TYPE (map #(Double/parseDouble (second %)) split-lines))]
      [xs ys])))

(defn write-cols [file sep & cols]
  (with-open [out-w (writer file)]
    (let [split-lines (apply map vector cols)
          lines (map #(apply str (interpose sep %)) split-lines)]
      (doseq [line lines]
        (.write out-w (str line "\r\n"))))))

(defn negate-xs [[xs ys]]
  (doseq [i (range (count xs))]
    (aset-double xs i (- (aget xs i))))
  [xs ys])

(defn sqrt-xs [[xs ys]]
  (doseq [i (range (count xs))]
    (aset-double xs i (Math/sqrt (aget xs i))))
  [xs ys])

(defn read-transform-write [in-file out-subdir-name & transforms]
  (let [[xs ys] (read-cols in-file #"\t")
        [new-xs new-ys] ((apply comp (reverse transforms)) [xs ys])]
    (write-cols (file-in-subdir in-file "out") "\t" new-xs new-ys)))
