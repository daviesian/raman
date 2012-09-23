(ns raman.core
  (:use [incanter.core]
        [incanter.stats]
        [incanter.charts])
  (:require [clojure.java.io :as io])
  (:import [java.io File]))




(defn list-all-files [dir extension]
  (let [entries (map #(java.io.File. (str (.getAbsolutePath dir) "\\" %)) (.list dir))
        files (filter #(and (.isFile %) (re-find extension (.getName %))) entries)]
    files))



(defn file-in-subdir [f subdir-name]
  (File. (str (.getParent f) "\\" subdir-name "\\" (.getName f))))

(defn read-cols [file sep]
  (with-open [in-rdr (io/reader file)]
    (let [lines      (line-seq in-rdr)
          line-count (count lines)
          split-lines (map #(clojure.string/split % sep) lines)
          xs (into-array Double/TYPE (map #(Double/parseDouble (first %)) split-lines))
          ys (into-array Double/TYPE (map #(Double/parseDouble (second %)) split-lines))]
      [xs ys])))

(defn write-cols [file sep & cols]
  (with-open [out-w (io/writer file)]
    (let [split-lines (apply map vector cols)
          lines (map #(apply str (interpose sep %)) split-lines)]
      (doseq [line lines]
        (.write out-w (str line "\r\n"))))))


(defn read-transform-write [in-file out-subdir-name & transforms]
  (let [[xs ys] (read-cols in-file #"\t")
        [new-xs new-ys] ((apply comp (reverse transforms)) [xs ys])]
    (write-cols (file-in-subdir in-file out-subdir-name) "\t" new-xs new-ys)))

(defn negate-xs [[xs ys]]
  (doseq [i (range (count xs))]
    (aset-double xs i (- (aget xs i))))
  [xs ys])

(defn lr-beta [xs ys]
  (let [mean-x (mean xs)]
    (/ (- (mean (map #(* %1 %2) xs ys)) (* mean-x (mean ys)))
       (- (mean (map #(* % %) xs)) (* mean-x mean-x)))))

(defn lr-alpha [xs ys lr-beta]
  (- (mean ys) (* lr-beta (mean xs))))

(defn lr [xs a b]
  [xs (map #(+ a (* b %)) xs)])

(def ^:dynamic *draw-plot* false)

(defn subtract-background [[xs ys]]
  (let [end-window-size 150
        plot (when *draw-plot* (scatter-plot))]
    (when *draw-plot*
      (view plot)
      (add-lines plot (vec xs) (vec ys) ))
    (let [start-xs     (take end-window-size xs)
          start-ys     (take end-window-size ys)
          end-xs       (take-last end-window-size xs)
          end-ys       (take-last end-window-size ys)

          b-start      (lr-beta start-xs start-ys)
          a-start      (lr-alpha start-xs start-ys b-start)
          b-end        (lr-beta end-xs end-ys)
          a-end        (lr-alpha end-xs end-ys b-end)


          x-step-start (- (aget xs 0) (aget xs 1))
          x-step-end   (- (aget xs (- (count xs) 2)) (aget xs (- (count xs) 1)))

          bg1          (double-array (count xs))
          bg2          (double-array (count xs))
          _            (doseq [i (range end-window-size)]
                         (aset-double bg2 (- (count xs) (+ 1 i)) (+ a-end (* (aget xs (- (count xs) (+ 1 i))) b-end)))
                         (aset-double bg1 i (+ a-start (* (aget xs i) b-start))))

          alpha        0.001
          _            (doseq [i (range end-window-size (- (count xs) end-window-size))]
                         (let [pos                 (/ i (count xs))
                               x-step-here         (+ (* (- 1 pos) x-step-start) (* pos x-step-end))

                               start-gradient-here (* b-start x-step-here)
                               end-gradient-here   (* b-end x-step-here)

                               start-angle-here    (Math/atan start-gradient-here)
                               end-angle-here      (Math/atan end-gradient-here)

                               interp-pos          (/ (- (aget xs i) (aget xs end-window-size))
                                                      (- (aget xs (- (count xs) end-window-size)) (aget xs end-window-size)))

                               interp-angle        (+ (* (- 1 interp-pos) start-angle-here) (* interp-pos end-angle-here))
                               interp-gradient     (Math/tan interp-angle)]
                           (aset-double bg1 i (+ (* (- 1 alpha) (- (aget bg1 (- i 1)) interp-gradient)) (* alpha (aget ys i))))))

          _            (doseq [i (reverse (range end-window-size (- (count xs) end-window-size)))]
                         (let [pos                 (/ i (count xs))
                               x-step-here         (+ (* (- 1 pos) x-step-start) (* pos x-step-end))

                               start-gradient-here (* b-start x-step-here)
                               end-gradient-here   (* b-end x-step-here)

                               start-angle-here    (Math/atan start-gradient-here)
                               end-angle-here      (Math/atan end-gradient-here)

                               interp-pos          (/ (- (aget xs i) (aget xs end-window-size))
                                                      (- (aget xs (- (count xs) end-window-size)) (aget xs end-window-size)))

                               interp-angle        (+ (* (- 1 interp-pos) start-angle-here) (* interp-pos end-angle-here))
                               interp-gradient     (Math/tan interp-angle)]
                           (aset-double bg2 i (+ (* (- 1 alpha) (+ (aget bg2 (+ i 1)) interp-gradient)) (* alpha (aget ys i))))))
          _            (when *draw-plot*
                         (add-lines plot (vec xs) (vec bg1))
                         (add-lines plot (vec xs) (vec bg2)))

          bg           (double-array (count xs))
          _            (doseq [i (range end-window-size (- (count xs) end-window-size))]
                         (let [interp-pos (/ (- (aget xs i) (aget xs end-window-size))
                                             (- (aget xs (- (count xs) end-window-size)) (aget xs end-window-size)))]
                           (aset-double bg i (+ (* interp-pos (aget bg2 i)) (* (- 1 interp-pos) (aget bg1 i))))))
          _            (doseq [i (range end-window-size)]
                         (aset-double bg (- (count xs) (+ 1 i)) (aget bg2 (- (count xs) (+ 1 i))))
                         (aset-double bg i (aget bg1 i)))

          _            (when *draw-plot*
                         (add-lines plot (vec xs) (vec bg)))
          new-ys       (map #(- %1 %2) ys bg)]
      (when *draw-plot*
        (add-lines plot (vec xs) (vec new-ys)))
      [xs new-ys])))

;;-------------------------------

(def src-dir (File. "T:\\raman"))
(def ffile (first (list-all-files src-dir #".txt")))

(def peaks (File. "D:\\Dropbox\\Share with Anna H\\peaks.txt"))

(binding [*draw-plot* true]
  (read-transform-write peaks "out" subtract-background))


(time
 (dorun
  (pmap (fn [f]
         (read-transform-write f "out" subtract-background)
         (println "Processing" (.getName f)))
       (list-all-files src-dir #".txt"))))
