(ns convert-keymap.core
  (:require [clojure.java.io :as io]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :as zxml]
            [clojure.data.xml :as xml])
  (:gen-class))

(def sample-keymap-file (io/resource "Mac OS X Emacs 1.xml"))

(defn parse-keymap [^java.io.File keymap-file]
  (-> keymap-file
      io/input-stream
      xml/parse))

(defn match-keyboard-shortcut? [loc]
  (let [tag (:tag (zip/node loc))]
    (= :keyboard-shortcut tag)))

(defn edit-shortcut [node]
  (println (-> node :attrs :first-keystroke))
  node)

(defn convert-keyboard-shortcuts [keymap-xml]
  (let [keymap-zip (zip/xml-zip keymap-xml)]
    (loop [loc keymap-zip]
      (if (zip/end? loc)
        (zip/root loc)
        (recur (zip/next (if (match-keyboard-shortcut? loc)
                           (zip/edit loc edit-shortcut)
                           loc)))))))

(defn make-new-keymap-file [input-file]
  (-> input-file
      .toPath
      (#(.. % (resolveSibling (str (.getFileName %) ".converted"))))
      .toFile))

(defn write-keymap [keymap-file new-keymap-xml]
  (with-open [output (-> keymap-file
                         make-new-keymap-file
                         io/writer)]
    (xml/indent new-keymap-xml output)))

(defn convert-keymap [keymap-path]
  (let [keymap-file (io/file keymap-path)]
    (-> keymap-file
        parse-keymap
        convert-keyboard-shortcuts
        ((partial write-keymap keymap-file)))))

(defn -main [& args]
  (println "Hello, world!"))
