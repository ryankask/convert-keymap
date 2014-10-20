(ns convert-keymap.core
  (:require [clojure.java.io :as io]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :as zxml]
            [clojure.data.xml :as xml]
            [clojure.string :as str])
  (:gen-class))

(def ^:const keystroke-attrs [:first-keystroke :second-keystroke])
(def ^:const qwerty-to-colemak
  {\E \F
   \R \P
   \T \G
   \Y \J
   \U \L
   \I \U
   \O \Y
   \P \;
   \S \R
   \D \S
   \F \T
   \G \D
   \J \N
   \K \E
   \L \I
   \; \O
   \N \K})

(def sample-keymap-file (io/resource "Mac OS X Emacs 1.xml"))

(defn parse-keymap [^java.io.File keymap-file]
  (-> keymap-file
      io/input-stream
      xml/parse))

(defn match-keyboard-shortcut? [loc]
  (let [tag (:tag (zip/node loc))]
    (= :keyboard-shortcut tag)))

(defn keystroke-replacer [[_ modifier ch]]
  (str modifier " " (or (-> ch
                            (.charAt 0)
                            qwerty-to-colemak)
                        ch)))

(defn replace-keystroke [keystroke]
  (str/replace-first keystroke #"(control|alt|meta) ([A-Z])$" keystroke-replacer))

(defn edit-shortcut [node]
  (assoc node :attrs (into {} (map (fn [keystroke]
                                     (when-let [k (keystroke (:attrs node))]
                                       [keystroke (replace-keystroke k)]))
                                   keystroke-attrs))))

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

(defn prettier-xml [xml-str]
  (-> xml-str
      (str/replace-first "?><" "?>\n<")
      (str/replace "\"/>" "\" />")))

(defn write-keymap [keymap-file new-keymap-xml]
  (with-open [output (-> keymap-file
                         make-new-keymap-file
                         io/writer)]
    (.write output (-> new-keymap-xml
                       xml/indent-str
                       prettier-xml))))

(defn convert-keymap [keymap-path]
  (let [keymap-file (io/file keymap-path)]
    (-> keymap-file
        parse-keymap
        convert-keyboard-shortcuts
        ((partial write-keymap keymap-file)))))

(defn -main [& args]
  (println "Hello, world!"))
