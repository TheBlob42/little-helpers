#!/usr/bin/env bb
(require '[babashka.process :refer [shell]])
(require '[babashka.fs :as fs])
(require '[clojure.string :as str])

; ~~~~~~~~
; COLORIZE
; ~~~~~~~~

; Check here for a collection of all available terminal escape codes:
; https://stackoverflow.com/a/33206814

(def escape-codes
  {:red 31
   :green 32
   :yellow 33
   :blue 34
   :magenta 35
   :cyan 36
   :bright-blue 94
   :bright-magenta 95
   :bright-cyan 96
   :bold 1
   :italic 3
   :underline 4
   :crossed-out 9})

(def dir-colors
  [:blue :magenta :cyan :bright-blue :bright-magenta :bright-cyan])

(defn fmt-string
  "Wrap the given string in the respective terminal escape codes indicated by mods.
  If the mods are empty or a mod does not exist the string is returned without any change."
  [s & mods]
  (if (or (empty? mods)
          (not-every? (partial get escape-codes) mods))
    s
    (str "\u001b["
          (str/join ";" (map (partial get escape-codes) mods))
          "m"
          s
          "\u001b[0m")))

(defn fmt-error [s] (fmt-string s :red))
(defn fmt-delete [s] (fmt-string s :red :crossed-out))
(defn fmt-modified [s] (fmt-string s :yellow :italic))
(defn fmt-add [s] (fmt-string s :green))
(defn fmt-dir
  [s level]
  (let [color (nth dir-colors (mod level (count dir-colors)))]
    (fmt-string s color)))

; ~~~~~~~
; UTILITY
; ~~~~~~~

(defn exit
  "Exit the current process with the given status code and printing the given success/error message."
  [status msg]
  (let [msg (if (not= 0 status) (fmt-error msg) msg)]
    (println msg)
    (System/exit status)))

(defn confirm
  "Show a confirmation prompt to the user that he has to answer with 'yes' or 'no' and return the result (true or false).
  After `max-tries` unsuccessful repetitions abort the process."
  [msg max-tries]
  (let [ask-question (fn []
                       (print (str msg " [yes/no]: "))
                       (flush)
                       (read-line))]
    (loop [retries (dec max-tries)
           answer (ask-question)]
      (cond
        (= answer "yes")
        true
      
        (= answer "no")
        false
      
        (<= retries 0)
        false
      
        :else
        (recur (dec retries) (ask-question))))))

(defn expand
  [dir]
  (str (fs/expand-home dir) "/"))

(defn append-to-file!
  [file s]
  (spit file (str s "\n") :append true))

(defn print-dir-to-file!
  ([file dir] (print-dir-to-file! file dir 0))
  ([file dir & [level]]
   (let [prefix (when (pos? level)
                  (fmt-dir (str (apply str (repeat level "  ")) "> ") (dec level)))]
     (doseq [[name value] dir]
       (if (map? value)
         (do
           (append-to-file! file (str prefix (fmt-dir (str name "/") level)))
           (print-dir-to-file! file value (inc level)))
         (condp = value
           :delete
           (append-to-file! file (str prefix (fmt-delete name)))
           :modified
           (append-to-file! file (str prefix (fmt-modified name)))
           :add
           (append-to-file! file (str prefix (fmt-add name)))
           ; else
           (println (str "ERROR: Unknown action value '" value "'"))))))))

(defn call-rsync!
  "Call the rsync command for the given sources and target."
  [source target {:keys [delete? dry-run?] :or {delete? false dry-run? true}}]
  (->> (shell {:out (if dry-run? :string :inherit)}
              (str "rsync "
                   "--verbose "
                   "--recursive "
                   "--human-readable "
                   ;; only check the modified time to speed up the comparison
                   "--times "
                   ;; output a change-summary for all updates (-i)
                   "--itemize-changes "
                   (when dry-run? "--dry-run ")
                   (when delete? "--delete ")
                   source
                   " "
                   target))))

(defn parse-element-into
  [m element]
  (let [action (cond
                 (str/starts-with? element "*deleting") :delete
                 (str/starts-with? element ">f+") :add
                 :else :modified)
        element (str/replace-first element #"[^ ]+ +" "")
        path (str/split element #"/")]
    (assoc-in m path action)))

(defn gen-sync-diff
  "Transforms the dry-run output of the rsync command into a proper diff format map structure.
  
  For example:

  (gen-sync-diff (call-rsync! sources target {:out :string :dry-run! true}))

  {:file-1 :add
   :file-2 :modified
   :dir-A {:file-3 :delete
           :file-4 :add}}

  Files map to an appropriate status keyword: :delete, :add or :modified.
  Directories map to another maps with the same structure representing its content."
  [rsync-output]
  (->> rsync-output
      :out
      str/split-lines
      (drop 1)                               ; drop header line
      (take-while (comp not empty?))         ; everything after the first paragraph is not interesting for us
      (filter #(not (str/ends-with? % "/"))) ; filter out directories
      (reduce parse-element-into {})
      (into (sorted-map))))

; ~~~~~~~~~~
; VALIDATION
; ~~~~~~~~~~

(defn validate-input
  [input]
  (let [[config target] input
        results [(when (not (fs/exists? config))
                   (str "The config file '" config "' does not exist"))
                 (when (not (fs/directory? target))
                  (str "The target location '" target "' does not exists or is not a directory"))]
        errors (remove nil? results)]
    (when (not-empty errors)
      (exit 1 (str/join \newline errors)))))

(when (not= 2 (count *command-line-args*))
  (exit 1 "Need exact two arguments (config & target location)"))
(validate-input *command-line-args*)

(def target-location (second *command-line-args*))

(defn prepare-config
  [config]
  (let [loc (if (str/ends-with? target-location "/")
              target-location
              (str target-location "/"))]
    (into [] (map (fn [{:keys [source target mode]}]
                    (let [target (if (str/starts-with? target "/")
                                  (subs target 1)
                                  target)]
                      {:source (expand source)
                       :target (expand (str loc target))
                       :mode mode}))
                  config))))

(defn validate-config
  [config]
  (let [results (for [{:keys [source target mode]} config]
                  [(when (not (fs/directory? source))
                    (str "Source '" source "' is not an existing directory"))
                   (when (not (#{:add :delete} mode))
                     "Mode can only be :add or :delete")])
        errors (remove nil? (flatten results))]
    (when (not-empty errors)
      (exit 1 (str/join \newline errors)))))

(def config (->> *command-line-args*
                 first
                 slurp
                 clojure.edn/read-string
                 prepare-config))

(validate-config config)

; ~~~~~~~~~~~~~~
; MAIN EXECUTION
; ~~~~~~~~~~~~~~

(def tmp-file "rsync-diff.tmp")

(let [sync-sources (map (fn [{:keys [source target mode] :as s}]
                          (assoc s :diff (gen-sync-diff (call-rsync! source target {:delete? (= mode :delete)
                                                                                    :dry-run? true}))))
                       config)]
  (if (every? (comp empty? :diff) sync-sources)
    (exit 0 (fmt-string "No changes, everything up to date" :bold :green))
    (doseq [{:keys [source target mode diff]} sync-sources]
      (append-to-file! tmp-file "")
      (append-to-file! tmp-file (fmt-string (str "Diff for '" source "' (mode: " mode ") to '" target "'") :underline))
      (append-to-file! tmp-file "")
      (if (empty? diff)
        (append-to-file! tmp-file (fmt-string (str "No changes for '" source "'") :cyan :bold))
        (print-dir-to-file! tmp-file diff)))))

;; use -R to let less handle escape codes
(shell "less" "-R" tmp-file)

(fs/delete tmp-file)

(flush)

(when (confirm "Syncing?" 5)
  (doseq [{:keys [source target mode]} config]
    (call-rsync! source target {:delete? (= mode :delete) :dry-run? false})))
