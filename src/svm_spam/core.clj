
(ns svm.core)
(use 'clj-ml.data)


  (def raw-data [["Grow your penis to 20 inches in just 1 week" :spam]
                 ["Buy Viagra cheap" :spam]
                 ["Cheap drugs with no prescriptions" :spam]
                 ["Originally you were invited by yassin captain to the beta product launch" :spam]
                 ["Order Today Super fast delivery" :spam]
  
                 ["One week ago I installed org-drill using the last version of org-mode" :ham]
                 ["Sorry you forgot to reply to the mailing list I am copying mailing list in my reply" :ham]
                 ["happy to announce the first release of the newest Clojure library" :ham]
                 ["Javascript Language Extensions for Clojurescript" :ham]
                 ["Fatih bla bla bla" :gay]
                 ["bla Fatih bla bla" :gay]
                 ["bla bla Fatih bla" :gay]
                 ["bla bla bla Fatih" :gay]
                 ["Once upon a time there was a man Nurullah" :kel]
                 ["This guy Nurullah is a kellest man in the world" :kel]
                 ["Nurullah sdfasdf" :kel]
                 ["asdfasd Nurulalh" :kel]])




(defn tokenize-str [str]
  (map clojure.string/lower-case (clojure.string/split str #"\s")))


(defn tokens []
  (->> raw-data
       (map #(tokenize-str (first %)))
       flatten
       (reduce (fn[h v] (assoc h v 0)) {})))

(defn parse-messages []
  (let [header (->> [(keys (tokens)) {:kind [:spam :ham :gay :kel]}]
                    flatten
                    (into []))
        tokens (tokens)
        messages (reduce (fn[ds [msg type]]
                            (let [msg (->> (tokenize-str msg)
                                           (reduce (fn[h v] (assoc h v 1)) {})
                                           (merge tokens))]
                              (conj ds (assoc msg :kind type)))) [] raw-data)]
    [header messages]))


(defn dataset []
  (let [[header messages] (parse-messages)
        dataset (make-dataset "SpamSet" header 9)]
    (dataset-set-class dataset (-> header count dec))
    (doseq [msg messages]
      (.add dataset (make-instance dataset msg)))
    dataset))


(def dataset (dataset))


(defn build-classifier [ds]
  (doto (weka.classifiers.functions.SMO.)
    (.buildClassifier ds)))


(def spam-svm (build-classifier dataset))


(defn classify [classyfier ds instance]
  (.classifyInstance classyfier instance)
  (keyword (.value (.attribute ds (.classIndex ds)) (.classifyInstance classyfier instance))))


(defn classify-message [dataset svm msg]
  (let [tokens (tokens)
        msg (->> msg
                 tokenize-str
                 (reduce (fn[h v]
                           (if (h v)
                             (assoc h v 1)
                             h)) tokens)
                 (make-instance dataset))]
    (classify svm dataset msg)))


;(comment
 ;   (classify-message dataset spam-svm "Last week I forgot to pick up the dog")
  ;  (classify-message dataset spam-svm "I will pick up the happy dog with cheap viagra")
   ; (classify-message dataset spam-svm "Nurullah")
   ; (classify-message dataset spam-svm "Fatih")\)
