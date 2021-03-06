(ns nn-sim.core
  (:gen-class)
  (:require [oz.core :as oz]
            [incanter.stats :as stats]
            [clojure.spec.alpha :as spec]
            [clojure.spec.gen.alpha :as gen]))

(def n 20)
(def periods 30)
(def highprior 0.8)
(def prob 0.2)
(def off-prob 0.2)
(def states [40 80])
(def top-offer 100)

;; spec
(spec/def :offer/investor (into #{} (range n)))
(spec/def :offer/type #{:bid :ask})
(spec/def :offer/price (into #{} (range top-offer)))
(spec/def :offer/accepted? true?)
(spec/def :nn/offer (spec/keys :req [:offer/investor :offer/type :offer/price]
                               :opt [:offer/accepted?]))

(spec/def :investor/risk-parameter (spec/and integer? #(> % 0))) ;; restricted to integer because floats were producing NaNs

(defn search-history
  ([history price offer-type compar]
   (count (filter (fn [offer] (and (= offer-type (:offer/type offer))
                                   (compar (:offer/price offer) price))) history)))
  ([history price offer-type compar accepted?]
   (search-history (filter #(= accepted? (:offer/accepted? %)) history)
                   price offer-type compar)))

(defn offer-likelihood [history offer-type price]
  (let [[compar other-type] (if (= offer-type :bid) [< :ask] [> :bid])
        search-fn (partial search-history history price)
        numerator (+ (search-fn offer-type = true) (search-fn other-type compar))]
    (/ numerator (+ numerator (search-fn offer-type (complement compar) false)))))

(defn profit [history offer-type price value]
  (* (offer-likelihood history offer-type price)
     (if (= offer-type :bid) (- value price) (- price value))))

(defn weighted-draw [s1 s2 prob]
  (let [n (* 10 prob)]
    (rand-nth (into
               (apply vector (take n (repeat s1)))
               (take (- 10 n) (repeat s2))))))

(defn states-and-probs [[s1 s2]]
  (let [n (* 10 prob)
        state (weighted-draw s1 s2 prob)
        comp (first (filter #(not= state %) states))
        signal-vec-fn (fn [] (weighted-draw comp state off-prob))]
    {:state state
     :pub-signal (signal-vec-fn) ;; this doesn't work yet. need cond. probs. from Panel D in Table 1.
     :pri-signal (signal-vec-fn)}))

(defn update-prior [investor signal]
  (if-let [prior (:prior investor)]
    prior
    highprior))

(defn utility
  ([rp w]
   (Math/pow (- (Math/E)) (* (- rp) w)))
  ([rp wl wh ph]
   (+ (* (- 1 ph) (utility rp wl)) (* ph (utility rp wh)))))

;; (defn value [phigh]
;;   (let [[ls hs] (sort states)]
;;     (+ (* (- 1 phigh) ls) (* phigh hs))))

(defn investor [i]
  {:index i
   :risk (gen/generate (spec/gen :investor/risk-parameter))
   :wealth 0})

(defn- current-bid-ask
  [mkt]
  (map (fn [k d] (or (k mkt) d)) [:bid :ask] [0 top-offer]))

(defn offer
  ([]
   (gen/generate (spec/gen :nn/offer)))
  ([^Integer investor ^clojure.lang.Keyword type ^Integer price]
   {:offer/investor investor
    :offer/type type
    :offer/price price})
  ([^Integer investor ^clojure.lang.Keyword type ^Integer price ^Boolean accepted?]
   (conj (offer investor type price) {:accepted? accepted?})))

;; Instead of the mkt checking offers to add, have the investors restrict their decision space to only the space between bid and ask.
;; Thanks Ying!

(defn top-choice [investor mkt]
  (let [[bid ask] (current-bid-ask mkt)
        [ls hs] (sort states)
        fns {:buy (fn [value o] (- value o))
             :sell (fn [value o] (- o value))}
        profits (apply into (mapv (fn [[k f]]
                                 (mapv (fn [o] [k
                                                o
                                                (utility (:risk investor)
                                                         (f ls o)
                                                         (f hs o)
                                                         (or (:prior investor) highprior))])
                                       (range bid ask)))
                               fns))]
    profits #_(first (reverse (sort-by #(val %) profits)))))

(defn add-offer
  "Add offer to history unless there is an outstanding bid/ask and the outstanding bid/ask is higher/lower than the offer price."
  [offer mkt]
  (let [{type :offer/type
         offer-price :offer/price} offer
        {outstanding-price type
         history :history} mkt]
    (cond
      (and outstanding-price ((if (= type :bid) > <) outstanding-price offer-price)) mkt
      :else (conj mkt {type offer-price
                       :history (conj history (offer type offer-price false))}))))


(defn accept-offer [mkt]
  (assoc mkt :history
         (conj (pop (:history mkt))
               (assoc (last (:history mkt)) :offer/accepted? true))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [mkt {:history (vec (take periods (repeatedly #(offer))))
             :investors (mapv investor (range n))}]
    mkt))
