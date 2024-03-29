#!/usr/bin/env bb

(ns donate
  (:import [java.time Instant OffsetDateTime]
           [java.nio.file Path])
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.stacktrace :as st]
            [babashka.deps :as deps]
            [babashka.pods :as pods]
            [cheshire.core :as json]
            [org.httpkit.client :as http]
            [org.httpkit.server :as httpd]
            [taoensso.timbre :as log]))


(deps/add-deps
  '{:deps {honeysql/honeysql         {:mvn/version "1.0.461"}
           nilenso/honeysql-postgres {:mvn/version "0.4.112"}
           ring/ring                 {:mvn/version "1.9.0"}
           ring/ring-codec           {:mvn/version "1.2.0"}}})
(pods/load-pod 'org.babashka/go-sqlite3 "0.1.0")
(require
  '[pod.babashka.go-sqlite3 :as sqlite]
  '[honeysql.core :as sql]
  '[honeysql-postgres.format]
  '[hiccup.util :as hutil]
  '[hiccup2.core :as hi]
  '[ring.util.codec :as codec]
  '[ring.middleware.params :as ring-params])

(alter-var-root #'log/*config* #(assoc % :min-level :debug))


;;; i18n

(def TRANS
  {"Скануй та "    "Scan the code "
   "допоможи ЗСУ"  "and help Ukraine"
   "В середньому " "Average "
   "Максимум від " "Largest "
   "Зібрано "      "Raised "})


;;; Config

(def -me (Path/of (.toURI (io/file *file*))))

(defn rel [who path]
  (str (.resolveSibling who path)))


(def *opts (when *command-line-args* (apply assoc {} *command-line-args*)))

(comment
  (alter-var-root #'*opts (constantly {"--json" "donation.json"})))

(def config-path
  (let [path (get *opts "--cfg" ".config.edn")]
    (cond
      (.exists (io/file path))
      path

      (.exists (io/file (rel -me path)))
      (rel -me path)

      :else
      (throw (ex-info (str "Cannot find config " path) {:path path})))))


(def config
  (memoize
    (fn []
      (edn/read-string (slurp config-path)))))


(def dbpath (rel (Path/of (.toURI (io/file config-path))) (:db (config))))
(def lang (-> (config) :ui :lang))


;;; Core

(if (= lang :en)
  (defn t [s] (get TRANS s))
  (def t identity))


(defn q! [query]
  (let [query (if (map? query)
                (sql/format query)
                query)]
    (try
      (sqlite/query dbpath query)
      (catch Exception e
        (prn "ERROR QUERY" query)
        (throw e)))))


(defn o! [query] (first (q! query)))


(defn balance [kopeks]
  (- (quot (or kopeks 0) 100)
     (:start-balance (config) 0)))


(defn human-n [n]
  (.replace (format "%,d" (long n)) "," " "))


(defn z-zi-choose [n]
  (let [s (str n)]
    (if (= (first s) \1) "зі" "з")))


(defn sha1
  [s]
  (let [hashed (-> (java.security.MessageDigest/getInstance "SHA-1")
                   (.digest (.getBytes s)))]
    (str/join (map #(format "%02x" %) hashed))))


;;; Time

(def DAY (* 3600 24))
(def UAH "₴")

(defn now []
  (quot (System/currentTimeMillis) 1000))


(defn ->seconds
  "From 2022-06-06T16:54:30Z to unix timestamp"
  [s]
  (when s
    (let [i (if (str/ends-with? s "Z")
              (Instant/parse s)
              (.toInstant (OffsetDateTime/parse s)))]
      (.getEpochSecond i))))


;;; Logic

(defn mono! [method url & [data]]
  (let [full  (str "https://api.monobank.ua" url)
        res  @(http/request
                {:method  method
                 :url     full
                 :headers {"X-Token" (-> (config) :mono :token)}
                 :timeout 60000
                 :body    (some-> data json/generate-string)})
        data (-> res :body (json/parse-string true))]
    (if (= (:status res) 200)
      data
      (throw (ex-info (:errorDescription data "Unknown error!") res)))))


(defn pzh! [url params]
  (let [params (for [[k v] params]
                 {:id k :value [v]})
        full   (str
                 "https://report.comebackalive.in.ua/api/public/dashboard/e4f44bc7-05f4-459b-a10b-cc10e6637217/dashcard"
                 url
                 "?"
                 (codec/form-encode {:parameters (json/generate-string params)}))
        res    @(http/request
                  {:method  :get
                   :url     full
                   :timeout 60000})
        parsed (-> res :body (json/parse-string true))
        cols   (->> parsed :data :cols (mapv (comp keyword :name)))
        data   (mapv #(zipmap cols %) (-> parsed :data :rows))]
    (if (< (:status res) 300)
      data
      (throw (ex-info (:errorDescription data "Unknown error!") res)))))


(defn insert-txs [txs]
  (when-let [txs (not-empty (filter #(pos? (:amount %)) txs))]
    (q! {:insert-into :tx
         :values      txs
         :upsert      {:on-conflict   [:id]
                       :do-update-set (keys (first txs))}})
    (q! {:update :info
         :from   [[{:from     [:tx]
                    :select   [:tx.account
                               [(sql/call :sum :tx.amount) :balance]
                               [(sql/call :max :tx.created_at) :created_at]
                               [(sql/call :max :tx.updated_at) :updated_at]]
                    :group-by [:account]}
                   :data]]
         :set    {:balance    :data.balance
                  :balance_at :data.created_at
                  :updated_at :data.updated_at}
         :where [:= :info.account :data.account]})))


(def CURRENCY
  {980 "UAH"
   978 "EUR"
   840 "USD?"})


(defn manual []
  (or (-> (config) :manual :name) "manual"))


(defn parse-orig-amount [s]
  (when-let [[_ amt cur] (re-find #"\((\d+) ?([\w\p{Sc}]+)\)$" s)]
    (let [sign (get {"USD" "$" "EUR" "€" "UAH" "₴"} cur)]
      (if sign
        (str amt sign)
        (str amt " " cur)))))

(comment
  (parse-orig-amount "xxx (1 USD)"))


(defn mono-tx [account item]
  {:id         (:id item)
   :account    account
   :amount     (quot (:amount item) 100)
   :balance    (balance (:balance item))
   :desc       (:description item)
   :comment    (:comment item)
   :created_at (:time item)
   :updated_at (now)})


(defn update-mono! []
  (when-let [id (-> (config) :mono :account)]
    (let [res        (mono! :get "/personal/client-info")
          acc        (->> (concat (:accounts res) (:jars res))
                          (filter #(= (:id %) id))
                          first)
          since-time (or (->seconds (-> (config) :mono :since))
                         (- (now) (* 30 DAY)))
          items      (mono! :get (format "/personal/statement/%s/%s"
                                   id since-time))]
      (q! {:insert-into :info
           :values      [{:account    (:id acc)
                          :pan        (or (first (:maskedPan acc))
                                          (:title acc))
                          :send_id    (:sendId acc)
                          :iban       (:iban acc)
                          :balance    (balance (:balance acc))
                          :balance_at (now)
                          :updated_at (now)}]
           :upsert      {:on-conflict   [:account]
                         :do-update-set [:pan :send_id :iban
                                         :balance_at :updated_at]}})
      (insert-txs (mapv #(mono-tx id %) items)))))


(comment
  (mono! :get (format "/personal/statement/%s/%s"
               (-> (config) :mono :account)
               (- (now) DAY))))


(defn set-mono-webhook! []
  (let [url (str (:absolute (config)) "webhook")
        res (mono! :post "/personal/webhook"
              {:webhookUrl url})]
    (println
      (format (if (= "ok" (:status res))
                "Webhook was set to url %s"
                "Webhook was not set to url %s")
        url))))


(defn pzh-tx [account item]
  {:id          (sha1 (pr-str item))
   :account     account
   :amount      (:amount item)
   :orig_amount (parse-orig-amount (:comment item))
   :desc        (:comment item)
   :created_at  (->seconds (:date item))
   :updated_at  (now)})


(def TAG "9372d2ab")
(def DATE "b6f7e9ea")


(defn update-pzh! []
  (let [tag     (-> (config) :pzh :tag)
        params  {TAG  tag
                 DATE (-> (config) :pzh :date)}
        balance (pzh! "/2/card/2" params)
        items   (pzh! "/5/card/5" params)
        id      (str "pzh-" tag)]
    (q! {:insert-into :info
         :values      [{:account    id
                        :pan        tag
                        :balance    (:sum (first balance))
                        :balance_at (now)
                        :updated_at (now)}]
         :upsert      {:on-conflict   [:account]
                       :do-update-set [:balance :balance_at :updated_at]}})
    (insert-txs (mapv #(pzh-tx id %) items))))


(comment
  (update-pzh!))


(defn create-schema []
  (q! "CREATE TABLE IF NOT EXISTS info
         (account TEXT PRIMARY KEY,
          pan TEXT,
          send_id TEXT,
          iban TEXT,
          balance INT,
          balance_at DATETIME,
          updated_at DATETIME)")
  (q! "CREATE TABLE IF NOT EXISTS tx
         (id TEXT PRIMARY KEY,
          account TEXT,
          amount INT,
          orig_amount TEXT,
          balance INT,
          desc TEXT,
          comment TEXT,
          created_at DATETIME,
          updated_at DATETIME)"))


(defn get-stats []
  (let [accs     [(manual)
                  (str "pzh-" (-> (config) :pzh :tag))
                  (-> (config) :mono :account)]
        balance  (o! {:from   [:info]
                      :select [[(sql/call :sum :balance) :balance]]
                      :where  [:in :account accs]})
        sendid   (o! {:from   [:info]
                      :select [:send_id]
                      :where  [:= :account (-> (config) :mono :account)]})
        target   (:target-balance (config))
        maxvalue (o! {:from     [:tx]
                      :select   [:created_at
                                 :amount
                                 :desc
                                 :comment]
                      :where    [:in :account accs]
                      :order-by [[:amount :desc]
                                 :created_at]
                      :limit    1})
        avgvalue (o! {:from   [:tx]
                      :select [[(sql/call :avg :amount) :amount]]
                      :where  [:and
                               [:in :account accs]
                               [:> :amount 100]]})]
    {:balance (or (:balance balance) 0)
     :sendid  (:send_id sendid)
     :target  target
     :avg     (Math/round (or (:amount avgvalue) 0.0))
     :max     (:amount maxvalue)
     :maxname (some-> (:desc maxvalue) (str/replace #"^Від: " ""))}))


(defn write-json [path]
  (let [stats (get-stats)]
    (spit path (json/generate-string
                 {:target (:target stats)
                  :amount (:balance stats)
                  :avg    (:avg stats)
                  :max    (:max stats)
                  :text   {:target  (human-n (:target stats))
                           :amount  (human-n (:balance stats))
                           :avg     (human-n (:avg stats))
                           :max     (human-n (:max stats))
                           :maxname (:maxname stats)}}))))


;;; HTTP progress server

(def logo
  (hi/html
    [:svg
     {:fill "none", :viewbox "0 0 41 68", :height "2.43em", :width "1.46em"}
     [:path
      {:fill      "#FFCC00",
       :d         "M20.4511 0C18.8661 1.80226 17.8947 4.16692 17.8947 6.74889C17.997 12.4368 18.6872 18.112 18.7511 23.8C18.8789 29.0917 17.294 34.0639 15.3639 38.9083C14.712 40.2504 14.0218 41.5669 13.3188 42.8835L11.2737 42.4617C9.42028 42.091 8.21878 40.3015 8.58945 38.4609C8.92178 36.8376 10.3406 35.7256 11.9255 35.7128L12.6669 35.7895L11.0052 21.9083C10.4556 15.7218 7.27291 10.2895 2.56917 6.74892C1.75113 6.14817 0.894735 5.59854 0 5.10005V54.4638H11.4015C12.2579 59.0908 14.8015 63.1172 18.3805 65.8908C19.2368 66.466 19.9398 67.2457 20.4511 68.1532C20.9624 67.2457 21.6654 66.466 22.5218 65.8908C26.1008 63.1171 28.6444 59.0908 29.5007 54.4638H40.9023V5.10005C40.0075 5.59855 39.1511 6.14817 38.3331 6.74892C33.6293 10.2895 30.4466 15.7219 29.897 21.9083L28.2354 35.7895L28.9767 35.7128C30.5617 35.7256 31.9805 36.8376 32.3128 38.4609C32.6835 40.3015 31.482 42.091 29.6286 42.4617L27.5835 42.8835C26.8805 41.5669 26.1902 40.2504 25.5384 38.9083C23.6083 34.0639 22.0233 29.0918 22.1512 23.8C22.2151 18.112 22.9053 12.4368 23.0075 6.74889C23.0075 4.16693 22.0361 1.80226 20.4512 0H20.4511ZM3.41276 12.2196C5.62403 14.8015 7.10674 18.0226 7.54133 21.5759L8.909 33.0414C7.17065 33.9105 5.84133 35.4955 5.34283 37.4128H3.41276V12.2195V12.2196ZM37.4894 12.2196V37.4128H35.5594C35.0609 35.4955 33.7316 33.9106 31.9932 33.0414L33.3609 21.576C33.7955 18.0226 35.2782 14.8015 37.4894 12.2196V12.2196ZM20.4511 35.2526C21.3586 38.2436 22.624 41.0812 24.1834 43.7271C22.7007 44.1872 21.397 45.0692 20.4511 46.2451C19.5052 45.0564 18.2015 44.1872 16.7188 43.7271C18.2782 41.0812 19.5436 38.2436 20.4511 35.2526ZM3.41276 40.8256H5.34283C5.95637 43.1775 7.78419 45.0436 10.1233 45.6827L11.7594 46.0662C11.3248 47.6511 11.0819 49.3256 11.0819 51.0512H3.41279V40.8256L3.41276 40.8256ZM35.5593 40.8256H37.4894V51.0511H29.8203C29.8203 49.3256 29.5774 47.6511 29.1428 46.0662L30.7789 45.6827C33.1052 45.0436 34.9458 43.1774 35.5594 40.8256L35.5593 40.8256ZM15.0955 46.8459C17.1533 47.1399 18.7511 48.9038 18.7511 51.0512H14.4947C14.4947 49.594 14.712 48.188 15.0955 46.8459ZM25.8067 46.8459C26.1902 48.188 26.4075 49.594 26.4075 51.0512H22.1511C22.1511 48.9038 23.7488 47.1399 25.8067 46.8459ZM14.8781 54.4639H18.7511V61.6346C16.8721 59.6662 15.5044 57.1993 14.8781 54.4639ZM22.1511 54.4639H26.024C25.3977 57.1993 24.03 59.6662 22.1511 61.6346V54.4639Z",
       :clip-rule "evenodd",
       :fill-rule "evenodd"}]]))


(defn qr [sendid]
  (let [url (or (-> (config) :ui :donate-url)
                (format "https://send.monobank.ua/%s" sendid))]
    (hi/html
      [:a {:href   url
           :target "_blank"}
       [:img {:style {:width "100%"}
              :src   (str "https://chart.googleapis.com/chart?"
                       (codec/form-encode
                         {:cht  "qr"
                          :chs  "200x200"
                          :chl  url
                          :chld "M|2"}))}]])))


(defn base [content]
  (str
    (hi/html
      (hutil/raw-string "<!doctype html>\n")
      [:html
       [:head
        [:title "Progress Tracker"]
        ;;[:meta {:http-equiv "refresh" :content "1"}]
        [:link {:rel "icon" :href "data:;base64,="}]
        [:meta {:charset "utf-8"}]
        [:meta {:name "viewport" :content "width=device-width,initial-scale=1.0"}]
        ;;[:script {:src "http://localhost:3000/twinspark.js"}]
        [:script {:src "https://twinspark.js.org/static/twinspark.js"}]
        [:script (hiccup.util/raw-string "
twinspark.func({'retry-req': function(o) {
  var target = o.el;
  var e = o.input;
  // presumably e.type is `ts-req-error`
  var spec = 'wait ' + e.type + ', retry-req';
  // this is what makeReq returns
  var req = {el: target,
             event: null,
             url: e.detail.url,
             method: e.detail.opts.method,
             batch: e.detail.opts.batch};

  //console.log(o);
  setTimeout(function() {
    // set another retry action
    twinspark.action(target, e, spec, null);
    // retry the actual request
    twinspark.executeReqs([req]);
  }, 1000);
}});
")]

        [:style (hutil/raw-string "
html {font-family: Helvetica, Arial, sans-serif;
      font-size:   14px;
      line-height: 1.36em}

strong {color: white}

.embed {border-radius: 1rem;
        background:    #87CEEB;
        padding:       0.86em;
        padding-left:  0;}
.widget {background:  linear-gradient(360deg, rgba(38, 40, 44, 0.8) 0%,
                                             rgba(38, 40, 44, 0) 100%);
         color:       rgba(255, 255, 255, 0.8);
         position:    fixed; bottom:0; left:0; right:0;
         padding:     0.86em;
         min-height:  6.86em;
         font-size:   24px;
         line-height: 1.36em}

.icon {height: 1.2em; display: inline-block; vertical-align: middle;}
.pill-height {height: 1.48em;}
.pill {height:        1.48em;
       box-sizing:    border-box;
       background:    rgba(68, 190, 89, 0.5);
       display:       inline-block;
       padding:       0 0.57em;
       margin-left:   0.57em;
       border:        1px solid #44BE59;
       border-radius: 0.68em;}

.enable-shadow .shadow {text-shadow: 0 0 10px black;}
.flex {display: flex; align-items: center;}
.grow {flex-grow: 1;}
.justify-between {justify-content: space-between; }
.mt-1 {margin-top: 1rem;}
.ml-1 {margin-left: 1rem;}
.nowrap {white-space: nowrap;}
.overflow-hidden {overflow: hidden;}
.text-right {text-align: right;}

.ts-enter.pill {
  animation-name: animate-pop;
  animation-duration: 0.5s;
  animation-timing-function: cubic-bezier(.26, .53, .74, 1.48);
}

.ts-enter.pill.k1 {
  animation-name: animate-pop, animate-color-1;
}

@keyframes animate-pop {
  0%   { opacity: 0; transform: scale(0.5, 0.5); }
  100% { opacity: 1; transform: scale(1, 1); }
}

@keyframes animate-color-1 {
  0%, 30% { background: rgba(152, 190, 68, 0.5); }
  50%  { background: rgba(190, 68, 169, 0.5); }
  100% { background: rgba(152, 190, 68, 0.5); }
}
")]]
       [:body {} content]])))


(defn fireworks [x]
  [:div.pyro {:id x}
   [:style (hutil/raw-string "
.ts-enter.pyro > .before, .ts-enter.pyro > .after {
  position: absolute;
  width: 5px;
  height: 5px;
  border-radius: 50%;
  box-shadow: 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff, 0 0 #fff;
  animation: 1s bang ease-out 5 backwards, 1s gravity ease-in 5 backwards, 5s position linear 1 backwards;
  animation-iteration-count: 5, 5, 1;
}

.ts-enter.pyro > .after {
  animation-delay: 1.25s, 1.25s, 1.25s;
  animation-duration: 1.25s, 1.25s, 6.25s;
}

@keyframes bang {
  to {
    box-shadow: 68px -168px #009dff, 190px -193px #ff00bb, -240px 4px #00ff11, -197px -41px #ffcc00, -244px -271px #006fff, 141px -45px #ff00ae, -60px -91px #a200ff, -121px -156px #ff8800, -76px -382px #00ffcc, -172px -326px #00d0ff, -169px -358px #8800ff, -198px -305px #00ff1e, 214px -84px #008cff, 215px -172px #ae00ff, 28px 19px #ffcc00, 90px -265px #00ffd0, -246px -326px #0048ff, 149px 63px red, -107px -411px #ff9500, -149px -307px #ff4000, -160px 41px #00ffae, 143px 14px #ff8400, -142px -130px #ff00d5, -6px -87px #00d0ff, 64px -49px #ff6600, 88px -355px #59ff00, -163px -261px #55ff00, 141px -403px #ff8800, -7px 7px aqua, -52px -145px #ff0400, 214px -229px #d5ff00, 27px -108px blue, -105px 40px #ae00ff, 37px -136px #00a6ff, 231px -78px #ee00ff, 56px -22px #00ff04, 185px -229px #ff2f00, 201px -202px #ff0400, -221px -296px #3700ff, -87px -321px #44ff00, 71px 73px #00d9ff, -38px -285px #ff9500, -3px -258px red, 133px -340px #00ffc4, 77px -188px #00ff55, -127px -316px #ffe600, 54px -24px #ddff00, -223px -107px #0080ff, 84px -305px #0055ff, 93px -326px #00ffaa, -212px -171px #ffcc00;
  }
}

@keyframes gravity {
  to {
    transform: translateY(200px);
    opacity: 0;
  }
}

@keyframes position {
  0%, 19.9%  { margin-top: 10%; margin-left: 40%; }
  20%, 39.9% { margin-top: 40%; margin-left: 30%; }
  40%, 59.9% { margin-top: 20%; margin-left: 70%; }
  60%, 79.9% { margin-top: 30%; margin-left: 20%; }
  80%, 99.9% { margin-top: 30%; margin-left: 80%; }
}")]
   [:div.before]
   [:div.after]])


(defn donation-pill [{:keys [id amount orig_amount]}]
  (hi/html
    [:div.pill {:id    id
                :class (cond
                         (> amount 5000) "k5"
                         (> amount 1000) "k1")}
     [:strong (hutil/raw-string (if orig_amount
                                  (format "+&nbsp;%s" orig_amount)
                                  (format "+&nbsp;%d₴" (long amount))))]]))


(defn progress-bar [embed?]
  (let [stats     (get-stats)
        donations (q! {:from     [:tx]
                       :select   [:id :amount :orig_amount]
                       :where    [:> :amount 0]
                       :order-by [[:created_at :desc]]
                       :offset   0
                       :limit    5})
        progress  (* 100 (/ (float (:balance stats)) (:target stats)))]
    (hi/html
      [:div.ml-1.grow {:style (when-not embed? {:max-width "35em"})}

       [:div.justify-between.flex {:style {:margin-bottom "0.36em"}}

        ;; "Зібрано 69 420 ₴ / 100 000 ₴"
        [:div.shadow.nowrap
         (t "Зібрано ")
         [:strong (human-n (:balance stats)) " ₴"]
         " / " (human-n (:target stats)) " ₴"]

        [:div.overflow-hidden.text-right.pill-height
         #_(donation-pill {:id "qwe2" :amount 2000})
         (for [d donations]
           (donation-pill d))]]

       ;; progress bar
       [:div {:style {:height        "1.36em"
                      :background    "rgba(0, 0, 0, 0.5)"
                      :border-radius "0.68em"}}
        [:div {:id    :progress-bar
               :style {:height        "100%"
                       :border-radius "0.68em"
                       :position      "relative"
                       :background    (if (< progress 100)
                                        "#44BE59"
                                        (format "linear-gradient(to right, #44BE59 %s%%, #418265 0)"
                                          (/ 10000.0 progress)))
                       :display       "inline-block"
                       :width         (format "%s%%" (min 100 progress))
                       :transition    "width 0.7s ease"
                       :text-align    "right"}}

         [:strong {:style {:font-size "0.86em"
                           :margin    "0 0.57em"}}
          (when (> progress 100)
            [:img.icon {:style {:height "100%;"}
                        :src   "https://www.webfx.com/wp-content/themes/fx/assets/img/tools/emoji-cheat-sheet/graphics/emojis/godmode.png"}])
          (format "%.2f%%" progress)]

         (when (> progress 100)
           [:span {:style {:font-size     "0.86em"
                           :position      "absolute"
                           :right         (str (- 100 (/ 10000.0 progress)) "%")
                           :border-right  "solid #58D642 1px"
                           :padding-right "0.1em"}}
            (if (> progress 120)
              "100%"
              " ")])]]])))


(defn progress [req]
  (let [embed?    (contains? (:query-params req) "embed")
        debug?    (contains? (:query-params req) "debug")
        stats     (get-stats)
        pyro-step (-> (config) :ui (:pyro-step 50000))]
    {:status  200
     :headers {"Content-Type" "text/html"}
     :body
     (base
       (hi/html
         [:div#main {:ts-req          (str "progress"
                                        (when embed? "?embed=1")
                                        (when debug? "?debug=1"))
                     :ts-action       "wait ts-req-error, retry-req"
                     :ts-trigger      (cond
                                        debug? "click"
                                        embed? "load delay 60000"
                                        :else  "load delay 5000")
                     :ts-req-selector "#main"}

          #_(fireworks 3)
          (for [x (range (dec pyro-step) (:balance stats) pyro-step)]
            (fireworks x))

          [:div.flex {:class (if embed?
                               "embed"
                               "widget enable-shadow")
                      :style (cond-> "align-items: flex-end;"
                               embed? (str (-> (config) :ui :embed-style)))}

           (when-not embed?
             [:div.qr {:style {:height        "6.86em"
                               :width         "6.86em"
                               :border-radius "0.57em"
                               :background    "white"}}
              (qr (:sendid stats))])


           [:div.justify-between.grow
            {:class (when-not embed? "flex")
             :style {:max-width "100%"}}
            (when-not embed?
              [:div.title.flex.ml-1
               logo ;; Державний герб України
               [:span.ml-1.shadow
                (t "Скануй та ") [:br] (or (-> (config) :ui :donate-label)
                                           (t "допоможи ЗСУ"))]])

            (progress-bar embed?)

            ;; stats
            [:div.ml-1.shadow.nowrap {:class (if embed?
                                               "mt-1"
                                               "text-right")}
             [:div (t "В середньому ") [:strong (format "%d ₴" (:avg stats))]]
             [:div (t "Максимум від ")
              [:strong (:maxname stats)]
              " "
              [:strong (:max stats) " ₴"]]]]]]))}))


(defn input-t [content]
  (base
    (hi/html
      [:form {:method "post" :action "input"}
       [:input {:type "text" :name "amount" :placeholder "Сума, грн"}]
       [:input {:type "text" :name "desc" :placeholder "Відправник"}]
       [:input {:type "text" :name "orig"
                :placeholder "Оригінальна сума (eg. 120 usd)"}]
       [:button "Save"]]
      content)))


(defn input [{:keys [request-method form-params]}]
  (if (= request-method :post)
    (try
      (insert-txs [{:account    (manual)
                    :id         (str (random-uuid))
                    :amount     (Long. (get form-params "amount"))
                    :desc       (get form-params "desc")
                    :comment    (get form-params "orig")
                    :created_at (now)
                    :updated_at (now)}])
      (when-let [path (get *opts "--json")]
        (write-json path))
      {:status  301
       :headers {"Location" "input"}}
      (catch Exception e
        (prn "Error" (str e))
        {:status  400
         :headers {"Content-Type" "text/html"}
         :body    (input-t
                    (hi/html [:code [:pre (with-out-str
                                            (st/print-stack-trace e))]]))}))
    (let [txs (q! {:from   [:tx]
                   :select [:amount :desc :comment]
                   :where  [:= :account (manual)]})]
      {:status  200
       :headers {"Content-Type" "text/html"}
       :body    (input-t
                  [:table
                   [:tr [:th "Сума, грн"] [:th "Відправник"] [:th "Оригінальна сума"]]
                   (for [tx txs]
                     [:tr [:td (:amount tx)] [:td (:desc tx)] [:td (:comment tx)]])])})))


(defn webhook [req]
  (case (:request-method req)
    :get {:status 200
          :body   "ok"}
    :post
    (let [data    (:data (json/parse-stream
                           (io/reader (:body req) :encoding "UTF-8")
                           true))
          account (:account data)]
      (when (= account (-> (config) :mono :account))
        (insert-txs [(mono-tx account (:statementItem data))])
        (when-let [path (get *opts "--json")]
          (write-json path)))
      {:status 200
       :body   "ok"})))


(defn embed-js
  "Usage:

  <script src='https://mono.d.solovyov.net/embed.js' data-style='width: 100%;'></script>
  "
  [_req]
  (let [url (str (:absolute (config)) "progress?embed=1")]
    {:status  200
     :headers {"Content-Type" "application/javascript"}
     :body    (format "
(function(url) {
  var script = document.currentScript;

  var iframe = document.createElement('iframe');
  iframe.src = url;
  iframe.style = 'display: block; border: 0;' +
    script.getAttribute('data-style');

  script.insertAdjacentElement('afterend', iframe);
})('%s');
" url)}))


(defn -app [req]
  (case (:uri req)
    "/progress" (progress req)
    "/webhook"  (webhook req)
    "/input"    (input req)
    "/embed.js" (embed-js req)
    {:status 404
     :body   "Not Found\n"}))


(defn log-req [handler]
  (fn [req]
    (let [method (.toUpperCase (name (:request-method req)))
          res    (handler req)]
      (if (= method "GET")
        (log/debug method (:uri req) "-" (:status res))
        (log/info method (:uri req) "-" (:status res)))
      res)))


(def app (-> -app
             log-req
             ring-params/wrap-params))


(defn start-scheduler! []
  (let [stop (atom false)
        id   (format "s%x" (mod (System/currentTimeMillis)
                             1000000))
        t    (Thread.
               (fn []
                 (if @stop
                   (log/infof "scheduler %s: stop signal" id)

                   (do
                     (log/debugf "scheduler %s" id)
                     (try
                       (update-pzh!)
                       (catch Exception e
                         (log/error e "scheduler error")))
                     (try
                       (Thread/sleep (* 60 1000))
                       (catch InterruptedException _
                         (log/infof "scheduler %s: sleep interrupt" id)))
                     (recur)))))]
    (log/infof "scheduler %s: start" id)
    (.start t)
    (fn []
      (reset! stop true)
      (.interrupt t))))


(defonce *server nil)
(defonce *scheduler nil)


(comment
  ;; stop server
  (*server))


(defn -main [{:strs [--accounts --json --server --webhook]}]
  (when --accounts
    (let [res (mono! :get "/personal/client-info")]
      (doseq [acc (:accounts res)]
        (println (format "Account for card %s (%s %s), id %s"
                   (first (:maskedPan acc))
                   (:type acc)
                   (CURRENCY (:currencyCode acc))
                   (:id acc))))
      (doseq [acc (:jars res)]
        (println (format "Account for jar \"%s\", id %s"
                   (:title acc)
                   (:id acc)))))
    (System/exit 0))

  (create-schema)
  (when (-> (config) :mono :token)
    (update-mono!))
  (when (-> (config) :pzh :tag)
    (update-pzh!))

  (when --server
    (alter-var-root #'*server
      (fn [v]
        (when v (v))
        (let [ip   (:ip (config) "127.0.0.1")
              port (:port (config) 1301)]
          (println (format "running on http://%s:%s" ip port))
          (httpd/run-server app {:ip ip :port port}))))

    (when (-> (config) :pzh :tag)
      (alter-var-root #'*scheduler
        (fn [v]
          (when v (v))
          (start-scheduler!))))

    (when --webhook
      (set-mono-webhook!))
    @(promise)
    (System/exit 0))

  (when --json
    (write-json --json)))


(when (= *file* (System/getProperty "babashka.file"))
  (-main *opts))
