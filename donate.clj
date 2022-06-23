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

(alter-var-root #'log/*config* #(assoc % :min-level :info))


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
    (sqlite/query dbpath query)))


(defn o! [query] (first (q! query)))


(defn balance [kopeks]
  (- (quot kopeks 100)
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


(defn telegram! [token chat tx]
  (let [stats (str (t "Зібрано ")
                   (human-n (:balance tx)) " ₴"
                   " / " (human-n (:target-balance (config))) " ₴")
        full  (str "https://api.telegram.org/bot" token
                   "/sendMessage?chat_id=" chat
                   "&text=" (codec/form-encode (format "+ %d₴\n%s" (long (:amount tx)) stats)))
        res  @(http/request
                {:method  :post
                 :url     full
                 :timeout 60000})
        data (-> res :body (json/parse-string true))]
    (if (= (:status res) 200)
      data
      (throw (ex-info (:errorDescription data "Unknown error!") res)))))


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
          last-tx    (o! {:from   [:tx]
                          :select [[(sql/call :max :updated_at) :updated_at]]
                          :where  [:= :account id]})
          since-time (or (:updated_at last-tx)
                         (->seconds (-> (config) :mono :since))
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
                         :do-update-set [:pan :send_id :iban :balance
                                         :balance_at :updated_at]}})
      (when (seq items)
        (q! {:insert-into :tx
             :values      (mapv (partial mono-tx id) items)})))))


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
    (when (seq items)
      (let [txes (mapv (partial pzh-tx id) items)]
        (q! {:insert-into :tx
             :values      txes
             :upsert      {:on-conflict   [:id]
                           :do-update-set (keys (first txes))}})))))

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
                               [:> :amount 0]]})]
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
        [:script {:src "https://kasta-ua.github.io/twinspark-js/twinspark.js"}]
        [:script (hiccup.util/raw-string "
twinspark.func({retry: function(o) {
  //console.log(o);
  setTimeout(function() {
    twinspark.action(o.el, o.input, o.el.getAttribute('ts-action'), null);
    twinspark.executeReqs([twinspark.makeReq(o.el)]);
  }, 1000);
}});
")]

        [:style "
html {font-family: Helvetica, Arial, sans-serif;
      font-size: 14px;
      line-height: 1.36em}

strong {color: white}

.embed {border-radius: 1rem;
        background: #87CEEB;
        padding: 0.86em;}
.widget {background: linear-gradient(360deg, rgba(38, 40, 44, 0.8) 0%,
                                           rgba(38, 40, 44, 0) 100%);
         color: rgba(255, 255, 255, 0.8);
         position: fixed; bottom:0; left:0; right:0;
         padding: 0.86em;
         min-height: 6.86em;
         font-size: 24px;
         line-height: 1.36em}

.enable-shadow .shadow { text-shadow: 0 0 10px black; }
.flex {display: flex; align-items: center;}
.grow {flex-grow: 1;}
.justify-between {justify-content: space-between; }
.mt-1 {margin-top: 1rem;}
.ml-1 {margin-left: 1rem;}
.nowrap {white-space: nowrap;}
.overflow-hidden {overflow: hidden;}
.text-right {text-align: right;}

.ts-enter.pill {
  animation: animate-pop 0.5s;
  animation-timing-function: cubic-bezier(.26, .53, .74, 1.48);
}

@keyframes animate-pop {
  0%   { opacity: 0; transform: scale(0.5, 0.5); }
  100% { opacity: 1; transform: scale(1, 1); }
}
"]]
       [:body {} content]])))


(defn donation-pill [{:keys [id amount orig_amount]}]
  (hi/html
    [:div.pill {:id    id
                :style {:height        "1.36em"
                        :display       "inline-block"
                        :padding       "0 0.57em"
                        :margin-left   "0.57em"
                        :background    "rgba(68, 190, 89, 0.5)"
                        :border        "1px solid #44BE59"
                        :border-radius "0.68em"}}
     [:strong (hutil/raw-string (if orig_amount
                                  (format "+&nbsp;%s" orig_amount)
                                  (format "+&nbsp;%d₴" (long amount))))]]))


(defn progress-bar [embed?]
  (let [stats     (get-stats)
        donations (q! {:from     [:tx]
                       :select   [:id :amount :orig_amount]
                       :order-by [[:created_at :desc]]
                       :offset   0
                       :limit    5})
        progress  (* 100 (/ (float (:balance stats)) (:target stats)))]
    (hi/html
      [:div.ml-1.grow {:style (when-not embed? {:max-width "33.75em"})}

       [:div.justify-between.flex {:style {:margin-bottom "0.36em"}}

        ;; "Зібрано 69 420 ₴ / 100 000 ₴"
        [:div.shadow.nowrap
         (t "Зібрано ")
         [:strong (human-n (:balance stats)) " ₴"]
         " / " (human-n (:target stats)) " ₴"]

        [:div.nowrap.overflow-hidden
         (for [d donations]
           (donation-pill d))]]

       ;; progress bar
       [:div {:style {:height        "1.36em"
                      :background    "rgba(0, 0, 0, 0.5)"
                      :border-radius "0.68em"}}
        [:div {:id    :progress-bar
               :style {:height        "100%"
                       :border-radius "0.68em"
                       :background    "#44BE59"
                       :display       "inline-block"
                       :width         (format "%s%%" progress)
                       :transition    "width 0.5s ease"
                       :text-align    "right"
                       :padding       "0 0.57em"}}
         [:strong {:style {:font-size "0.86em"}}
          (format "%.2f%%" progress)]]]])))


(defn progress [req]
  (let [embed? (contains? (:query-params req) "embed")
        stats  (get-stats)]
    {:status  200
     :headers {"Content-Type" "text/html"}
     :body
     (base
       (hi/html
         [:div#show.flex {:class           (if embed?
                                             "embed"
                                             "widget enable-shadow")
                          :ts-req          (str "progress"
                                             (when embed?
                                               "?embed=1"))
                          :ts-action       "wait ts-req-error, retry"
                          :ts-trigger      (if embed?
                                             "load delay 60000"
                                             "load delay 1000")
                          ;;:ts-trigger      "click"
                          :ts-req-selector "#show"
                          :style           {:align-items "flex-end"}}

          (when-not embed?
            [:div.qr {:style {:height        "6.86em"
                              :width         "6.86em"
                              :border-radius "0.57em"
                              :background    "white"}}
             (qr (:sendid stats))])


          [:div.justify-between.grow {:class (when-not embed? "flex")}
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
             [:strong (:max stats) " ₴"]]]]]))}))


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
      (o! {:insert-into :tx
           :values
           [{:account    (manual)
             :id         (str (random-uuid))
             :amount     (Long. (get form-params "amount"))
             :desc       (get form-params "desc")
             :comment    (get form-params "orig")
             :created_at (now)
             :updated_at (now)}]})
      (o! {:insert-into :info
           :values      [{:account    (manual)
                          :balance_at (now)
                          :updated_at (now)
                          :balance    {:from   [:tx]
                                       :select [(sql/call :sum :amount)]
                                       :where  [:= :account (manual)]}}]
           :upsert      {:on-conflict   [:account]
                         :do-update-set [:balance_at :updated_at :balance]}})
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


(defn webhook [req]
  (case (:request-method req)
    :get {:status 200
          :body "ok"}
    :post
    (let [data    (:data (json/parse-stream
                           (io/reader (:body req) :encoding "UTF-8")
                           true))
          account (:account data)]
      (when (= account (-> (config) :mono :account))
        (let [tx (mono-tx account (:statementItem data))]
          (q! {:insert-into :tx
               :values      [tx]})
          (q! {:update :info
               :set    {:balance    (:balance tx)
                        :balance_at (:created_at tx)}
               :where  [:and
                        [:= :account account]
                        [:or
                         [:< :balance_at (:created_at tx)]
                         [:= :balance_at nil]]]})
          (when-let [token (-> (config) :telegram :token)]
            (when-let [chat (-> (config) :telegram :chat)]
              (telegram! token chat tx))))
        (when-let [path (get *opts "--json")]
          (write-json path)))
      {:status 200
       :body "ok"})))


(defn -app [req]
  (let [method (.toUpperCase (name (:request-method req)))]
    (if (= method "GET")
      (log/debug method (:uri req))
      (log/info method (:uri req))))
  (case (:uri req)
    "/progress" (progress req)
    "/webhook"  (webhook req)
    "/input"    (input req)
    "/embed.js" (embed-js req)
    {:status 404
     :body   "Not Found\n"}))

(def app (-> -app
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

