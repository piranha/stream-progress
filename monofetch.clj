#!/usr/bin/env bb

(ns monofetch
  (:import [java.time Instant]
           [java.nio.file Path])
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [babashka.deps :as deps]
            [babashka.pods :as pods]
            [cheshire.core :as json]
            [org.httpkit.client :as http]
            [org.httpkit.server :as httpd]
            [taoensso.timbre :as log]))


(deps/add-deps
  '{:deps {honeysql/honeysql         {:mvn/version "1.0.461"}
           nilenso/honeysql-postgres {:mvn/version "0.4.112"}
           hiccup/hiccup             {:mvn/version "2.0.0-alpha2"}
           ring/ring-codec           {:mvn/version "1.2.0"}}})
(pods/load-pod 'org.babashka/go-sqlite3 "0.1.0")
(require
  '[pod.babashka.go-sqlite3 :as sqlite]
  '[honeysql.core :as sql]
  '[honeysql-postgres.format]
  '[hiccup.util :as hutil]
  '[hiccup2.core :as hi]
  '[ring.util.codec :as codec])


(alter-var-root #'log/*config* #(assoc % :min-level :info))


;;; Core

(def -me (Path/of (.toURI (io/file *file*))))

(defn rel-to-me [path]
  (str (.resolveSibling -me path)))


(def *opts (when *command-line-args* (apply assoc {} *command-line-args*)))

(comment
  (alter-var-root #'*opts (constantly {"--json" "donation.json"})))


(defn config []
  (let [path (get *opts "--cfg" ".config.edn")]
    (edn/read-string (slurp (rel-to-me path)))))


(defn q! [query]
  (let [query (if (map? query)
                (sql/format query)
                query)
        dbpath (rel-to-me (:db (config) "mono.db"))]
    (sqlite/query dbpath query)))


(defn o! [query] (first (q! query)))


(defn req! [method url & [data]]
  (let [res  @(http/request
                {:method  method
                 :url     (str "https://api.monobank.ua" url)
                 :headers {"X-Token" (:token (config))}
                 :timeout 60000
                 :body    (some-> data json/generate-string)})
        data (-> res :body (json/parse-string true))]
    (if (= (:status res) 200)
      data
      (throw (ex-info (:errorDescription data "Unknown error!") res)))))


(defn balance [in-balance]
  (- (quot in-balance 100)
     (:start-balance (config))))


(defn human-n [n]
  (.replace (format "%,d" n) "," " "))


(defn z-zi-choose [n]
  (let [s (str n)]
    (if (= (first s) \1) "зі" "з")))


;;; Time

(def DAY (* 3600 24))
(def UAH "₴")

(defn now []
  (quot (System/currentTimeMillis) 1000))


;;; Logic


(defn make-tx [account item]
  {:id         (:id item)
   :account    account
   :amount     (quot (:amount item) 100)
   :balance    (balance (:balance item))
   :desc       (:description item)
   :comment    (:comment item)
   :created_at (:time item)
   :updated_at (now)})

(defn update-info! []
  (let [account    (:account (config))
        accs       (->> (:accounts (req! :get "/personal/client-info"))
                        (filter #(= (:id %) account)))
        last-tx    (o! {:from   [:tx]
                        :select [[(sql/call :max :updated_at) :updated_at]]})
        since-time (or (:updated_at last-tx)
                       (some-> (:since (config))
                         Instant/parse
                         .getEpochSecond)
                       (- (now) (* 30 DAY)))
        items      (req! :get (format "/personal/statement/%s/%s"
                                account since-time))]
    (q! {:insert-into :info
         :values      (for [acc accs]
                        {:account    (:id acc)
                         :pan        (first (:maskedPan acc))
                         :send_id    (:sendId acc)
                         :iban       (:iban acc)
                         :balance    (balance (:balance acc))
                         :balance_at (now)
                         :updated_at (now)})
         :upsert      {:on-conflict   [:account]
                       :do-update-set [:pan :send_id :iban :balance
                                       :balance_at :updated_at]}})
    (when (seq items)
      (q! {:insert-into :tx
           :values      (mapv (partial make-tx account) items)}))))

(comment
  (req! :get (format "/personal/statement/%s/%s"
               (:account (config))
               (- (now) DAY))))


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
          balance INT,
          desc TEXT,
          comment TEXT,
          created_at DATETIME,
          updated_at DATETIME)"))


(defn get-stats []
  (let [info     (o! {:from   [:info]
                      :select [:balance]
                      :where  [:= :account (:account (config))]})
        target   (:target-balance (config))
        maxvalue (o! {:from     [:tx]
                      :select   [:created_at
                                 :amount
                                 :desc
                                 :comment]
                      :where    [:= :account (:account (config))]
                      :order-by [[:amount :desc]
                                 :created_at]
                      :limit    1})
        avgvalue (o! {:from   [:tx]
                      :select [[(sql/call :avg :amount) :amount]]
                      :where  [:and
                               [:= :account (:account (config))]
                               [:> :amount 0]]})]
    {:balance (:balance info)
     :target  target
     :avg     (Math/round (:amount avgvalue))
     :max     (:amount maxvalue)
     :maxname (some-> (:desc maxvalue) (str/replace #"^Від: " ""))}))


(defn write-json [path]
  (let [stats (get-stats)]
    (spit path (json/generate-string
                 {:target  (human-n (:target stats))
                  :amount  (human-n (:balance stats))
                  :avg     (human-n (:avg stats))
                  :max     (human-n (:max stats))
                  :maxname (:maxname stats)}))))


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

(def qr
  (hi/html
    [:img {:style {:width "100%"}
           :src   (str "https://chart.googleapis.com/chart?"
                    (codec/form-encode
                      {:cht  "qr"
                       :chs  "200x200"
                       :chl  "https://fwdays.com/event/stream-zaharchenko#donate"
                       :chld "M|2"}))}]))


(defn donation-pill [{:keys [id amount]}]
  (hi/html
    [:div {:id    id
           :style {:height        "1.36em"
                   :display       "inline-block"
                   :padding       "0 0.57em"
                   :margin-left   "0.57em"
                   :background    "rgba(68, 190, 89, 0.5)"
                   :border        "1px solid #44BE59"
                   :border-radius "0.68em"}}
     [:strong (hutil/raw-string (format "+&nbsp;%d&nbsp;₴" amount))]]))


(defn progress-bar []
  (let [stats     (get-stats)
        donations (q! {:from     [:tx]
                       :select   [:id :amount]
                       :order-by [[:created_at :desc]]
                       :offset   0
                       :limit    5})
        progress  (* 100 (/ (float (:balance stats)) (:target stats)))]
    (hi/html
      [:div.ml-1.grow {:style {:max-width "33.75em"}}

       [:div.justify-between.flex {:style {:margin-bottom "0.36em"}}

        ;; "Зібрано 69 420 ₴ / 100 000 ₴"
        [:div.shadow.nowrap
         "Зібрано "
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
  (let [stats (get-stats)]
    {:status  200
     :headers {"Content-Type" "text/html"}
     :body
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

           [:style "
html {font-family: Helvetica, Arial, sans-serif; color: rgba(255, 255, 255, 0.8);
      font-size: 28px;
      line-height: 1.36em}

strong {color: white}

.bg {background: linear-gradient(360deg, rgba(38, 40, 44, 0.8) 0%, rgba(38, 40, 44, 0) 100%);
     position: fixed; bottom:0; left:0; right:0;
     padding: 0.86em;
     min-height: 6.86em;}

.shadow { text-shadow: 0 0 10px black; }
.flex {display: flex; align-items: center;}
.grow {flex-grow: 1;}
.justify-between {justify-content: space-between; }
.ml-1 {margin-left: 1rem;}
.nowrap {white-space: nowrap;}
.overflow-hidden {overflow: hidden;}

.ts-enter {
  animation: animate-pop 0.3s;
  animation-timing-function: cubic-bezier(.26, .53, .74, 1.48);
}

@keyframes animate-pop {
  0%   { opacity: 0; transform: scale(0.5, 0.5); }
  100% { opacity: 1; transform: scale(1, 1); }
}
"]
           ]
          [:body {:ts-req          "progress"
                  :ts-trigger      "load delay 1000"
                  ;;:ts-trigger      "click"
                  :ts-req-selector "body"}
           [:div.bg.flex {:style {:align-items "flex-end"}}
            [:div.qr {:style {:height        "6.86em"
                              :width         "6.86em"
                              :border-radius "0.57em"
                              :background    "white"}}
             qr]


            [:div.flex.justify-between.grow
             [:div.title.flex.ml-1
              logo ;; Державний герб України

              [:span.ml-1.shadow
               "Скануй та " [:br] "допоможи ЗСУ"]]

             (progress-bar)

             ;; stats
             [:div.ml-1.shadow.nowrap
              {:style {:text-align "right"}}
              [:div "В середньому " [:strong (format "%d ₴" (:avg stats))]]
              [:div "Максимум від "
               [:strong (:maxname stats)]
               " "
               [:strong (format "%s ₴" (:max stats))]]]]]]]))}))


(defn webhook [req]
  (let [data    (:data (json/parse-stream
                         (io/reader (:body req) :encoding "UTF-8")
                         true))
        account (:account data)]
    (when (= account (:account (config)))
      (let [tx (make-tx account (:statementItem data))]
        (q! {:insert-into :tx
             :values      [tx]})
        (q! {:update :info
             :set    {:balance    (:balance tx)
                      :balance_at (:created_at tx)}
             :where  [:and
                      [:= :account account]
                      [:or
                       [:< :balance_at (:created_at tx)]
                       [:= :balance_at nil]]]}))
      (when-let [path (get *opts "--json")]
        (write-json path)))
    {:status 200
     :body   "ok"}))


(defn app [req]
  (let [method (.toUpperCase (name (:request-method req)))]
    (if (= method "GET")
      (log/debug method (:uri req))
      (log/info method (:uri req))))
  (case (:uri req)
    "/progress" (progress req)
    "/webhook"  (webhook req)
    {:status 404
     :body   "Not Found\n"}))


(defonce *server nil)


(defn -main [{:strs [--json --server --webhook]}]
  (create-schema)
  (update-info!)

  (when --webhook
    (req! :post "/personal/webhook"
      {:webhookUrl (:webhook (config))}))

  (when --json
    (write-json --json))

  (when --server
    (alter-var-root #'*server
      (fn [v]
        (when v (v))
        (let [port (:port (config) 1301)]
          (println (str "running on http://127.0.0.1:" port))
          (httpd/run-server app {:port port}))))
    @(promise)))


(when (= *file* (System/getProperty "babashka.file"))
  (-main *opts))
