#!/usr/bin/env bb

(ns monofetch
  (:require [clojure.edn :as edn]
            [babashka.deps :as deps]
            [babashka.pods :as pods]
            [cheshire.core :as json]
            [org.httpkit.client :as http]
            [org.httpkit.server :as httpd]
            [clojure.string :as str]))


(deps/add-deps
  '{:deps {honeysql/honeysql         {:mvn/version "1.0.461"}
           nilenso/honeysql-postgres {:mvn/version "0.4.112"}
           hiccup/hiccup             {:mvn/version "2.0.0-alpha2"}}})
(pods/load-pod 'org.babashka/go-sqlite3 "0.1.0")
(require
  '[pod.babashka.go-sqlite3 :as sqlite]
  '[honeysql.core :as sql]
  '[honeysql-postgres.format]
  '[hiccup.util :as hutil]
  '[hiccup2.core :as hi])

;;; Core

(defn config []
  (edn/read-string (slurp ".config.edn")))


(defn q! [query]
  (let [query (if (map? query)
                (sql/format query)
                query)]
    (sqlite/query (:db (config) "mono.db") query)))


(defn o! [query] (first (q! query)))


(defn req! [url]
  (let [res @(http/request
               {:method  :get
                :url     (str "https://api.monobank.ua/" url)
                :headers {"X-Token" (:token (config))}
                :timeout 60000})
        data (-> res :body (json/parse-string true))]
    (if (= (:status res) 200)
      data
      (throw (ex-info (:errorDescription data "Unknown error!") res)))))


;;; Time

(def DAY (* 3600 24))

(defn now []
  (quot (System/currentTimeMillis) 1000))


;;; Logic

(defn update-info! []
  (let [account (:account (config))
        accs    (->> (:accounts (req! "/personal/client-info"))
                     (filter #(= (:id %) account)))
        last-tx (q! {:from   [:tx]
                     :select [[(sql/call :max :updated_at) :updated_at]]})
        txs     (req! (format "/personal/statement/%s/%s"
                        account (or (:updated_at last-tx)
                                    (- (now) (* 30 DAY)))))]
    (q! {:insert-into :info
         :values      (for [acc accs]
                        {:account    (:id acc)
                         :pan        (first (:maskedPan acc))
                         :send_id    (:sendId acc)
                         :iban       (:iban acc)
                         :balance    (quot (:balance acc) 100)
                         :updated_at (now)})
         :upsert      {:on-conflict   [:account]
                       :do-update-set [:pan :send_id :iban :balance]}})
    (q! {:insert-into :tx
         :values      (for [tx txs]
                        {:id         (:id tx)
                         :account    account
                         :amount     (quot (:amount tx) 100)
                         :balance    (quot (:balance tx) 100)
                         :desc       (:description tx)
                         :comment    (:comment tx)
                         :created_at (:time tx)
                         :updated_at (now)})})))

(comment
  (req! (format "/personal/statement/%s/%s"
                        (:account (config))
                        (- (now) DAY))))


(defn create-schema []
  (q! "CREATE TABLE IF NOT EXISTS info
         (account TEXT PRIMARY KEY,
          pan TEXT,
          send_id TEXT,
          iban TEXT,
          balance INT,
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


;;; HTTP progress server


(defn human-n [n]
  (.replace (format "%,d" n) "," " "))


(defn z-zi-choose [n]
  (let [s (str n)]
    (if (= (first s) \1) "зі" "з")))


(defn progress-bar [value target]
  (let [progress (* 100 (/ (float value) target))]
    [:div.progress-bar
     [:style "
.bar {width: 100%; background-color: #ffcccb; display: flex;}
.fill {background-color: #41D150; color: white; text-shadow: 0 0 1px black; font-weight: bold; text-align: right; padding: 0.3rem}
"]
     [:div.info.flex.justify-between.pb-5
      [:div (human-n value) " " (z-zi-choose target) " " (human-n target) " грн"]
      [:div.right "Send: https://bit.ly/somewhere"]]
     [:div.bar
      [:div.fill {:style (format "width: %s%%" progress)}
       (format "%.2f%%" progress)]]]))


(defn progress [req]
  (let [info     (o! {:from   [:info]
                      :select [:balance]
                      :where  [:= :account (:account (config))]})
        target   (-> (quot (:balance info) 100000) inc (* 100000))
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
           [:style "
html {font-family: Helvetica, Arial, sans-serif; font-size: 2rem;}
.content {background-color: rgba(0,0,0,.5); color: white; text-shadow: 0 0 1px black; padding: 1rem 2rem;}
.bottom {position: fixed; left: 0; right: 0; bottom: 0;}

.wide {letter-spacing: .1rem;}
.right {float: right;}
.flex {display: flex;}
.grow {flex-grow: 1;}
.justify-between {justify-content: space-between; }
.pb-5 {padding-bottom: .5rem;}
.pl-1 {padding-left: 1rem;}
.nowrap {white-space: nowrap;}
"]
           ]
          [:body
           [:div.content.bottom.flex
            [:div.grow
             (progress-bar (:balance info) target)]
            [:div.nowrap.pl-1
             [:div.pb-5 (format "avg %.0f грн" (:amount avgvalue))]
             [:div (format "max %d грн" (:amount maxvalue))
              [:br]
              (:desc maxvalue)]]]]]))}))


(defn app [req]
  (case (:uri req)
    "/progress" (progress req)
    {:status 404
     :body "Not Found\n"}))


(defonce *server nil)


(defn -main []
  (create-schema)
  (update-info!)
  (alter-var-root #'*server
    (fn [v]
      (when v (v))
      (let [port (:port (config) 1301)]
        (println (str "running on http://127.0.0.1:" port))
        (httpd/run-server app {:port port}))))
  @(promise))


(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
