(ns enjoinder.core
  (:use [clojure.test]
        [korma.incubator.core :only [select where fields insert delete
                                     values sql-only]]
        [korma.db :only [defdb]]
        [fixdit.core :only [load-fixtures]]
        (lobos [connectivity :only [open-global close-global]])
        ring.mock.request))

(defn get-user-id [username]
  (:id (first (select users
                      (fields :id)
                      (where {:username username})))))

(defn delete-fixtures []
  (delete users))

(defn create-fixtures []
  (load-fixtures {:entity-namespace 'namespace.models.models
                  :yaml-file "resources/fixtures/fixtures.yaml"
                  :db-type :postgres}))

(defn fixture-func [f]
  (delete-fixtures)
  (create-fixtures)
  (f))

(defn delete-fixture-func [f]
  (delete-fixtures)
  (f))

(defn connection-fixture [f]
  (defdb mfgt-db test-db)
  (f))

(defn response-session-cookie [res]
  (nth (re-matches #"^(.*);Path=/$" 
                   (first (get-in res [:headers "Set-Cookie"])))
       1))

(defn wrap-logged-on-user [username password]
  (let [auth (app (-> (request :post "/login")
                      (body {:username username
                             :password password})))
        cookie (response-session-cookie auth)]
    (fn [req]
      (header req "cookie" cookie))))

