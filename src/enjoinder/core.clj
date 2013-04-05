(ns enjoinder.core
  (:use [clojure.test]
        [clojure.pprint :only [pprint]]
        [clojure.set :only [difference subset?]]
        [clojure.walk :only [postwalk]]
        [korma.incubator.core :only [select where fields insert delete
                                     values sql-only with-object
                                     order]]
        [fixdit.core :only [load-fixtures]]
        (lobos [connectivity :only [open-global close-global]])
        ring.mock.request))

(comment given
         (deftest users-roles-join
           (let [u_id (get-user-id "tom")
                 r_id (get-role-id "manager")
                 u (first (select users
                                  (where {:id u_id})
                                  (with-object roles
                                    (order :id))))
                 t (first (select roles
                                  (where {:id r_id})
                                  (with-object users)))]
             (is (= r_id (get-in u [:roles 0 :id])))
             (is (= u_id (get-in t [:users :id])))))
         
         (defn get-user-id [username]
           (:id (first (select users
                               (fields :id)
                               (where {:username username})))))
         
         (defn get-role-id [name]
           (:id (first (select roles
                               (fields :id)
                               (where {:name name}))))))

(defmacro one-many-relation-test [name
                                  rel1 id-exp1
                                  rel2 id-exp2]
  `(deftest ~name
     (let [id1# ~id-exp1
           id2# ~id-exp2
           o1# (first (select ~rel1
                              (where {:id id1#})
                              (with-object ~rel2
                                (order :id))))
           o2# (first (select ~rel2
                              (where {:id id2#})
                              (with-object ~rel1
                                (order :id))))]
       (is (not (nil? o1#))
           ~(str rel1 " instance exists"))
       (is (not (nil? o2#))
           ~(str rel2 " instance exists"))
       (is (some #(= (:id %) id2#) (get o1# ~(keyword rel2)))
           ~(str rel2 " instance in " rel1))
       (is (= id1# (get-in o2# [~(keyword rel1) :id]))
           ~(str rel1 " instance in " rel2)))))

(defmacro many-many-relation-test [name
                                  rel1 id-exp1
                                  rel2 id-exp2]
  `(deftest ~name
     (let [id1# ~id-exp1
           id2# ~id-exp2
           o1# (first (select ~rel1
                              (where {:id id1#})
                              (with-object ~rel2
                                (order :id))))
           o2# (first (select ~rel2
                              (where {:id id2#})
                              (with-object ~rel1
                                (order :id))))]
       (is (not (nil? o1#))
           ~(str rel1 " instance exists"))
       (is (not (nil? o2#))
           ~(str rel2 " instance exists"))
       (is (some #(= (:id %) id2#) (get o1# ~(keyword rel2)))
           ~(str rel2 " instance in " rel1))
       (is (some #(= (:id %) id1#) (get o2# ~(keyword rel1)))
           ~(str rel1 " instance in " rel2)))))

(defn response-session-cookie [res]
  (if-let [cookie (first (get-in res [:headers "Set-Cookie"]))]
    (first (re-matches #"^(.*);Path=/$" cookie))))

(defn wrap-logged-on-user [app username password]
  (let [auth (app (-> (request :post "/login")
                      (body {:username username
                             :password password})))
        cookie (response-session-cookie auth)]
    (fn [req]
      (header req "cookie" cookie))))

(defn logged-on-user-request [app username password req]
  (let [auth (app (-> (request :post "/login")
                      (body {:username username
                             :password password})))
        cookie (response-session-cookie auth)]
    (header req "cookie" cookie)))


(defn dissoc-ids
  "Remove id fields."
  [x]
  (let [ids (filter #(re-matches #"^id$|^.*:id$|^.*_id$" (str %))
                    (keys x))]
    (apply dissoc x ids)))

(defn dissoc-all-ids
  "Walk a data structure, removing id fields from maps."
  [x]
  (postwalk #(if (map? %) (dissoc-ids %) %) x))

(defn mapdiff [a b]
  (pprint
   (difference (set a) (set b))))

(defn contains-structure?
  "Returns true when structure is contained in coll, in the sense that
  structure is like a skeleton whose parts are all contained in
  coll. In that case, every key of structure is contained in coll,
  every val in structure that is not a collection is equal to the
  corresponding val in coll, and for every val s of structure that is
  a collection, the corresponding val c in coll is a collection for
  which (contains-structure c s) is true. Otherwise, returns false."
  [coll structure]

  (cond (sequential? structure)
        (and
         ;; every non-collection in structure is in coll
         (every? (set coll) (filter (complement coll?) structure))
         ;; every collection in structure is contains-structure?
         ;;   for some collection in coll
         (every? (fn [s] 
                   (some (fn [c] (contains-structure? c s))
                         (filter coll? coll)))

                 (filter coll? structure)))


        (associative? structure)
        (let [scalar-set ;; set of scalar map entries
              (fn [m]
                (set (select-keys m
                                  (keys (filter #((complement coll?)
                                                  (val %))
                                                m)))))
              non-scalar-set ;; set of collection map entries
              (fn [m]
                (set (select-keys m (keys (filter #(coll? (val %))
                                                  m)))))
              
              
              s-structure (scalar-set structure)
              ns-structure (non-scalar-set structure)
              ns-coll (non-scalar-set coll)]
          (and (subset? s-structure (set coll))
               (every? (fn [s] 
                         (some (fn [c] (contains-structure? (val c)
                                                            (val s)))
                               ns-coll))
                       ns-structure))
          )
        ))
