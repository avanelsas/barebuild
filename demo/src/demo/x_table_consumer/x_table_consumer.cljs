(ns demo.x-table-consumer.x-table-consumer
  (:require
   [barebuild.consumer-resource :as consumer-resource]
   [baredom.requests :as requests]
   [baredom.utils.dom :as du]
   [baremirror.alpha.events :as events]
   [baremirror.alpha.parts :as parts]
   [baremirror.alpha.places :as places]
   [baremirror.alpha.template :as template]
   [demo.alert :as alert]
   [demo.task-edit :as task-edit]
   [demo.x-table-consumer.model :as model]))

(def ^:private k-listening "__xTableConsumerListening")

(def ^:private header-row
  [:x-table-row {:data-x-part "header"}
   [:x-table-cell {:type "header"} "Actions"]])

(def ^:private header-cell
  (template/split :header
                  [:x-table-cell {:type           "header"
                                  :scope          "col"
                                  :sortable       true
                                  :sort-direction :sort-direction
                                  :data-x-part    "sort"}
                   :key]))

(def ^:private body-row
  [:x-table-row
   [:x-table-cell
    [:span {:style "display:inline-flex;gap:0.2rem"}
     [:x-button {:variant "primary" :size "sm" :data-x-part "edit"} "Edit"]
     [:x-button {:variant "danger" :size "sm" :data-x-part "delete"} "Delete"]]]])

(def ^:private make-header-cell! (parts/node-maker (:fixed header-cell)))

(def ^:private make-body-row! (parts/node-maker body-row))

(def ^:private make-body-cell! (parts/node-maker [:x-table-cell]))

(def ^:private make-page-bar! (parts/node-maker [:x-pagination {:size "md" :data-x-part "pages"}]))

(defn- sort-of [origin ^js e]
  {:field     (peek (:key-path origin))
   :direction (.. e -detail -direction)})

(defn- page-of [_origin ^js e]
  (.. e -detail -page))

(def ^:private consumer-events
  {["x-table-cell-sort" "sort"] [:sort sort-of]
   ["press" "edit"]             :edit
   ["press" "delete"]           :delete
   ["page-change" "pages"]      [:page page-of]})

(defn- sort! [^js this {:keys [field direction]}]
  (consumer-resource/submit-intent! this (model/translate-gesture field direction)))

(defn- page! [^js this page]
  (consumer-resource/submit-intent! this (model/translate-pagination-gesture page)))

(defn- delete! [^js this id]
  (consumer-resource/submit-write! this {:op :delete :id id}))

(def ^:private effects
  {:sort   sort!
   :page   page!
   :edit   task-edit/request!
   :delete delete!})

(defn- perform! [^js this [meaning arg]]
  ((effects meaning) this arg))

(defn- cells-of
  "The container of the keyed cells of `row`. They go before its last cell, the actions."
  [^js row]
  {:cells {:parent row :before (.-lastElementChild row)}})

(defn- sync-cells!
  "Brings the keyed cells of `row` to the fields of `columns`. Returns the cell of each field."
  [^js row columns make-cell!]
  (places/sync! (cells-of row) {:cells (mapv :key columns)} make-cell!))

(defn- header-of
  "The header row of `table`, or nil."
  [^js table]
  (get (parts/read-parts table) "header"))

(defn- write-header-cell! [cell-nodes {field :key :as column}]
  (parts/write-item! (cell-nodes field) header-cell column))

(defn- render-header! [^js table columns]
  (let [cell-nodes (sync-cells! (header-of table) columns make-header-cell!)]
    (run! (partial write-header-cell! cell-nodes) columns)))

(defn- write-body-cell! [cells [field ^js cell-node]]
  (parts/set-text! cell-node (get cells field)))

(defn- render-row! [columns row-nodes {:keys [cells] :as row}]
  (run! (partial write-body-cell! cells)
        (sync-cells! (row-nodes (model/row-key row)) columns make-body-cell!)))

(defn- render-rows! [^js table columns rows]
  (let [row-nodes (places/sync! {:rows {:parent table}} {:rows (mapv model/row-key rows)}
                                make-body-row!)]
    (run! (partial render-row! columns row-nodes) rows)))

(defn- render-page-bar!
  "One page bar while there is more than one page to reach, none otherwise."
  [^js this page-info]
  (let [shown (model/page-bar page-info)
        nodes (places/sync! {:bar {:parent this}} {:bar (if shown ["pages"] [])} make-page-bar!)]
    (when shown
      (parts/set-attrs! (nodes "pages") shown))))

(defn- render! [^js table {accepted :accepted} ^js this]
  (when accepted
    (let [{:keys [columns rows]} (model/accepted-response->view-model accepted)]
      (parts/set-attrs! table {:columns (model/grid-template columns)})
      (render-header! table columns)
      (render-rows! table columns rows)
      (render-page-bar! this (:page-info accepted)))))

(defn- on-failure! [_child {:keys [failure]} ^js this]
  (if failure
    (alert/show! this (model/failure-message failure))
    (alert/clear! this)))

(defn- add-header-row!
  "Gives `table` a header row when it has none."
  [^js table]
  (when-not (header-of table)
    (.append table (parts/make-node! header-row))))

(defn- listen-once!
  "Gives the consumer its listeners, once in its life."
  [^js this]
  (when-not (du/initialized? this k-listening)
    (du/mark-initialized! this k-listening)
    ;; The dispatch is async, so the cell does not show the asked direction yet and the request
    ;; is refused.
    (events/listen! this {:dispatch!        (partial perform! this)
                          :requested-values requests/requested-values
                          :events           consumer-events})))

(defn- on-connect! [^js table ^js this]
  (add-header-row! table)
  (listen-once! this))

(defn init! []
  (consumer-resource/register!
   {:tag        model/tag-name
    :child-tag  "x-table"
    :render     render!
    :on-failure on-failure!
    :on-connect on-connect!}))
