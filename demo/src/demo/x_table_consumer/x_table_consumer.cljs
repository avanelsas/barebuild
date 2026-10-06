(ns demo.x-table-consumer.x-table-consumer
  (:require
   [barebuild.consumer-resource :as consumer-resource]
   [baredom.utils.dom :as du]
   [baremirror.core :as mirror]
   [baremirror.template :as template]
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

(def ^:private body-cell
  [:x-table-cell])

(def ^:private page-bar-template
  [:x-pagination {:size "md" :data-x-part "pages"}])

(defn- sort-of [origin ^js e]
  {:field     (peek (:key-path origin))
   :direction (.. e -detail -direction)})

(defn- page-of [_origin ^js e]
  (.. e -detail -page))

;; x-table stops x-table-cell-sort and sends x-table-sort in its place, which names a column
;; index and no cell. So the sort is listened for on the header row, below the table.
(def ^:private header-events
  {["x-table-cell-sort" "sort"] [:sort sort-of]})

(def ^:private events
  {["press" "edit"]        :edit
   ["press" "delete"]      :delete
   ["page-change" "pages"] [:page page-of]})

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
  (mirror/sync! (cells-of row) {:cells (mapv :key columns)} make-cell!))

(defn- make-header-cell! [_key]
  (mirror/make-node! (:fixed header-cell)))

(defn- make-body-cell! [_key]
  (mirror/make-node! body-cell))

(defn- make-body-row! [_key]
  (mirror/make-node! body-row))

(defn- make-page-bar! [_key]
  (mirror/make-node! page-bar-template))

(defn- header-of
  "The header row of `table`, or nil."
  [^js table]
  (get (mirror/read-parts table) "header"))

(defn- write-header-cell! [cell-nodes {field :key :as column}]
  (mirror/write! (mirror/read-parts (cell-nodes field)) (template/writes header-cell column)))

(defn- render-header! [^js table columns]
  (let [cell-nodes (sync-cells! (header-of table) columns make-header-cell!)]
    (run! (partial write-header-cell! cell-nodes) columns)))

(defn- write-body-cell! [cells [field ^js cell-node]]
  (mirror/set-text! cell-node (get cells field)))

(defn- render-row! [columns row-nodes {:keys [cells] :as row}]
  (run! (partial write-body-cell! cells)
        (sync-cells! (row-nodes (model/row-key row)) columns make-body-cell!)))

(defn- render-rows! [^js table columns rows]
  (let [row-nodes (mirror/sync! {:rows {:parent table}} {:rows (mapv model/row-key rows)}
                                make-body-row!)]
    (run! (partial render-row! columns row-nodes) rows)))

(defn- render-page-bar!
  "One page bar while there is more than one page to reach, none otherwise."
  [^js this page-info]
  (let [shown (model/page-bar page-info)
        nodes (mirror/sync! {:bar {:parent this}} {:bar (if shown ["pages"] [])} make-page-bar!)]
    (when shown
      (mirror/set-attrs! (nodes "pages") shown))))

(defn- render! [^js table {accepted :accepted} ^js this]
  (when accepted
    (let [{:keys [columns rows]} (model/accepted-response->view-model accepted)]
      (mirror/set-attrs! table {:columns (model/grid-template columns)})
      (render-header! table columns)
      (render-rows! table columns rows)
      (render-page-bar! this (:page-info accepted)))))

(defn- on-failure! [_child {:keys [failure]} ^js this]
  (if failure
    (alert/show! this (model/failure-message failure))
    (alert/clear! this)))

(defn- header-row!
  "The header row of `table`. One is made when the table has none."
  [^js table]
  (or (header-of table)
      (let [header (mirror/make-node! header-row)]
        (.append table header)
        header)))

(defn- listen-once!
  "Gives the header row and the consumer their listeners, once in the life of the consumer."
  [^js header ^js this]
  (when-not (du/initialized? this k-listening)
    (du/mark-initialized! this k-listening)
    (mirror/listen! header {:dispatch! (partial perform! this) :events header-events})
    (mirror/listen! this {:dispatch! (partial perform! this) :events events})))

(defn- on-connect! [^js table ^js this]
  (listen-once! (header-row! table) this))

(defn init! []
  (consumer-resource/register!
   {:tag        model/tag-name
    :child-tag  "x-table"
    :render     render!
    :on-failure on-failure!
    :on-connect on-connect!}))
