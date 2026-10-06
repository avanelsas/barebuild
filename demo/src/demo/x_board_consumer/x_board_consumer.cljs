(ns demo.x-board-consumer.x-board-consumer
  "The kanban board consumer, driving three x-drop-zones."
  (:require
   [barebuild.consumer-resource :as consumer-resource]
   [baredom.utils.dom :as du]
   [baremirror.core :as mirror]
   [baremirror.template :as template]
   [demo.x-board-consumer.model :as model]))

(def ^:private k-pending-zone "__xBoardPendingZone")
(def ^:private k-listening "__xBoardListening")

(defn- avatar-style
  "The style of a card's avatar: the colour of its hue."
  [{:keys [hue]}]
  (str "background-color:hsl(" hue " 55% 48%)"))

(def ^:private card
  (template/split :card
                  [:x-drag-panel {:kind "task" :grab "surface" :value :value :label :title}
                   [:span {:slot "header"} :title]
                   [:div {:class "board-card-body"}
                    [:span {:class "board-card-avatar" :style avatar-style} :initial]
                    [:span {:class "board-card-meta"} :subtitle]]]))

(defn- drop-of [origin ^js e]
  (let [^js detail (.-detail e)]
    {:zone  (:node origin)
     :panel (.-panel detail)
     :value (.-value detail)
     :to    (.-to detail)
     :index (.-index detail)}))

(def ^:private events
  {["x-drop-zone-drop" "zone"] [:drop drop-of]})

;; --- render: the cards of each zone --------------------------------------------

(defn- zone-container [^js zone]
  [(.getAttribute zone "value") {:parent zone}])

(defn- zone-containers
  "The zones of the board as containers, by status."
  [^js this]
  (into {} (map zone-container) (array-seq (.querySelectorAll this "x-drop-zone"))))

(defn- make-card! [_id]
  (mirror/make-node! (:fixed card)))

(defn- write-card! [cards [id ^js card-node]]
  (mirror/write! (mirror/read-parts card-node) (template/writes card (cards id))))

(defn- place-cards!
  "Brings the cards of every zone to `cols`. A card that changes column keeps its node."
  [^js this cols]
  (run! (partial write-card! (model/cards cols))
        (mirror/sync! (zone-containers this) (model/order cols) make-card!)))

;; --- drop: reserve -> write -> release ----------------------------------------

(defn- reserved-zone
  "The zone holding a gap open for an unconfirmed drop, or nil."
  [^js this]
  (du/getv this k-pending-zone))

(defn- reserve!
  "Holds a gap open in `zone` for `panel` at `index`, until the drop settles."
  [^js this ^js zone panel index]
  (.reserve zone panel index)
  (du/setv! this k-pending-zone zone))

(defn- drop! [^js this {:keys [zone panel value to index]}]
  (let [view (consumer-resource/view this)
        row  (get (model/rows-by-id (model/columns-for view)) value)]
    (when (and row (not (:writing? view)) (not (reserved-zone this)))
      (reserve! this zone panel index)
      (consumer-resource/submit-write! this (model/translate-drop-gesture row to index)))))

(def ^:private effects
  {:drop drop!})

(defn- perform! [^js this [meaning arg]]
  ((effects meaning) this arg))

(defn- on-writing!
  "Release the reserved zone once this board's own drop settles. Another consumer's write moves
  `:writing?` too, and releasing on that drops the card back early."
  [_child {:keys [writing?] :as view} ^js this]
  (when-let [^js zone (and (not writing?)
                           (consumer-resource/own-write? this view)
                           (reserved-zone this))]
    (.release zone)
    (du/setv! this k-pending-zone nil)
    ;; render deferred placing while the drop was reserved
    (place-cards! this (model/columns-for view))))

(defn- listen-once!
  "Gives the consumer its listeners, once in its life."
  [^js this]
  (when-not (du/initialized? this k-listening)
    (du/mark-initialized! this k-listening)
    (mirror/listen! this {:dispatch! (partial perform! this) :events events})))

(defn- on-connect! [_child ^js this]
  (listen-once! this))

(defn- render!
  [_child {:keys [intent] :as view} ^js this]
  (mirror/set-attrs! this {:data-empty (if (model/project-selected? intent) "false" "true")})
  (when-not (reserved-zone this)
    (place-cards! this (model/columns-for view))))

(defn init! []
  (consumer-resource/register!
   {:tag        model/tag-name
    :child-tag  "x-drop-zone"
    :render-key model/render-key
    :render     render!
    :on-writing on-writing!
    :on-connect on-connect!}))
