(ns demo.x-project-selector-consumer.x-project-selector-consumer
  "A PROJECTS consumer that drives TASKS. A selection is a targeted intent to \"tasks\", which
  writes tasks.project to the URL and refetches."
  (:require
   [barebuild.consumer-resource :as consumer-resource]
   [baredom.utils.dom :as du]
   [baremirror.alpha.parts :as parts]
   [baremirror.alpha.places :as places]
   [baremirror.alpha.template :as template]
   [barereplay.dock :as dock]
   [demo.url :as url]
   [demo.x-project-selector-consumer.model :as model]))

(def ^:private tasks-resource-id "tasks")
(def ^:private k-listening "__xProjectSelectorListening")
(def ^:private all-part "all")
(def ^:private url-moved-events ["popstate" dock/url-changed-event])

(def ^:private all-option
  [:option {:value model/all-projects-value :data-x-part all-part} "All projects"])

(def ^:private project-option
  (template/split :option [:option {:value :id} :name]))

(def ^:private make-option! (parts/node-maker (:fixed project-option)))

;; --- the current selection -----------------------------------------------------

(defn- current-project-id
  "The selection as the x-select spells it, blank meaning all projects. See `demo.url`."
  []
  (or (url/tasks-project-id) model/all-projects-value))

(defn- show-selection!
  "Drives the x-select to the URL selection."
  [^js x-select]
  (parts/set-attrs! x-select {:value (current-project-id)}))

;; --- option rendering ---------------------------------------------------------

(defn- write-option! [option-nodes {:keys [id] :as option}]
  (parts/write-item! (option-nodes id) project-option option))

(defn- render-options! [^js x-select options]
  (let [option-nodes (places/sync! {:options {:parent x-select}} {:options (mapv :id options)}
                                   make-option!)]
    (run! (partial write-option! option-nodes) options)))

(defn- render! [^js x-select {accepted :accepted} _this]
  (render-options! x-select (model/project-options accepted))
  (show-selection! x-select))

;; --- wiring -------------------------------------------------------------------

(defn- on-select-change! [^js el ^js e]
  (let [id (.. e -detail -value)]
    (when (not= id (current-project-id))
      (consumer-resource/submit-intent! el (model/translate-project-gesture id) tasks-resource-id))))

(defn- reselect! [^js x-select _e]
  (show-selection! x-select))

(defn- add-all-option!
  "Gives `x-select` its all-projects option when it has none."
  [^js x-select]
  (when-not (get (parts/read-parts x-select) all-part)
    (.prepend x-select (parts/make-node! all-option))))

(defn- listen-once!
  "Gives the consumer its listeners, once in its life."
  [^js x-select ^js el]
  (when-not (du/initialized? el k-listening)
    (du/mark-initialized! el k-listening)
    (.addEventListener x-select "select-change" (partial on-select-change! el))
    (run! #(.addEventListener js/window % (partial reselect! x-select)) url-moved-events)))

(defn- on-connect! [^js x-select ^js el]
  (add-all-option! x-select)
  (listen-once! x-select el))

(defn init! []
  (consumer-resource/register!
   {:tag        model/tag-name
    :child-tag  "x-select"
    :render-key (fn [view] (model/project-options (:accepted view)))
    :render     render!
    :on-connect on-connect!}))
