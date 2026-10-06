(ns demo.x-board-consumer.model)

(def tag-name "x-board-consumer")

(def statuses ["todo" "doing" "done"])

(defn columns
  "Tasks grouped by status into the three columns, each ordered by rank."
  [accepted-response]
  (let [by-status (group-by #(get % "status") (:value accepted-response))]
    (into {}
          (map (fn [s] [s (vec (sort-by #(or (get % "rank") 0) (get by-status s)))]))
          statuses)))

(def empty-columns
  "The three columns, empty."
  (zipmap statuses (repeat [])))

(defn project-selected?
  "True when the intent names a project."
  [intent]
  (some? (:project intent)))

(defn columns-for
  "The columns `view` paints, empty until a project is selected."
  [{:keys [accepted intent]}]
  (if (and (project-selected? intent) accepted)
    (columns accepted)
    empty-columns))

(defn render-key
  "The slice the board repaints on: the rows, and whether a project is selected."
  [{:keys [accepted intent]}]
  [(:value accepted) (project-selected? intent)])

(defn- row-id
  "The key of a row: its id as text."
  [row]
  (str (get row "id")))

(defn rows-by-id
  "The rows of `cols`, by the key `order` gives them."
  [cols]
  (into {} (map (juxt row-id identity)) (mapcat val cols)))

(defn order
  "The card keys of each column, in order."
  [cols]
  (update-vals cols (partial mapv row-id)))

(defn- name-hue
  "A stable hue (0-359) per name, so an assignee keeps one avatar colour. The modulo folds into
   every step rather than landing at the end, where a long name would already have run past the
   precision a double carries."
  [name]
  (reduce (fn [h ch] (mod (+ (* h 31) (.charCodeAt ch 0)) 360)) 7 (str name)))

(defn card-vm
  "What a card renders."
  [row]
  (let [assignee (str (or (get row "assigneeName") (get row "owner")))
        project  (str (get row "projectName"))]
    {:value    (row-id row)
     :title    (get row "title")
     :assignee assignee
     :project  project
     :subtitle (str assignee " · " project)
     :initial  (if (seq assignee) (subs assignee 0 1) "?")
     :hue      (name-hue assignee)}))

(defn cards
  "What each card of `cols` renders, by the key `order` gives it."
  [cols]
  (update-vals (rows-by-id cols) card-vm))

(defn translate-drop-gesture
  "A drop as a move: a positional command carrying only the destination, not a record edit."
  [row to-status index]
  {:op     :move
   :id     (get row "id")
   :record {"status" to-status "index" index}})
