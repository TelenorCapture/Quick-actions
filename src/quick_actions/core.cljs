(ns ^:figwheel-hooks quick-actions.core
  (:require
   [clojure.edn :refer [read-string]]
   [clojure.string :as str]
   [clojure.spec.alpha :as spec]
   [quick-actions.utils :refer [assoc-db]]
   [re-frame.core :as re-frame]
   [reagent.dom :as rdom]))

(def date-format
  #js{:day   "2-digit"
      :month "long"
      :year  "numeric"})

(spec/def ::month-name
  #{"January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"})

(spec/def ::month-num (spec/and nat-int? #(>= 1 %) #(<= 12 %)))

(spec/def ::year
  (spec/and nat-int? #(> 1800 %) #(< 2023 %)))

(spec/def ::query
  (spec/cat
   :year ::year
   :month-name ::month-name
   :month-num ::month-num))

(re-frame/reg-event-db
 :initialize
 (fn [_ _]
   {:show-quick-actions? false
    :query               ""
    :use-suggestions     0
    :image-formats       ["HEIC" "jpg"]
    :photo               3
    :albums              [{:id     1
                           :title  "Trip to Ukraine"
                           :photos #{}}
                          {:id     2
                           :title  "Art"
                           :photos #{}}
                          {:id     3
                           :title  "Geometry"
                           :photos #{}}]
    :photos              [{:id         1
                           :src        "/images/art1.jpg"
                           :date-taken #inst "2016-03-16"}
                          {:id         2
                           :src        "/images/art2.jpg"
                           :date-taken #inst "2019-10-26"}
                          {:id         3
                           :src        "/images/art3.jpg"
                           :date-taken #inst "2022-01-06"}]}))

(re-frame/reg-event-db
 :populate-suggestions
 (fn [db [k v]]
   (assoc db :suggestions
          {0 (flatten [(map-indexed (fn [idx {:keys [date-taken]}] {:id         idx
                                                                   :type       :by-date
                                                                   :suggestion (str "Show photos taken " (.toLocaleString date-taken "en-GB" date-format))
                                                                   :action     [:show-date date-taken]}) (get db :photos []))
                       (map-indexed (fn [idx {:keys [title]}] {:id idx :type :show-album :suggestion (str "Show the \"" title "\" album")}) (get db :albums []))])
           1 (flatten [(map-indexed (fn [idx {:keys [title]}] {:id         idx
                                                              :type       :add-to-album
                                                              :suggestion (str "Add photo to the \"" title "\" album")
                                                              :action     [:add-to-album idx]
                                                              }) (get db :albums []))
                       (map-indexed (fn [idx image-format] {:id idx :type :download :suggestion (str "Download photo as " image-format)}) (get db :image-formats []))])})))

(re-frame/reg-event-db
 :add-to-album
 (fn [db [k album-id]]
   (let [photo-id (get db :photo)
         album    (get-in db [:albums album-id])]
     (assoc-in db [:albums album-id :photos] (conj (:photos album) photo-id)))))

(re-frame/reg-event-db :show-date assoc-db)

(re-frame/reg-event-db :show-quick-actions? assoc-db)

(re-frame/reg-event-db :query assoc-db)

(re-frame/reg-event-db
 :use-suggestions
 (fn [db [k v]]
   (when (nat-int? v)
     (assoc db k v))))

(re-frame/reg-sub :query get-in)

(re-frame/reg-sub :scroll-y get-in)

(re-frame/reg-sub :use-suggestions get-in)

(re-frame/reg-sub :suggestions get-in)

(re-frame/reg-sub :show-date get-in)

(re-frame/reg-sub
 :filtered-suggestions
 (fn [_ _]
   [(re-frame/subscribe [:suggestions])
    (re-frame/subscribe [:use-suggestions])
    (re-frame/subscribe [:query])])
 (fn [[suggestions k query] _]
   (filter (fn [{:keys [suggestion]}]
        (re-matches (re-pattern (str "(?i).*" (str/join ".*" query) ".*")) suggestion)) (get suggestions k))))

(re-frame/reg-sub
 :nth-by-type-suggestions
 (fn [_ _]
   [(re-frame/subscribe [:filtered-suggestions])])
 (fn [[suggestions]]
   (->> (group-by :type suggestions)
        (map second)
        (map first))))

(re-frame/reg-sub :photos get-in)

(re-frame/reg-sub :albums get-in)

(re-frame/reg-sub :show-quick-actions? get-in)

(enable-console-print!)

(defn- debug-mode []
  (let [message (str "🦄 Quick-actions")]
    (enable-console-print!)
    (println message)))

(defn squiggly-line []
  (let [height 857
        width  243
        d      ["m65.0.0c-75"
                "93-84"
                "178-28"
                "254"
                "84"
                "114"
                "205"
                "74"
                "205"
                "224s-56"
                "172-66"
                "254c-6"
                "55-3"
                "95"
                "10"
                "122"]]
    [:<>
     (for [n (range 4)]
       [:svg.squiggly-line {:key     (str "squiggly-" n)
                            :viewbox (str/join " " [0 0 width height])
                            :height  height
                            :width   width}
        [:path {:d    (str/join " " d)
                :fill :none}]])]))

(defn list-suggestions []
  (let [suggestions @(re-frame/subscribe [:nth-by-type-suggestions])
        query       @(re-frame/subscribe [:query])]
    [:div.Suggestions
     (for [{:keys [id type suggestion action]} suggestions]
       (let [patt        (str "(?i)" query) 
             pattern     (re-pattern patt)
             query-match #(str "<span class=\"query-match\">" % "</span>")]
         [:a {:key                     (str (name type) "-" id)
              :href                    "#"
              :on-click                (when (some? action) #(re-frame/dispatch action))
              :dangerouslySetInnerHTML {:__html (str/replace suggestion pattern query-match)}}]))]))

(defn input-action-handler [event]
  (let [value (-> event .-target .-value)]
    (re-frame/dispatch [:query value])))

(defn modal []
  (let [show-quick-actions? (re-frame/subscribe [:show-quick-actions?])
        ref                 (atom nil)]
    (fn []
      (do (when-let [input @ref] (when @show-quick-actions? (.focus input)))
          [:div.Modal {:class [(when @show-quick-actions? :visible)]}
           [:input#action {:ref #(reset! ref %) :placeholder "Find action…" :type :text :on-change input-action-handler}]
           [list-suggestions]]))))

(defn album [{:keys [id title photos]}]
  [:div.Album {:key (str "album-" id)}
   [:span.badge (count photos)]
   title])

(defn photo [{:keys [id src date-taken]}]
  (let [show-date @(re-frame/subscribe [:show-date])]
    [:figure {:key   (str "photo-" id)
              :class [(when (= show-date date-taken) "active")]}
     [:img {:src src}]
     [:figcaption (.toLocaleString date-taken "en-GB" date-format)]]))

(defn main []
  (let [show-quick-actions? @(re-frame/subscribe [:show-quick-actions?])
        photos              @(re-frame/subscribe [:photos])
        albums              @(re-frame/subscribe [:albums])]
    [:main
     [squiggly-line]
     [modal]
     [:section.Page
      [:div.Hero
       [:h1 "Find what you're looking for faster"]
       [:h4 "Use " [:pre {:on-click #(re-frame/dispatch [:show-quick-actions? (not show-quick-actions?)])} ":"] " to show the quick-actions dialog"]]
      (into [:div.Photos] (map photo photos))]
     [:section.Page
      [:div.Hero
       [:h1 "Act upon an item with ease"]
       [:h4 "Use " [:pre {:on-click #(re-frame/dispatch [:show-quick-actions? (not show-quick-actions?)])} ":"] " to show the quick-actions dialog"]]
      [:div.Photo
       [photo (nth photos 2)]]
      (into [:div.Albums] (map album albums))]]))

(defn mount []
  (let [el (.getElementById js/document "app")]
    (re-frame/clear-subscription-cache!)
    (re-frame/dispatch-sync [:initialize])
    (re-frame/dispatch-sync [:populate-suggestions])
    (rdom/render [main] el)))

(defn key-pressed [event]
  (when (not= (.-id (.-target event)) "action")
    (case (.-key event)
      ":" (let [show-quick-actions? @(re-frame/subscribe [:show-quick-actions?])]
            (re-frame/dispatch [:show-quick-actions? (not show-quick-actions?)])))))

(defn ^:after-load re-render []
  (mount))

(defn reset-css-property! [property value]
  (.documentElement.style.setProperty js/document (str "--" (name property)) value))

(defonce first-init
  (let [doc-classes (-> js/document.documentElement .-classList)]
    (js/addEventListener "keypress" key-pressed)
    (js/addEventListener "scroll" #(do (re-frame/dispatch [:use-suggestions (/ (.-scrollTop js/document.documentElement) (.-innerHeight js/window))])
                                       (reset-css-property! :scroll-y (.-scrollY js/window))))
    (when goog.DEBUG (debug-mode))
    (-> doc-classes (.remove "no-js"))
    (mount)
    true))
