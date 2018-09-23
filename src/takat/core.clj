(ns takat.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hiccup.core :as h]
            [hiccup.element :as e]
            [hiccup.page :as p]
            )

  (:gen-class))


(def xref
  {"SAPPHIRE"   "Sapphires"
   "PINK DIAMO" "Pink Diamonds"
   "ORANGE DIA" "Orange Diamonds"
   "MORGANITE"  "Morganites"
   "EMERALD"    "Emeralds"
   "TANZANITE"  "Tanzanite"
   "RUBY"       "Rubies"
   "PINK SAPPH" "Pink Sapphires"
   "PARAIBA TO" "Paraiba To"
   "PARAIBA"    "Paraiba"
   "GREEN DIAM" "Green Diamonds"
   "RING"       "Rings"
   ,
   "EARRING"    "Earrings"
   "PENDANT"    "Pendants"
   "BRACELET"   "Bracelets"
   "NECKLACE"   "Necklaces"
   "MENS RING"  "Men's Rings"
   "CUFFLINK"   "Cufflinks"})

(def css-style "text-decoration:none;font-family:fantasy")


(def photos-base "https://www.takat.com/images/inventory/")
(def no-image "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQq0b-4ooQSdi5985KBuLezX7oG0dOFIiVgC8CdJRH--z9gxqUJ")

(defn load-csv [fn]
  (with-open [in-file (io/reader fn)]
    (doall
      (csv/read-csv in-file))))


(defn create-keys [row]
  (map #(keyword(str/replace (str/trim %) #"\s+" "_")) row))

(defn transform-row [fields row]
  (zipmap fields row))

(defn trim-values [values]
  (map #(str/trim %) values))

(defn transform [fields csv]
  (map #(zipmap fields (trim-values %)) csv))


(defn re-org [row results]
  (assoc-in results [(:Jewel row) (:Stone_Kind row)]
            (conj (get-in results [(:Jewel row) (:Stone_Kind row)]) row)))



(defn create-data [file-name skip-n]
  (let [csv    (drop skip-n (load-csv file-name))
        fields (create-keys (first csv))
        data   (transform fields (drop 1 csv))]

    (loop [results {}
           data    data]
      (if (empty? data)
          results
          (recur (re-org (first data) results) (rest data))))))

(defn prep-field [f]
  (str/replace f " " ""))

(defn get-image [id i class alt ]
  (let [ext (if (= 0 i) ".jpg" (str "_" i ".jpg"))]
    (e/image {:title alt :onerror (str "this.src='" no-image "'") :class class} (str photos-base "/" id ext))))

(defn do-header []
  (h/html [:head [:title "Inventory"] (p/include-css "/inventory/inventory.css") [:meta {:name "viewport" :content "width=device-width,initial-scale=1"}]])
  )
(defn do-top []
  (h/html
     [:div (e/image "http://ariagems.com/wp-content/uploads/2014/07/Aria-Logo.png")]
          )
  )
(defn do-footer []
  ""
  )

(defn gen-details [details]
  (h/html [:html (do-header)
    [:body (do-top)
     [:table {:style css-style}
      (for [k (keys details)]
        [:tr
                          [:td k]
         [:td (k details)]])
      [:tr [:td {:colspan 2}
            (for [i (range 0 3)]
              (get-image (:Style_# details) i "img-normal" "" ))]]]]]))


(defn create-details [data base details-fn]
  (for [j (keys data)]
    (for [s (keys (get data j))]
      (for [d (get-in data [j s])]
        (let [file-name (str base "/" (prep-field j) "/" (prep-field s) "/" (:Style_# d) ".html")]
          (io/make-parents file-name)
          (spit file-name (details-fn d)))))))

(defn gen-alt-tag [detail]
  (str (:Shape detail) " " (:Unit_Price detail))
  )
(defn gen-leaf-index-table [parent data]
  (let [n (atom 0)]
    (h/html
      [:html (do-header) [:body (do-top) [:div {:style "display:block"}
                (for [detail data]
                  (let [i (:Style_# detail) pos (swap! n inc)]
                    [:div {:style "display:inline"}
                     (e/link-to (str "/" parent "/" (prep-field i) ".html") (get-image i 0 "img-small" (gen-alt-tag detail)))]

                    )

                )]]])))


(defn gen-leaf-index [parent data]
  (h/html
    [:html (do-header)
    [:body (do-top) [:ul {:style css-style}
            (for [i (map #(:Style_# %) data)]
              [:li (e/link-to (str "/" parent "/" (prep-field i) ".html") (get-image i 0 "img-small" ""))])]]]))

(defn gen-index-content-table [base parent data-fn leaf-fn]
  (let [n (atom 0)]
    (h/html [:html (do-header)
      [:body (do-top) [:div {:style "display:block"}
              (for [i (data-fn)]
                (let [file-name (str base "/" parent "/" (prep-field i) "/index.html")
                      pos       (swap! n inc)]
                  (if (some? leaf-fn)
                      (let [leaf-parent (str base "/" parent "/" (prep-field i))]
                        (io/make-parents (str leaf-parent "/indez.html"))
                        (spit (str leaf-parent "/index.html") (gen-leaf-index-table leaf-parent (leaf-fn i)))))
                  [:div  {:style "display:inline"}
                   [:span {:style "padding: 25px 50px 75px 100px;"} (e/link-to file-name (get xref i i))]
                         [:div (when (not (= (mod pos 5) 0)) {:style "display:inline"})]]))]]])))

(defn gen-index-content [base parent data-fn leaf-fn]
  (h/html [:html (do-header)
    [:body (do-top) [:ul {:style css-style}
            (for [i (data-fn)]
              (let [file-name (str "/" base "/" parent "/" (prep-field i) "/index.html")]
                (if (some? leaf-fn)
                    (let [leaf-parent (str base "/" parent "/" (prep-field i))]
                      (io/make-parents (str leaf-parent "/indez.html"))
                      (spit (str leaf-parent "/index.html") (gen-leaf-index-table leaf-parent (leaf-fn i)))))
                [:li (e/link-to {:style css-style} file-name (str (get xref i i)))]))]]]))

(defn get-leaf-node [data i ii]
  (get-in data [i ii]))

(defn create-index-files [data base]
  (let [root (str base "/index.html")]
    (io/make-parents root)
    (spit root (gen-index-content base "" #(keys data) nil))
    (doseq [i (keys data)]
      (let [jewelery (prep-field i)
            parent   (str base "/" jewelery "/index.html")]
        (do
          (io/make-parents parent)
          (spit parent (gen-index-content base jewelery #(keys (get data i)) (partial get-leaf-node data i)))))))
  data)

(defn create-takat [file-name base skip-n]
  (let [data (create-data file-name skip-n)]
    (-> data
        (create-index-files base)
        (create-details base gen-details))))

(defn -main [& args ]
  (create-takat (first args) (second args) 4))
