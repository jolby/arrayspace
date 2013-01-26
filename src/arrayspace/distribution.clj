(ns arrayspace.distribution)

;;
;; Contains common functions for all distributions
;;

(defn set-data-1d!
  "Set the data in the distribution, flattening the data if necessary"
  [dist data]
  (doall (map-indexed #(.set-1d! dist %1 %2) (flatten data))))