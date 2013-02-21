(ns arrayspace.distribution)

;;
;; Contains common functions for all distributions
;;

(defn set-data-flat!
  "Set the data in the distribution, flattening the data if necessary"
  [dist data]
  (try     
    (doall (map-indexed #(.set-flat! dist %1 %2) (flatten data)))
    (catch Exception e (println (format "dist: %s, data: %s, ex: %s" dist data (.getMessage e))))))