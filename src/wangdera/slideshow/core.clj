(ns wangdera.slideshow.core
  (:use clojure.contrib.test-is
	clojure.contrib.import-static)
  (:import (java.io File)
	   (javax.imageio ImageIO)
	   (javax.swing JFrame JPanel Timer)
	   (java.awt Dimension Frame Color)
	   (java.awt.event ActionListener WindowAdapter KeyAdapter KeyEvent))
  (:gen-class))

(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_SPACE VK_PAUSE)

(def image-path-list (atom []))
(def rotation-rate (atom 12)) ; changes per minute
(def current-time (atom 0))
(def last-rotation (atom 0))
(def paused (atom false))
(def image-cursor (atom {:image-path-history []
			 :current-image-index nil
			 :current-image nil}))

(def timer-granularity 100)

(defn- jpeg? [f]
  (and 
   (.isFile f)
   (.endsWith (.toLowerCase (.getName  f)) ".jpg")))

(defn populate-imagelist [dir]
  (doseq [file (filter jpeg? (file-seq (File. dir)))]
    (swap! image-path-list conj (.getPath file))))

(defn make-frame []
  (JFrame. "Slideshow"))

(defn center [image-dims region-dims]
  (vec (map #(- (/ %2 2) (/ %1 2)) image-dims region-dims)))

(defn scale [factor dims]
  (vec (map #(* factor %) dims)))

(defn compute-bounds [image-dims region-dims]
  (let [scaling (apply min 1 (map / region-dims image-dims))]
    [(center (scale scaling image-dims) region-dims)
     scaling]))

(defn fit-to [image-dims region-dims]
  (let [[[x y] scaling] (compute-bounds image-dims region-dims)
	[width height] (map #(int (* % scaling)) image-dims)] 
    [x y width height]))

(defn current-image []
  (:current-image @image-cursor))

(defn paint [g]
  (.setColor g Color/black)
  (let [region-rect (.getClipBounds g)
	region-width (.getWidth region-rect)
	region-height (.getHeight region-rect)]
    (.fillRect g 0 0 region-width region-height)
    (if-let [current-image (current-image)]
      (let [image-width (.getWidth current-image) 
	    image-height (.getHeight current-image) 
	    image-dims [image-width image-height]
	    region-dims [region-width region-height]
	    [x y width height] (fit-to image-dims region-dims)]
	;; (println "drawing at" x y "dimensions" width height)
	(.drawImage g current-image x y width height (Color. 0 0 0) nil))
      (doto g
	(.setColor Color/white)
	(.drawString "Working..." 800 600)))))

(defn random-image-path []
  (let [n (count @image-path-list)]
    (if (= n 0)
      nil
      (@image-path-list (int (rand n))))))

(defn random-image []
  (if-let [image-path (random-image-path)]
    (do 
      ;(println "Loading image " image-path)
      (ImageIO/read (File. image-path)))))

(defn handle-timer-event [e panel]
  ;;   (println "Timer firing")
  (when-not @paused
    (swap! current-time + timer-granularity)))

(defn make-panel []
  (proxy [JPanel ActionListener] []
    (getPreferredSize [] nil)
    (paintComponent 
     [g] 
     (proxy-super paintComponent g)
     (paint g))
    (actionPerformed [e] (handle-timer-event e this))))

(defn make-window-listener [timer]
  (proxy [WindowAdapter] []
    (windowClosed [e])
    (windowClosing 
     [e]
     (.stop timer))
    (windowIconified [e])))

(defn make-timer [panel]
  (Timer. timer-granularity panel))

(defn change-with-limit [old-val f limit-fn limit-val]
  (limit-fn limit-val (f old-val)))

(def char-actions
  {\+ #(swap! rotation-rate change-with-limit inc min 100)
   \- #(swap! rotation-rate change-with-limit dec max 1)})

(defn toggle-pause []
  (swap! paused not))

(def key-actions
  {VK_LEFT (fn [])
   VK_RIGHT (fn [])
   VK_SPACE toggle-pause
   VK_PAUSE toggle-pause})

(defn invoke-from-map [m k]
  (if-let [f (get m k)]
    (f)))

(defn rotation-time []
  (/ 60000 @rotation-rate))

(defn time-to-rotate? []
  (> (- @current-time @last-rotation)
     (rotation-time)))

(defn conj-random-item [coll items]
  (conj coll (rand-nth items)))

(defn maybe-rand-conj
  "Given a collection coll, if necessary, conj random members of
  items to it until its length is n"
  [coll items n]
  (if (< (count coll) n)
    (recur (conj-random-item coll items) items n)
    coll))

;;; {:current-image #<image>
;;;  :image-path-history [...]
;;;  :current-image-index n}
(defn move-image-cursor
  ([cursor] (move-image-cursor cursor 1))
  ([cursor n]
     (let [{:keys [current-image image-path-history current-image-index]} cursor
	   next-index ((fnil + -1) current-image-index n)
	   image-path-history (maybe-rand-conj image-path-history @image-path-list (inc next-index))]
       {:current-image (ImageIO/read (File. (nth image-path-history next-index)))
	:image-path-history image-path-history
	:current-image-index next-index})))

(defn next-image []
  (swap! image-cursor move-image-cursor))

(defn rotate! []
  (swap! image-cursor move-image-cursor)
  (reset! last-rotation @current-time))

(defn handle-time-change []
  (if (time-to-rotate?)
    (rotate!)))

(defn make-key-listener []
  (proxy [KeyAdapter] []
    (keyPressed [e] (invoke-from-map key-actions (.getKeyCode e)))
    (keyReleased [e]) 
    (keyTyped [e] (invoke-from-map char-actions (.getKeyChar e)))))

(defn start-imagelist-population [dir]
  ;; Could have used future here, but it is better
  ;; to run with a low-pri thread because the CPU
  ;; utilization of populate-imagelist can be
  ;; more than you'd want out of a slideshow app
  ;;
  ;;(future (populate-imagelist dir))
  (let [thread (Thread. #(populate-imagelist dir))]
    (.setPriority thread Thread/MIN_PRIORITY)
    (.start thread)
    thread))

(defn slideshow [dir]
  (start-imagelist-population dir)
  (let [frame (make-frame)
	panel (make-panel)
	timer (make-timer panel)]
    (doto frame 
      ; EXIT_ON_CLOSE is problematic when running at the REPL
      ;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.addWindowListener (make-window-listener timer))
      (.setExtendedState Frame/MAXIMIZED_BOTH)
      (.setUndecorated true)
      (.pack)
      (.show)
      (.add panel)
      (.addKeyListener (make-key-listener)))
    (.start timer)
    (add-watch current-time ::slideshow (fn [k r n o] (handle-time-change)))
    (add-watch image-cursor ::slideshow (fn [k r n o] (.repaint panel)))
    frame))

(defn -main [& args]
  (slideshow (first args)))