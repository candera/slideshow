(ns wangdera.slideshow.core
  (:use [clojure.contrib.test-is])
  (:import (java.io File)
	   (javax.imageio ImageIO)
	   (javax.swing JFrame JPanel Timer)
	   (java.awt Dimension Frame Color)
	   (java.awt.event ActionListener WindowAdapter KeyAdapter KeyEvent)))

(def imagelist (atom []))
(def current-image (atom nil))

(defn- jpeg? [f]
  (and 
   (.isFile f)
   (.endsWith (.toLowerCase (.getName  f)) ".jpg")))

(defn populate-imagelist [dir]
  (doseq [file (filter jpeg? (file-seq (File. dir)))]
    (swap! imagelist conj (.getPath file))))

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

(defn paint [g]
  (.setColor g Color/black)
  (let [region-rect (.getClipBounds g)
	region-width (.getWidth region-rect)
	region-height (.getHeight region-rect)]
    (.fillRect g 0 0 region-width region-height)
    (if @current-image
      (let [image-width (.getWidth @current-image) 
	    image-height (.getHeight @current-image) 
	    image-dims [image-width image-height]
	    region-dims [region-width region-height]
	    [x y width height] (fit-to image-dims region-dims)]
;	(println "drawing at" x y "dimensions" width height)
	(.drawImage g @current-image x y width height (Color. 0 0 0) nil))
      (doto g
	(.setColor Color/white)
	(.drawString "Working..." 800 600)))))

(defn random-image-path []
  (let [n (count @imagelist)]
    (if (= n 0)
      nil
      (@imagelist (int (rand n))))))

(defn random-image []
  (if-let [image-path (random-image-path)]
    (do 
      ;(println "Loading image " image-path)
      (ImageIO/read (File. image-path)))))

(defn handle-timer-event [e panel]
;  (println "Timer firing")
  (reset! current-image (random-image))
  (.repaint panel))

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
  (Timer. 5000 panel))

(def actions
  {\+ (fn [] (println "increasing speed"))
   \- (fn [] (println "decreasing speed"))})

(defn default-action [] (println "some other key"))

(defn handle-key [c]
  (let [f (get actions c default-action)]
    (f)))

(defn handle-keypress [event-type #^KeyEvent e]
  (if (= event-type :typed)
    (handle-key (.getKeyChar e))))

(defn make-key-listener [f]
  (proxy [KeyAdapter] []
    (keyPressed [e] (f :pressed e))
    (keyReleased [e] (f :released e)) 
    (keyTyped [e] (f :typed e))))

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
      (.addKeyListener (make-key-listener handle-keypress)))
;;   (.start timer)
    frame))