;; Setup or clogpower package.
(defpackage #:clogpower
  (:use #:cl #:clog #:clog-gui)
  (:export start-site))

(in-package :clogpower)

(defun main-screen (body)
  "The main screen of the clogpower.com website"
  ;; Load some nicer fonts from google
  (load-css (html-document body)
	    "https://fonts.googleapis.com/css2?family=Smythe&family=Special+Elite&display=swap")
  ;; We will take a screen approach, not a document approach for our initial interaction
  ;; so we will use the panel-box-layout
  (let* ((layout (create-panel-box-layout body :left-width 175 :right-width 0))
	 (street (create-img (center-panel layout) :url-src "/img/clog-street.jpg")))
    (setf (text-alignment (center-panel layout)) :center)
    (setf (height street) "100%")
    ;; Setup menu
    (setf (width (create-img (left-panel layout)
			     :url-src "/img/clog-lights.png")) "100%")
    (create-br (left-panel layout))
    (create-br (left-panel layout))
    (flet ((item (text url)
	     (let ((item (create-div (left-panel layout))))
	       (create-br (left-panel layout))
	       (create-a item :content text :link url :class "w3-text-yellow")
	       (setf (cursor item) :pointer)
	       (setf (style item "font-size") "18px")
	       (setf (style item "font-family") "Special Elite"))))
      (item "Learn to CLOG" "https://github.com/rabbibotton/clog/blob/main/README.md")
      (item "Learn Common Lisp" "https://github.com/rabbibotton/clog/blob/main/LEARN.md")
      (item "Get CLOG" "https://github.com/rabbibotton/clog")
      (item "About CLOG" "https://github.com/rabbibotton/clog/blob/main/README.md")
      (item "The CLOG Team" "https://www.reddit.com/r/lisp/search/?q=clog"))))
      

(defun on-new-window (body)
  "New browser connection made, display splash screen"
  ;; Set our title in browser
  (setf (title (html-document body)) "clogpower.com")
  ;; If a hard break of connectivity reload the website.
  (set-html-on-close body "<script>location.reload();</script>")
  ;; We use clog-gui for screen oriented pages. We will use in the future
  ;; clog-web for document oriented pages.
  (clog-gui-initialize body)
  ;; Set background color to black
  (add-class body "w3-black")
  ;; Animate using CSS a drop of the clog-open.png
  (create-div body :class "w3-center w3-black w3-animate-top"
		   :content "<br><img width='60%' src='/img/clog-open.png'")
  ;; Load the blimp but don't display yet
  (let ((blimp (create-img body :url-src "/img/clog-blimp.png" :hidden t))
	;; Set a flag to break out of blimp animation early
	(done  nil))
    ;; Clicking anything on page transfers us to the main-screen
    (set-on-click body (lambda (obj)
			 (declare (ignore obj))
			 (setf done t)
			 (setf (inner-html body) "")
			 (main-screen body))
		  :one-time t)
    ;; Animate blimp
    (setf (positioning blimp) :absolute)
    (setf (width blimp) "20%")
    (set-geometry blimp :bottom 0 :right 0)
    (setf (visiblep blimp) t)
    (dotimes (n (width body))
      (unless done
	(set-geometry blimp :bottom (+ n (random 3)) :right (+ n (random 3)))
	(sleep .05)))
    (unless done
      (setf (inner-html body) "")
      (main-screen body))))

(defun start-site ()
  (initialize 'on-new-window
   :static-root (merge-pathnames "./www/"
		  (asdf:system-source-directory :clogpower)))
  (open-browser))
