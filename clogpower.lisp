(defpackage #:clogpower
  (:use #:cl #:clog #:clog-gui)
  (:export start-app))

(in-package :clogpower)

(defun main-screen (body)
  (load-css (html-document body)
	    "https://fonts.googleapis.com/css2?family=Smythe&family=Special+Elite&display=swap")
  (let* ((layout (create-panel-box-layout body :left-width 175 :right-width 0))
	 (street (create-img (center-panel layout) :url-src "/img/clog-street.jpg")))
    (setf (text-alignment (center-panel layout)) :center)
    (setf (height street) "100%")
    (setf (width (create-img (left-panel layout)
			     :url-src "/img/clog-lights.png")) "100%")
    (create-br (left-panel layout))
    (create-br (left-panel layout))
    (flet ((item (text url)
	     (let ((item (create-div (left-panel layout))))
	       (set-on-click item (lambda (obj)
				    (open-window (window body) url)))
	       (create-br (left-panel layout))
	       (create-span item :content text :class "w3-text-yellow")
	       (setf (cursor item) :alias)
	       (setf (style item "font-size") "18px")
	       (setf (style item "font-family") "Special Elite"))))
      (item "Learn to CLOG" "https://github.com/rabbibotton/clog/blob/main/README.md")
      (item "Learn Common Lisp" "https://github.com/rabbibotton/clog/blob/main/LEARN.md")
      (item "Get CLOG" "https://github.com/rabbibotton/clog")
      (item "About CLOG" "https://github.com/rabbibotton/clog/blob/main/README.md")
      (item "The CLOG Team" "https://www.reddit.com/r/lisp/search/?q=clog"))))
      

(defun on-new-window (body)
  (setf (title (html-document body)) "clogpower.com")
  (clog-gui-initialize body)
  (add-class body "w3-black")
  (let ((about-div (create-div body :class "w3-center w3-black w3-animate-top"
				    :content "<br><img width='60%' src='/img/clog-open.png'"))
	(blimp     (create-img body :url-src "/img/clog-blimp.png" :hidden t))
	(done      nil))
    (declare (ignore about-div))
    (set-on-click body (lambda (obj)
			 (declare (ignore obj))
			 (setf done t)
			 (setf (inner-html body) "")
			 (main-screen body))
		  :one-time t)
    (setf (positioning blimp) :absolute)
    (setf (width blimp) "20%")
    (set-geometry blimp :bottom 0 :right 0)
    (setf (visiblep blimp) t)
    (dotimes (n (width body))
      (unless done
	(set-geometry blimp :bottom (+ n (random 3)) :right (+ n (random 3)))	  
	(sleep .05)))
    (unless done (main-screen body))))

(defun start-app ()
  (initialize 'on-new-window
   :static-root (merge-pathnames "./www/"
		  (asdf:system-source-directory :clogpower)))
  (open-browser))
