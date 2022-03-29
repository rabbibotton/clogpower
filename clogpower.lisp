(defpackage #:clogpower
  (:use #:cl #:clog #:clog-gui)
  (:export start-app))

(in-package :clogpower)

(defclass app-data ()
  ((data
    :accessor data)))

(defun on-new-window (body)
  (let ((app (make-instance 'app-data)))
    (setf (connection-data-item body "app-data") app)
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
			   (setf (inner-html body) "")))
      (setf (positioning blimp) :absolute)
      (setf (width blimp) "20%")
      (set-geometry blimp :bottom 0 :right 0)
      (setf (visiblep blimp) t)
      (dotimes (n (width body))
	(unless done
	  (set-geometry blimp :bottom (+ n (random 3)) :right (+ n (random 3)))	  
	  (sleep .05))))))

(defun start-app ()
  (initialize 'on-new-window
   :static-root (merge-pathnames "./www/"
		  (asdf:system-source-directory :clogpower)))
  (open-browser))
