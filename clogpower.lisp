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
      (item "The CLOG Team" "https://www.reddit.com/r/lisp/search?q=clog&restrict_sr=on"))
    ;; Create a chat button that launches in a floating window
    (let ((chat-button (create-button (left-panel layout) :content "Chat")))
      (setf (style chat-button "font-size") "18px")
      (setf (style chat-button "font-family") "Special Elite")
      (set-on-click chat-button
		    (lambda (obj)
		      ;; We use keep-on-top for floating clog-gui windows not in a clog-gui
		      ;; desktop
		      (input-dialog obj "Enter your chat handle:"
				    (lambda (result)
				      (when result
					(let* ((win    (create-gui-window obj :keep-on-top t
									      :title "lichat"))
					       ;; We use builder to create the chat box
					       (talker (create-clog-lichat-talker (window-content win))))

					  (setf (chat talker) (make-instance 'lichat-tcp-client:client
									     :username result
									     :hostname "chat.tymoon.eu"))
					  (lichat-tcp-client:open-connection (chat talker))
					  (lichat-tcp-client:s (chat talker)
							       'join
							       :channel "lichatters")
					  (defmethod lichat-tcp-client:process ((update lichat-protocol:message)
										(client (eql (chat talker))))
					    (create-div (chat-box talker)
							:content (format nil "~a - ~a"
									 (lichat-protocol:from update)
									 (lichat-protocol:text update)))
					    (setf (scroll-top (chat-box talker)) (scroll-height (chat-box talker))))
					  ;; Use run body to hold a thread until connection to browser dies
					  ;; Once gone, sever connection with the chat server.
					  (run body)
					  (lichat-tcp-client:close-connection (chat talker))
					  ;; Log disconnect
					  (format t "Chat disconnect - ~a" result))))
				    :title "Handle"))))
    (sleep 1)
    (let ((alien (create-img (center-panel layout)
			     :url-src "/img/clog-alien.png"
			     :hidden t)))
      (setf (positioning alien) :absolute)
      (set-geometry alien :bottom -250 :left 0)
      (add-class alien "w3-animate-left")
      (setf (visiblep alien) t)
      (sleep 2)
      (destroy alien))))

(defun send-message (panel)
  "Chat send button event handler"
  (lichat-tcp-client:s (chat panel)
		       'message
		       :channel "lichatters"
		       :text (text-value (chat-area panel)))
  (setf (text-value (chat-area panel)) ""))

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
