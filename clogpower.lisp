;; Setup or clogpower package.
(defpackage #:clogpower
  (:use #:cl #:clog #:clog-web #:clog-gui #:clog-auth #:clog-web-dbi)
  (:export start-site))

(in-package :clogpower)

;;
;; Setup website structure, database and CLOG
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *sql-connection* nil)

;; We use authorizations to control what menus appear if logged in or not.
;; We use the :authorize key on create-web-page to limit access as well
;; directly to a page when not using menus.
;; We use our menu with the convenient clog-web-routes-from-menu to
;; setup the routes from URLs to handlers:

                      ; Menu         Menu Item         URL        Handler         Actions Auth
(defparameter *menu* `(("Features" (("Login"           "/login"   on-login        :login)
				    ("Signup"          "/signup"  on-signup       :signup)
				    ("Change Password" "/pass"    on-new-pass     :change-password)
				    ("Home"            "/content" on-clogpower    :content)
				    ("Demo"            "/demo"    on-demo-window)
				    ("Logout"          "/logout"  on-logout       :logout)))
		       ("Admin"    (("User List"       "/users"   on-users        :users)))
		       ("Help"     (("About"           "/about"   on-about))))
  "Setup website menu")

(defun add-search-optimizations (path content)
  ;; The default boot.html that comes with CLOG has template
  ;; markers inside of the meta section and body section
  ;; that are set to be transparent to the user but show to
  ;; search engines and text browser. This allows setting
  ;; custom data for search engine optimizations which are
  ;; aware of these type of dynamic sites.
  (declare (ignore path))
  (funcall (cl-template:compile-template content)
	   (list :meta "<meta name='description' content='CLOGPower.com CLOG Framework Home Page'>
                        <meta name='author' content='David Botton'>
                        <meta name='keywords' content='Common Lisp, GUI, Web Framework, CLOG'>"
		 :body "CLOGPower.com - CLOG the Common Lisp Omnificient GUI
                        a site dedicated to CLOG.")))

(defun start-site (&key (port 8080))
  ;; Setup authorizations between roles and actions
  ;; multiple roles can be activated to multiple actions
  ;; A user can be assign many roles
  (add-authorization '(:guest :member) '(:content
					 :content-show-comments))
  (add-authorization '(:guest)  '(:login :signup))
  (add-authorization '(:member) '(:logout
				  :change-password
				  :content-comment))
  (add-authorization '(:editor) '(:content-edit))
  (add-authorization '(:admin)  '(:users :content-admin))
  ;; Setup database connection
  (when *sql-connection*
    (dbi:disconnect *sql-connection*))
  (let ((db-dir (format nil "~A~A" (asdf:system-source-directory :clogpower) "clogpower.db")))
    (setf *sql-connection* (dbi:connect :sqlite3 :database-name db-dir))
    (format t "Database location: ~A~%" db-dir))
  ;; Check if need to setup sample data
  (handler-case
      (dbi:fetch (dbi:execute (dbi:prepare *sql-connection* "select * from config")))
    (error ()
      (print "Create database and tables.")
      (create-base-tables *sql-connection*)))
  ;; Turn off debugger launches for production runs
  (setf clog-connection:*break-on-error* nil)
  (initialize 'on-clogpower :boot-function 'add-search-optimizations
			    :long-poll-first t
			    :extended-routing t
			    :port port
			    :static-root (merge-pathnames "./www/"
							  (asdf:system-source-directory :clogpower)))
  (clog-web-routes-from-menu *menu*)
  (loop (sleep 360)))
;;
;; Look and Feel
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init-site (body)
  "Setup the website, called on each url switch"
  ;; Initialize the clog-web environment
  (clog-web-initialize body)
  ;; Instantly reload other windows open on authentication change
  (set-on-authentication-change body (lambda (body)
				       (url-replace (location body) "/")))
  ;; Initialzie the clog-web-site environment
  (let ((profile (get-profile body *sql-connection*)))
    (create-web-site body
		     :settings '(:color-class  "w3-amber"
				 :border-class ""
				 :signup-link  "/signup"
				 :login-link   "/login")
		     :profile profile
		     :roles (if profile
				(if (equalp "admin"
					    (getf profile :|username|))
				    '(:member :editor :admin)
				    '(:member))
				'(:guest))
		     :title "CLOG - The Common Lisp Omnificent GUI"
		     :footer "(c) 2022 David Botton"
		     :logo "/img/clog-logo.png")))

;;
;; URL Path Handlers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun on-clogpower (body)
  (init-site body)
  (create-web-page body :index `(:menu    ,*menu*
				 :content ,(clog-web-content *sql-connection*
							     :comment-table "content"))))

(defun on-login (body)
  (init-site body)
  (create-web-page
   body
   :login `(:menu      ,*menu*
	    :on-submit ,(lambda (obj)
			  (if (login body *sql-connection*
				     (name-value obj "username")
				     (name-value obj "password"))
			      (url-replace (location body) "/")
			      (clog-web-alert obj "Invalid" "The username and password are invalid."
					      :time-out 3
					      :place-top t))))
   :authorize t))

(defun on-logout (body)
  (logout body)
  (url-replace (location body) "/"))

(defun on-signup (body)
  (init-site body)
  (create-web-page body
		   :signup `(:menu    ,*menu*
			     :content ,(lambda (body)
					 (sign-up body *sql-connection*)))
		   :authorize t))

(defun on-about (body)
  (init-site body)
  (create-web-page body :about `(:menu    ,*menu*
				 :content "<img width=300 src='/img/clog-logo.png'>")))

(defun on-users (body)
  (init-site body)
  (create-web-page body :users
		   `(:menu    ,*menu*
		     :content ,(lambda (body)
				 (let ((users (dbi:fetch-all
					       (dbi:execute
						(dbi:prepare
						 *sql-connection*
						 "select * from users")))))
				   (dolist (user users)
				     (let* ((box   (create-div body))
					    (suser (create-span box :content (getf user :|username|)))
					    (rbut  (create-button box :content "Reset Password"
								      :class "w3-margin-left")))
				       (declare (ignore suser))
				       (set-on-click rbut (lambda (obj)
							    (declare (ignore obj))
							    (reset-password *sql-connection*
									    (getf user :|username|))
							    (setf (disabledp rbut) t)
							    (setf (text rbut) "Done"))))))))
			:authorize t))

(defun on-new-pass (body)
  (init-site body)
  (create-web-page body
		   :change-password `(:menu    ,*menu*
				      :content ,(lambda (body)
						  (change-password body *sql-connection*)))
		   :authorize t))

;; Old Demosite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun demo-main-screen (body)
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

(defun on-demo-window (body)
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
			 (demo-main-screen body))
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
