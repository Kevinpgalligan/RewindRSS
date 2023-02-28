;;;; SECTION: configuration
(defvar *port* 4542)
(defvar *save-dir* (uiop:native-namestring "~/.rewindrss.d/"))

(defun get-link-to-server ()
  (format nil "http://127.0.0.1:~a" *port*))

;;;; SECTION: web server's XML generation.
(hunchentoot:define-easy-handler (feed :uri "/feed.xml") (id-as-string)
  (setf (hunchentoot:content-type*) "application/rss+xml")
  (with-output-to-string (out)
    (write-string "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" out)
    ;; Would prefer to include the rss tag in cl-who bit, but
    ;; the colon in xmlns:atom makes it go crazy.
    (write-string "<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">" out)
    ;; TODO: error if feed does not exist, error if there's a failure
    ;;       to fetch feed items? (Maybe both happen in `load-feed`).
    (let* ((feed (load-feed (intern (string-upcase id-as-string)))))
      (cl-who:with-html-output (out)
        (:channel
         (:title (cl-who:str (name feed)))
         (:link (cl-who:str (format nil "~a/feed.xml?id=~a" (get-link-to-server) id-as-string)))
         (:description (cl-who:str (description feed)))
         (:|pubDate| (cl-who:str (pub-date feed)))
         (:|lastBuildDate| (cl-who:str (last-build-date feed)))
         (loop for item in (items feed)
               do (cl-who:htm
                   (:item
                    (:title (cl-who:str (title item)))
                    (:description (cl-who:str (description item)))
                    (:link (cl-who:str (link item)))
                    (:|pubDate| (cl-who:str (publication-date item)))
                    (:guid (cl-who:str (guid item)))))))))
    (write-string "</rss>" out)))

;;;; SECTION: interface for feeds, their items & defining them.
(defclass feed ()
  ((id
    :initarg :id
    :reader id)
   (name
    :initarg :name
    :reader name)
   (items
    :initarg :items
    :reader items)
   (description
    :initarg :description
    :reader description)
   (publication-date
    :initarg :publication-date
    :reader publication-date)
   (last-build-date
    :initarg :last-build-date
    :reader last-build-date)))

(defun make-feed (&key id name items description publication-date last-build-date)
  "Creates a feed, the only required arguments are ID and NAME.
ID is a unique identifier for the feed, NAME is its display name.
All arguments are strings except ID, which should be a symbol, and ITEMS, which should
be a list of FEED-ITEM objects.
If DESCRIPTION is not provided, NAME will be used as the description."
  (assert (and id name))
  (make-instance 'feed :id id
                       :name name
                       :items items
                       :description (or description name)
                       :publication-date publication-date
                       :last-build-date last-build date))

(defun load-feed (id)
  (let* ((config (load-feed-config id))
         (items (load-feed-items id config)))
    (make-feed :id id
               :name (config-get config :name)
               :items items
               :description (config-get config :description)
               :publication-date (get-oldest-item items))))

(defun load-feed-config (id)
  (let* ((id-as-string (id->string id))
         (path (format nil "~a~a/~a.conf" *save-dir* id-as-string id-as-string)))
    (if (not (uiop:file-exists-p path))
        (error (format nil "Config file ~a does not exist." path))
        (loop for ))))


(defun id->string (id)
  (string-downcase (symbol-name id)))

(defun config-get (config key)
  ;; TODO
  )

(defun load-feed-items (id config)
  ;; TODO
  )

(defun get-oldest-item (items)
  ;; TODO
  )

(defparameter *feed-functions* (make-hash-table))

(defmacro def-load-index-fun ((feed-id) &body body)
  `(push (cons :load-index
               (lambda (,feed-id)
                 ,@body))
         (gethash ,feed-id *feed-functions*)))

(defmacro def-load-content-fun ((feed-id content-data) &body body)
  `(push (cons :load-content
               (lambda (,feed-id ,content-data)
                 ,@body))
         (gethash ,feed-id *feed-functions*)))

(defun get-load-index-fun (feed-id)
  (cdr (assoc :load-index (gethash feed-id *feed-functions*))))

(defun get-load-content-fun (feed-id)
  (cdr (assoc :load-content (gethash feed-it *feed-functions*))))

(defclass feed-item ()
  ((title
    :initarg :title
    :reader title)
   (content
    :initarg :content
    :reader content)
   (link
    :initarg :link
    :reader link)
   (publication-date
    :initarg :publication-date
    :reader publication-date)
   (guid
    :initarg :guid
    :reader guid)))

(defun make-feed-item (&key title content link publication-date, guid)
  "Required arguments: TITLE, DESCRIPTION, and either LINK or GUID.
If GUID is not provided, then LINK is used as the GUID.
The eventual GUID should be unique for each item so that they can be
distinguished from each other"
  (assert (or guid link))
  (assert title)
  (assert description)
  (make-instance 'feed-item
                 :title title
                 :content content
                 :link (or link (get-link-to-server))
                 :publication-date (or publication-date (serialise-time-rfc822 (get-current-time)))
                 :guid (or guid link)))

(defun serialise-time-rfc822 (time)
  "Returns TIME as a string according to the RFC822 standard."
  ;; TODO
  ; "Wed, 02 Oct 2002 08:00:00 EST"
  )

(defun get-current-time ()
  ;; TODO
  )

;;;; SECTION: XKCD example.
(defparameter *xkcd-link* "https://xkcd.com")

(defun fetch-and-parse (link)
  (lquery:$ (initialize (drakma:http-request link))))


(def-load-index-fun (xkcd)
  (let ((root (fetch-and-parse *xkcd-link*)))
    (loop for link across (lquery:$ root "a" (attr :href))
          for num-posts = (multiple-value-bind (full-match substrings)
                              (cl-ppcre:scan-to-strings (format nil "~a/(\\d+)" *xkcd-link*) link)
                            (declare (ignore full-match))
                            (when substrings
                              (aref substrings 0)))
          when num-posts
            return (loop for k from 1 upto (parse-integer num-posts)
                         collect (format nil "~a/~a/" *xkcd-link* k)))))

(def-load-content-fun (xkcd link)
    (let ((root (fetch-and-parse link)))
      (make-feed-item :title (xkcd-get-title root)
                      :content (xkcd-get-content root)
                      :link link
                      :publication-date (serialise-time-rfc822 (get-current-time))
                      :guid link)))

(defun xkcd-get-title (root)
  (aref (lquery:$ root "div #ctitle" (text)) 0))

(defun xkcd-get-content (root)
  (aref (lquery:$ root "div #comic" (serialize)) 0))
