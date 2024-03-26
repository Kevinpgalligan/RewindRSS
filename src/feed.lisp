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
All arguments are strings except ID, which should be a symbol, and
ITEMS, which should be a list of FEED-ITEM objects."
  (assert (and id name))
  (make-instance 'feed :id id
                       :name name
                       :items items
                       :description (or description "")
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

(defvar *save-dir* (uiop:native-namestring "~/.rewindrss.d/"))

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

(defun fetch-and-parse (link)
  (lquery:$ (initialize (drakma:http-request link))))

;;;;;; BETTER INTERFACE
(defparameter *xkcd-link* "https://xkcd.com")

(def-feed xkcd
    (:load-index
     (let ((root (fetch-and-parse *xkcd-link*)))
       (loop for link across (lquery:$ root "a" (attr :href)))))
  (:load-item or maybe :load-content (link)
              ;; do stuff here
              ))

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
