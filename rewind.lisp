;;;; SECTION: dummy data.
(defparameter *somedate* "Wed, 02 Oct 2002 08:00:00 EST")

(defparameter *posts*
  `(("First" "Description" "http://127.0.0.1:4242/item1.html" ,*somedate* "http://127.0.0.1:4242/item1.html")
    ("Second" "Description" "http://127.0.0.1:4242/item2.html" ,*somedate* "http://127.0.0.1:4242/item2.html")))

;;;; SECTION: web server's XML generation.
(hunchentoot:define-easy-handler (feed :uri "/feed.xml") (name)
  (setf (hunchentoot:content-type*) "application/rss+xml")
  (with-output-to-string (out)
    (write-string "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" out)
    ;; Would prefer to include the rss tag in cl-who bit, but
    ;; the colon in xmlns:atom makes it go crazy.
    (write-string "<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">" out)
    (cl-who:with-html-output (out)
      (:channel
       (:title "NameHere")
       (:link (cl-who:str (format nil "http://127.0.0.1:4242/feed.xml?name=~a" name)))
       (:description "Description here.")
       (:|pubDate| (cl-who:str *somedate*))
       (:|lastBuildDate| (cl-who:str *somedate*))
       (loop for (title description link pubdate guid) in *posts*
             do (cl-who:htm
                 (:item
                  (:title (cl-who:str title))
                  (:description (cl-who:str description))
                  (:link (cl-who:str link))
                  (:|pubDate| (cl-who:str pubdate))
                  (:guid (cl-who:str guid)))))))
    (write-string "</rss>" out)))


;;;; SECTION: interface for defining per-feed scraping.
(defgeneric scrape-index (source))
(defgeneric scrape-content (source content-identifier))

(defparameter *feed-sources* nil)

(defmacro def-feed-source (name)
  `(progn
     (push ',name *feed-sources*)
     (defclass ,name () ())))

(defmacro def-scrape-index-fun ((source) &body body)
  `(defmethod scrape-index ((source ,source))
     ,@body))

(defmacro def-scrape-content-fun ((source identifier-var) &body body)
  `(defmethod scrape-content ((source ,source) ,identifier-var)
     ,@body))

(defclass feed-item ()
  ((title
    :initarg :title
    :reader title)
   (content
    :initarg :content
    :reader content)))

(defun make-feed-item (&key title content)
  (make-instance 'feed-item :title title :content content))

;;;; SECTION: XKCD example.
(defparameter *xkcd-link* "https://xkcd.com")

(defun fetch-and-parse (link)
  (lquery:$ (initialize (drakma:http-request link))))

(def-feed-source xkcd)

(def-scrape-index-fun (xkcd)
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

(def-scrape-content-fun (xkcd link)
    (let ((root (fetch-and-parse link)))
      (make-feed-item :title (xkcd-get-title root)
                      :content (xkcd-get-content root))))

(defun xkcd-get-title (root)
  (aref (lquery:$ root "div #ctitle" (text)) 0))

(defun xkcd-get-content (root)
  (aref (lquery:$ root "div #comic" (serialize)) 0))
