;; Dependencies: hunchentoot, cl-who, drakma, lquery

;; References:
;; https://edicl.github.io/cl-who/#with-html-output-to-string
;; https://edicl.github.io/hunchentoot/#port80
;; And to start up the server from the REPL...
;;   (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))

;; TODO:
;; * Finish example interface for XKCD.
;; * Create code to tie it all together:
;;    - On start-up, load files in config directory. Allows user to define new feeds. And configure
;;      the port. And URL?
;;    - Don't cache the feed itself, as that can change depending on user config.
;;    - Well, maybe cache it just to compare the output, then if it has changed u can go back
;;      and increase the ID of the feed so that feed readers reload it?
;;    - On receiving a request: load cached index / info (if it exists... if
;;      not, load from scratch); if a new post is due, parse it & cache it; return the feed.
;;    - Need a way to configure each of the feeds (active/inactive, which days of week, weekly or fortnightly or what have you).
;;    - Can it be made platform independent somehow?
;; * Can we somehow link the config for each feed & the code that's used to parse it?
;; * Allow configuring an icon.
;; * Error handling.
;; * Allow caching of resources, e.g. could save XKCD image and inject a link to local server's copy of resource.
;; * Force rate limiting to each site so that we don't spam.
;; * Customise HTTP request, should maybe indicate that it's us.

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

;;;; SECTION: XKCD example.
(defparameter *xkcd-link* "https://xkcd.com")
(def-feed-source xkcd)
(def-scrape-index-fun (xkcd)
  (let ((root (lquery:$ (initialize (drakma:http-request *xkcd-link*)))))
    (loop for link across (lquery:$ root "a" (attr :href))
          for num-posts = (multiple-value-bind (full-match substrings)
                              (cl-ppcre:scan-to-strings (format nil "~a/(\\d+)" *xkcd-link*) link)
                            (declare (ignore full-match))
                            (when substrings
                              (aref substrings 0)))
          when num-posts
            return (loop for k from 1 upto (parse-integer num-posts)
                         collect (format nil "~a/~a/" *xkcd-link* k)))))
(def-scrape-content-fun (xkcd id)
  ;; TODO implement function to scrape the content for a particular link
  )
