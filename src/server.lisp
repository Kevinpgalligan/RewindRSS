(in-package rewind)

(defvar *port* 4542)

(defun get-link-to-server ()
  (format nil "http://127.0.0.1:~a" *port*))

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
