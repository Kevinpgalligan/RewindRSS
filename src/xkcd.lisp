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
