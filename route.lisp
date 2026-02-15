(in-package :fcgi)

(defstruct route
  uri bareword subs string integer splat funcs)

(defstruct uri
  scheme user pass domain path port query)

(defvar +methods+ #("GET" "POST" "PUT" "PATCH" "DELETE" "OPTIONS"))
(defvar *routes* (make-route :funcs (make-array 6 :initial-element nil)))

(defun split-string (by string)
  "Split STRING into a list delimited by character BY"
  (loop for i = 0 then (1+ j)
        for j = (position by string :start i :test #'char=)
        unless (eql i j)
          collect (subseq string i j)
        while j))

(defun add-to-route-tree (uri method func)
  (loop with route = *routes*
        for part in (split-string #\/ uri)
        do (cond ((string= part "") (loop-finish))
                 ((char= (char part 0) #\:) (setf route (or (route-integer route) (setf (route-integer route) (make-route)))))
                 ((char= (char part 0) #\$) (setf route (or (route-string route) (setf (route-string route) (make-route)))))
                 ((char= (char part 0) #\*) (setf route (or (route-splat route) (setf (route-splat route) (make-route)))))
                 (t (setf route (or (find part (route-subs route) :test #'string-equal :key #'route-bareword)
                                    (car (setf (route-subs route) (cons (make-route :bareword part) (route-subs route))))))))
        finally (setf (route-funcs route) (if (arrayp (route-funcs route)) (route-funcs route) (make-array 6 :initial-element nil)) 
                      (aref (route-funcs route) (or (position method +methods+ :test #'string-equal) 0)) func)))

(defun find-function-by-route (route method)
  "Find the (function . list-of-arguments) represented by string ROUTE for method METHOD"
  (find-function-in-tree (split-string #\/ (mime:decode-percentage-hex route)) *routes* nil (or (position method +methods+ :test #'string-equal) 0)))

(defun find-function-in-tree (parts tree args idx)
  (lw:if-let (part (car parts))
             ;;     First try barewords
             (or (lw:when-let (bare (find part (route-subs tree) :test #'string-equal :key #'route-bareword))
                   (find-function-in-tree (cdr parts) bare args idx))
                 ;; Then try string vars
                 (lw:when-let (string (route-string tree))
                   (find-function-in-tree (cdr parts) string (cons part args) idx))
                 ;; Then try integer vars
                 (lw:when-let (pnum (ignore-errors (parse-integer part)))
                   (lw:when-let (int (route-integer tree))
                     (find-function-in-tree (cdr parts) int (cons pnum args) idx)))
                 ;; Then try splats, greediest match downwards
                 (lw:when-let (splat (route-splat tree))
                   (loop for i from (length parts) downto 0 thereis (find-function-in-tree (last parts i) splat (cons (butlast parts i) args) idx))))
             ;;     We've reached the end, so see if there's a func here or in a child splat
             (lw:when-let (funcs (or (route-funcs tree) (and (route-splat tree) (route-funcs (route-splat tree)))))
               (cons (aref funcs idx) (nreverse (if (route-funcs tree) args (cons nil args)))))))

(defun parse-uri (string)
  (destructuring-bind (&optional ign1 scheme userinfo user ign3 pass domain ign4 port path ign5 query ign6)
      (#~m_^((https?)://)?((\w+)(:(\w+))?@)?([^\s:/?]*)?(:(\d+))?([^\s?]*)(\?(.*))?(\s+HTTP/\d\.\d)?.*$_ string)
    (declare (ignore ign1 ign3 ign4 ign5 ign6))
    (make-uri :scheme (or scheme "http")
              :user (when userinfo user)
              :pass (when userinfo pass)
              :domain domain
              :port (or (when port (parse-integer port :junk-allowed t)) (and (equalp scheme "https") 443) 80)
              :path (if (or (null path) (equal path "")) "/" path)
              :query (when query (mime:decode-uri-query (subseq query 0 (position #\Space query :test #'char=)))))))

(defun make-request* (method uri headers body)
  (labels ((update-alist (k v alist)
             (lw:if-let (cons (assoc k alist :test #'equalp))
                        (progn (setf (cdr cons) (append (cdr cons) (list v))) alist)
                        (acons k (list v) alist)))

           (make-alist (&optional alist element)
             (cond ((null element) 
                    nil)
                   ((consp element) 
                    (update-alist (car element) (cdr element) alist))
                   ((mime:mime-p element) 
                    (update-alist (mime:subheader :content-disposition :name element)
                                  (cons (mime:subheader :content-dispotion :filename element)
                                        (mime:mime-body element)) alist))
                   (t (error "Unexpected BODY type ~S" (type-of element)))))

           (maybe-make-alist (body)
             (if (consp body)
                 (reduce #'make-alist body :initial-value  nil)
                 body)))

    (make-rq :uri (parse-uri uri) :headers-in headers :method method :body (maybe-make-alist (mime:decode-body body headers)))))

(defun cookie (key)
  (mime:subheader :cookie key (rq-headers-in *request*)))

(defun session (key)
  (gethash (cookie key) (server-sessions *fcgi*)))

(defun add-session (cookie value)
  (setf (gethash cookie (server-sessions *fcgi*)) value))

(defun (setf session) (value key)
  (if value
      (let ((cookie (loop for try = (random-alpha-ascii-string 12) thereis (and (null (gethash try (server-sessions *fcgi*))) try))))
        (setf (response-header :set-cookie) (format nil "~A=~A; Secure; SameSite=Lax; Path=/" key cookie)
              (gethash cookie (server-sessions *fcgi*)) value))
      (let ((cookie (cookie key)))
        (setf (response-header :set-cookie) (format nil "~A= ; Expires=~A" key (date:format-date (date:date+ (date:now) :hour -1)) date:+rfc2822+))
        (remhash cookie (server-sessions *fcgi*))
        (list (server-sessions *fcgi*))))
  value)

(defun (setf request-code) (code)
  (setf (rq-code *request*) code))

(defun request-header (header)
  "Get the value of HEADER in request REQUEST"
  (mime:header header (rq-headers-in *request*)))

(defun response-header (header)
  "Get the value of HEADER in response REQUEST"
  (mime:header header (rq-headers-in *request*)))

(defun (setf response-header) (value header)
  "Update the response REQUEST with the new HEADER name and VALUE"
  (lw:if-let (cons (assoc header (rq-headers-out *request*)))
             (rplacd cons value)
             (setf (rq-headers-out *request*) (acons header value (rq-headers-out *request*)))))

(defun response-code ()
  (rq-code *request*))

(defun (setf response-code) (code)
  "Update the response REQUEST with the result code"
  (setf (rq-code *request*) code))

(defun body-permitted-p (method code)
  "s4.3 no message-body for :HEAD or for codes in 100 series, 204, 205, or 304 responses"
  (not (or (eq method :head) (find code '(100 101 204 205 304) :test #'=))))

(defun content-length-not-required-p (method code headers)
  "s4.4.3 ignore Content-Length if we have a transfer encoding (or if it's already set)
s4.3 or if we don't have a body to transmit"
  (or (mime:header :transfer-encoding headers) (mime:header :content-length headers) (not (body-permitted-p method code))))

(defun dispatch-request ()
  "Find the matching function for the given request and return the result of invocation"
  (let ((method (or (rq-method *request*) :get)))
    (lw:when-let (func (find-function-by-route (uri-path (rq-uri *request*)) (if (eq method :head) :get method)))
      (funcall (car func) (cdr func) (append (uncons (uri-query (rq-uri *request*))) (rq-body *request*))))))

(defgeneric serve-file (file content-type &optional download-filename-p)
  (:documentation "Blast a copy of FILE back as the response in REQUEST"))

(defmethod serve-file ((file pathname) content-type &optional download-filename-p)
  (lw:when-let (file (and (not (lw:file-directory-p file)) (probe-file file)))
    (with-open-file (stream file :direction :input :element-type '(unsigned-byte 8))
      (let ((type (or content-type (mime:sniff-pathname-mime-type file))))
        (setf (response-header "content-type") type
              (response-header "content-length") (file-length stream))
        (when download-filename-p
          (setf (response-header "content-disposition") (format nil "attachment; filename=\"~A\"" download-filename-p)))))
    file))

(defmethod serve-file ((file vector) content-type &optional download-filename-p)
  (setf (response-header "content-type") content-type
        (response-header "content-length") (length file)
        (response-header "content-disposition") (format nil "~:[inline~;attachment; filename=\"~:*~A\"~]" download-filename-p))

  ;; Cache images unless we were asked not to
  (unless (or (string-not-equal content-type "image" :end1 (min (length content-type) 5))
              (equalp (request-header "cache-control") "no-cache"))
    (setf (response-header "cache-control") "public, max-age=604800, immutable"))
  file)

(defun response-code-text (code)
  "Default response code description"
  (case code
    (200 "OK")
    (201 "Created")
    (202 "Accepted")
    (203 "Non-Authoritative Information")
    (204 "No Content")
    (205 "Reset Content")
    (206 "Partial Content")
    (300 "Multiple Choices")
    (301 "Moved Permanently")
    (302 "Found")
    (303 "See Other")
    (304 "Not Modified")
    (305 "Use Proxy")
    (307 "Temporary Redirect")
    (400 "Bad Request")
    (401 "Unauthorised")
    (402 "Payment Required")
    (403 "Forbidden")
    (404 "Not Found")
    (405 "Method Not Allowed")
    (406 "Not Acceptable")
    (407 "Proxy Authentication Required")
    (408 "Request Timeout")
    (409 "Conflict")
    (410 "Gone")
    (411 "Length Required")
    (412 "Precondition Failed")
    (413 "Request Entity Too Large")
    (414 "Request-URI Too Long")
    (415 "Unsupported Media Type")
    (416 "Requested Range Not Satisfiable")
    (417 "Expectation Failed")
    (500 "Internal Server Error")
    (501 "Not Implemented")
    (502 "Bad Gateway")
    (503 "Service Unavailable")
    (504 "Gateway Timeout")
    (505 "HTTP Version Not Supported")
    (t "Unknown")))
