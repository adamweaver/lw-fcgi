(in-package :fcgi)

(defstruct server
  acceptor sessions)

(defstruct route
  name uri bareword subs string integer splat func)

(defstruct rq
  connection method uri body headers-in (code nil) user headers-out)

(defstruct uri
  scheme user pass domain path port query)

(defvar *fetches* nil)
(defvar *gets* (make-route :name "*"))
(defvar *posts* (make-route :name "*"))

;;; ============================================================================
;;; FCGI
;;; ============================================================================
(define-condition fcgi-close-handle () ())

(defun output-fcgi-record (stream type id content)
  (let ((content-length (length content)))
    (loop for start from 0 below content-length by 32768
          for end = (min content-length (+ start 32768))
          for width = (- end start)
          for mod = (mod width 8)
          for padding = (if (plusp mod) (- 8 mod) 0)
          do
             (write-byte 1 stream)
             (write-byte type stream)
             (write-byte (ldb (byte 8 8) id) stream)
             (write-byte (ldb (byte 8 0) id) stream)
             (write-byte (ldb (byte 8 8) width) stream)
             (write-byte (ldb (byte 8 0) width) stream)
             (write-byte padding stream)
             (write-byte 0 stream)
             (write-sequence content stream :start start :end end)
             (loop repeat padding do (write-byte 0 stream)))))

(defun get-encoded-int (buffer offset)
  (let ((val (aref buffer offset)))
    (if (< val 128)
        val
        (+ (ash (logand val 127) 24) (ash (aref buffer (1+ offset)) 16) (ash (aref buffer (+ offset 2)) 8) (aref buffer (+ offset 3))))))

(defun get-name-value-pair (c-buffer offset)
  "Get the string NAME and string VALUE"
  (let* ((nlen (get-encoded-int c-buffer offset))
         (noff (+ offset (if (< nlen 128) 1 4)))
         (vlen (get-encoded-int c-buffer noff))
         (voff (+ noff (if (< vlen 128) 1 4))))
    (list (ef:decode-external-string c-buffer :utf-8 :start voff :end (+ voff nlen))
          (ef:decode-external-string c-buffer :utf-8 :start (+ voff nlen) :end (+ voff nlen vlen))
          (+ voff nlen vlen))))

(defun get-alist-of-names-and-values (array)
  (when (plusp (length array))
    (loop with len = (length array)
          for (name value end) = (get-name-value-pair array 0) then (get-name-value-pair array end)
          collect (cons (nsubstitute #\- #\_ name :test #'char=) value)
          unless (< end len) do (loop-finish))))

(defun make-fcgi-headers (buffer)
  (when (plusp (length buffer))
    (loop with len = (length buffer)
          for (name value end) = (get-name-value-pair buffer 0) then (get-name-value-pair buffer end)
          nconc (mime:make-header* (nsubstitute #\- #\_ (if (starts-with name "http_" :test #'string-equal) (nsubseq name 5) name) :test #'char=) value)
          unless (< end len) do (loop-finish))))

(defun read-fcgi-record (connection)
  (unless (= (read-byte connection) 1)
    (signal 'fcgi-close-handle))
  (let ((type (read-byte connection))
        (id (+ (ash (read-byte connection) 8) (read-byte connection)))
        (len (+ (ash (read-byte connection) 8) (read-byte connection)))
        (pad (read-byte connection)))
    (read-byte connection)
    (let ((buffer (make-array len :element-type '(unsigned-byte 8))))
      (read-sequence buffer connection)
      (loop repeat pad do (read-byte connection))
      (values type id buffer))))

(defconstant +fcgi-begin-request+ 1)
(defconstant +fcgi-abort-request+ 2)
(defconstant +fcgi-end-request+ 3)
(defconstant +fcgi-params+ 4)
(defconstant +fcgi-stdin+ 5)
(defconstant +fcgi-stdout+ 6)
(defconstant +fcgi-stderr+ 7)
(defconstant +fcgi-data+ 8)
(defconstant +fcgi-get-values+ 9)
(defconstant +fcgi-get-values-result+ 10)
(defconstant +fcgi-unknown-type+ 11)
(defconstant +fcg-max-type+ 11)

(defconstant +fcgi-null-request-id+ 0)

(defconstant +fcgi-keep-conn+ 1)
(defconstant +fcgi-responder+ 1)
(defconstant +fcgi-authorizer+ 2)
(defconstant +fcgi-filter+ 3)

(defconstant +fcgi-request-complete+ 0)
(defconstant +fcgi-cant-mpx-conn+ 1)
(defconstant +fcgi-overloaded+ 2)
(defconstant +fcgi-unknown-role+ 3)

(defvar *fcgi* nil)
(defvar *fcgi-requests* (make-array 128 :adjustable t))
(defstruct fcgi-request id headers mode stdin data close)

(defun start-fcgi (port)
  "Start up a new thread to serve requests listening on PORT"
  (setf *fcgi* (make-server :acceptor (comm:start-up-server :process-name "FCGI Acceptor" :function #'accept-fcgi-request :service port) :sessions (make-hash-table :test #'equalp))))

(defun stop-fcgi ()
  "Kill the FCGI process"
  (when *fcgi*
    (mp:process-kill (server-acceptor *fcgi*))
    (setf (server-acceptor *fcgi*) nil *fcgi* nil)))

(defun create-fcgi-request (id close)
  (unless (< id (length *fcgi-requests*))
    (setf *fcgi-requests* (adjust-array *fcgi-requests* (1+ id))))
  (setf (aref *fcgi-requests* id) (make-fcgi-request :id id :headers nil :mode nil :stdin nil :data nil :close close)))

(defun get-fcgi-request (id)
  (aref *fcgi-requests* id))

(defun accept-fcgi-request (handle)
  (process-fcgi-connection (make-instance 'comm:socket-stream :socket handle :direction :io :element-type '(unsigned-byte 8))))

(defun process-fcgi-connection (connection)
  (loop
    (multiple-value-bind (type id buffer) (read-fcgi-record connection)
      (handler-bind ((fcgi-close-handle (lambda (c) (declare (ignore c)) (ignore-errors (close connection) (return-from process-fcgi-connection))))
                     (error (lambda (c) (output-fcgi-response nil id connection (princ-to-string c) "text/plain" 503))))
        (declare (type fixnum type id) (type (array (unsigned-byte 8)) buffer))
        (case* #'eql type
          (+fcgi-begin-request+ (fcgi-begin-request id buffer))
          (+fcgi-abort-request+ (fcgi-abort-request connection id))
          (+fcgi-end-request+ (fcgi-end-request connection id 0 0))
          (+fcgi-params+ (fcgi-params id buffer))
          (+fcgi-stdin+ (fcgi-stdin connection id buffer))
          (+fcgi-data+ (fcgi-data connection id buffer))
          (+fcgi-get-values+ (fcgi-get-values connection id buffer))
          (t (error "Unknown FCGI type ~S" type)))))))

(defun fcgi-begin-request (id buffer)
  (declare (type fixnum id) (type (array (unsigned-byte 8)) buffer))
  (let ((role (+ (ash (aref buffer 0) 8) (aref buffer 1))) (flags (aref buffer 2)))
    (create-fcgi-request id (zerop (logand flags 1)))
    (case* #'eql role
      (+fcgi-responder+ (fcgi-begin-responder id))
      (+fcgi-authorizer+ (fcgi-begin-authorizer id))
      (+fcgi-filter+ (fcgi-begin-filter id))
      (t (error "Unknown FCGI role ~A" role)))))

(defun fcgi-get-values (connection id in-buffer)
  (declare (type fixnum id) (type (array (unsigned-byte 8)) in-buffer))
  (let ((queries (get-alist-of-names-and-values in-buffer))
        (buffer (make-array 256 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (flet ((vpe (key value)
             (vector-push (length key) buffer)
             (vector-push (length value) buffer)
             (loop for k across key do (vector-push (char-code k) buffer))
             (loop for v across value do (vector-push (char-code v) buffer))))
      (loop for (q) in queries
            if (string-equal q "FCGI-MAX-CONNS")
              do (vpe "FCGI_MAX_CONNS" "10")
            else
              if (string-equal q "FCGI-MAX-REQS")
                do (vpe "FCGI_MAX_REQS" "50")
            else
              if (string-equal q "FCGI-MPXS-CONNS")
                do (vpe "FCGI_MPXS_CONNS" "0")))
    (output-fcgi-record connection +fcgi-get-values-result+ id buffer)))

(defun fcgi-params (id buffer)
  (declare (type fixnum id) (type (array (unsigned-byte 8)) buffer))
  (when (plusp (length buffer))
    (let ((fcgi (get-fcgi-request id)))
      (setf (fcgi-request-headers fcgi) (make-fcgi-headers buffer)))))

(defun fcgi-stdin (connection id buffer)
  (declare (type fixnum id) (type (array (unsigned-byte 8)) buffer))
  (let ((fcgi (get-fcgi-request id)) (len (length buffer)))
    (cond ((and (zerop len) (eq (fcgi-request-mode fcgi) :responder))
           (handle-fcgi-response id connection fcgi))
          ((and (zerop len) (eq (fcgi-request-mode fcgi) :authorizer))
           (handle-fcgi-authorizer id connection fcgi))
          (t (merge-stdins fcgi buffer)))))

(defun merge-stdins (fcgi buffer)
  (if (fcgi-request-stdin fcgi)
      (let ((old-len (length (fcgi-request-stdin fcgi))))
        (setf (fcgi-request-stdin fcgi) (adjust-array (fcgi-request-stdin fcgi) (+ old-len (length buffer))))
        (replace (fcgi-request-stdin fcgi) buffer :start1 old-len))
      (setf (fcgi-request-stdin fcgi) buffer)))

(defun fcgi-data (connection id buffer)
  (declare (type fixnum id) (type (array (unsigned-byte 8)) buffer))
  (let ((fcgi (get-fcgi-request id)) (len (length buffer)))
    (if (and (zerop len) (eq (fcgi-request-mode fcgi) :filter))
        (handle-fcgi-filter id connection fcgi)
        (setf (fcgi-request-data fcgi) buffer))))

(defun fcgi-abort-request (connection id)
  (fcgi-end-request connection id 0 0))

(defun fcgi-end-request (connection id app-status protocol-status)
  (let ((closep (fcgi-request-close (aref *fcgi-requests* id))))
    (setf (aref *fcgi-requests* id) nil)
    (let ((buffer (make-array 8 :element-type '(unsigned-byte 8) :initial-contents (list 0 0 0 app-status protocol-status 0 0 0))))
      (output-fcgi-record connection +fcgi-end-request+ id buffer))
    (when closep
      (signal 'fcgi-close-handle))))

(defun fcgi-begin-responder (id)
  (let ((fcgi (get-fcgi-request id)))
    (setf (fcgi-request-mode fcgi) :responder)))

(defun handle-fcgi-response (id connection fcgi)
  (let* ((headers-in (fcgi-request-headers fcgi))
         (method (mime:header :request-method headers-in))
         (uri (mime:header :request-uri headers-in))
         (config:*request* (make-request* method uri headers-in (fcgi-request-stdin fcgi))))
    (multiple-value-bind (body content code) (dispatch-request)
      (output-fcgi-response fcgi id connection body content code))))

(defun output-fcgi-response (fcgi id connection body content code)
  (let ((buffer (if (pathnamep body)
                    (make-fcgi-pathname-buffer body content code)
                    (make-fcgi-buffer (if (stringp body) (ef:encode-lisp-string body :utf-8) body) content code))))
    (output-fcgi-record connection +fcgi-stdout+ id buffer)
    (when (plusp (length buffer)) (output-fcgi-record connection +fcgi-stdout+ id nil))
    (fcgi-end-request connection id 0 0)
    (when (or (null fcgi) (fcgi-request-close fcgi)) (signal 'fcgi-close-handle))))

(defun make-fcgi-pathname-buffer (pathname content-type code)
  (with-open-file (in pathname :direction :input :element-type '(unsigned-byte 8))
    (setf (response-header :content-length) (file-length in)
          (response-header :content-type) content-type
          (response-code) code)
    (when (config:image-p pathname)
      (setf (response-header :cache-control) "max-age=3600, public"))
    (let* ((headers (make-fcgi-response-headers))
           (buffer (make-array (+ (length headers) (file-length in)) :element-type '(unsigned-byte 8))))
      (replace buffer headers)
      (read-sequence buffer in :start (length headers))
      buffer)))

(defun make-fcgi-buffer (body content-type code)
  (setf (response-code) code)
  (when body (setf (response-header :content-type) content-type))
  (let* ((headers (make-fcgi-response-headers))
         (buffer (make-array (+ (length headers) (length body)) :element-type '(unsigned-byte 8))))
    (replace buffer headers)
    (replace buffer body :start1 (length headers))
    buffer))

(defun make-fcgi-response-headers ()
  (ef:encode-lisp-string
   (format nil "Status: ~D ~A~A~{~{~:(~A~): ~A~A~}~}~A"
           (or (rq-code config:*request*) 200) (response-code-text (or (rq-code config:*request*) 200)) mime:+crlf+
           (mapcar (lambda (cons) (list (car cons) (cdr cons) mime:+crlf+)) (rq-headers-out config:*request*)) mime:+crlf+) :utf-8))

(defun fcgi-begin-authorizer (id)
  (let ((fcgi (get-fcgi-request id)))
    (setf (fcgi-request-mode fcgi) :authorizer)))

(defun handle-fcgi-authorizer (id connection fcgi)
  (declare (ignore fcgi))
  (fcgi-end-request connection id 200 0))

(defun fcgi-begin-filter (id)
  (let ((fcgi (get-fcgi-request id)))
    (setf (fcgi-request-mode fcgi) :filter)))

(defun handle-fcgi-filter (id connection fcgi)
  (output-fcgi-record connection +fcgi-stdout+ id (fcgi-request-data fcgi))
  (fcgi-end-request connection id 0 0))

;;; ============================================================================
;;; ROUTING
;;; ============================================================================

(defun split-string (by string)
  "Split STRING into a list delimited by character BY"
  (loop for i = 0 then (1+ j)
        for j = (position by string :start i :test #'char=)
        unless (eql i j)
          collect (subseq string i j)
        while j))

(defun add-to-route-tree (name uri method func)
  (loop with route = (if (eq method :post) *posts* *gets*)
        for part in (split-string #\/ uri)
        do (cond ((string= part "") (loop-finish))
                 ((char= (char part 0) #\:) (setf route (or (route-integer route) (setf (route-integer route) (make-route)))))
                 ((char= (char part 0) #\$) (setf route (or (route-string route) (setf (route-string route) (make-route)))))
                 ((char= (char part 0) #\*) (setf route (or (route-splat route) (setf (route-splat route) (make-route)))))
                 (t (setf route (or (find part (route-subs route) :test #'string-equal :key #'route-bareword)
                                    (car (setf (route-subs route) (cons (make-route :bareword part) (route-subs route))))))))
        finally (setf (route-name route) name (route-func route) func)))

(defun find-function-by-route (route method)
  "Find the (function . list-of-arguments) represented by string ROUTE for method METHOD"
  (find-function-in-tree (split-string #\/ (mime:decode-percentage-hex route)) (if (eq method :post) *posts* *gets*) nil))

(defun find-function-in-tree (parts tree args)
  (lw:if-let (part (car parts))
             ;;     First try barewords
             (or (lw:when-let (bare (find part (route-subs tree) :test #'string-equal :key #'route-bareword))
                   (find-function-in-tree (cdr parts) bare args))
                 ;; Then try string vars
                 (lw:when-let (string (route-string tree))
                   (find-function-in-tree (cdr parts) string (cons part args)))
                 ;; Then try integer vars
                 (lw:when-let (pnum (ignore-errors (parse-integer part)))
                   (lw:when-let (int (route-integer tree))
                     (find-function-in-tree (cdr parts) int (cons pnum args))))
                 ;; Then try splats, greediest match downwards
                 (lw:when-let (splat (route-splat tree))
                   (loop for i from (length parts) downto 0 thereis (find-function-in-tree (last parts i) splat (cons (butlast parts i) args)))))
             ;;     We've reached the end, so see if there's a func here or in a child splat
             (lw:when-let (func (or (route-func tree) (and (route-splat tree) (route-func (route-splat tree)))))
               (cons func (nreverse (if (route-func tree) args (cons nil args)))))))

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
                    (update-alist (mime:subheader :content-disposition :name element) (cons (mime:subheader :content-dispotion :filename element) (mime:mime-body element)) alist))
                   (t (error "Unexpected BODY type ~S" (type-of element)))))

           (maybe-make-alist (body)
             (if (consp body)
                 (reduce #'make-alist body :initial-value  nil)
                 body)))

    (make-rq :uri (parse-uri uri) 
             :headers-in headers
             :method (if (string-equal method "POST") :post :get)
             :body (maybe-make-alist (mime:decode-body body headers))
             :user (gethash (mime:subheader :cookie :riskmate headers) (server-sessions *fcgi*)))))

(defun set-cookie (user)
  (let ((cookie (loop for try = (random-alpha-ascii-string 12) thereis (and (null (gethash try (server-sessions *fcgi*))) try))))
    (setf (response-header :set-cookie) (format nil "riskmate=~A; Secure; SameSite=Lax; Path=/" cookie)
          (gethash cookie (server-sessions *fcgi*)) user)))

(defun unset-cookie ()
  (setf (response-header :set-cookie) (format nil "riskmate=z; Expires=~A" (date:format-date (date:date+ (date:now) :hour -1)) date:+rfc2822+)))

(defun (setf request-code) (code)
  (setf (rq-code config:*request*) code))

(defun request-header (header)
  "Get the value of HEADER in request REQUEST"
  (mime:header header (rq-headers-in config:*request*)))

(defun request-user ()
  (rq-user config:*request*))

(defun response-header (header)
  "Get the value of HEADER in response REQUEST"
  (mime:header header (rq-headers-in config:*request*)))

(defun (setf response-header) (value header)
  "Update the response REQUEST with the new HEADER name and VALUE"
  (lw:if-let (cons (assoc header (rq-headers-out config:*request*)))
             (rplacd cons value)
             (setf (rq-headers-out config:*request*) (acons header value (rq-headers-out config:*request*)))))

(defun response-code ()
  (rq-code config:*request*))

(defun (setf response-code) (code)
  "Update the response REQUEST with the result code"
  (setf (rq-code config:*request*) code))

(defun body-permitted-p (method code)
  "s4.3 no message-body for :HEAD or for codes in 100 series, 204, 205, or 304 responses"
  (not (or (eq method :head) (find code '(100 101 204 205 304) :test #'=))))

(defun content-length-not-required-p (method code headers)
  "s4.4.3 ignore Content-Length if we have a transfer encoding (or if it's already set)
s4.3 or if we don't have a body to transmit"
  (or (mime:header :transfer-encoding headers) (mime:header :content-length headers) (not (body-permitted-p method code))))

(defun dispatch-request ()
  "Find the matching function for the given request and return the result of invocation"
  (let ((method (or (rq-method config:*request*) :get)))
    (lw:when-let (func (find-function-by-route (uri-path (rq-uri config:*request*)) (if (eq method :head) :get method)))
      (funcall (car func) (cdr func) (append (uncons (uri-query (rq-uri config:*request*))) (rq-body config:*request*))))))

(defgeneric serve-file (file content-type &optional download-filename-p)
  (:documentation "Blast a copy of FILE back as the response in REQUEST"))

(defmethod serve-file ((file pathname) content-type &optional download-filename-p)
  (lw:when-let (file (and (not (config:directoryp file)) (probe-file file)))
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
