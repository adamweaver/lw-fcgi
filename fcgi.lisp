(in-package :fcgi)

(defstruct server
  acceptor sessions)

(defstruct rq
  connection method uri body headers-in (code nil) headers-out)

(defvar *request* nil)

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
        (+ (ash (logand val 127) 24)
           (ash (aref buffer (1+ offset)) 16)
           (ash (aref buffer (+ offset 2)) 8)
           (aref buffer (+ offset 3))))))

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
          nconc (mime:make-header* (nsubstitute #\- #\_ (if (starts-with name "http_" :test #'string-equal)
                                                            (nsubseq name 5)
                                                            name)
                                                :test #'char=) value)
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
  (setf *fcgi* (make-server :acceptor (comm:start-up-server :process-name "FCGI" :function #'accept-fcgi-request :service port) 
                            :sessions (make-hash-table :test #'equalp))))

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
  (handler-bind ((error (lambda (c) (declare (ignore c)) (return-from accept-fcgi-request))))
    (process-fcgi-connection (make-instance 'comm:socket-stream :socket handle :direction :io :element-type '(unsigned-byte 8)))))

(defun process-fcgi-connection (connection)
  (handler-bind ((error (lambda (c)
                          (declare (ignore c))
                          (ignore-errors (close connection) (return-from process-fcgi-connection))))
                 (fcgi-close-handle (lambda (c)
                                      (declare (ignore c))
                                      (ignore-errors (close connection) (return-from process-fcgi-connection)))))
    (loop
      (multiple-value-bind (type id buffer) (read-fcgi-record connection)
        (handler-bind ((error (lambda (c) (output-fcgi-response nil id connection (princ-to-string c) "text/plain" 503))))
          (declare (type fixnum type id) (type (array (unsigned-byte 8)) buffer))
          (case* #'eql type
            (+fcgi-begin-request+ (fcgi-begin-request id buffer))
            (+fcgi-abort-request+ (fcgi-abort-request connection id))
            (+fcgi-end-request+ (fcgi-end-request connection id 0 0))
            (+fcgi-params+ (fcgi-params id buffer))
            (+fcgi-stdin+ (fcgi-stdin connection id buffer))
            (+fcgi-data+ (fcgi-data connection id buffer))
            (+fcgi-get-values+ (fcgi-get-values connection id buffer))
            (t (error "Unknown FCGI type ~S" type))))))))

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

(defun fcgi-end-request (connection id app protocol)
  (let ((closep (fcgi-request-close (aref *fcgi-requests* id))))
    (setf (aref *fcgi-requests* id) nil)
    (let ((buffer (make-array 8 :element-type '(unsigned-byte 8) :initial-contents (list 0 0 0 app protocol 0 0 0))))
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
         (*request* (make-request* method uri headers-in (fcgi-request-stdin fcgi))))
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
    (when (mime:image-p pathname)
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
           (or (rq-code *request*) 200) (response-code-text (or (rq-code *request*) 200)) mime:+crlf+
           (mapcar (lambda (cons) (list (car cons) (cdr cons) mime:+crlf+)) (rq-headers-out *request*)) mime:+crlf+) :utf-8))

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

