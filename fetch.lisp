(in-package :fetch)

(defvar *fetches*)

(define-condition http-request-error (error)
  ((message :initarg :message :reader message))
  (:report (lambda (error stream) (format stream "HTTP error: ~A"  (message error)))))

(defun update-alist (key value alist)
  (lw:if-let (cons (assoc key alist))
             (progn (rplacd cons value) alist)
             (acons key value alist)))

(defun get! (uri &key headers)
  (fetch! uri "GET" headers nil nil))

(defun post! (uri &key headers body content-type)
  (fetch! uri "POST" headers content-type body))

(defun put! (uri &key headers body content-type)
  (fetch! uri "PUT" headers content-type body))

(defun patch! (uri &key headers body content-type)
  (fetch! uri "PATCH" headers content-type body))

(defun delete! (uri &key headers body content-type)
  (fetch! uri "PATCH" headers content-type body))

(defun fetch! (uri verb headers content-type body)
  "Open a connection to URI, return (VALUES body-of-reply reply-content-type other-reply-headers)"
  (let ((*fetches* nil))
    (fetch* (if (stringp uri) (fcgi:parse-uri uri) uri) verb headers content-type body)))

(defun fetch* (uri verb headers content-type body)
  (let ((*fetches* (cons uri *fetches*)))
    (lw:if-let (stream (open-fetch-stream uri))
               (send-request verb (if (fcgi:uri-p uri) uri (fcgi:parse-uri uri)) body headers content-type stream)
               (error 'http-request-error :message (format nil "Unable to open fetch stream ~A~@[:~A~]" (fcgi:uri-domain uri) (fcgi:uri-port uri))))))

(defun open-fetch-stream (uri)
  (let ((domain (fcgi:uri-domain uri)) (port (or (fcgi:uri-port uri) (and (string-equal (fcgi:uri-scheme uri) "https") 443) 80)))
    (comm:open-tcp-stream domain port :element-type '(unsigned-byte 8) :direction :io :ssl-ctx (string-equal (fcgi:uri-scheme uri) "https"))))

(defun output-fetch-request-string (stream verb uri body headers content-type)
  (setf headers (update-alist :host (fcgi:uri-domain uri) headers))
  (when body (setf headers (update-alist :content-type (or content-type "text/plain") (update-alist :content-length (length body) headers))))
  (format stream "~:@(~A~) ~A~@[?~A~] HTTP/1.1~A~{~{~:(~A~): ~A~A~}~}" verb (fcgi:uri-path uri)
          (mime:encode-uri-query (fcgi:uri-query uri)) mime:+crlf+ (mapcar (lambda (cons) (list (car cons) (cdr cons) mime:+crlf+)) headers))
  (when body
    (write-string mime:+crlf+ stream)
    (write-sequence body stream))
  (write-string mime:+crlf+ stream))

(defun send-request (verb uri body headers content-type stream)
  (output-fetch-request-string stream verb uri body headers content-type)
  (finish-output stream)
  (multiple-value-bind (response-code response-text response-headers response-body) (read-fetch-response stream)
    (cond
      ((null response-code) (error 'http-request-error :message "Unexpected NIL response"))
      ((<= 200 response-code 299)  (values response-body (mime:header :content-type response-headers) response-headers))
      ((and (<= 300 response-code 399) (< (length *fetches*) 6))
       (fetch* (merge-uri (mime:header :location response-headers) uri) verb headers content-type body))
      ((<= 300 response-code 399) (error 'http-request-error :message (format nil "Too many redirects: ~{~A~^ redirected from ~}." *fetches*)))
      (t (error 'http-request-error :message (format nil "Unexpected reply ~D: ~A" response-code response-text))))))

(defun read-fetch-response (stream)
  (destructuring-bind (code text) (#~m_^HTTP/...\s+(\d\d\d)\s+(.*)_ (read-line stream))
    (let ((doc (mime:parse stream)))
      (values (when code (parse-integer code)) text (mime:mime-headers doc) (mime:mime-body doc)))))

(defun merge-uri (redir uri)
  (let ((redir (fcgi:parse-uri redir)))
    (fcgi:make-uri :scheme (if (and (fcgi:uri-domain redir) (string/= "" (fcgi:uri-domain redir))) (fcgi:uri-scheme redir) (fcgi:uri-scheme uri))
                   :user (fcgi:uri-user redir)
                   :pass (fcgi:uri-pass redir)
                   :domain (if (and (fcgi:uri-domain redir) (string/= "" (fcgi:uri-domain redir))) (fcgi:uri-domain redir) (fcgi:uri-domain uri))
                   :path (fcgi:uri-path redir)
                   :query (fcgi:uri-query redir))))

