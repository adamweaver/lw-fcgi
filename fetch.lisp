(in-package :fetch)

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
    (fetch* (if (stringp uri) (parse-uri uri) uri) verb headers content-type body)))

(defun fetch* (uri verb headers content-type body)
  (let ((*fetches* (cons uri *fetches*)))
    (lw:if-let (stream (open-fetch-stream uri))
               (send-request verb (if (uri-p uri) uri (parse-uri uri)) body headers content-type stream)
               (error 'http-request-error :message (format nil "Unable to open fetch stream ~A~@[:~A~]" (uri-domain uri) (uri-port uri))))))

(defun open-fetch-stream (uri)
  (let ((domain (uri-domain uri)) (port (or (uri-port uri) (and (string-equal (uri-scheme uri) "https") 443) 80)))
    (comm:open-tcp-stream domain port :element-type '(unsigned-byte 8) :direction :io :ssl-ctx (string-equal (uri-scheme uri) "https"))))

(defun output-fetch-request-string (stream verb uri body headers content-type)
  (setf headers (update-alist :host (uri-domain uri) headers))
  (when body (setf headers (update-alist :content-type (or content-type "text/plain") (update-alist :content-length (length body) headers))))
  (format stream "~:@(~A~) ~A~@[?~A~] HTTP/1.1~A~{~{~:(~A~): ~A~A~}~}" verb (uri-path uri)
          (mime:encode-uri-query (uri-query uri)) mime:+crlf+ (mapcar (lambda (cons) (list (car cons) (cdr cons) mime:+crlf+)) headers))
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
  (let ((redir (parse-uri redir)))
    (make-uri :scheme (if (and (uri-domain redir) (string/= "" (uri-domain redir))) (uri-scheme redir) (uri-scheme uri))
              :user (uri-user redir)
              :pass (uri-pass redir)
              :domain (if (and (uri-domain redir) (string/= "" (uri-domain redir))) (uri-domain redir) (uri-domain uri))
              :path (uri-path redir)
              :query (uri-query redir))))

