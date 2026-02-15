(defpackage :fcgi
  (:use :cl :utils)
  (:export
   "ADD-SESSION"
   "ADD-TO-ROUTE-TREE"
   "COOKIE"
   "FIND-FUNCTION-BY-ROUTE"
   "MAKE-URI"
   "PARSE-URI"
   "REQUEST-HEADER"
   "RESPONSE-CODE"
   "RESPONSE-HEADER"
   "SERVE-FILE"
   "SESSION"
   "SET-COOKIE"
   "START-FCGI"
   "STOP-FCGI"
   "URI"
   "URI-DOMAIN"
   "URI-P"
   "URI-PASS"
   "URI-PATH"
   "URI-PORT"
   "URI-QUERY"
   "URI-SCHEME"
   "URI-USER"))


(defpackage :fetch
  (:use :cl :utils)
  (:export "GET!" "POST!" "PUT!" "PATCH!" "DELETE!"))
