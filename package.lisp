(defpackage :fcgi
  (:use :cl :utils)
  (:export
   "ADD-TO-ROUTE-TREE"
   "FIND-FUNCTION-BY-ROUTE"
   "MAKE-URI"
   "PARSE-URI"
   "REQUEST-HEADER"
   "REQUEST-USER"
   "RESPONSE-CODE"
   "RESPONSE-HEADER"
   "SERVE-FILE"
   "SET-COOKIE"
   "START-FCGI"
   "STOP-FCGI"
   "UNSET-COOKIE"
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
