(defpackage :fcgi
  (:use :cl :utils)
  (:export
   "URI" "MAKE-URI" "START-FCGI" "STOP-FCGI" "ADD-TO-ROUTE-TREE" "FIND-FUNCTION-BY-ROUTE" "SET-COOKIE" "UNSET-COOKIE"
   "REQUEST-HEADER" "REQUEST-USER" "RESPONSE-HEADER" "RESPONSE-CODE" "SERVE-FILE"))

(defpackage :fetch
  (:use :cl :utils)
  (:export "GET!" "POST!" "PUT!" "PATCH!" "DELETE!"))
