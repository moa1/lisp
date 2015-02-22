;(asdf:oos 'asdf:load-op 'rsm-string)
(asdf:oos 'asdf:load-op :utils)
(asdf:oos 'asdf:load-op 'regex)

(use-package :utils)

; types:
; list-all-packages returns a list, result type is: (VALUES LIST &optional)
; apropos-list returns a list, result type is (VALUES LIST &OPTIONAL)

(defstruct geoip-range
  (ip-start nil :type integer)
  (ip-stop nil :type integer)
  (country nil :type string))


(define-condition parse-geoip-match-error (error)
  ((str :initarg :str :reader str)))

(let ((matcher (regex:compile-str "\"(\\d+)\",\"(\\d+)\",(\".+\")\\r?$")))
  (defun parse-geoip-webhosting-info (stream )
    ;; http://ip-to-country.webhosting.info/downloads/ip-to-country.csv.zip
    (labels ((rec (stream)
	       (let ((line (read-line stream nil)))
		 (if (null line)
		     nil
		     (destructuring-bind (whole ip-start ip-stop country)
			 (match matcher line)
		       (declare (ignore whole))
		       (register-geoip-range
			(make-geoip-range :ip-start (string->integer ip-start)
					  :ip-stop (string->integer ip-stop)
					  :country country))
		       (rec stream))))))
      (rec stream))))

(defvar *geoip-ranges* nil)

(defun register-geoip-range (geoip-range)
  ;; (format t "~A~%" geoip-range))
  (push geoip-range *geoip-ranges*))

(defun parse-geoip ()
  (with-open-file (stream "/home/toni/text/soft/ip-to-country.csv")
    (parse-geoip-webhosting-info stream))
  (setq *geoip-ranges* (sort *geoip-ranges* #'<= :key #'geoip-range-ip-start))
  (length *geoip-ranges*))

(let ((ip-dotted-matcher
       (regex:compile-str "^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)$")))
  (defun ip-dotted->long (ip-dotted)
    (destructuring-bind (whole a b c d)
	(match ip-dotted-matcher ip-dotted)
      (declare (ignore whole))
      (reduce (lambda (x y) (+ (* x 256) y))
	      (map 'list #'string->integer (list a b c d))))))

(defun geoip-lookup (ip)
  (flet ((geoip-cmp (ip g)
	   (let ((g-start (geoip-range-ip-start g))
		 (g-stop (geoip-range-ip-stop g)))
	     (if (and (>= ip g-start) (<= ip g-stop))
		 0
		 (if (< ip g-start)
		     -1
		     1)))))
;; compilation for single query takes a looong time..
;;    (let ((bsearch (compile-binary-search *geoip-ranges*
;;					  :predicate #'geoip-cmp
;;					  :exact t)))
;;      (format t "searching~%")
;;      (funcall bsearch (ip-dotted->long ip)))))
    (binary-search (ip-dotted->long ip)
		   *geoip-ranges* :predicate #'geoip-cmp :exact t)))


(defun binary-search-c-predicate-emitter (m-elt lower-emitted win-emitted
					  higher-emitted)
  (format nil "if (obj < ~A) {~%~A~%} else if (obj > ~A) {~%~A~%} else {~%~A~%}"
	  m-elt lower-emitted m-elt higher-emitted win-emitted))

(defun binary-search-c-geoip-predicate-emitter (m-elt lower-emitted win-emitted
						higher-emitted)
  (let ((g-start (geoip-range-ip-start m-elt))
	(g-stop (geoip-range-ip-stop m-elt)))
    (format nil "if(obj<~A){~A}else if(obj>~A){~A}else{if(obj>=~A&&obj<=~A){~A}}"
	    m-elt lower-emitted m-elt higher-emitted g-start g-stop
	    win-emitted)))

(defun binary-search-c-win-emitter (win-elt)
  (format nil "win=~A;found=1;" win-elt))

(defun binary-search-c-geoip-win-emitter (win-elt)
  (let ((g-country (geoip-range-country win-elt)))
    (format nil "win=~A;found=1;" g-country)))

(defconstant binary-search-c-fail-emitted ";")

(defun binary-search-c-body-emitter (bsearch)
  (format nil "#include <stdio.h>~%#include <stdlib.h>~%int main(int argc, char** argv)~%{~%int obj,win,found=0;~%if (argc > 1) obj = atoi(argv[1]); else return 1;~%~A~%if (found) printf(\"%i\\n\",win); else printf(\"not found\\n\");}~%"
	  bsearch))

(defun binary-search-c-geoip-body-emitter (bsearch)
  (format nil "#include <stdio.h>~%#include <stdlib.h>~%int main(int argc, char** argv)~%{~%int obj,found=0;char* win=\"not found\";~%if (argc > 1) obj = atoi(argv[1]); else return 1;~%~A~%if (found) printf(\"%s\\n\",win); else printf(\"not found\\n\");}~%"
	  bsearch))

(defun bs-pe (m-elt lower-emitted win-emitted higher-emitted)
  `(:pe ,m-elt ,lower-emitted ,win-emitted ,higher-emitted))

(defun bs-we (win-elt)
  `(:we ,win-elt))

(defconstant bs-fe '(:fe))

(defun bs-be (bsearch)
  `(:be ,bsearch))

(defun geoip-wc (filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede
			  :if-does-not-exist :create)
    (let ((tree (emit-compile-binary-search
		 *geoip-ranges*
		 #'bs-pe
		 #'bs-we
		 bs-fe
		 :body-emitter #'bs-be)))
      (labels ((fs (&rest args) (apply #'format stream args))
	       (walk (form)
		 (ecase (car form)
		   (:pe (destructuring-bind
			      (m-elt le we he) (cdr form)
			  (fs "if(obj<~A){" (geoip-range-ip-start m-elt))
			  (walk le)
			  (fs "}else if(obj>~A){"
			      (geoip-range-ip-stop m-elt))
			  (walk he)
			  (fs "}else if((obj>=~A)&&(obj<=~A)){"
			      (geoip-range-ip-start m-elt)
			      (geoip-range-ip-stop m-elt))
			  (walk we)
			  (fs "}")))
		   (:we (destructuring-bind
			      (we) (cdr form)
			  (fs "win=\"~A\";found=1;"
			      (string-escape-c (geoip-range-country we)))))
		   (:fe (fs ";"))
		   (:be (destructuring-bind (be) (cdr form)
			  (fs "#include <stdio.h>~%#include <stdlib.h>~%int main(int argc, char** argv)~%{~%int obj,found=0;char* win=\"not found\";~%if (argc > 1) obj = atoi(argv[1]); else return 1;~%")
			  (walk be)
			  (fs "~%if (found) printf(\"%s\\n\",win); else printf(\"not found\\n\");}~%")
			  )))))
	(walk tree))))
  t)


(defun geoip-write-c (filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede
			  :if-does-not-exist :create)
    
    (let ((src (emit-compile-binary-search
		*geoip-ranges*
		#'binary-search-c-geoip-predicate-emitter
		#'binary-search-c-geoip-win-emitter
		binary-search-c-fail-emitted
		:body-emitter #'binary-search-c-geoip-body-emitter)))
      (write-string src stream)))
  t)

