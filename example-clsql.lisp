;; Trying out clsql.

(load "~/quicklisp/setup.lisp")
(ql:quickload :clsql)

(defpackage :example-clsql
  (:use :cl :clsql))
(in-package :example-clsql)

(file-enable-sql-reader-syntax)

;;; Begin: from the documentation of #'DEF-VIEW-CLASS
;;(def-view-class person (thing)
(def-view-class person ()
  ((height :db-kind :base :accessor height :type float
           :initarg :height)
   (married :db-kind :base :accessor married :type boolean
            :initarg :married)
   (birthday :type clsql:wall-time :initarg :birthday)
   (bd-utime :type clsql:universal-time :initarg :bd-utime)
   (hobby :db-kind :virtual :initarg :hobby :initform nil)))

(def-view-class employee (person)
  ((emplid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :emplid)
   (groupid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :groupid)
   (first-name
    :accessor first-name
    :type (varchar 30)
    :initarg :first-name)
   (last-name
    :accessor last-name
    :type (varchar 30)
    :initarg :last-name)
   (email
    :accessor employee-email
    :type (varchar 100)
    :initarg :email)
   (ecompanyid
    :type integer
    :initarg :companyid)
   (company
    :accessor employee-company
    :db-kind :join
    :db-info (:join-class company
                          :home-key ecompanyid
                          :foreign-key companyid
                          :set nil))
   (managerid
    :type integer
    :initarg :managerid)
   (manager
    :accessor employee-manager
    :db-kind :join
    :db-info (:join-class employee
                          :home-key managerid
                          :foreign-key emplid
                          :set nil))
   (addresses
    :accessor employee-addresses
    :db-kind :join
    :db-info (:join-class employee-address
                          :home-key emplid
                          :foreign-key aemplid
                          :target-slot address
                          :set t))
   (addresses-no-target
    :accessor employee-addresses-no-target
    :db-kind :join
    :db-info (:join-class employee-address
                          :home-key emplid
                          :foreign-key aemplid
                          :set t))
   (deferred-addresses
    :accessor employee-deferred-addresses
    :db-kind :join
    :db-info (:join-class deferred-employee-address
                          :home-key emplid
                          :foreign-key aemplid
                          :target-slot address
                          :set t)))
  (:base-table employee))

(def-view-class company ()
  ((companyid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :companyid)
   (groupid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :groupid)
   (name
    :type (varchar 100)
    :initarg :name)
   (presidentid
    :type integer
    :initarg :presidentid)
   (president
    :reader president
    :db-kind :join
    :db-info (:join-class employee
                          :home-key presidentid
                          :foreign-key emplid
                          :set nil))
   (employees
    :reader company-employees
    :db-kind :join
    :db-info (:join-class employee
                          :home-key (companyid groupid)
                          :foreign-key (ecompanyid groupid)
                          :set t))))

(def-view-class address ()
  ((addressid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :addressid)
   (street-number
    :type integer
    :initarg :street-number)
   (street-name
    :type (varchar 30)
    :void-value ""
    :initarg :street-name)
   (city
    :column "city_field"
    :void-value "no city"
    :type (varchar 30)
    :initarg :city)
   (postal-code
    :column zip
    :type integer
    :void-value 0
    :initarg :postal-code))
  (:base-table addr))

;; many employees can reside at many addressess
(def-view-class employee-address ()
  ((aemplid :type integer :initarg :emplid)
   (aaddressid :type integer :initarg :addressid)
   (verified :type boolean :initarg :verified)
   (address :db-kind :join
            :db-info (:join-class address
                                  :home-key aaddressid
                                  :foreign-key addressid
                                  :retrieval :immediate)))
  (:base-table "ea_join"))

(def-view-class deferred-employee-address ()
  ((aemplid :type integer :initarg :emplid)
   (aaddressid :type integer :initarg :addressid)
   (verified :type boolean :initarg :verified)
   (address :db-kind :join
            :db-info (:join-class address
                                  :home-key aaddressid
                                  :foreign-key addressid
                                  :retrieval :deferred
                                  :set nil)))
  (:base-table "ea_join"))

;;; End: from the documentation of #'DEF-VIEW-CLASS

;;; MAIN

(defun create-tables ()
  (clsql:create-view-from-class 'person)
  (clsql:create-view-from-class 'employee)
  (clsql:create-view-from-class 'company)
  (clsql:create-view-from-class 'address)
  ;; uncomment one of the following two lines
  ;;(clsql:create-view-from-class 'employee-address)
  (clsql:create-view-from-class 'deferred-employee-address)
  )

(defun drop-tables ()
  (clsql:drop-table 'employee)
  (clsql:drop-table 'addr)
  (clsql:drop-table 'ea_join))

(defvar *database-name* "/tmp/example-clsql.sqlite3")

(defun my-connect (&optional delete-database)
  (when *default-database*
    (disconnect))
  (when delete-database
    (ignore-errors (delete-file *database-name*)))
  (connect `(,*database-name*) :database-type :sqlite3 :encoding :UTF-8))

(defun my-disconnect ()
  (disconnect))

(defun insert-some-entries ()
  (execute-command "insert into employee(emplid,groupid,first_name,last_name,email,ecompanyid,managerid,height,married,birthday,bd_utime) values (1,1,\"Max\",\"Mustermann\",\"mmustermann@company.com\",1,1,1.70,0,\"1981-01-01\",2556140400)")
  (execute-command "insert into employee(emplid,groupid,first_name,last_name,email,ecompanyid,managerid,height,married,birthday,bd_utime) values (2,1,\"Alex\",\"Farmer\",\"alex.farmer@company.com\",1,1,1.85,0,\"1980-01-01\",2524518000)")
  (execute-command "insert into company(companyid,groupid,name,presidentid) values (1,1,\"Acme, Inc.\",1)")
  (execute-command "insert into addr(addressid,street_number,street_name,city_field,zip) values (1,123,\"Wheatfield Avenue\",\"Elkhart\",90112)")
  (execute-command "insert into addr(addressid,street_number,street_name,city_field,zip) values (2,124,\"Wheatfield Avenue\",\"Elkhart\",90112)")
  (execute-command "insert into addr(addressid,street_number,street_name,city_field,zip) values (3,456,\"Powerplant Street\",\"Shreveport\",91234)")
  (execute-command "insert into ea_join(aemplid,aaddressid,verified) values (1,1,0)")
  (execute-command "insert into ea_join(aemplid,aaddressid,verified) values (1,2,0)")
  (execute-command "insert into ea_join(aemplid,aaddressid,verified) values (2,3,0)"))

(defun query-some-queries ()
  (let* ((company (first (select 'company :where (sql-operation '== (sql-expression :table 'company :attribute 'companyid) 1) :flatp t))) ;same as (SELECT 'COMPANY :WHERE [= [SLOT-VALUE 'COMPANY 'COMPANYID] 1] :FLATP T)
	 (president (president company))
	 (president-name (concatenate 'string (slot-value president 'first-name) " " (slot-value president 'last-name))))
    (format t "company: ~A~%" company)
    (format t "presdnt: ~A~%" president)
    (format t "presnam: ~A~%" president-name)
    (format t "emplads: ~A~%" (employee-addresses president))
    (format t "empladn: ~A~%" (employee-addresses-no-target president))
    ;;(format t "~A~%" (employee-deferred-addresses president)) error
    company
    ))

(my-connect t)
(create-tables)
(insert-some-entries)
(clsql:start-sql-recording)
(clsql:stop-sql-recording)
(query-some-queries)
;;(my-disconnect)
