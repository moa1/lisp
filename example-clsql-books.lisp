;; Trying out clsql.

(load "~/quicklisp/setup.lisp")
(ql:quickload :clsql)
(clsql:file-enable-sql-reader-syntax)

(clsql:def-view-class person ()
  ((first-name :type string :initarg :first-name :accessor person-first-name :db-kind :key :db-constraints :not-null)
   (last-name :type string :initarg :last-name :accessor person-last-name :db-kind :key :db-constraints :not-null)
   (birthday :type clsql:date :initarg :birthday :accessor person-birthday :db-kind :base))
  (:base-table person))

(defmethod print-object ((person person) stream)
  (print-unreadable-object (person stream)
    (format stream "~A,~A,~A" (person-first-name person) (person-last-name person) (person-birthday person))))

(clsql:def-view-class book ()
  ((title :type string :initarg :title :accessor book-title :db-kind :key :db-constraints :not-null)
   ;; here we allow only one author
   (author-first-name :type string :initarg :author-first-name :db-kind :base)
   (author-last-name :type string :initarg :author-last-name :db-kind :base)
   (author :type person :initarg :author :accessor book-author :db-kind :join :db-info (:join-class person :home-key author-first-name :foreign-key first-name :home-key author-last-name :foreign-key last-name :set nil))
   (stores :reader book-stores :db-kind :join :db-info (:join-class store-book :home-key title :foreign-key book-title :target-slot store :set t)))
  (:base-table book))

(defmethod print-object ((book book) stream)
  (print-unreadable-object (book stream)
    (format stream "~A,~A" (book-title book) (book-author book))))

(clsql:def-view-class store ()
  ((name :type string :initarg :name :accessor store-name :db-kind :key :db-constraints :not-null)
   (address :type string :initarg :address :accessor store-address :db-kind :base :db-constraints :not-null)
   (owner-first-name :type string :initarg :owner-first-name :db-kind :base)
   (owner-last-name :type string :initarg :owner-last-name :db-kind :base)
   (owner :type person :initarg :owner :accessor store-owner :db-kind :join :db-info (:join-class person :home-key owner-first-name :foreign-key first-name :home-key owner-last-name :foreign-key last-name :retrieval :immediate))
   (books :reader store-books :db-kind :join :db-info (:join-class store-book :home-key name :foreign-key store-name :target-slot book :set t)))
  (:base-table store))

(defmethod print-object ((store store) stream)
  (print-unreadable-object (store stream)
    (format stream "~A,~A,~A" (store-name store) (store-address store) (store-owner store))))

(clsql:def-view-class store-book ()
  ((store-name :type string :initarg :store-name :db-kind :key :db-constraints :not-null)
   (store :type store :initarg :store :accessor store-book-store :db-kind :join :db-info (:join-class store :home-key store-name :foreign-key name :retrieval :immediate))
   (book-title :type string :initarg :book-title :db-kind :key :db-constraints :not-null)
   (book :type book :initarg :book :accessor store-book-book :db-kind :join :db-info (:join-class book :home-key book-title :foreign-key title :retrieval :immediate)))
  (:base-table store-book))

(defmethod print-object ((store-book store-book) stream)
  (print-unreadable-object (store-book stream)
    (format stream "~A,~A" (store-book-store store-book) (store-book-book store-book))))

;; create the tables
(defun create-tables ()
  (ignore-errors ;ignore that a table may already exist
    (clsql:create-view-from-class 'person)
    (clsql:create-view-from-class 'book)
    (clsql:create-view-from-class 'store)
    (clsql:create-view-from-class 'store-book)))

(defun drop-tables ()
  (clsql:drop-table 'person)
  (clsql:drop-table 'book)
  (clsql:drop-table 'store)
  (clsql:drop-table 'store-book))

(defun add-person (first-name last-name birthday)
  (let ((person (make-instance 'person :first-name first-name :last-name last-name :birthday birthday)))
    (clsql:update-records-from-instance person)
    person))

(defun add-book (title &optional author)
  (let ((book (make-instance 'book :title title :author author)))
    (when author
      (setf (slot-value book 'author-first-name) (person-first-name author)
	    (slot-value book 'author-last-name) (person-last-name author)))
    (clsql:update-records-from-instance book)
    book))

(defun add-store (name address &optional owner)
  (let ((store (make-instance 'store :name name :address address :owner owner)))
    (when owner
      (setf (slot-value store 'owner-first-name) (person-first-name owner)
	    (slot-value store 'owner-last-name) (person-last-name owner)))
    (clsql:update-records-from-instance store)
    store))

(defun add-store-book (store book)
  (let ((store-book (make-instance 'store-book :store store :book book)))
    (setf (slot-value store-book 'store-name) (store-name store)
	  (slot-value store-book 'book-title) (book-title book))
    (clsql:update-records-from-instance store-book)
    store-book))

(defun insert-or-select-person (first-name last-name date)
  (clsql:select 'person :where [and [= [slot-value 'person 'first-name] first-name] [= [slot-value 'person 'last-name] last-name]]))

;;; MAIN

(defvar *database-file* "/tmp/example-clsql.sqlite3")

(defun connect (&optional delete-database)
  (when delete-database
    (ignore-errors (delete-file *database-file*)))
  (clsql:connect `(,*database-file*) :database-type :sqlite3 :encoding :UTF-8)
  (create-tables))

(defun create-some-entries ()
  ;; create some entries
  (let* ((jacob (add-person "Jacob" "Grimm" (clsql:make-date :year 1785 :month 1 :day 4)))
	 (wilhelm (add-person "Wilhelm" "Grimm" (clsql:make-date :year 1786 :month 2 :day 24)))
	 (book1 (add-book "Deutsches Wörterbuch" jacob)) ;actually both jacob and wilhelm
	 (book2 (add-book "Kinder- und Hausmärchen" wilhelm)) ;actually both jacob and wilhelm
	 (book3 (add-book "Beowulf")) ;anonymous author
	 (hugendubel (add-person "Heinrich Karl Gustav" "Hugendubel" (clsql:make-date :year 1840 :month 2 :day 5)))
	 (store (add-store "Hugendubel" "Marienplatz München" hugendubel)))
    (add-store-book store book1)
    (add-store-book store book3)))

(defun main ()
  (ignore-errors (create-some-entries))
  (format t "Books in store: ~A~%" (store-books (car (car (clsql:select 'store))))))

(defun disconnect ()
  (clsql:disconnect))

(connect)
(clsql:start-sql-recording)
(clsql:stop-sql-recording)
(main)
(disconnect)
