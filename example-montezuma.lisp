(load "~/quicklisp/setup.lisp")
(ql:quickload :montezuma)

(ql:quickload :montezuma-indexfiles)
(ql:quickload :osicat)

(defun good-symlink-exists-p (pathspec)
  "Checks whether the file named by the pathname designator
PATHSPEC exists and is a symlink pointing to an existent file."
  (not (eq :broken (nth-value 1 (osicat:file-kind pathspec :follow-symlinks t)))))

(defun index-quicklisp ()
  (let ((montezuma-indexfiles::*accept-function*
	 (lambda (x)
	   (and (good-symlink-exists-p x)
		(< (osicat-posix:stat-size (osicat-posix:stat x)) 1000000)))))
    (montezuma-indexfiles::index-directory-tree
     "/home/toni/quicklisp/"
     :index-dir "/home/toni/tmp/montezuma-quicklisp/"
     :merge-factor 100
     :min-merge-docs 100)))

(defun search-quicklisp (text)
  (montezuma-indexfiles::quicksearch text))
