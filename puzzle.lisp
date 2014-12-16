(asdf:oos 'asdf:load-op 'utils)

(defparameter +puzzle+
  (substitute #\  #\Newline 
	      "fle lin tlg bte eee tai ttt rec emm sej aet adc aib yns em lei aat ohe
tuh xln ohc nhm tsn ane eaa msy gio onl ulc ote isk pys ten hte clc
fet gaa ese sda sgt mta mlt mdl yuh mi - JOE"))

(defparameter +triple+ (split +puzzle+ #\Space))

(prind (unique +triple+ :only t))
(prind (unique +triple+ :not t))

(defparameter +a-z+ (mapcar #'code-char (range (char-code #\a)
					 :stop (char-code #\z) :incl t)))

(defparameter +freq+ (mapcar (lambda (x) (cons x (count x +puzzle+))) +a-z+))

(defparameter +sorted-freq+ (sort (copy-seq +freq+) #'> :key #'cdr))
;; frequencies could be normal (see WP: Letter frequency):
;; etaoin shrdlu cmfwyp vbgkqj xz

(defparameter +triple-s0+
  (stable-sort (copy-seq +triple+)
	       #'<
	       :key (lambda (x)
		      (char-code
		       (elt x 0)))))
(defparameter +triple-s1+
  (stable-sort (copy-seq +triple+)
	       #'<
	       :key (lambda (x)
		      (char-code
		       (elt x (min (1- (length x)) 1))))))
(defparameter +triple-s2+
  (stable-sort (copy-seq +triple+)
	       #'<
	       :key (lambda (x)
		      (char-code
		       (elt x (min (1- (length x)) 2))))))

(prind "salt soon?" (map 'string (lambda (x) (elt x (min (1- (length x)) 0))) +triple-s1+))

(defparameter +nospace+ (remove #\Space +puzzle+))
