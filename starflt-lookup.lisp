(asdf:oos 'asdf:load-op 'utils)
(asdf:oos 'asdf:load-op 'cl-ppcre)

(defparameter first-alist
  '(("arth" . 1)
    ("thoss/eleran" . 2)
    ("harrison base" . 3)
    ("sphexi" . 4)
    ("spewta" . 5)
    ("earth" . 6)
    ("mardan 2" . 7)
    ("new scotland" . 8)
    ("koann 3" . 9)
    ("heaven" . 10)
    ("uhlek brain wd." . 11)
    ("gall" . 12)
    ("akteron" . 13)
    ("nirvana" . 14)
    ("the staff" . 15)
    ("the cross" . 16)
    ("pythagoras" . 17)
    ("the 4 seedlings" . 18)
    ("the axe" . 19)
    ("city of ancients" . 20)
    ("mars" . 21)
    ("crystal planet" . 22)
    ("elan" . 23)
    ("votiputox" . 24)))

(defparameter second-alist
  '(("dodecahedron" . 0)
    ("black box" . 23)
    ("mobius device" . 22)
    ("crystal orb" . 21)
    ("fright apparatus" . 20)
    ("rod device" . 19)
    ("red cylinder" . 18)
    ("rubber widget" . 17)
    ("throb mass" . 16)
    ("suprise utensil" . 15)
    ("wee green blobbie" . 14)
    ("tesser act" . 13)
    ("whining orb" . 12)
    ("bladed toy" . 11)
    ("nice thing" . 10)
    ("ellipsoid" . 9)
    ("humming gizzy" . 8)
    ("glowing disk" . 7)
    ("black egg" . 6)
    ("amazing artifact" . 5)
    ("shimmer ball" . 4)
    ("flat device" . 3)
    ("blue bauble" . 2)
    ("crystal cone" . 1)))

(let* ((third-1
	'("VELOX    " 877443 336818 944682   536992   100139   259789   298483
	  "THRYNN   " 100119 743593 981215   555412   133909   218651   726134
	  "ELOWAN   " 780433 991615 562162   864256   875009   100151   100163
	  "MECHANS  " 10174  90610  51932    72507    12957    79279    33548
	  "SPEMIN   " 10180  70354  62683    74048    10200    67312    84209
	  "GAZURTOID" 93385  10190  66682    90020    75292    18200    76235
	  "UHLEK    " 47038  36602  62394    10210    45830    95267    10240
	  "MINSTERLS" 7754   9291   6532     8073     7160     3793     5647
	  "MYSTERION" 9303   3165   1941     5324     4104     9026     9038))
       (third-2
	'("VELOX    " 556684 600601 334143   532485   153669   810980   924289
	  "THRYNN   " 100175 347633 307434   632874   404795   602834   256564
	  "ELOWAN   " 701897 877210 483347   210444   100253   100277   902494
	  "MECHANS  " 10230  10246  64296    96244    15218    83396    22943
	  "SPEMIN   " 97117  48934  69521    84584    35793    59456    73911
	  "GAZURTOID" 40944  10260  60319    42226    62817    46570    87734
	  "UHLEK    " 93144  19173  21033    90742    22917    62237    32177
	  "MINSTERLS"  7503   2300   4160     1110     3895     6375     8550
	  "MYSTERION"  6596   5691   2946     9413     1760     1170     1810))
       (third-3
	'("VELOX    " 100022 922505 876180   250241   975718   776513   100232
	  "THRYNN   " 873662 100052 100084   537286   313212   100192   228865
	  "ELOWAN   " 889321 461700 987316   758635   124102   298209   462801
	  "MECHANS  "  77617  10721  90880    76792    10209    69498    86116
	  "SPEMIN   "  92052  89933  17820    97060    18811    10253    87657
	  "GAZURTOID"  90218  49417  23968    15713    16670    65214    31791
	  "UHLEK    "  77027  89337  56213    21556    90360    84565    76623
	  "MINSTERLS"   8885   3082   7412     7139     2877     3836     9095
	  "MYSTERION"   5522   1868   8347     7767     8110     1701     3583))
       (third-4
	'("VELOX    "   153078   444465      157773
	  "THRYNN   "   137421   382451      850672
	  "ELOWAN   "   834006   800894      270444
	  "MECHANS  "    76948    17127       69977
	  "SPEMIN   "    88006    10072       71518
	  "GAZURTOID"    10036    10080       22713
	  "UHLEK    "    10046    71490       26100
	  "MINSTERLS"     5146     5190        1245
	  "MYSTERION"     6081     6739        3101)))

  (defanaphoric arplacd)
  (defun third-parser (l &optional (alist nil) name)
    (declare (optimize (debug 3)))
    (format t "car l:~A (car alist):~A name:~A~%" (and (consp l) (car l)) (car alist) name)
    (if (null l)
	alist
	(if (stringp (car l))
	    (aif (assoc (car l) alist :test #'equal)
		 (third-parser (cdr l) alist (car l))
		 (third-parser (cdr l) (acons (car l) nil alist) (car l)))
	    (let* ((number (car l)))
	      (assert (integerp number))
	      (arplacd (assoc name alist :test #'equal)
		       (nconc (list number) (cdr it)))
	      (third-parser (cdr l) alist name)))))
  
  (defparameter third-alist
    (let (alist)
      (loop for third in (list third-1 third-2 third-3 third-4)
	 do (setf alist (third-parser third alist)))
      alist)))

(defun assoc-search-1 (substring alist)
  (let ((found (remove substring alist
		       :test (compose (not (search :a (car :b)))))))
    (if (> (length found) 1)
	(setf found (remove substring alist
			    :test (compose (not (equal :a (car :b)))))))
    (and (= (length found) 1) (cdar found))))

(defun security-code (first second third)
  (let ((first-val (assoc-search-1 (string-tolower first) first-alist))
	(second-val (assoc-search-1 (string-tolower second) second-alist))
	(third-val (assoc-search-1 (string-toupper third) third-alist)))
    (format t "first:~A second:~A third:~A~%" first-val second-val third-val)
    (if (or (null first-val) (null second-val) (null third-val))
	nil
	(let* ((first-val (1- first-val))
	       (column (mod (+ first-val second-val) 24)))
	  (nth column third-val)))))
