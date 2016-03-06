(load "refal-patmat2.lisp")

(defun specific-random-state ()
  (let ((random-state #+sbcl '#S(RANDOM-STATE :STATE #.(MAKE-ARRAY 627 :ELEMENT-TYPE '(UNSIGNED-BYTE 32)
                                     :INITIAL-CONTENTS
                                     '(0 2567483615 459 342356071 3531489510
                                       2331605190 910510831 3852095994
                                       4202880162 1527001109 1423137360
                                       1497663022 11727107 2966933401
                                       2813248289 4231446232 1298940309
                                       2003327448 3410065843 338997485
                                       2581164106 1825993724 2896753516
                                       3280497728 3610815070 624026016
                                       1451757706 1348391397 3106175362
                                       3584823840 3051798390 2947295603
                                       1664100055 2803849467 3920647366
                                       1693707886 490731719 302461619
                                       3737999297 1394839664 4228713627
                                       1697243927 1958593516 4282874565
                                       3969917809 3518298967 3417696408
                                       3041927890 462301860 569657629
                                       1158413058 4037726451 2655892867
                                       2228663279 3536157597 614999141
                                       247219327 639389729 379663007 1237710599
                                       1413421193 266283264 2973365297
                                       3441228628 3525041971 3188687859
                                       421587397 3271095977 613947257
                                       2049087192 3671195113 893763367
                                       1301529008 535423016 59024731 3388416732
                                       3097531775 1656668087 322602302
                                       2185327525 2798166193 590435172
                                       2623992906 3555566275 3891919751
                                       764963539 356565127 433005935 2845910488
                                       347746604 2313044898 3731380088
                                       3376022583 3614653152 1884299836
                                       3623125032 118489741 4035206462
                                       3932959384 3023718315 3364191260
                                       2245140121 1333071173 2054927196
                                       2207128959 446854282 2168099879
                                       2900458807 1724201022 3793771891
                                       2617694133 301813571 313726463
                                       1290141575 4181567244 2039846765
                                       2061670507 2170371104 4231698981
                                       1967418915 1517728564 713436507
                                       553233325 3175121003 1755935476
                                       3856957552 2720647366 3396466985
                                       674662271 1807143535 2143932277
                                       3119863456 594645515 517231231
                                       3333810332 1583738791 529013062
                                       425407806 361673727 3861545406
                                       3227055049 793042916 2053590196
                                       2954318366 4117060584 804799153
                                       1243136012 3369015107 2108839289
                                       2371037911 929795473 60793658 3478557054
                                       2740070287 2305359292 2338163338
                                       1195787871 2801488327 384611551
                                       1887103223 1702440399 348020356
                                       2854693453 564463937 2145972519
                                       459107265 366630108 3506293016
                                       2018002996 403046825 795528816
                                       2370859746 2419827044 1147694397
                                       2016579573 3224051923 1883031447
                                       745200135 2298517560 385272032
                                       2172153055 69489288 2629291163
                                       1129222570 965659200 1155796049
                                       3928619149 1659190775 1537083257
                                       3471671777 2174138364 3051061010
                                       2040561856 313465648 599774017
                                       1753703409 933511560 466554574 267173538
                                       2185195631 4094730659 2833774394
                                       3279791375 910811291 1474602565
                                       4233456620 1140655447 3660760279
                                       1187407560 4288222814 455840018
                                       719310903 973071888 4145529182
                                       1242025915 2772660878 416732638
                                       3036790309 4214532125 2080798503
                                       1198620460 1304193484 3133550204
                                       129298334 2251674348 3943715771
                                       257939585 3374280140 1254037535
                                       3658834551 3130342464 69922196
                                       1934298010 3304439724 3091871582
                                       3686720449 3262141780 3897999143
                                       1964694553 2101821337 1486173869
                                       1068753904 4094895082 2262227164
                                       1615595154 2676899399 2121789744
                                       2304962548 2102964776 2944534556
                                       1995741696 61974444 4085785442
                                       2192937711 2599295070 3288774604
                                       1777259253 4031098415 921014748
                                       171508675 1237852088 1239168217
                                       820917217 3264664027 222950123
                                       2875259163 3667518194 2441152677
                                       344178408 1559478973 4293254217
                                       743782196 37681975 1291102430 2002818375
                                       747389544 3275566238 3956971939
                                       1981820012 689082530 2304993907
                                       4220370413 287079016 552850515
                                       1776970372 290473974 382589397
                                       1415569603 3966303834 1504900275
                                       2792480605 2510281068 3419609447
                                       3073140306 2107290405 4229297061
                                       316094428 2310476804 1920400586
                                       757356561 4225625100 2882513183
                                       2599621865 3907351779 205069488
                                       2409035038 2925016874 266395043
                                       1773908431 3904118629 1147734296
                                       300375969 2286717561 3564248522
                                       3495498719 1023803051 945456547
                                       1264364535 3231608563 1378655916
                                       3846558795 1556598467 926540887
                                       3410108984 929408372 706372534
                                       3403142580 1115994939 986595585
                                       2007565629 543512255 2418473068
                                       3825463277 1531869277 1642414192
                                       2046312211 1296515696 1562742994
                                       1848834470 2759026753 2013813965
                                       329384848 3158010380 619680419
                                       2462142382 3540074558 4062930309
                                       3405007752 3693995437 3070022794
                                       3735994777 2297555538 3988710477
                                       3511088731 1301887930 4095215042
                                       4023294730 3614360577 1182979211
                                       260454173 1325514449 2341585004
                                       1801572076 60195649 3306042421
                                       3701314928 1650537004 696393296
                                       1227754209 3614308529 155852724
                                       3303884471 4269552420 4220679938
                                       441734492 3577448193 2149516342
                                       3816976454 3985069136 1570316162
                                       1610904123 1956549018 3883864033
                                       2714214744 4117701521 1418508986
                                       2927279455 2682121715 1627258994
                                       2402725982 3068749684 2112476842
                                       2664548074 3923891900 1977236077
                                       630152456 3018667797 1598781580
                                       159075906 1898305707 271055881
                                       3229247773 2454673633 1407583344
                                       1737592538 1902696259 1845898862
                                       482330873 2264877980 637351596
                                       1901132154 1319007120 2333267171
                                       998505585 4282560935 504523050
                                       1641739403 4108123736 3556981237
                                       2710898870 4051560986 2490719873
                                       414464851 261440870 3208044958
                                       1128224283 1958804751 2094740042
                                       1109676315 1467347069 2902731556
                                       1348457958 443060129 3756421876
                                       3112831914 2464736583 678306321
                                       340863420 1839855771 2673489230
                                       3765511803 2161005337 1819096756
                                       1705175721 321692852 3257776644
                                       634810272 1395115670 1379581059
                                       3161857328 205644762 2885167874
                                       3839758006 3015134811 3459946759
                                       2936545016 1783273096 3482452452
                                       546156936 1585022986 3871559550 50866935
                                       2730304177 1560369716 1441276093
                                       1886176732 4245509335 1390484423
                                       1498994828 2181637891 176332687
                                       1274570902 3888845820 3198841227
                                       3275199641 3184359130 3372879515
                                       2603643576 1641135947 2262810253
                                       827744497 1920085539 192673116
                                       1748874440 2861548757 2707952706
                                       1884519319 694598487 414938621 477524198
                                       2533639185 4016935996 3764406212
                                       3141263681 345262425 3019663839
                                       2819188486 1290322549 328227962
                                       4063561793 2029599851 2005959022
                                       2008965792 1714094596 17719375
                                       2798977865 3670851562 2490910722
                                       3791128697 1334022894 107859597
                                       1675825105 3457762305 790710923
                                       747003369 3631307388 381899322
                                       1017711874 368026066 2304894888
                                       3966238897 3911224296 3877365412
                                       21809795 3635055734 1777446963
                                       1236379298 2404581663 137975924
                                       665504771 1569629486 3770187407
                                       2514770236 1624498690 1383422909
                                       3191706511 2725669271 645935718
                                       3258319282 716798710 2161085541
                                       2240588074 478683702 2411706703
                                       124483497 3841702244 3152473350
                                       3639772494 372343803 4023035020
                                       1059942317 1833908388 828949747
                                       1721658905 1979373920 3157575587
                                       203666631 471175682 564680895 2808502943
                                       2751530411 4251805242 1613180502
                                       3310240096 3343523010 3717756947
                                       2680159318 1709542511 3053045519
                                       4181219402 1478032163 3212255056
                                       907550976 1623460999 510898614
                                       2315417921 1948965158 1388117905
                                       3506145128 574100255 1132395905
                                       4107155016 1274514825 4035043485
                                       3436711560 573429070 2240963303
                                       4241093724 2441281561 3646049126
                                       244262740 639914648 667065410 2975092258
                                       1941802615 3713231663 3321212043
                                       1863008084 3710651319 485908780
                                       572464902 1317007614 1100184214
                                       3232057334 1969145531 4002617049
                                       2246793623 803033061 1524204879
                                       3380853201 2955332832 3374886794
                                       3655724309 3696319408 4291537153
                                       541650108 3019281367 2835527281
                                       1339202218 4096643635 806422991
                                       2552384368 1710065464 2756887688
                                       1140454553 3295426802 3793653831)))
		      #+clisp #S(RANDOM-STATE
   #*0101001010001110101111101000100101000100001011100110110111000110)))
    (make-random-state random-state)))

;;;; Implementation of a better sxhash for lists
;; this is copied from SBCL
;; originally this was "(ftype (sfunction". what's an sfunction? 
(declaim (ftype (function ((and fixnum unsigned-byte)
			   (and fixnum unsigned-byte))
			  (and fixnum unsigned-byte))
                mix))
(declaim (inline mix))
(defun mix (x y)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (and fixnum unsigned-byte) x y))
  (let* ((xy (+ (* x 3) y)))
    (logand most-positive-fixnum
            (logxor 441516657
                    xy
                    (ash xy -5)))))

;;(declaim (inline lsxhash))
(defun lsxhash (x)
  "Return a hash value for value X.
X, (car X), and (cdr X) may be a list, a symbol, or a number."
  ;; FIXME: handle circular lists
  (declare (optimize (speed 3) (safety 0) (compilation-speed 0) (space 0)))
  ;;(declare (values (and fixnum unsigned-byte))) ;inferred automatically (see describe 'lsxhash)
  (declare (ftype (function (t) (and fixnum unsigned-byte))))
  ;; in SBCL, etypecase takes 80% of the time of defmethod-ing on the different types of X: (let ((h (make-hash-table))) (timediff (lsxhash h) (mlsxhash h) :showtimes t))
  (etypecase x
    (single-float (sxhash x))
    (double-float (sxhash x))
    (ratio (sxhash x))
    (fixnum (sxhash x)) ;;in SBCL, close numbers seem to have close hashes.
    (string (sxhash x)) ;;in SBCL, (sxhash "ABC") == (sxhash 'abc).
    ;;(number (sxhash x))
    (symbol (sxhash x))
    ;; here, X can't be nil since (symbolp nil) == T.
    (list (mix (lsxhash (car x)) (lsxhash (cdr x))))
    (hash-table (let ((ret 448291823))
		  (declare (type (and fixnum unsigned-byte) ret))
		  (setf ret (mix (sxhash (hash-table-count x))
				 (mix ret (sxhash (hash-table-test x)))))
		  ;; use logxor for speed and so that the order of key/value pairs does not matter
		  (maphash (lambda (k v) (setf ret (logxor ret (mix (lsxhash k) (lsxhash v)))))
			   x)
		  ret))
    (simple-array (let* ((size (array-total-size x))
			 (dim (array-dimensions x))
			 (type (array-element-type x))
			 (ret 518591303))
		    (declare (type (and fixnum unsigned-byte) ret))
		    (setf ret (mix (mix ret (sxhash type))
				   (lsxhash dim)))
		    (ecase type
		      ((fixnum)
		       (loop for i below size do
			    (let ((e (row-major-aref x i)))
			      (declare (type fixnum e))
			      (setf ret (mix ret (sxhash e))))))
		      ((t)
		       (loop for i below size do
			    (let ((e (row-major-aref x i)))
			      (setf ret (mix ret (lsxhash e))))))
		      )
		    ret))))

;;(ql:quickload :sb-sprof)

(defun test (&optional (evaluator #'refal-eval-replace))
  #+sbcl (gc :full t)
  #+clisp (gc)
  (let* ((*random-state* (specific-random-state))
	 (joy-ops '(+ and branch branch1 concat cons dip / dup equal gensym i ifte list * nill not or patmat patsub pop pred quote rem si sample < stack step - succ swap times true uncons unstack while define))
	 (generator `((genenv
		       ((e.1) [ sample 1 0 -1 ] [ sample walk-x walk-y ] e.1))
		      (gengen
		       ((e.1) [ helper (e.1) ]
			;; allow inserting symbols at the end
			[ extend [ insert ] [ sample ,@(loop for i below 400 collect 'del) ins nop mod ] ]))
		      (helper
		       (((s.1 e.1) e.2) [ helper (e.1) e.2 [ extend s.1
			[ sample
			del ins mod ,@(loop for i below 400 collect 'nop) ] ] ]
			;;		     nop ] ] ]
			)
		       ((((e.3) e.1) e.2) [ helper (e.1) e.2 ([ helper (e.3) ]) ])
		       ((() e.2) e.2))
		      (insert
		       (() [ sample 0 1 () \\ [ \\ ] ,@joy-ops ]))
		      (extend
		       ((s.1 del) )
		       ((s.1 ins) [ insert ] [ extend s.1 [ sample ins nop nop nop ] ])
		       ((s.1 mod) [ insert ])
		       ((s.1 nop) s.1)))))
    ;;(utils:timesec (lambda () (funcall evaluator generator generator :c (make-counter 1000000) :view-function 'gengen)))
    (time
     ;;(sb-sprof:with-profiling (:max-samples 10000 :report :flat :loop nil)
       (lsxhash
	(loop for i below 10 collect
	     (let* ((c (make-counter 1000000))
		    (ret (progn (funcall evaluator generator generator :c c :view-function 'gengen))))
	       ;;(print (funcall c))
	       ret)))
       ;;)
     )))

;;(test)



;; SBCL: with dlist2 which uses default #'MAKE-DCONS:

;; Number of samples:   1679
;; Sample interval:     0.01 seconds
;; Total sampling time: 16.789999 seconds
;; Number of cycles:    0
;; Sampled threads:
;;  #<SB-THREAD:THREAD "main thread" RUNNING {B3E2281}>

;;            Self        Total        Cumul
;;   Nr  Count     %  Count     %  Count     %    Calls  Function
;; ------------------------------------------------------------------------
;;    1    452  26.9    452  26.9    452  26.9        -  DLIST2::MAKE-DCONS
;;    2    164   9.8    743  44.3    616  36.7        -  COPY-DLIST
;;    3    142   8.5    142   8.5    758  45.1        -  SB-KERNEL::%%TYPEP
;;    4    102   6.1    102   6.1    860  51.2        -  "foreign function __kernel_vsyscall"
;;    5     99   5.9    214  12.7    959  57.1        -  SB-KERNEL:VALUES-SPECIFIER-TYPE
;;    6     86   5.1     86   5.1   1045  62.2        -  SB-KERNEL:CLASSOID-TYPEP
;;    7     63   3.8    273  16.3   1108  66.0        -  SB-KERNEL:SPECIFIER-TYPE
;;    8     59   3.5    560  33.4   1167  69.5        -  DLIST->LIST
;;    9     58   3.5     58   3.5   1225  73.0        -  SB-INT:EQUAL-BUT-NO-CAR-RECURSION
;;   10     52   3.1    316  18.8   1277  76.1        -  SB-KERNEL:%TYPEP
;;   11     47   2.8     47   2.8   1324  78.9        -  (LAMBDA (SB-PCL::.ARG0.) :IN "/build/sbcl-OjOZyH/sbcl-1.2.4/src/pcl/dlisp3.fasl")
;;   12     30   1.8     75   4.5   1354  80.6        -  DLIST-NCONC
;;   13     30   1.8     30   1.8   1384  82.4        -  SXHASH
;;   14     28   1.7     28   1.7   1412  84.1        -  SB-VM::ALLOC-SIGNED-BIGNUM-IN-EAX
;;   15     27   1.6     27   1.6   1439  85.7        -  (LAMBDA (SB-KERNEL:INSTANCE) :IN SB-PCL::MAKE-OPTIMIZED-STD-READER-METHOD-FUNCTION)
;;   16     22   1.3   1652  98.4   1461  87.0        -  EVAL-VIEW
;;   17     21   1.3     29   1.7   1482  88.3        -  CLOSE-L
;;   18     18   1.1     18   1.1   1500  89.3        -  (LAMBDA (SB-PCL::.PV. SB-PCL::.NEXT-METHOD-CALL. SB-KERNEL:INSTANCE) :IN SB-PCL::MAKE-STD-READER-METHOD-FUNCTION)
;;   19     17   1.0     44   2.6   1517  90.4        -  (LAMBDA (SB-PCL::|.P0.| SB-PCL::|.P1.|) :IN "/home/toni/.cache/common-lisp/sbcl-1.2.4.debian-linux-x86/home/toni/lisp-utils/dlist2.fasl")
;;   20     17   1.0     17   1.0   1534  91.4        -  (LABELS SB-IMPL::SXHASH-RECURSE :IN SXHASH)
;;   21     16   1.0     21   1.3   1550  92.3        -  SB-VM::GENERIC-+
;;   22     14   0.8     14   0.8   1564  93.2        -  SB-PCL::GET-INSTANCE-HASH-CODE
;;   23     11   0.7     11   0.7   1575  93.8        -  LENGTH
;;   24     10   0.6     10   0.6   1585  94.4        -  SB-PCL::%MAKE-STANDARD-INSTANCE
;;   25     10   0.6     10   0.6   1595  95.0        -  "foreign function closure_tramp"
;;   26      8   0.5     51   3.0   1603  95.5        -  PATMAT
;;   27      7   0.4      8   0.5   1610  95.9        -  SB-INT:PROPER-LIST-OF-LENGTH-P
;;   28      7   0.4      7   0.4   1617  96.3        -  (FLET SB-IMPL::FAST-NTHCDR :IN NTHCDR)
;;   29      7   0.4      7   0.4   1624  96.7        -  SB-KERNEL:%ASSOC-EQ
;;   30      6   0.4     37   2.2   1630  97.1        -  DCONS-LIST
;;   31      6   0.4      6   0.4   1636  97.4        -  EQL
;;   32      6   0.4      6   0.4   1642  97.8        -  SVARP
;;   33      5   0.3      5   0.3   1647  98.1        -  "foreign function funcallable_instance_tramp"
;;   34      4   0.2     14   0.8   1651  98.3        -  CLOSE-LR
;;   35      4   0.2      4   0.2   1655  98.6        -  RANDOM
;;   36      4   0.2      4   0.2   1659  98.8        -  SB-KERNEL:CLASSOID-CELL-TYPEP
;;   37      3   0.2     62   3.7   1662  99.0        -  EVAL-CALL-USERDEF
;;   38      3   0.2      9   0.5   1665  99.2        -  SB-KERNEL:%ASSOC
;;   39      2   0.1     40   2.4   1667  99.3        -  EVAL-CALL-BUILTIN
;;   40      2   0.1      2   0.1   1669  99.4        -  (LABELS CONSTS-REGION :IN PARSE-RESULT)
;;   41      1   0.1      6   0.4   1670  99.5        -  (LABELS SUBST-SYMBOL :IN REFAL-EVAL-REPLACE)
;;   42      1   0.1      1   0.1   1671  99.5        -  SB-IMPL::GETHASH3
;;   43      1   0.1      1   0.1   1672  99.6        -  EVARP
;;   44      1   0.1      1   0.1   1673  99.6        -  OPEN-DCONS
;;   45      1   0.1      1   0.1   1674  99.7        -  SB-C:RETURN-MULTIPLE
;;   46      1   0.1      1   0.1   1675  99.8        -  SB-KERNEL:%COERCE-CALLABLE-TO-FUN
;;   47      1   0.1      1   0.1   1676  99.8        -  CLOSE-EXP-NULL
;;   48      1   0.1      1   0.1   1677  99.9        -  NCONC
;;   49      1   0.1      1   0.1   1678  99.9        -  (LAMBDA (SB-PCL::.ARG0. SB-PCL::.ARG1.) :IN "/build/sbcl-OjOZyH/sbcl-1.2.4/src/pcl/braid.fasl")
;;   50      1   0.1      1   0.1   1679 100.0        -  LSXHASH
;;   51      0   0.0   1669  99.4   1679 100.0        -  (LAMBDA NIL :IN TEST)
;;   52      0   0.0   1669  99.4   1679 100.0        -  CALL-WITH-TIMING
;;   53      0   0.0   1669  99.4   1679 100.0        -  TEST
;;   54      0   0.0   1669  99.4   1679 100.0        -  SB-INT:SIMPLE-EVAL-IN-LEXENV
;;   55      0   0.0   1669  99.4   1679 100.0        -  EVAL-TLF
;;   56      0   0.0   1669  99.4   1679 100.0        -  (FLET SB-FASL::EVAL-FORM :IN SB-INT:LOAD-AS-SOURCE)
;;   57      0   0.0   1669  99.4   1679 100.0        -  SB-INT:LOAD-AS-SOURCE
;;   58      0   0.0   1669  99.4   1679 100.0        -  (FLET SB-FASL::LOAD-STREAM :IN LOAD)
;;   59      0   0.0   1669  99.4   1679 100.0        -  LOAD
;;   60      0   0.0   1669  99.4   1679 100.0        -  SB-IMPL::PROCESS-EVAL/LOAD-OPTIONS
;;   61      0   0.0   1669  99.4   1679 100.0        -  SB-IMPL::TOPLEVEL-INIT
;;   62      0   0.0   1669  99.4   1679 100.0        -  (FLET #:WITHOUT-INTERRUPTS-BODY-77 :IN SAVE-LISP-AND-DIE)
;;   63      0   0.0   1669  99.4   1679 100.0        -  (LABELS SB-IMPL::RESTART-LISP :IN SAVE-LISP-AND-DIE)
;;   64      0   0.0   1668  99.3   1679 100.0        -  REFAL-EVAL-REPLACE
;;   65      0   0.0   1662  99.0   1679 100.0        -  REFAL-EVAL
;;   66      0   0.0    102   6.1   1679 100.0        -  "foreign function interrupt_handle_pending"
;;   67      0   0.0    102   6.1   1679 100.0        -  "foreign function handle_trap"
;;   68      0   0.0    102   6.1   1679 100.0        -  "foreign function __kernel_rt_sigreturn"
;;   69      0   0.0     16   1.0   1679 100.0        -  DLIST
;;   70      0   0.0      4   0.2   1679 100.0        -  PARSE-RESULT
;;   71      0   0.0      2   0.1   1679 100.0        -  PARSE-CLAUSE
;;   72      0   0.0      2   0.1   1679 100.0        -  PARSE-FUNCTION
;;   73      0   0.0      2   0.1   1679 100.0        -  PARSE-PROGRAM
;;   74      0   0.0      1   0.1   1679 100.0        -  LIST->DLIST
;;   75      0   0.0      1   0.1   1679 100.0        -  MAKE-CONSTS
;; ------------------------------------------------------------------------
;;           0   0.0                                     elsewhere
;; Evaluation took:
;;   19.382 seconds of real time
;;   18.124000 seconds of total run time (17.432000 user, 0.692000 system)
;;   [ Run times consist of 2.836 seconds GC time, and 15.288 seconds non-GC time. ]
;;   93.51% CPU
;;   51,556,893,230 processor cycles
;;   2,784,431,688 bytes consed



;; SBCL: with dlist2 which uses a by-order-of-arguments #'MAKE-DCONS:

;; Number of samples:   1449
;; Sample interval:     0.01 seconds
;; Total sampling time: 14.49 seconds
;; Number of cycles:    0
;; Sampled threads:
;;  #<SB-THREAD:THREAD "main thread" RUNNING {B3E2281}>

;;            Self        Total        Cumul
;;   Nr  Count     %  Count     %  Count     %    Calls  Function
;; ------------------------------------------------------------------------
;;    1    267  18.4    267  18.4    267  18.4        -  DLIST2::MAKE-DCONS
;;    2    165  11.4    165  11.4    432  29.8        -  SB-KERNEL::%%TYPEP
;;    3    107   7.4    528  36.4    539  37.2        -  COPY-DLIST
;;    4    102   7.0    102   7.0    641  44.2        -  "foreign function __kernel_vsyscall"
;;    5    101   7.0    203  14.0    742  51.2        -  SB-KERNEL:VALUES-SPECIFIER-TYPE
;;    6     91   6.3     91   6.3    833  57.5        -  SB-KERNEL:CLASSOID-TYPEP
;;    7     68   4.7    265  18.3    901  62.2        -  SB-KERNEL:SPECIFIER-TYPE
;;    8     62   4.3    569  39.3    963  66.5        -  DLIST->LIST
;;    9     54   3.7    317  21.9   1017  70.2        -  SB-KERNEL:%TYPEP
;;   10     47   3.2     47   3.2   1064  73.4        -  (LAMBDA (SB-PCL::.ARG0.) :IN "/build/sbcl-OjOZyH/sbcl-1.2.4/src/pcl/dlisp3.fasl")
;;   11     41   2.8     41   2.8   1105  76.3        -  SB-INT:EQUAL-BUT-NO-CAR-RECURSION
;;   12     33   2.3   1424  98.3   1138  78.5        -  EVAL-VIEW
;;   13     30   2.1     30   2.1   1168  80.6        -  SXHASH
;;   14     30   2.1     30   2.1   1198  82.7        -  SB-VM::ALLOC-SIGNED-BIGNUM-IN-EAX
;;   15     22   1.5     59   4.1   1220  84.2        -  DLIST-NCONC
;;   16     22   1.5     23   1.6   1242  85.7        -  SB-VM::GENERIC-+
;;   17     22   1.5     22   1.5   1264  87.2        -  (LAMBDA (SB-KERNEL:INSTANCE) :IN SB-PCL::MAKE-OPTIMIZED-STD-READER-METHOD-FUNCTION)
;;   18     21   1.4     21   1.4   1285  88.7        -  (LABELS SB-IMPL::SXHASH-RECURSE :IN SXHASH)
;;   19     17   1.2     27   1.9   1302  89.9        -  CLOSE-L
;;   20     17   1.2     17   1.2   1319  91.0        -  (LAMBDA (SB-PCL::.PV. SB-PCL::.NEXT-METHOD-CALL. SB-KERNEL:INSTANCE) :IN SB-PCL::MAKE-STD-READER-METHOD-FUNCTION)
;;   21     16   1.1     16   1.1   1335  92.1        -  SB-PCL::GET-INSTANCE-HASH-CODE
;;   22     12   0.8     38   2.6   1347  93.0        -  (LAMBDA (SB-PCL::|.P0.| SB-PCL::|.P1.|) :IN "/home/toni/.cache/common-lisp/sbcl-1.2.4.debian-linux-x86/home/toni/lisp-utils/dlist2.fasl")
;;   23     10   0.7     10   0.7   1357  93.7        -  "foreign function closure_tramp"
;;   24      9   0.6     47   3.2   1366  94.3        -  PATMAT
;;   25      8   0.6      9   0.6   1374  94.8        -  SB-INT:PROPER-LIST-OF-LENGTH-P
;;   26      7   0.5      7   0.5   1381  95.3        -  SB-PCL::%MAKE-STANDARD-INSTANCE
;;   27      6   0.4     21   1.4   1387  95.7        -  DCONS-LIST
;;   28      6   0.4      6   0.4   1393  96.1        -  SB-KERNEL:%ASSOC-EQ
;;   29      6   0.4      6   0.4   1399  96.5        -  LENGTH
;;   30      5   0.3      5   0.3   1404  96.9        -  (LAMBDA (SB-PCL::.ARG0. SB-PCL::.ARG1.) :IN "/build/sbcl-OjOZyH/sbcl-1.2.4/src/pcl/braid.fasl")
;;   31      4   0.3     16   1.1   1408  97.2        -  CLOSE-LR
;;   32      4   0.3      6   0.4   1412  97.4        -  SB-KERNEL:%ASSOC
;;   33      4   0.3      4   0.3   1416  97.7        -  "foreign function funcallable_instance_tramp"
;;   34      4   0.3      4   0.3   1420  98.0        -  SB-KERNEL:CLASSOID-CELL-TYPEP
;;   35      3   0.2      3   0.2   1423  98.2        -  (FLET SB-IMPL::FAST-NTHCDR :IN NTHCDR)
;;   36      3   0.2      3   0.2   1426  98.4        -  SVARP
;;   37      2   0.1     55   3.8   1428  98.6        -  EVAL-CALL-USERDEF
;;   38      2   0.1     10   0.7   1430  98.7        -  DLIST
;;   39      2   0.1      3   0.2   1432  98.8        -  (LAMBDA (HEAD) :IN REFAL-EVAL-REPLACE)
;;   40      2   0.1      2   0.1   1434  99.0        -  EQL
;;   41      2   0.1      2   0.1   1436  99.1        -  ACONS
;;   42      2   0.1      2   0.1   1438  99.2        -  EVARP
;;   43      1   0.1     21   1.4   1439  99.3        -  EVAL-CALL-BUILTIN
;;   44      1   0.1      6   0.4   1440  99.4        -  (LABELS SUBST-SYMBOL :IN REFAL-EVAL-REPLACE)
;;   45      1   0.1      1   0.1   1441  99.4        -  LIST->DLIST
;;   46      1   0.1      1   0.1   1442  99.5        -  (LABELS CONSTS-REGION :IN PARSE-RESULT)
;;   47      1   0.1      1   0.1   1443  99.6        -  NCONC
;;   48      1   0.1      1   0.1   1444  99.7        -  (LABELS CHOP :IN NEST-BRACKETS)
;;   49      1   0.1      1   0.1   1445  99.7        -  SB-IMPL::LIST-NREVERSE*
;;   50      1   0.1      1   0.1   1446  99.8        -  RANDOM
;;   51      1   0.1      1   0.1   1447  99.9        -  SB-KERNEL:%CONCATENATE-TO-STRING
;;   52      1   0.1      1   0.1   1448  99.9        -  NTHCDR
;;   53      1   0.1      1   0.1   1449 100.0        -  LSXHASH
;;   54      0   0.0   1442  99.5   1449 100.0        -  (LAMBDA NIL :IN TEST)
;;   55      0   0.0   1442  99.5   1449 100.0        -  CALL-WITH-TIMING
;;   56      0   0.0   1442  99.5   1449 100.0        -  TEST
;;   57      0   0.0   1442  99.5   1449 100.0        -  SB-INT:SIMPLE-EVAL-IN-LEXENV
;;   58      0   0.0   1442  99.5   1449 100.0        -  EVAL-TLF
;;   59      0   0.0   1442  99.5   1449 100.0        -  (FLET SB-FASL::EVAL-FORM :IN SB-INT:LOAD-AS-SOURCE)
;;   60      0   0.0   1442  99.5   1449 100.0        -  SB-INT:LOAD-AS-SOURCE
;;   61      0   0.0   1442  99.5   1449 100.0        -  (FLET SB-FASL::LOAD-STREAM :IN LOAD)
;;   62      0   0.0   1442  99.5   1449 100.0        -  LOAD
;;   63      0   0.0   1442  99.5   1449 100.0        -  SB-IMPL::PROCESS-EVAL/LOAD-OPTIONS
;;   64      0   0.0   1442  99.5   1449 100.0        -  SB-IMPL::TOPLEVEL-INIT
;;   65      0   0.0   1442  99.5   1449 100.0        -  (FLET #:WITHOUT-INTERRUPTS-BODY-77 :IN SAVE-LISP-AND-DIE)
;;   66      0   0.0   1442  99.5   1449 100.0        -  (LABELS SB-IMPL::RESTART-LISP :IN SAVE-LISP-AND-DIE)
;;   67      0   0.0   1441  99.4   1449 100.0        -  REFAL-EVAL-REPLACE
;;   68      0   0.0   1435  99.0   1449 100.0        -  REFAL-EVAL
;;   69      0   0.0    102   7.0   1449 100.0        -  "foreign function interrupt_handle_pending"
;;   70      0   0.0    102   7.0   1449 100.0        -  "foreign function handle_trap"
;;   71      0   0.0    102   7.0   1449 100.0        -  "foreign function __kernel_rt_sigreturn"
;;   72      0   0.0      2   0.1   1449 100.0        -  PARSE-RESULT
;;   73      0   0.0      1   0.1   1449 100.0        -  PARSE-CLAUSE
;;   74      0   0.0      1   0.1   1449 100.0        -  PARSE-FUNCTION
;;   75      0   0.0      1   0.1   1449 100.0        -  PARSE-PROGRAM
;;   76      0   0.0      1   0.1   1449 100.0        -  (LABELS REC :IN NEST-BRACKETS)
;;   77      0   0.0      1   0.1   1449 100.0        -  NEST-BRACKETS
;;   78      0   0.0      1   0.1   1449 100.0        -  PARSE-PROGRAM*
;; ------------------------------------------------------------------------
;;           0   0.0                                     elsewhere
;; Evaluation took:
;;   17.481 seconds of real time
;;   16.300000 seconds of total run time (15.572000 user, 0.728000 system)
;;   [ Run times consist of 2.920 seconds GC time, and 13.380 seconds non-GC time. ]
;;   93.24% CPU
;;   46,497,821,670 processor cycles
;;   2,783,233,936 bytes consed




;; CLISP: with dlist2 which uses a by-order-of-arguments #'MAKE-DCONS:

;; CL-USER> (test)
;; Real time: 77.183365 sec.
;; Run time: 72.06 sec.
;; Space: 588057472 Bytes
;; GC: 247, GC time: 5.496 sec.
;; 15712809

;;                                                          Cons     
;;                        %      %                          Per        Total     Total
;; Function              Time   Cons    Calls  Sec/Call     Call       Time      Cons
;; --------------------------------------------------------------------------------------
;; COPY-DLIST:          51.37   56.46    88025  0.000421       3772    37.056   332032808
;; DLIST2::MAKE-DCONS:  23.43   31.34  7679684  0.000002         24    16.900   184312416
;; EVAL-VIEW:            5.38    3.36       10  0.388000    1978614     3.880    19786136
;; DLIST-NCONC:          4.29    0.00    68402  0.000045          0     3.092        4224
;; DLIST->LIST:          2.55    5.18     9513  0.000193       3200     1.840    30438264
;; DCONS-LIST:           2.49    2.07   119239  0.000015        102     1.796    12164240
;; DLIST-LAST:           2.40    0.00   972460  0.000002          0     1.732           0
;; DLIST-FIRST:          1.76    0.00   669078  0.000002          0     1.272           0
;; PATMAT:               1.64    0.17    48825  0.000024         21     1.180     1007024
;; --------------------------------------------------------------------------------------
;; TOTAL:               95.30   98.58  9655236                         68.748   579745112
;; Estimated monitoring overhead:  0.00 seconds
;; Estimated total monitoring overhead:  0.00 seconds
;; Fifty-nine monitored functions were not called. 
;; See the variable swank-monitor::*no-calls* for a list.
