(load "refal-patmat.lisp")

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
			[ extend [ insert ] [ sample ,@(loop for i below 4 collect 'del) ins nop mod ] ]))
		      (helper
		       (((s.1 e.1) e.2) [ helper (e.1) e.2 [ extend s.1
			[ sample
			del ins mod ,@(loop for i below 4 collect 'nop) ] ] ]
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
    (time
     (loop for i below 120 collect
	  (let ((ret (progn (funcall evaluator generator generator :c (make-counter 1000000) :view-function 'gengen))))
	    ret)))
    nil
    ))

;;   seconds  |     gc     |    consed   |    calls   |  sec/call  |  name  
;; --------------------------------------------------------------
;;      2.909 |      0.796 | 381,555,784 |    351,180 |   0.000008 | LIST->DLIST
;;      2.195 |      0.316 |  70,766,640 |     44,848 |   0.000049 | DCONS->LIST
;;      0.066 |      0.000 |   2,260,992 |     61,498 |   0.000001 | DLIST->LIST*
;;      0.052 |      0.000 |     454,656 |     66,930 |   0.000001 | EVAL-CALL-BUILTIN
;;      0.025 |      0.000 |           0 |     21,620 |   0.000001 | SVARP
;;      0.005 |      0.000 |           0 |     21,620 |   0.000000 | EVARP
;;      0.001 |      0.000 |           0 |     21,620 |   0.000000 | TVARP
;;      0.000 |      0.000 |           0 |        782 |   0.000001 | PATTERNP
;;      0.000 |      0.000 |       4,088 |    112,323 |   0.000000 | PATMAT
;;      0.000 |      0.000 |           0 |         23 |   0.000000 | MAKE-COUNTER
;;      0.000 |      0.168 |     982,152 |          1 |   0.000000 | TEST
;;      0.000 |      0.000 |           0 |         23 |   0.000000 | PARSE-PROGRAM*
;;      0.000 |      0.000 |           0 |        368 |   0.000000 | MAKE-NEST[]
;;      0.000 |      0.000 |     822,680 |         23 |   0.000000 | REFAL-EVAL-REPLACE
;;      0.000 |      0.000 |           0 |        115 |   0.000000 | PARSE-FUNCTION
;;      0.000 |      0.000 |           0 |         23 |   0.000000 | REFAL-EVAL
;;      0.000 |      0.000 |     192,368 |     45,103 |   0.000000 | EVAL-CALL-USERDEF
;;      0.000 |      0.000 |           0 |         23 |   0.000000 | PARSE-PROGRAM
;;      0.000 |      0.000 |           0 |        230 |   0.000000 | PARSE-CLAUSE
;;      0.000 |      0.000 |           0 |        368 |   0.000000 | MAKE-NEST[]-BOA
;;      0.000 |      0.000 |   2,682,888 |    241,983 |   0.000000 | CLOSE-VAR-NOTNULL
;;      0.000 |      0.408 | 266,713,576 |  9,042,981 |   0.000000 | EVAL-VIEW
;;      0.000 |      0.000 |           0 |     45,102 |   0.000000 | OPEN-VAR
;;      0.000 |      0.000 |      81,920 |      3,314 |   0.000000 | CLOSE-VAR-EXP-NULL
;;      0.000 |      0.000 |           0 |        483 |   0.000000 | REFAL-FUNCTION-NAME-P
;;      0.000 |      0.000 |     188,416 |        828 |   0.000000 | NEST-BRACKETS
;;      0.000 |      0.000 |           0 |        391 |   0.000000 | MAKE-CALL
;;      0.000 |      0.000 |     229,168 |     66,930 |   0.000000 | EVAL-CALL
;;      0.000 |      0.000 |           0 |          1 |   0.000000 | SPECIFIC-RANDOM-STATE
;;      0.000 |      0.000 |     188,664 |        713 |   0.000000 | PARSE-RESULT
;; --------------------------------------------------------------
;;      5.254 |      1.688 | 727,123,992 | 10,151,447 |            | Total

;; estimated total profiling overhead: 41.42 seconds
;; overhead estimation parameters:
;;   2.4e-8s/call, 4.08e-6s total profiling, 1.792e-6s internal profiling
