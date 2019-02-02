"
Es gibt 2 logische Evaluatoren
A sagt immer die Wahrheit
B sagt immer das Gegenteil

Es gibt mehrere logische Aussagen, die entweder wahr oder falsch sind.
Das Ziel ist es, dass man eine logische Verknüpfung von diesen Aussagen hat, die bei sowohl A als auch B

2 logische Aussagen: X, Y, z.B. 1==1, mit Aussage wahr oder falsch
2 Evaluatoren (A, B), das eine sagt immer die Wahrheit, das andere immer das Gegenteil
',' ist logisches AND
'v' ist logisches ODER
     X=T,Y=T X=T,Y=F X=F,Y=T X=F,Y=F
A    T       F       F       T
B    F       T       T       F
     X=TvY=T X=TvY=F X=FvY=T X=FvY=F
A    T       T       T       F
B    F       F       F       T
Ziel: FRAGE
A     T
B     T
     (X=T,Y=T)v(X=TvY=T) (X=T,Y=T)v(X=TvY
A    T
B    T
"


(defun  ()

