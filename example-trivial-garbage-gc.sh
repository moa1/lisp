#!/bin/sh

# Do a binary search of a parameter so that it is maximal and sbcl is not killed.

WORKS=2650
FAILS=2750

TMPFILE=`tempfile`
handler() {
    rm "$TMPFILE"
    exit 1
}
trap handler INT QUIT

abs() {
    VAL="$1"
    if test "$VAL" -lt 0; then
	VAL=$((-$VAL))
    fi
    echo $VAL
    return 0
}

testfun_random() {
    VAL=`random`
    #echo "VAL:$VAL"
    test $VAL -gt 10000
    return $?
}

testfun() {
    TESTING=$1
    
    sbcl --load trivial-garbage-gc-test.lisp --eval "(progn (test $TESTING :rep 100) (quit))"
    return $?

    #echo "(load \"trivial-garbage-gc-test.lisp\") (test $TESTING :rep 100) (quit)" > $TMPFILE
    #clisp $TMPFILE
    #return $?

    #testfun_random
    #return $?
}

testfun $WORKS
if test $? -ne 0; then
    echo "Error: \$(testfun $WORKS) failed unexpectedly!"
    echo "Choose WORKS and FAILS so that they do what they mean."
    exit 1
fi
testfun $FAILS
if test $? -eq 0; then
    echo "Error: \$(testfun $FAILS) worked unexpectedly!"
    echo "Choose WORKS and FAILS so that they do what they mean."
    exit 1
fi


while test $(abs $(($FAILS-$WORKS))) -gt 1; do
    MIDDLE=$((($FAILS+$WORKS)/2))
    echo "MIDDLE:$MIDDLE WORKS:$WORKS FAILS:$FAILS"

    testfun $MIDDLE
    RET=$?
    echo RET:$RET
    
    if test $RET -eq 0; then
	WORKS=$MIDDLE
    else
	FAILS=$MIDDLE
    fi
done

echo "WORKS:$WORKS FAILS:$FAILS"
