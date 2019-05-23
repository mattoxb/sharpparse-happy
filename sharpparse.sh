stack build 
if [ $? -ne 0 ]; then
    echo BUILD FAILED
    exit 1
fi

stack exec parse < $1 > src/Grammar2.y
if [ $? -ne 0 ]; then
    echo INITIAL PARSE FAILED
    cp src/Grammar2-reference.y src/Grammar2.y
    exit 1
fi

stack build 
if [ $? -ne 0 ]; then
    echo PARSER COMPILE FAILED
    echo Investigate src/Grammar2.y, then do
    echo cp src/Grammar2-reference.y src/Grammar2.y
    exit 1
fi

stack exec subparse < $2
