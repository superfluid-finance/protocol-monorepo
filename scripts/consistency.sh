#!/bin/bash
echo "starting consistency tests";
for i in `seq $1`;
    do 
        echo "$i/$1...";
        truffle test ../test/*.js | grep Error;
done
