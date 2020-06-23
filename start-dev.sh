#!/bin/sh
cd `dirname $0`
exec erl -sname edc -config $PWD/sys.config \
-pa $PWD/_build/default/lib/*/ebin $PWD/test -boot start_sasl \
-setcookie start-dev -run c erlangrc .