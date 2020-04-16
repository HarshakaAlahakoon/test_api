#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname test_api_dev \
    -s test_api \
    -s reloader
