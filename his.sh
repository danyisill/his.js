#!/usr/bin/bash

touch posted.his
touch posted.lit
go build his.go

for (( ; ; )); do
	./his
	sleep 300  # 5 min.
done

