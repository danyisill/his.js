#!/usr/bin/bash

boards=(his lit)

for board in "${boards[@]}"; do
	touch "posted.$board"
done

./build.sh 4ch50replies

for (( ; ; )); do
	./4ch50replies "${boards[@]}"
	echo "Finsihed, sleeping."
	sleep 300  # 5 min.
done

