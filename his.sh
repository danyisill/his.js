#!/usr/bin/bash

[ -z "$HIS_BOT_TOKEN" ] && { echo "No API token in ENV!"; exit 1; }
[ ! -f ./posted ] && touch posted

update () {
	curl -s 'https://a.4cdn.org/his/threads.json' \
		| tr n '\n' \
		| tr -cd '1234567890:\n' \
		| sed 1d \
		| while read -r thread; do
			if [[ "$(cut -d : -f 4 <<< "$thread")" -gt 50 ]]; then
				echo "--- Checking if thread already exists... ---"
				if grep -q "$(cut -d : -f 2 <<< "$thread")" posted; then
					echo "  -> Exists, not posting."
					continue
				else
					echo "  -> Posting new message!"
					cut -d : -f 2 <<< "$thread" \
						| tee -a posted \
						| xargs -I {} curl -s "https://api.telegram.org/bot$HIS_BOT_TOKEN/sendMessage?chat_id=@his50replies&text=https://boards.4channel.org/his/thread/{}" >/dev/null
				fi
				sleep 0.25
			fi
		  done
}

for (( ; ; )); do
	echo "=== Checking. ==="
	update
	sleep 300  # 5 min.
done

