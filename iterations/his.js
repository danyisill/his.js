var https = require('https'), posted = require('./posted.json');

const update = () => https.get('https://a.4cdn.org/his/threads.json', res => {
		var j = ''
		res.on('data', c => j += c)
		res.on('end', () => {
			var newThreads = JSON.parse(j).map(e => e.threads).flat().filter(e => e.replies > 50 && !posted.includes(e.no))
			if(newThreads.length){
				newThreads.forEach(thread => {
					posted.push(thread.no)
					console.log(thread.no)
					//First entry in posted.json has to be the API token.
					https.get(`https://api.telegram.org/bot${posted[0]}/sendMessage?chat_id=@his50replies&text=https://boards.4channel.org/his/thread/` + thread.no)
				})
				require('fs').writeFileSync('./posted.json', JSON.stringify(posted))
			}
		})
})

setInterval(update, 300_000)  // 5 min.
