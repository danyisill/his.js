package main
import ("fmt"
	"net/http"
	"encoding/json"
	"sync"
	"io/ioutil"
	"strconv"
	"strings"
	"net/url"
	"html"
)
var wg sync.WaitGroup
type Page struct{
	Page int `json:"page"`
	Thread []Thread `json:"threads"`
}
type Thread struct{
	No int `json:"no"`
	Replies int `json:"replies"`
	Tim int `json:"tim"`
	Sub string `json:"sub"`
	Com string `json:"com"`
	Ext string `json:"ext"`
}
func procThread(th Thread, b string){
	defer wg.Done()
	params := url.Values{}
	params.Add("chat_id", fmt.Sprintf("@%s50replies", b))
	params.Add("photo", fmt.Sprintf("https://is2.4chan.org/%s/%d%s", b, th.Tim, th.Ext))
	comment := strings.ReplaceAll(strings.ReplaceAll(strings.ReplaceAll(strings.ReplaceAll(html.UnescapeString(th.Com), "<span class=\"quote\">", ""), "</span>", "\n"), "<br>", "\n"), "class=\"quotelink\"", "")
	params.Add("caption", fmt.Sprintf("https://boards.4channel.org/%s/thread/%d\n%s\n%s", b, th.No, th.Sub, comment))
	params.Add("parse_mode", "HTML")

	http.Get(fmt.Sprintf("https://api.telegram.org/bot%s/sendPhoto?%s", "1364237567:AAGjywcrfrePt3Of_fGnaXyei4JL7KVa4ZE", params.Encode()))
}
func board(b string){
	res, _ := http.Get(fmt.Sprintf("https://a.4cdn.org/%s/catalog.json", b))
	postedBytes, _ := ioutil.ReadFile("posted." + b)
	posted := string(postedBytes)
	dec := json.NewDecoder(res.Body)
	dec.Token()
	for dec.More(){
		var p Page
		dec.Decode(&p)
		for _, th := range p.Thread{
			if th.Replies > 50 && !strings.Contains(posted, strconv.Itoa(th.No)){
				posted += strconv.Itoa(th.No) + "\n"
				wg.Add(1)
				go procThread(th, b)
			}
		}
	}
	ioutil.WriteFile("posted." + b, []byte(posted), 0777)
}
func main(){
	board("his")
	board("lit")
	wg.Wait()
}