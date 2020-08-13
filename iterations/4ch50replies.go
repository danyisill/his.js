package main

import (
	"bytes"
	"os"
	"fmt"
	"net/http"
	"encoding/json"
	"sync"
	"io"
	"io/ioutil"
	"mime/multipart"
	"strconv"
	"strings"
	"log"
	"html"
	"regexp"
	"os/exec"
)

var token = os.Getenv("HIS_BOT_TOKEN")

// When `true` prints debug information.
var debug = false

var wg sync.WaitGroup
var re *regexp.Regexp

type Page struct {
	Page int        `json:"page"`
	Thread []Thread `json:"threads"`
}

type Thread struct {
	No int      `json:"no"`
	Replies int `json:"replies"`
	Tim int     `json:"tim"`
	Sub string  `json:"sub"`
	Com string  `json:"com"`
	Ext string  `json:"ext"`
}

func procThread(th Thread, b string) (*http.Request, error) {
	wg.Add(1)
	defer wg.Done()

	var err error

	fileURI := "https://is2.4chan.org/" +
		b + "/" + strconv.Itoa(th.Tim) + th.Ext

	body := new(bytes.Buffer)
	writer := multipart.NewWriter(body)

	err = writer.WriteField("chat_id", "@" + b + "50replies")
	if err != nil {
		log.Println("Could not write field `char_id'.")
		return nil, err
	}

	err = writer.WriteField("caption", fmt.Sprintf(
		"https://boards.4channel.org/%s/thread/%d\n%s\n%s",
		b, th.No, html.UnescapeString(th.Sub), re.ReplaceAllString(
			strings.ReplaceAll(html.UnescapeString(th.Com), "<br>", "\n"),
		"")))
	if err != nil {
		log.Println("Could not write field `caption'.")
		return nil, err
	}

	postURI := "https://api.telegram.org/bot" + token

	if debug {
		fmt.Println("Lookin file,", fileURI)
	}

	if th.Ext == ".webm" {
		postURI += "/sendAnimation"

		part, err := writer.CreateFormFile("animation", "webm.mp4")
		if err != nil {
			log.Println("Failed to create part for animation.")
			return nil, err
		}

		res, err := http.Get(fileURI)
		if err != nil {
			log.Println("Could not get .webm from 4chan.", err)
			return nil, err
		}

		cmd := exec.Command(
			"ffmpeg", "-f", "webm", "-i", "pipe:0",
			"-hide_banner", "-loglevel", "warning",
			"-movflags", "frag_keyframe+empty_moov", "-preset", "fast",
			"-vf", "pad=ceil(iw/2)*2:ceil(ih/2)*2", "-threads", "0",
			"-f", "mp4", "pipe:1")

		if debug {
			cmd.Stderr = os.Stderr
		}

		stdin, err := cmd.StdinPipe();
		if err != nil {
			log.Println("Could not grab stdin pipe.", err)
			return nil, err
		}

		cmd.Stdout = part

		err = cmd.Start()
		if err != nil {
			log.Println("ffmpeg command failed to start. ", err)
			return nil, err
		}

		_, err = io.Copy(stdin, res.Body)
		if err != nil {
			log.Println("Could not write webm to stdin: ", err)
			return nil, err
		}

		err = stdin.Close()
		if err != nil {
			log.Println("Could not close os.Pipe writer")
			return nil, err
		}
		err = res.Body.Close()
		if err != nil {
			log.Println("Could not close response body.")
			return nil, err
		}

		err = cmd.Wait()
		if err != nil {
			log.Println("Command failed to execute. ", err)
			return nil, err
		}

		if debug {
			fmt.Println("FFMPEG was succesful.")
		}
	} else {
		return nil, nil
		postURI += "/sendPhoto"

		err = writer.WriteField("photo", fileURI)
		if err != nil {
			log.Println("Failed to write field `photo'.")
			return nil, err
		}
	}

	err = writer.Close()
	if err != nil {
		log.Println("Failed to close multipart writer.")
		return nil, err
	}

	req, err := http.NewRequest("POST", postURI, body)
	if err != nil {
		log.Println("Failed to generate post request.")
		return nil, err
	}

	req.Header.Add("Content-Type", writer.FormDataContentType())
	return req, nil
}

func board(b string) {
	defer wg.Done()

	res, _ := http.Get(fmt.Sprintf("https://a.4cdn.org/%s/catalog.json", b))
	postedBytes, _ := ioutil.ReadFile("posted." + b)
	posted := string(postedBytes)
	dec := json.NewDecoder(res.Body)

	defer res.Body.Close()

	client := &http.Client{}
	req_count := 0

	dec.Token()
	for dec.More() {
		var p Page
		dec.Decode(&p)
		for _, th := range p.Thread {
			if debug || (th.Replies > 50 &&
			!strings.Contains(posted, strconv.Itoa(th.No))) {
				posted += strconv.Itoa(th.No) + "\n"

				// Making this a goroutine breaks ffmpeg... lol.
				// i.e. `go func() {¬...`
				func() {
					wg.Add(1)
					defer wg.Done()

					req, err := procThread(th, b)
					if err != nil {
						log.Println("Could not generate request. ", err)
						return
					}
					if req == nil {
						return
					}

					// Send request.
					pres, err := client.Do(req)
					if err != nil {
						log.Println("http request failed.")
						wg.Done()
						return
					}
					defer pres.Body.Close()

					req_count += 1

					if debug {
						fmt.Println("Status: ", pres.Status)
						fmt.Println("Header: ", pres.Header)
						fmt.Println()

						body, err := ioutil.ReadAll(pres.Body)
						if err != nil {
							log.Println("Could not read post response body.")
						}
						fmt.Printf("Body Content: \"%s\"\n", body)
					}
				} ()
			}
		}
	}

	ioutil.WriteFile("posted." + b, []byte(posted), 0777)
}

/// Pass in command line arguments corresponding to each 4chan board
func main() {
	re = regexp.MustCompile("<.+>")

	fmt.Println("Sending requests...")

	for _, arg := range os.Args[1:] {
		if arg == "--debug" {
			debug = true
			continue
		}

		wg.Add(1)
		fmt.Printf("Looking at board /%s/.\n", arg)
		go board(arg)
	}

	wg.Wait()
	fmt.Println("Finished.")
}

