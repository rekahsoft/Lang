package main

import (
	"fmt"
	"net/http"
	"strconv"
//	"io/ioutil"
)

type ServerConfig struct {
	port int
}


func handler(w http.ResponseWriter, r *http.Request) {
    fmt.Fprintf(w, "Hi there, I love %s!", r.URL.Path[1:])
}

func main() {
	config := ServerConfig{port: 3000}

	http.HandleFunc("/", handler)
	fmt.Printf("Running on port %d\n", config.port)
	http.ListenAndServe(":" + strconv.Itoa(config.port), nil)
}
