package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

const origin = "you"
const target = "out"

func bfs(graph map[string][]string) (res int) {
	res = 0
	var queue []string

	queue = append(queue, origin)

	for len(queue) > 0 {
		act := queue[0]
		queue = append(queue[:0], queue[1:]...)

		for _, elem := range graph[act] {
			if elem == target {
				res ++
				continue
			}
			queue = append(queue, elem)
		}
	}
	return
}

func main() {
	file, err := os.Open("../inputs/day11.txt")

	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	graph := make(map[string][]string)

	for scanner.Scan() {
		line := scanner.Text()
		splits := strings.Split(line, ":")
		source_node := splits[0]
		adjacent_nodes := strings.Split(strings.TrimSpace(splits[1]), " ")

		graph[source_node] = adjacent_nodes
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	res := bfs(graph)
	fmt.Println(res)
}
