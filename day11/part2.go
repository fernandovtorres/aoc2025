package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

type State struct {
	node string
	dac bool
	fft bool
}

const origin = "svr"
const target = "out"

func findPath(graph map[string][]string, source string, dac, fft bool, memo map[State]int) int {

	state := State{source, dac, fft}
	if val, eba := memo[state]; eba {
		return val
	}

	if source == target {
		if dac && fft {
			return 1
		}
		return 0
	}

	res := 0
	for _, elem := range graph[source] {
		act_dac := (dac || (elem == "dac"))
		act_fft := (fft || (elem == "fft"))

		res += findPath(graph, elem, act_dac, act_fft, memo)
	}

	memo[state] = res
	return res
}

func dfs(graph map[string][]string) (res int) {

	res = findPath(graph, origin, false, false, make(map[State]int))
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

	res := dfs(graph)
	fmt.Println(res)
}
