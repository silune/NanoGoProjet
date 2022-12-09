package main
import "fmt"

func f(x int, y int) int {
  return x + y
}

func g(x int) int {
  return x
}

func main() {
  fmt.Print(f(g(5)))
}
