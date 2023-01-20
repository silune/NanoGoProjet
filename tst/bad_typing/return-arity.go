package main
import "fmt"

func f(x int) (int, int) {
  return x, x
}

func g(x int) (int, int, int) {
  return f(x), 0
}

func main() {
  fmt.Print(g(0))
}
