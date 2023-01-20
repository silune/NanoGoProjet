package main
import "fmt"

func f(x int) int {
  return x, x
}

func main() {
  fmt.Print(f(0))
}
