package main
import "fmt"

func f(x int, _ string) int {
  return x
}

func main() {
  fmt.Print(f(0, "0"))
}
