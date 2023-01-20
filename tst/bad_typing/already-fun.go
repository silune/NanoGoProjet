package main
import "fmt"

func f(x int) int {
  return x
}

func f(x string) string {
  return x
}

func main() {
  fmt.Print(f(0))
}
