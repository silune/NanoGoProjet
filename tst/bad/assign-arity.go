package main
import "fmt"

func f (x int, y int) int {
  return x + y
}

func main() {
  var x int
  var y int
  x, y = f(x, y)
  fmt.Print(x)
}
