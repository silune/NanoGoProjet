package main
import "fmt"

func f(x int, y int) (int, int) {
  return x, y
}

func main() {
  var w, x, y, z int = 0, 0, 0, 0
  w, x, y, z = f(x, w), f(z, y)
  fmt.Print(w)
}
