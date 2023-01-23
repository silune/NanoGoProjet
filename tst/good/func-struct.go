package main
import "fmt"

type A struct {
  a int
  b int
}

type B struct {
  x int
  y A
  z int
}

func new_B(x int, y A, z int) B {
  var b B
  b.x, b.y, b.z = x, y, z
  return b
}

func main() {
  var a A
  a.a, a.b = 12, 13
  b := new_B(8, a, 9)
  fmt.Print(a, b, "\n")
}
