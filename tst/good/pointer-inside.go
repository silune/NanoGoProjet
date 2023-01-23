package main
import "fmt"

type A struct {
  a, b int
}

type B struct {
  a int
  b A
}

func main() {
  var a A
  a.a, a.b = 12, 13
  var b B
  b.a, b.b = 64, a
  fmt.Print("b : ", b, "\n")
  p := &(b.b)
  p.a = 14
  fmt.Print("p : ", p, " and b : ", b, "\n")
}
