package main
import "fmt"

type S struct {
  field int
}

func main() {
  var x S
  x.truc = 5
  fmt.Print(x)
}
