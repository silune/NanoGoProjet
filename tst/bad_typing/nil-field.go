package main
import "fmt"

type S struct {
  field int
}

func main() {
  nil.field = 5
  fmt.Print(5)
}
