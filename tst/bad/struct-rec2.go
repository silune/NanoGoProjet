package main
import "fmt"

type A struct {
  fieldA B
}

type B struct {
  field int
  fieldB C
}

type C struct {
  field int
  fieldC A
}

func main() {
  fmt.Print(0)
}
