package main
import "fmt"

//this code provide an example of warning appearing when printing a deep-recursive struct

type A struct {
  a *A;
}

type B struct {
  d D;
}

type C struct {
  b *B;
}

type D struct {
  c C;
}

func main() {
  var a A;
  fmt.Print(a, "\n");
  var b B;
  fmt.Print(b, "\n");
}
