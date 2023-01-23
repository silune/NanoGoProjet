package main
import "fmt"

type A struct {
  name string;
  content int;
}

type B struct {
  a A;
  ap *A;
}

type C struct {
  name string;
  b *B;
}

func main() {
  var a A
  a.name, a.content = "struct A avec :", 99
  fmt.Print(a, "\n")
  var a2 A
  a2.name, a2.content = "autre struct A avec :", 23
  var b B
  b.a, b.ap = a, &a2
  fmt.Print("strut B : ", b, "\n")
  var c C
  c.name, c.b = "grosse struc C avec : ", &b
  fmt.Print(c, "\n")
  fmt.Print("access b from c : ", c.b, "\n")
}
