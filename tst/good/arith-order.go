package main
import "fmt"

func side_effect(x *int) int {
  *x = *x + 1;
  return *x;
}

func main() {
  x := 0;
  fmt.Print(side_effect(&x) - side_effect(&x), "\n");
}
