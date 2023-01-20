package main
import "fmt"

func side_effect(x *int) int {
  *x = *x + 1;
  return *x;
}

func main() {
  test := 0
  var x, y, z int;
  x, y, z = side_effect(&test), side_effect(&test), side_effect(&test);
  fmt.Print(x, " ");
  fmt.Print(y, " ");
  fmt.Print(z, " ");
  fmt.Print("\n");
}
