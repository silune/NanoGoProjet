package main
import "fmt"

func side_effect(x *int) int {
  *x = *x + 1;
  return *x;
}

func test (x int, y int, z int) {
  fmt.Print(x, " ");
  fmt.Print(y, " ");
  fmt.Print(z, " ");
}

func main() {
  x := 0;
  test(side_effect(&x), side_effect(&x), side_effect(&x));
  fmt.Print("\n");
}
