package main
import "fmt"

func side_effect(x *int) int {
  *x = *x + 1;
  return *x;
}

func test() (int, int, int) {
  x := 0;
  return side_effect(&x), side_effect(&x), side_effect(&x);
}

func main() {
  fmt.Print("assuming Print is in right order : \n");
  fmt.Print(test());
  fmt.Print("\n");
}
