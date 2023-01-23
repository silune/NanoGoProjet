package main
import "fmt"

func return_mult() (int, string, int, int) {
  return 1, "aba", 1 + 1, 38
}

func main() {
  w, x, y, z := return_mult();
  fmt.Print(w);
  fmt.Print(x);
  fmt.Print(y);
  fmt.Print(z);
  fmt.Print("\n");
}
