package main
import "fmt"

func return_mult() (int, string, int, int) {
  return 1, "aba", 1 + 1, 3 * 1
}

func main() {
  fmt.Print(return_mult());
  fmt.Print("\n");
}
