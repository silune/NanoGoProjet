package main
import "fmt"

func main() {
  x, y := 0, 1;
  for i := 0; i < 10; i++ {
    fmt.Print(x, "\n");
    t := x + y;
    x, y = y, t;
  }
}
