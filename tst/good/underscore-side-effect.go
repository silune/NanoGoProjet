package main
import "fmt"

func side_effect(x *int) int {
  *x = *x + 1
  return *x
}

func main() {
  t := 0
  x, _, z := side_effect(&t), side_effect(&t), side_effect(&t)
  fmt.Print(x, z, "\n");
}
