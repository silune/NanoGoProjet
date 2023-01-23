package main
import "fmt"

func f(x int) (int, int, string) {
  fmt.Print("val in f :\n");
  a, b, c := x+1, x+2, "Hello";
  fmt.Print(a, b, c, "\n");
  return a, b, c
}

func g(a int, b int, c string) (string, int, int, string) {
  fmt.Print("val in g :\n");
  e, f, g, h := c, 0, b * a, "Wolrd : ";
  fmt.Print(e, f, g, h, "\n");
  return e, f, g, h
}

func h(t string, u int, v int, w string) (string, string, int, string) {
  fmt.Print("val in h :\n");
  i, j, k, l := t, w, u + v, "\n";
  fmt.Print(i, j, k, l, "\n");
  return i, j, k, l
}

func main() {
  w, x, y, z := h(g(f(5)));
  fmt.Print("final :\n");
  fmt.Print(w, x, y, z);
}
