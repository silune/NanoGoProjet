package main
import "fmt"

//this code provide example of failure due to local memory lost when returning 

func pointer_ret() *int {
  x := 42;
  return &x;
}

func use_stack() {
  x, y := 1 + 1, 1 + 1;
  x, y = y, x;
}

func main(){
  p := pointer_ret();
  use_stack(); //expression that use the stack and write over x
  fmt.Print(*p, "\n");
}
