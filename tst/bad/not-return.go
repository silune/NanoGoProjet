package main
import "fmt"

func f(x int) int {
  if true {
    if false {
      return 0
    } else {
      return x
    }
  } else {
    if false {
      return 0
    } else {
      fmt.Print("oubli return")
    }
  }
}

func main() {
  fmt.Print(f(0))
}
