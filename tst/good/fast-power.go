package main
import "fmt"

func pow(a int, b int) int {
  if (b == 0) {
    return 1;
  }
  if (b == 1) {
    return a;
  }
  semiRes := pow(a, b / 2)
  if (b % 2 == 0) {
    return semiRes * semiRes
  } else {
    return a * semiRes * semiRes
  }
}

func main() {
  fmt.Print("5 ^ 13 : ", pow(5, 13), "\n")
  fmt.Print("13 ^ 65 : ", pow(13, 65), "\n")
}
