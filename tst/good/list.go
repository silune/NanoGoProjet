package main
import "fmt"

type List struct {
  is_end bool
  content int
  next *List
}

func empty_list() *List {
  l := new(List)
  l.is_end = true
  return l
}

func cons(l **List, a int) {
  al := new(List)
  al.content, al.next = a, (*l)
  *l = al
}

func print_list(l *List) {
  fmt.Print("[")
  for ; !l.is_end && !l.next.is_end; l = l.next {
    fmt.Print(l.content, "; ")
  }
  if (!l.is_end) {
    fmt.Print(l.content)
  }
  fmt.Print("]")
}

func main() {
  l := empty_list()
  cons(&l, 1)
  cons(&l, 42)
  cons(&l, 13)
  print_list(l)
  fmt.Print("\n")
  print_list(empty_list())
  fmt.Print("\n")
}
