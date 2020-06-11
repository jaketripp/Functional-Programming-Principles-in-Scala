
def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys =>
    // x is smaller than the smallest in the sorted list => already in the right spot
    if (x <= y) x :: xs
    // x belongs somewhere in the rest of the ys
    else y :: insert(x, ys)
}

def insertionSort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, insertionSort(ys))
}

insert(1, List()) == List(1)
insert(2, List(1)) == List(1, 2)
insert(1, List(2)) == List(1, 2)
insert(3, List(1, 2)) == List(1, 2, 3)

insertionSort(List(1,9,4,3,-1))