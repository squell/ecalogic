package nl.ru.cs.spl.scala

trait Runtime {

  def main()

  def print(value: Any) {
    println(value)
  }

  def isEmpty[T](list: List[T]) = list.isEmpty

  def head[T](list: List[T]) = list.head

  def tail[T](list: List[T]) = list.tail

  def fst[A, B](tuple: (A, B)) = tuple._1

  def snd[A, B](tuple: (A, B)) = tuple._2

  def main(args: Array[String]) {
    val profile = args.contains("-profile")

    val start = System.currentTimeMillis()
    main()
    val end = System.currentTimeMillis()

    if (profile) Console.out.println("Execution time: "+(end-start)+" ms")
  }

}