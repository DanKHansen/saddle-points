case class Matrix(l: Seq[Seq[Int]]):
   val saddlePoints: Set[(Int, Int)] =
      val cs = l.head.indices.map(j => l.indices.map(i => l(i)(j)))
      val maxIdx = l.indices.map(i => l(i).indices.filter(j => l(i)(j) == l(i).max))

      l.indices.flatMap(i => maxIdx(i).filter(j => l(i)(j) == cs(j).min).map((i, _))).toSet
