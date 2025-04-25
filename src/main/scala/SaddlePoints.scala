case class Matrix(rows: List[List[Int]]):
   def saddlePoints: Set[(Int, Int)] = {
      for
         row <- rows
         idx <- row.indices
         if row(idx) == row.max && row(idx) == rows.map(r => r(idx)).min
      yield (rows.indexOf(row), idx)
   }.toSet
