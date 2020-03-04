import scala.io.Source

import java.io._

object THREESUM {
    
    def main(args: Array[String]) = {
        val input: String = args(0)
        val output: String = args(1)

        val bufferedSource = Source.fromFile(input)
        val (header :: ls) = bufferedSource.getLines.toList
        bufferedSource.close()

        val dataset: List[List[Int]] = ls.map(l => l.split(" ").toList.map(_.toInt))
        val response = dataset.map(threesum).map({
            case Some((a, b, c)) => List(a,b,c).mkString(" ")
            case None => "-1"
        }).mkString("\n")

        val bw = new BufferedWriter(new FileWriter(new File(output)))
        bw.write(response)
        bw.close()
    }

    def threesum(zss: List[Int]): Option[(Int, Int, Int)] = {
        val set: Map[Int, Int] = zss.zip(1 to zss.length).toMap
        def threesum_(yss: List[(Int, Int)], xss: List[(Int, Int)]): Option[(Int, Int, Int)] = (yss, xss) match {
            case (Nil, _)           => None
            case (y :: ys, Nil)     => threesum_(ys, ys)
            case (y :: ys, x :: xs) => set.get(-1 * (y._2 + x._2)) match {
                    case Some(i) => Some (y._1, x._1, i)
                    case None    => threesum_(y :: ys, xs)
                }
        }
        val ilst = (1 to zss.length).zip(zss).toList
        threesum_(ilst, ilst)
    }
}