import scala.io.Source

import java.io._

object PAR3 {
    def main(args: Array[String]) = {
        val input: String = args(0)
        val output: String = args(1)

        val bufferedSource = Source.fromFile(input);
        val lines: List[String] = bufferedSource.getLines.toList
        val dataset: List[Int] = lines(1).split(" ").toList.map(_.toInt)
        bufferedSource.close()

        val bw = new BufferedWriter(new FileWriter(new File(output)))
        bw.write(partition(dataset).map(_.toString).mkString(" "))
        bw.close()
    }

    def partition(xss: List[Int]): List[Int] = xss match {
        case Nil => Nil
        case x :: xs => {
            def partition_(lt: List[Int], et: List[Int], gt: List[Int]): List[Int] = (lt, et, gt) match {
                case (Nil, Nil, Nil) => List()
                case (Nil, Nil, gt) => gt
                case (Nil, (e :: ets), gt) => e :: partition_(Nil, ets, gt)
                case (l :: lts, et, gt) => {
                    if (l < x) l :: partition_(lts, et, gt)
                        else if (l > x) partition_(lts, et, l :: gt)
                            else partition_(lts, l :: et, gt)
                }
            }
            partition_(xs, List(x), List())
        }
    }
}