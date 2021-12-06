package compress.statistic

import compress.Compressor

/** A statistic compressor relies on the statistics of symbols in source
  * @param source the input source
  * @tparam S type of symbol used in source
  */
abstract class StatisticCompressor[S](source : Seq[S]) extends Compressor[S, Seq[Bit]]
  {
    /** A map giving the occurrences of each symbol in the source sequence */
    val occurrences : Map[S, Int] = source.distinct.map{symb => (symb,source.count(_ == symb))}.toMap

    /** SHANNON entropy of source */
    val entropy : Double = -(source.map(symb => (((source.count(_ == symb)).toDouble/source.length.toDouble)*scala.math.log((source.count(_ == symb)).toDouble/source.length.toDouble)))).sum

    /** The sequence of occurrences sorted by count */
    val orderedCounts : Seq[(S, Int)] = source.distinct.map{symb => (symb,source.count(_ == symb))}.toMap.toSeq.sortBy(_._2)

    /** The encoding tree (in most cases, depends from `source`) */
    def tree : Option[EncodingTree[S]]

    /** @inheritdoc */
    def compress(msg: Seq[S]): Seq[Bit] ={

      val _map = scala.collection.mutable.Map[Int,Bit]()
      val tmp = msg.map{
          x=>
          this.tree.get.encode(x)
      }

      tmp.foreach{
              y=>
              y.get.foreach{
                  z=>
                  _map += (_map.size -> z)
              }
      }
      _map.values.toSeq
    }

    /** @inheritdoc */
    def uncompress(res: Seq[Bit]): Option[Seq[S]] ={

      val tmp = this.tree.getOrElse(1)
      if (tmp == 1)
          None
      else
          this.tree.get.decode(res)
    }
  }
