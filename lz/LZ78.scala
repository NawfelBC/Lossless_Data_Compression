package compress.lz

import compress.Compressor

/** The LZ78 compression method */
object LZ78 extends Compressor[Char, Seq[(Int, Char)]]
  {
    /** @inheritdoc */
    def compress(msg : Seq[Char]) : Seq[(Int, Char)] ={
      
      val dict = scala.collection.mutable.Map("eps" -> 0)
      val combinaison = scala.collection.mutable.Map[Int,String](0 -> "")
      val sortie = scala.collection.mutable.Map[Int,(Int,Char)]()
      (0 to msg.length-1).foreach {
          i =>
          combinaison += (0 -> (combinaison(0)+msg(i).toString))
          if (dict.contains(combinaison(0)) == false){
              dict += (combinaison(0) -> dict.size)
              if (combinaison(0).length == 1)
                  sortie += (dict.size -> (0 -> combinaison(0)(0)))
              else
                  sortie += (dict.size -> (dict(combinaison(0).take(combinaison(0).length-1)) -> combinaison(0).last))
              
              combinaison += (0 -> "")
          }
      }
      sortie.values.toSeq
    }

    /** @inheritdoc */
    def uncompress(res : Seq[(Int, Char)]) : Option[Seq[Char]] ={

      val dict = scala.collection.mutable.Map(0 -> "")
      val sortie = scala.collection.mutable.Map[Int,Char]()
      (0 to res.length-1).foreach {
          i =>
          dict += (i+1 -> (dict(res(i)._1)+res(i)._2))
          ((dict(res(i)._1)+res(i)._2).toCharArray).foreach{
              j =>
              sortie += (sortie.size-1 -> j)
          }
      }

      Option(sortie.values.toSeq).filter(_.nonEmpty)
    }
  }
