package compress.lz

import compress.Compressor
import Dictionaries._

/** The LZW compression method
  * @param initialDictionary the starting dictionary
  */
class LZW(val initialDictionary : Dictionary = ASCII) extends Compressor[Char, Seq[Int]]
  {
    /** @inheritdoc */
    def compress(msg : Seq[Char]) : Seq[Int] ={

      val ASCII = scala.collection.mutable.Map[String,Int]()
        (0 to 255).foreach{
            x=>
            ASCII += (x.toChar.toString -> x)
        }
      val sortie = scala.collection.mutable.Map[Int,Int]()
      val string = scala.collection.mutable.Map[Int,String](0 -> "")

      msg.foreach{
          i =>
          if (ASCII.contains((string(0) + i.toString)))
              string += (0 -> (string(0) + i.toString))
          else{
              sortie += (sortie.size-1 -> ASCII(string(0)))
              ASCII += ((string(0) + i.toString) -> ASCII.size)
              string += (0 -> i.toString)
          }
      }

      if (ASCII.contains(string(0)))
          sortie += (sortie.size-1 -> ASCII(string(0)))

      sortie.values.toSeq
    }

    /** @inheritdoc */
    def uncompress(res : Seq[Int]) : Option[Seq[Char]] ={

      val ASCII = scala.collection.mutable.Map[Int,String]()
        (0 to 255).foreach{
            x=>
            ASCII += (x -> x.toChar.toString)
        }
      val sortie = scala.collection.mutable.Map[Int,String]()
      val string = scala.collection.mutable.Map[Int,String](0 -> "")
      val next_code = scala.collection.mutable.Map[Int,Int](0 -> 256)

      res.foreach{
          i =>
          if (!(ASCII.contains(i)))
              ASCII += (i -> (string(0) + string(0)(0).toString))
          sortie += (sortie.size-1 -> ASCII(i))
          if (string(0).length != 0){
              ASCII += (next_code(0) -> (string(0) + ASCII(i)(0).toString))
              next_code += (0 -> (next_code(0)+1))
          } 
          string += (0 -> ASCII(i))
      }

      Option(sortie.values.flatten.toSeq).filter(_.nonEmpty)
    }
  }


