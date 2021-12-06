package compress

/** The Run-length encoding compression method */
class RLE[T] extends Compressor[T, Seq[(T, Int)]]
  {
    /** @inheritdoc */
    def compress(msg : Seq[T]) : Seq[(T, Int)] ={
      
      if (msg.isEmpty){
            Seq()
        }
        else{
            val h = msg.head
            val (m,r) = msg.span(_ == h)
            (h, m.length) +:  compress(r)
        }        
    }

    /** @inheritdoc */
    def uncompress(seq : Seq[(T, Int)]) : Option[Seq[T]] ={

      Option(seq.flatMap(a => Seq.fill(a._2)(a._1))).filter(_.nonEmpty)
    }
  }
