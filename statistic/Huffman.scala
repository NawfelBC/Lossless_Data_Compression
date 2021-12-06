package compress.statistic
import scala.math.Ordering.Implicits._
import scala.collection.mutable.PriorityQueue

/** The HUFFMAN compression method */
class Huffman[S](source : Seq[S]) extends StatisticCompressor[S](source)
  {
    /** @inheritdoc */
    lazy val tree : Option[EncodingTree[S]] ={

        if (source.length == 0)
            None
        
        else if (source.length == 1)
            Option(EncodingLeaf(1,source(0)))
        
        else if (source.distinct.length == 1)
            Option(EncodingLeaf(source.length,source(0)))
        
        else{
            val queue = PriorityQueue[EncodingTree[S]]()(Ordering.by(_.label)).reverse

            val occ = this.orderedCounts.map(x=>EncodingLeaf(x._2,x._1))

            occ.map(y=>queue.enqueue(y))

            
            def recursive(queue:PriorityQueue[EncodingTree[S]]): EncodingNode[S] ={

                if (queue.size > 1){
                    val noeud1 : EncodingTree[S] = queue.dequeue()
                    val noeud2 : EncodingTree[S] = queue.dequeue()
                    queue.enqueue(EncodingNode((noeud1.label+noeud2.label),noeud1,noeud2))
                    recursive(queue)
                }
                else{
                    queue.dequeue() match{
                        case x:EncodingNode[S] => x
                        case x:EncodingLeaf[S] => EncodingNode(1,null,null)
                    }
                }
            }

            Option(recursive(queue))
        }
    }
  }


