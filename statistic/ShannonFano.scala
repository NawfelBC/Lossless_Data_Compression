package compress.statistic

/** The SHANNON-FANO compression method */
class ShannonFano[S](source : Seq[S]) extends StatisticCompressor[S](source)
  {
    /** @inheritdoc */
    lazy val tree : Option[EncodingTree[S]] ={

        if (source.length == 0)
            None
        else if (source.length == 1)
            Option(EncodingLeaf(1,source(0)))
        else{
        
            val proba = this.orderedCounts.reverse.map(i=>((i._1,(i._2.toDouble/source.length.toDouble))))

            def recursive(seq:Seq[(S,Double)]):EncodingTree[S] ={
                
                def recursive2(seq:Seq[(S,Double)], pos:Int):Int ={
                    val left = seq.slice(0,pos+1)
                    val right = seq.slice(pos+1,seq.length)
                    if (left.map(x=>x._2).sum < ((seq.map(x=>x._2).sum)/2))
                        recursive2(seq,pos+1)
                    else
                        pos
                }

                if (seq.length == 1)
                    EncodingLeaf(Math.ceil(seq(0)._2*source.length).toInt,seq(0)._1)
                else{
                    val pos = recursive2(seq,0)
                    val left = seq.slice(0,pos+1)
                    val right = seq.slice(pos+1,seq.length)
                    val label = Math.ceil((seq.map(x=>x._2).sum * source.length)).toInt
                    
                    EncodingNode(label,recursive(left),recursive(right))
                }
            }

            Option(recursive(proba))
        }
    }
  }
