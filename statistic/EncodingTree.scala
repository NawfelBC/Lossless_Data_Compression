package compress.statistic

/** Trait for binary encoding trees (integer-labeled binary trees)
  * @tparam S type of symbols (in leaves only)
  */
sealed abstract class EncodingTree[S](val label : Int)
  {
    /* OPERATIONS ON VALUES */
    
    /** Added function to get all leaves values of tree
      * @param y tree on which we apply the function
      * @return a map with values of all leaves      
      */
    def leaves(y : EncodingTree[S], map_ : scala.collection.mutable.Map[Int,S]) : scala.collection.mutable.Map[Int,S] = {
        y match {
            case y: EncodingNode[S] @unchecked => {
                if (y != null){
                    if (y.left != null)
                        leaves(y.left, map_)
                    
                    if (y.right != null)
                        leaves(y.right, map_)
                }
            }
            case y: EncodingLeaf[S] @unchecked => {
                map_ += (map_.size-1 -> y.value)
            }
        }
        
        map_
    }

    /** Checks if tree contains given value
      * @param x value to search
      * @return true if the tree has a leaf with value `x`
      */
    def has(x : S) : Boolean ={

      this match {
          case i: EncodingNode[S] @unchecked => leaves(i,scala.collection.mutable.Map[Int,S]()).values.toSeq.contains(x)
          case i: EncodingLeaf[S] @unchecked => i.value == x
      }
    }

    /** Reduce operation on tree values when applying a function on leaves beforehand
      * @param f the function applied to each leaf value
      * @param op the aggregation operation on a node
      * @tparam U the result type of `f`
      * @return the aggregated value of the tree
      */
    def reduceWith[U](f : S => U)(op : (U, U) => U) : U = this match
      {
        case EncodingLeaf(_, v   ) => f(v)
        case EncodingNode(_, l, r) => op((l reduceWith f)(op), (r reduceWith f)(op))
      }

    /** Reduce operation on tree values
      *
      * `t reduce op` is a shorthand for `(t reduceWith {v => v})(op)`
      * @param op the aggregation operation on a node
      * @return the aggregated value of the tree
      */
    def reduce(op : (S, S) => S) : S = (this reduceWith {v => v})(op)


    /* OPERATIONS ON LABELS */

    /** Reduce operation on tree labels when applying a function on leaves beforehand
      * @param fL the function applied to each leaf label
      * @param opL the aggregation operation on a node : first parameter is the node label
      * @tparam A the result type of `f`
      * @return the result of aggregation operation recursively applied to tree
      */
    def reduceLabelWith[A](fL : Int => A)(opL : (Int, A, A) => A) : A = this match
      {
        case EncodingLeaf(lbl, _   ) => fL(lbl)
        case EncodingNode(lbl, l, r) => opL(lbl, (l reduceLabelWith fL)(opL), (r reduceLabelWith fL)(opL))
      }

    /** Reduce operation on tree labels
      *
      * `t reduceLabel opL` is a shorthand for `(t reduceLabelWith {lbl => lbl})(opL)`
      * @param opL the aggregation operation on a node : first parameter is the node label
      * @return the aggregated label of the tree
      */
    def reduceLabel(opL : (Int, Int, Int) => Int) : Int = (this reduceLabelWith identity)(opL)


    /* ENCONDING/DECODING OPERATIONS */

    /** Computes the bit sequence corresponding to a tentative leaf value.
      * @param x value to encode
      * @return the corresponding bit sequence of `x` is a leaf of encoding tree, `None` otherwise
      */
    def encode(x : S) : Option[Seq[Bit]] ={

      def recursive(y : EncodingTree[S], map_ : scala.collection.mutable.Map[Int,Bit]): Option[Seq[Bit]]= {
          
          y match {
              case i: EncodingNode[S] @unchecked =>{
                  if (!(i.has(x))){
                      None
                  }
                  else{
                      if (!(i.left.has(x))){
                          map_ += (map_.size-1 -> One)
                          recursive(i.right, map_)
                      }
                      else{
                          map_ += (map_.size-1 -> Zero)
                          recursive(i.left, map_)
                      }                       
                  }
              }
              case i: EncodingLeaf[S] @unchecked => {
                  Option(map_.values.toSeq)
              }
          }
      }
      
      this match{
          case j: EncodingNode[S] @unchecked => recursive(j,scala.collection.mutable.Map[Int,Bit]())
          case j: EncodingLeaf[S] @unchecked => Option(Seq(One))
      }
    }

    /** Computes the next value corresponding to the beginning of bit sequence (if possible)
      * @param res the bit sequence to decode
      * @return the decoded value and the bit sequence left to be decoded or `None` if current bit sequence does not lead to a leaf in enconding tree
      */
    def decodeOnce(res : Seq[Bit]) : Option[(S, Seq[Bit])] ={

      def recursive2(y: EncodingTree[S], map_ :scala.collection.mutable.Map[Int,Int]): Option[(S,Int)] ={

          y match{
              case i: EncodingNode[S] @unchecked =>{
                  if (res(map_(0)-1) == One){
                      if (i.right == null)
                          None
                      else{
                          map_ += (0 -> (map_(0)+1))
                          recursive2(i.right,map_)
                      }
                  }
                  else{
                      if (i.left == null)
                          None
                      else{
                          map_ += (0 -> (map_(0)+1))
                          recursive2(i.left,map_)
                      }
                  }
              }
              case i: EncodingLeaf[S] @unchecked =>{
                  Option((i.value,map_(0)-1))
              }
          }
      }

      this match{
          case j: EncodingNode[S] @unchecked => {
              try {
                  val tmp = (recursive2(j,scala.collection.mutable.Map[Int,Int](0 -> 1)).get)
                  val tmp2 = res.drop(tmp._2)
                  Option((tmp._1, tmp2))
              } catch {
                  case e: Exception => None
              }
          }
          case j: EncodingLeaf[S] @unchecked => {
              Option((j.value,res))
          }            
      }
    }

    /** Computes the sequence of values from the sequence of bits
      * @param res the bit sequence to decode
      * @return the sequence of decoded values or `None` otherwise
      */
    def decode(res : Seq[Bit]) : Option[Seq[S]] ={

      def loop(res : Seq[Bit], map_ : scala.collection.mutable.Map[Int,S]) : Option[Seq[S]]={
                
          if (res.length != 0){
              try{
                  map_ += (map_.size -> (decodeOnce(res).get._1))
                  loop(decodeOnce(res).get._2, map_)
              } catch {
                  case e: Exception => None
              }
          }
          else
              Option(map_.values.toSeq)
      }

      loop(res,scala.collection.mutable.Map[Int,S]())
    }


    /* MISCELLANEOUS */

    /** Mean length of code associated to encoding tree */
    lazy val meanLength : Double ={

      val _map = scala.collection.mutable.Map[Int,S]()
      val source = leaves(this,_map).values.toSeq

      val total_bits = source.distinct.map{
          x=>
          source.count(_ == x) * encode(x).get.length
      }.toSeq.sum
      val total_symb = source.length

      (total_bits.toDouble/total_symb.toDouble)
    }

    /** @inheritdoc */
    override def toString : String = this match
     {
       case EncodingLeaf(lbl, v   ) => (v, lbl).toString()
       case EncodingNode(lbl, l, r) => s"EncodingNode([$lbl], $l, $r)"
     }
  }
case class EncodingNode[S](override val label : Int, left : EncodingTree[S], right : EncodingTree[S]) extends EncodingTree[S](label)
case class EncodingLeaf[S](override val label : Int, value : S) extends EncodingTree[S](label)

