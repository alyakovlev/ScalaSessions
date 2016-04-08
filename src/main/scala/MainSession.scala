import Huffman.Leaf

/**
 * Created by ayakovlev on 15.10.15.
 */

object MainSession {

  def main(args: Array[String]) {
    hoffmanTest()
  }

  def hoffmanTest(){
    val sampleTree = Huffman.makeCodeTree(
      Huffman.makeCodeTree(Leaf('a',1 ), Leaf('b', 2)),
      Leaf('c', 3))

    println(Huffman.encode(sampleTree)(List('a','c','b')))

    List(
      ('a', List(0,0)),
      ('c', List(1)),
      ('b', List(0,1))
    )
    println(Huffman.codeBits(List(
      ('a', List(0,0)),
      ('c', List(1)),
      ('b', List(0,1))
    ))('c'))

    println(Huffman.convert(sampleTree))
    val str = List('a','c','b')
    println(Huffman.convert(sampleTree).find(x => x._1 == str.head).map(x => x._2).toList.flatten)
    println(Huffman.quickEncode(sampleTree)(List('b','a','с')))
  }
}
