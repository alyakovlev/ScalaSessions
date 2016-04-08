/**
 * Created by ayakovlev on 23.11.15.
  *
 */
/**
  * Instructions and Descriptions here:
  * https://class.coursera.org/progfun-004/assignment/view?assignment_id=15
 */
object Huffman {

  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  def weight(tree: CodeTree): Int = {
    tree match {
      case Fork(l, r, cs, w) =>
        return weight(l) + weight(r)
      case Leaf(cs, w) =>
        return w
    }
  }

  def chars(tree: CodeTree): List[Char] = {
    tree match {
      case Fork(l, r, cs, w) =>
        return chars(l) ::: chars(r)
      case Leaf(c, w) =>
        return List(c)
    }
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] = {

    def createPairsList(cs : List[Char], pairs : List[(Char, Int)]) : List[(Char, Int)] = {

      if(cs.isEmpty)
        return pairs

      val curHead = cs.head

      val indexOfExistingPair = findPair(curHead, pairs)

      if (indexOfExistingPair != -1)
        createPairsList(cs.tail, pairs.updated(indexOfExistingPair, (curHead, pairs(indexOfExistingPair)._2 + 1)))
      else
        createPairsList(cs.tail, pairs :+ (curHead, 1))

    }

    def findPair(c : Char, pairs : List[(Char, Int)]) : (Int) = {
      val xs = for (e <- pairs if e._1 == c) yield pairs.indexOf(e)
      if (!xs.isEmpty)
        xs(0)
      else -1

    }

    createPairsList(chars, Nil)
  }

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    def subMake(leafsList : List[Leaf], freqs : List[(Char, Int)]) : List[Leaf] = {
      freqs match {
        case Nil => return leafsList
        case _ => subMake(leafsList :+ Leaf(freqs.head._1, freqs.head._2), freqs.tail)
      }
    }

    subMake(List(), freqs.sortWith((x, y) => x._2 < y._2))
  }

  def singleton(trees: List[CodeTree]): Boolean = {
    trees.size == 1
  }

  def combine(trees: List[CodeTree]): List[CodeTree] = {
    trees match {
      case Nil => List()
      case x :: Nil => List(x)
      case x :: y :: xyz => (xyz :+ makeCodeTree(x, y)).sortWith((x, y) => weight(x) < weight(y))
    }
  }

  def until(checkSingl: List[CodeTree] => Boolean, mergeNewForkIntoTree: List[CodeTree] => List[CodeTree]) : List[CodeTree] => List[CodeTree] = list => {
    if (singleton(list)) list else until(checkSingl, mergeNewForkIntoTree)(combine(list))}

  def createCodeTree(chars: List[Char]): CodeTree = {
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head
  }

  type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    val treeRoot = tree
    def decodeWithChars(tree : CodeTree, bits: List[Bit], charsList: List[Char]): List[Char] = {
      if ((bits.isEmpty) && (!tree.isInstanceOf[Leaf]))  return charsList
      tree match {
          case Fork(l, r, cs, w) => bits.head match{
            case 0 => decodeWithChars(l, bits.tail, charsList)
            case 1 => decodeWithChars(r, bits.tail, charsList)
          }
          case Leaf(c, w) => decodeWithChars(treeRoot, bits, charsList :+ c)
        }
      }

    decodeWithChars(tree, bits, List())
  }

  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  val secret: List[Bit] = List(1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0)

  def decodedSecret: List[Char] = decode(frenchCode, secret)

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val treeRoot = tree
    def encodeWithChars(tree: CodeTree, text: List[Char], bits: List[Bit]): List[Bit] = {
      if (text.isEmpty) return bits
      tree match {
        case Fork(l, r, cs, w) => if (chars(l).contains(text.head)) encodeWithChars(l, text, bits :+ 0) else encodeWithChars(r, text, bits :+ 1)
        case Leaf(c, w) => encodeWithChars(treeRoot, text.tail, bits)
      }
    }

    encodeWithChars(tree, text, List())
  }

  type CodeTable = List[(Char, List[Bit])]

  def codeBits(table: CodeTable)(char: Char): List[Bit] = {
    for(e <- table if e._1 == char) yield return e._2
  }

  def convert(tree: CodeTree): CodeTable = {
    def convertWithBits (tree: CodeTree, bits: List[Bit], table: CodeTable): CodeTable = {
      tree match {
        case Fork(l, r, _, _) => convertWithBits(l, bits :+ 0, table) ::: convertWithBits(r, bits :+ 1, table)
        case Leaf(c, w) => table :+ (c, bits)
      }
    }
    convertWithBits(tree, List(), List())
  }

  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = { (a ::: b).sortWith((x, y) => x._1 < y._1) }

  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def quickEncodeWithBits(tree: CodeTree, text: List[Char], bits: List[Bit]): List[Bit] = {
      if (text.isEmpty) return bits
      quickEncodeWithBits(tree, text.tail, bits ::: convert(tree).find(x => x._1 == text.head).map(x => x._2).toList.flatten)
    }

    quickEncodeWithBits(tree, text, List())
  }
}