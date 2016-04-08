import scala.collection.immutable.HashSet

/**
  * Created by ayakovlev on 09.01.2016.
  *
  * Water Pouring Problem
  * Here you can find ideal description and implementation:
  * https://www.youtube.com/watch?v=gjTDp4-UBsY&index=5&list=PLO9y7hOkmmSHz-a9tj_HvCM2KDB6MTYI5
  *
  * Below you can see my own implementation
  */


type State = List[Int]
//glasses
val capacities = List(4, 9)
//our goal
val target = 6

val pouring = new Pouring(capacities)
//get first solution from the stream
val firstSolution = pouring.Solutions(target).head

//moves: Empty, Fill and Pour
trait Move {
  def change(pouring: Pouring, state: State): State
}

class Empty(glass: Int) extends Move {
  var _glass: Int = glass

  override def change(pouring: Pouring, state: State): State = {
    state.updated(_glass, 0)
  }

  override def toString(): String = {
    String.format("Empty({%s})", _glass.toString)
  }
}

class Fill(glass: Int) extends Move {
  var _glass: Int = glass

  override def change(pouring: Pouring, state: State): State = {
    state.updated(_glass, pouring.Capacities(_glass))
  }

  override def toString(): String = {
    String.format("Fill({%s})", _glass.toString)
  }
}

class Pour(fromGlass: Int, toGlass: Int) extends Move {
  var _fromGlass: Int = fromGlass;
  var _toGlass: Int = toGlass;

  override def change(pouring: Pouring, state: State): State = {
    val toCapacity = pouring.Capacities(_toGlass);
    val fromState = state(_fromGlass);
    val toState = state(_toGlass);
    //check: how much water can we pour?
    val amount = Math.min(fromState, toCapacity - toState);
    state.updated(_fromGlass, fromState - amount)
      .updated(_toGlass, toState + amount);
  }

  override def toString(): String = {
    String.format("Pour({%s},{%s})", _fromGlass.toString, _toGlass.toString);
  }
}

class Pouring(capacities: List[Int]) {

  var _capacities: List[Int] = capacities

  //start state
  var _initialState: State = _capacities map (_ => 0)
  var _initialPath: Path = new Path(this, _initialState, List[Move]())
  var glasses = List.range(0, _capacities.size)

  //generate all possible moves
  var emptyMoves = glasses.map(g => new Empty(g)).map(_.asInstanceOf[Move])
  var fillMoves = glasses.map(g => new Fill(g)).map(_.asInstanceOf[Move])
  var pourMoves = glasses.flatMap(g1 => glasses.filter(g2 => g2 != g1).map(g2 => new Pour(g1, g2)).map(_.asInstanceOf[Move]))
  var _allPossibleMoves: List[Move] = emptyMoves ::: fillMoves ::: pourMoves

  def Capacities: List[Int] = {
    return _capacities;
  }

  class Path(pouring: Pouring, endState: State, history: List[Move]) {
    var _pouring: Pouring = pouring;

    //end state of this path
    var _endState: State = endState;

    //history: what happened earlier
    var _history: List[Move] = history;

    def EndState: State = {
      return _endState;
    }

    def History: List[Move] = {
      return _history;
    }

    def Extend(move: Move): Path = {
      return new Path(_pouring, move.change(_pouring, EndState), move :: _history);
    }

    override def toString(): String = {

      /*
      List(1,2,3,4,5).foldLeft(0)(_ + _)
      // This is the only valid order of operations
  0+1 = 1
        1+2 = 3
              3+3 = 6
                    6+4 = 10
                          10 + 5 = 15
                                   15  // done
      */

      /*
     List(1,2,3,4,5).foldRight(0)(_ + _)
     // This is the only valid order of operations
                           5 = 5+0
                     9 = 4+5
                12=3+9
           14=2+12
      15=1+14
     */


      val movesAndStates = _history.foldRight(
        List[(Move, State, String)]())((move: Move, acc: List[(Move, State, String)]) => {
        val previousState = if (acc.isEmpty) _pouring._initialState else acc.head._2
        val nextState = move.change(_pouring, previousState)
        val moveDescription = String.format("{%s} => {%s}", move, nextState).padTo(acc.size, ' ')

        (move, nextState, moveDescription) :: acc
      })
      movesAndStates
        .map(x => x._3)
        .reverse
        .mkString(sys.props("line.separator"))
    }
  }

  def From(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] = {
    if (paths.isEmpty) return Stream[Set[Path]]()

    val morePaths = paths
      .flatMap(p => _allPossibleMoves.map(m => p.Extend(m)))
      .filter(p => !explored.contains(p.EndState))

    return Stream.cons(
      paths,
      From(
        morePaths,
        explored ++ (morePaths.map(p => p.EndState))));
  }

  def Solutions(target: Int): Seq[Path] = {
    val pathSets = From(CreatePathSet(_initialPath), CreateStateSet(_initialState))
    pathSets
      .toSeq
      .flatMap(pathSet => pathSet.filter(path => path.EndState.contains(target)))
  }

  def CreatePathSet(path: Path): Set[Path] = {
    HashSet(path);
  }

  def CreateStateSet(state: State): Set[State] = {
    HashSet(state);
  }
}


