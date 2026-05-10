package kuplrg

/** The type definition of configurations */
case class Config(state: State, word: Word, stack: List[Alphabet])

/** The definition of pushdown automata (PDA)
  *
  * @constructor
  *   create a new PDA
  *
  * @param states
  *   the set of states
  * @param symbols
  *   the set of symbols
  * @param alphabets
  *   the set of stack alphabets
  * @param trans
  *   the transition function
  * @param initState
  *   the initial state
  * @param initAlphabet
  *   the initial stack alphabet
  * @param finalStates
  *   the set of final states
  */
case class PDA(
  states: Set[State],
  symbols: Set[Symbol],
  alphabets: Set[Alphabet],
  trans: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]],
  initState: State,
  initAlphabet: Alphabet,
  finalStates: Set[State],
) extends Acceptable {

  /** The safe read of the transition function of PDA */
  def getTrans(
    q: State,
    a: Option[Symbol],
    x: Alphabet,
  ): Set[(State, List[Alphabet])] =
    trans.getOrElse((q, a, x), Set())

  /** The acceptance of a word by PDA */
  def accept(w: Word): Boolean = acceptByFinalState(w)

  /** The initial configuration */
  def init(word: Word): Config = Config(initState, word, List(initAlphabet))

  /** Acceptance by final states */
  def acceptByFinalState(word: Word): Boolean =
    acceptBySaturation(word, byFinal = true)

  /** Acceptance by empty stacks */
  def acceptByEmptyStack(word: Word): Boolean =
    acceptBySaturation(word, byFinal = false)

  /** Saturation-based acceptance (Bouajjani–Esparza–Maler 1997) */
  private def acceptBySaturation(word: Word, byFinal: Boolean): Boolean =
    val n = word.length
    import PDA.Node
    import PDA.Node.*

    val rules: List[(Node, Alphabet, List[Alphabet], Node)] =
      (for {
        i <- (0 to n).toList
        ((q, optA, x), targets) <- trans.toList
        if optA.isEmpty
        (p, ys) <- targets
      } yield (Pos(q, i), x, ys, Pos(p, i))) ++
      (for {
        i <- (0 until n).toList
        ((q, optA, x), targets) <- trans.toList
        if optA.contains(word(i))
        (p, ys) <- targets
      } yield (Pos(q, i), x, ys, Pos(p, i + 1)))

    val finalNodes: Set[Node] =
      if (byFinal) finalStates.map(Pos(_, n)) + End
      else states.map(Pos(_, n))

    val initialDelta: Map[(Node, Alphabet), Set[Node]] =
      if (byFinal)
        (for { x <- alphabets } yield (End, x) -> Set[Node](End)).toMap ++
        (for { q <- finalStates; x <- alphabets }
          yield (Pos(q, n), x) -> Set[Node](End))
      else Map.empty

    def follow(delta: Map[(Node, Alphabet), Set[Node]])(
      start: Node,
      ys: List[Alphabet],
    ): Set[Node] =
      ys.foldLeft(Set(start))((cur, x) =>
        cur.flatMap(s => delta.getOrElse((s, x), Set.empty)),
      )

    def step(
      delta: Map[(Node, Alphabet), Set[Node]],
    ): (Map[(Node, Alphabet), Set[Node]], Boolean) =
      rules.foldLeft((delta, false)) {
        case ((d, ch), (src, x, ys, dst)) =>
          val targets = follow(d)(dst, ys)
          if (targets.isEmpty) (d, ch)
          else
            val key = (src, x)
            val cur = d.getOrElse(key, Set.empty)
            val updated = cur ++ targets
            if (updated.size > cur.size) (d + (key -> updated), true)
            else (d, ch)
      }

    def saturate(
      delta: Map[(Node, Alphabet), Set[Node]],
    ): Map[(Node, Alphabet), Set[Node]] =
      val (next, changed) = step(delta)
      if (changed) saturate(next) else next

    val finalDelta = saturate(initialDelta)
    follow(finalDelta)(Pos(initState, 0), List(initAlphabet))
      .exists(finalNodes.contains)

  /** The language of the PDA by final states */
  def langByFinalStates: Lang = Lang(symbols, acceptByFinalState)

  /** The language of the PDA by empty stacks */
  def langByEmptyStacks: Lang = Lang(symbols, acceptByEmptyStack)

  /** Checks if the PDA is valid, or throws an exception */
  lazy val mustValid: this.type =
    for {
      q <- states
      a <- symbols.map(Some(_)) + None
      x <- alphabets
      opt <- trans.get((q, a, x))
      (p, ys) <- opt
      if !states.contains(p) || ys.exists(!alphabets.contains(_))
      aStr = a.fold("ε")(_.toString)
      tranStr = s"$q -> $p - $aStr [$x -> ${ys.mkString(" ")}]"
    } error(s"Invalid transition: $tranStr")
    if (!states.contains(initState))
      error(s"Invalid initial state: $initState")
    if (!alphabets.contains(initAlphabet))
      error(s"Invalid initial alphabet: $initAlphabet")
    val invalidFinals = finalStates.filter(!states.contains(_))
    if (invalidFinals.nonEmpty)
      error(s"Invalid final states: ${invalidFinals.mkString(", ")}")
    this

  /** Check whether the pushdown automaton is deterministic */
  def isDeterministic: Boolean = trans.forall {
    case ((q, a, x), set) =>
      set.size <= 1 && (
        a != None || set.isEmpty || trans.forall {
          case ((p, b, y), set) =>
            p != q || x != y || b == None || set.isEmpty
        }
      )
  }

  /** Check whether the PDA has epsilon-transitions increasing the stack */
  def incEpsTrans = for {
    ((q, a, x), set) <- trans.toList
    (p, ys) <- set
    if a.isEmpty && ys.length > 1
  } yield (q, a, x) -> (p, ys)
}

object PDA {

  /** Node used by the saturation acceptance algorithm */
  enum Node:
    case Pos(q: State, i: Int)
    case End

  def apply(
    initState: State,
    initAlphabet: Alphabet,
    finalStates: Set[State],
    transSeq: ((State, Option[Symbol], Alphabet), (State, List[Alphabet]))*,
  ): PDA =
    val states = for {
      ((p, _, _), (q, _)) <- transSeq.toSet; q <- Set(p, q)
    } yield q
    val symbols = for { ((_, opt, _), _) <- transSeq.toSet; a <- opt } yield a
    val alphabets = for {
      ((_, _, x), (_, ys)) <- transSeq.toSet; x <- x :: ys
    } yield x
    val map = transSeq.groupMap(_._1)(_._2).map(_ -> _.toSet)
    val trans = (for {
      q <- states
      a <- symbols.map(Some(_)) + None
      x <- alphabets
    } yield (q, a, x) -> map.getOrElse((q, a, x), Set())).toMap
    PDA(
      states = states,
      symbols = symbols,
      alphabets = alphabets,
      trans = trans,
      initState = initState,
      initAlphabet = initAlphabet,
      finalStates = finalStates,
    )

  def apply(
    states: Set[State],
    symbols: Set[Symbol],
    alphabets: Set[Alphabet],
    initState: State,
    initAlphabet: Alphabet,
    finalStates: Set[State],
  )(
    pairs: ((State, Symbol | EPS, Alphabet), (State, List[Alphabet]))*,
  ): PDA =
    val trans = pairs
      .groupBy(_._1)
      .map {
        case ((state, symbol, alphabet), values) =>
          val opt: Option[Symbol] = symbol match
            case a: Symbol => Some(a)
            case _: EPS    => None
          (state, opt, alphabet) -> values.map(_._2).toSet
      }
      .withDefaultValue(Set())
    PDA(states, symbols, alphabets, trans, initState, initAlphabet, finalStates)
}
