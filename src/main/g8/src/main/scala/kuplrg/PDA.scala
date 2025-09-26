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

  /** Configurations reachable from the initial configuration by one-step moves
    *
    * @param init
    *   the initial configuration
    * @return
    *   the set of reachable configurations
    */
  def reachableConfig(init: Config): Set[Config] =
    def aux(
      targets: List[Config],
      visited: Set[Config],
    ): Set[Config] = targets match
      case Nil => visited
      case config :: targets =>
        val Config(q, w, xs) = config
        aux(
          targets = (
            eclo(q, xs).map {
              case (q, xs) => Config(q, w, xs)
            } ++ ((w, xs) match
              case (a <| w, x :: xs) =>
                trans((q, Some(a), x)).map {
                  case (q, ys) => Config(q, w, ys ++ xs)
                }
              case _ => Set()
            ) -- visited - config
          ).toList ++ targets,
          visited = visited + config,
        )
    aux(List(init), Set())

  /** The epsilon-closures for pairs of states and stacks
    *
    * @param q
    *   the current state
    * @param xs
    *   the current stack
    * @return
    *   the set of pairs of states and stacks reachable from `(q, xs)` by
    *   epsilon-transitions
    */
  def eclo(q: State, xs: List[Alphabet]): Set[(State, List[Alphabet])] =
    def aux(
      rest: List[(State, List[Alphabet])],
      visited: Set[(State, List[Alphabet])],
    ): Set[(State, List[Alphabet])] = rest match
      case Nil => visited
      case (q, xs) :: targets =>
        aux(
          rest = (xs match
            case x :: ys =>
              (trans(q, None, x).map {
                case (q, zs) => (q, zs ++ ys)
              } -- visited - ((q, xs))).toList
            case _ => Nil
          ) ++ targets,
          visited = visited + ((q, xs)),
        )
    aux(List((q, xs)), Set())

  /** The acceptance of a word by PDA
    *
    * @param w
    *   the word to be checked for acceptance
    * @return
    *   `true` if the word is accepted, `false` otherwise
    */
  def accept(w: Word): Boolean = acceptByFinalState(w)

  /** The initial configuration */
  def init(word: Word): Config = Config(initState, word, List(initAlphabet))

  /** Acceptance by final states
    *
    * @param word
    *   the word to be checked for acceptance
    * @return
    *   `true` if the word is accepted, `false` otherwise
    */
  def acceptByFinalState(word: Word): Boolean =
    reachableConfig(init(word)).exists(config => {
      val Config(q, w, _) = config
      w.isEmpty && finalStates.contains(q)
    })

  /** Acceptance by empty stacks
    *
    * @param word
    *   the word to be checked for acceptance
    * @return
    *   `true` if the word is accepted, `false` otherwise
    */
  def acceptByEmptyStack(word: Word): Boolean =
    reachableConfig(init(word)).exists(config => {
      val Config(_, w, xs) = config
      w.isEmpty && xs.isEmpty
    })

  /** The language of the PDA by final states */
  def langByFinalStates: Lang = Lang(symbols, acceptByFinalState)

  /** The language of the PDA by empty stacks */
  def langByEmptyStacks: Lang = Lang(symbols, acceptByEmptyStack)

  /** Check whether the PDA has epsilon-transitions increasing the stack */
  def incEpsTrans = for {
    ((q, a, x), set) <- trans.toList
    (p, ys) <- set
    if a.isEmpty && ys.length > 1
  } yield (q, a, x) -> (p, ys)
}
