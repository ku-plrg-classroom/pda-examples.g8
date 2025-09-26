package kuplrg

object Implementation extends Template {

  /** This is the playground for you to run your implementation. Do whatever you
    * want here and run `sbt run` to see the result.
    */
  @main def playground: Unit = {
    println("------------------- PLAYGROUND -------------------")

    // You can check your implementation here.
    val accept = pda_an_bn_final.acceptByFinalState
    println(s"pda_an_bn_final.acceptByFinalState(\"ab\")   = ${accept("ab")}")
    println(s"pda_an_bn_final.acceptByFinalState(\"aba\")  = ${accept("aba")}")
    println(s"pda_an_bn_final.acceptByFinalState(\"aabb\") = ${accept("aabb")}")

    println("--------------------------------------------------")
  }

  // PDA accepting L = { a^n b^n | n >= 0 } by final states
  val pda_an_bn_final: PDA = PDA(
    states = Set(0, 1, 2),
    symbols = Set('a', 'b'),
    alphabets = Set("X", "Z"),
    trans = Map(
      (0, Some('a'), "Z") -> Set((0, List("X", "Z"))),
      (0, Some('a'), "X") -> Set((0, List("X", "X"))),
      (0, None, "Z") -> Set((1, List("Z"))),
      (0, None, "X") -> Set((1, List("X"))),
      (1, Some('b'), "X") -> Set((1, List())),
      (1, None, "Z") -> Set((2, List("Z"))),
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(2),
  )

  // PDA accepting L = { w \in {0, 1}* | w = w^R and |w| is even }
  // by final states
  def pda_even_pal_final: PDA = ???

  // PDA accepting L = { w \in {a, b}* | N_a(w) <= N_b(w) } by empty stacks
  def pda_more_bs_empty: PDA = ???

  // PDA accepting L = { a^i b^j c^k | i, j, k >= 0 and (i = j or j = k) } by
  // final states
  def pda_abc_ij_jk_final: PDA = ???

  // PDA accepting L = { a^i b^j | (i = 2n + 1 and j = 3n + 2) for n >= 0 } by
  // empty stacks
  def pda_a2n1_b3n2_empty: PDA = ???

  // PDA accepting L = { a^i b^j c^k | i, j, k >= 0 and j = i + 2k } by final
  // states
  def pda_abc_j_i2k_final: PDA = ???

  // PDA accepting L = { w \in { '(', ')', '{', '}', '[', ']' }* | w is
  // well-formed and satisfies the order: '()' <= '{}' <= '[]' } by empty stacks
  def pda_ord_brace_empty: PDA = ???

  // PDA accepting L = { a_1 a_2 ... a_2n \in {a, b}* | n >= 1 and a_i = a_{n+i}
  // for some 1 <= i <= n } by final states
  def pda_eq_pair_final: PDA = ???

  // PDA accepting L = { x$y | x, y \in {0, 1}* and N(x) + 1 = N(y^R) }
  // where N(w) is the natural number represented by w in binary
  def pda_inc_empty: PDA = ???

}
