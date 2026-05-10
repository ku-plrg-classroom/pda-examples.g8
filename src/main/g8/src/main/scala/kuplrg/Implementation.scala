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
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(2),
  )(
    (0, 'a', "Z") -> (0, List("X", "Z")),
    (0, 'a', "X") -> (0, List("X", "X")),
    (0, EPS, "Z") -> (1, List("Z")),
    (0, EPS, "X") -> (1, List("X")),
    (1, 'b', "X") -> (1, List()),
    (1, EPS, "Z") -> (2, List("Z")),
  )

  // (Problem #1) PDA for L = { w \in {a, b, c}* | N_a(w) = N_c(w) } by
  // empty stacks
  def pda_eq_a_c_empty: PDA = ???

  // (Problem #2) PDA for L = { w \in {a, b}* | N_a(w) >= N_b(w) + 2 } by
  // final states
  def pda_excess_a_final: PDA = ???

  // (Problem #3) PDA for L = { a^i b^j c^k | i, j, k >= 0 and i + j = 2k }
  // by empty stacks
  def pda_ab_2c_empty: PDA = ???

  // (Problem #4) PDA for L = { x # y | x, y \in {a, b}* and |x| = |y|
  // and y differs from x^R at exactly one position } by final states
  def pda_hamming_one_final: PDA = ???

  // (Problem #5) PDA for L = { u u^R v v^R | u, v \in {a, b}+ } by
  // empty stacks
  def pda_pal_concat_empty: PDA = ???

  // (Problem #6) PDA for L = { w \in {x, +, (, )}* | w is a valid
  // arithmetic expression with an odd number of x's } by final states
  def pda_expr_x_odd_final: PDA = ???

  // (Problem #7) PDA for L = { w \in {a, b}* | w is a concatenation of
  // palindromes, each of length >= 2 } by empty stacks
  def pda_pal_factor_empty: PDA = ???

  // (Problem #8) PDA for L = { x $ y | x, y \in {0, 1}* and
  // N(y^R) = 3 * N(x) } by final states, where N(w) is the natural
  // number represented by w in binary
  def pda_triple_final: PDA = ???

}
