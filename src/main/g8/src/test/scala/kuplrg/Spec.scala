package kuplrg

import scala.util.parsing.combinator.*

import Implementation.*

class Spec extends SpecBase with RegexParsers with PackratParsers {

  // Number of trials for `mustEqual`
  private val TRIAL = 512

  // (Problem #1) L = { w \in {a, b, c}* | N_a(w) = N_c(w) }
  {
    val lang: Lang = Lang(
      "abc".toSet,
      w => w.count(_ == 'a') == w.count(_ == 'c'),
    )
    check(
      pda_eq_a_c_empty.mustValid.langByEmptyStacks.mustEqual(lang, TRIAL),
      weight = 10,
    )
  }

  // (Problem #2) L = { w \in {a, b}* | N_a(w) >= N_b(w) + 2 }
  {
    val lang: Lang = Lang(
      "ab".toSet,
      w => w.count(_ == 'a') >= w.count(_ == 'b') + 2,
    )
    check(
      pda_excess_a_final.mustValid.langByFinalStates.mustEqual(lang, TRIAL),
      weight = 10,
    )
  }

  // (Problem #3) L = { a^i b^j c^k | i + j = 2k }
  {
    val lang: Lang = Lang(
      "abc".toSet,
      w => {
        val i = w.count(_ == 'a')
        val j = w.count(_ == 'b')
        val k = w.count(_ == 'c')
        "a*b*c*".r.matches(w) && i + j == 2 * k
      },
    )
    check(
      pda_ab_2c_empty.mustValid.langByEmptyStacks.mustEqual(lang, TRIAL),
      weight = 10,
    )
  }

  // (Problem #4) L = { x # y | |x| = |y| and y differs from x^R at
  // exactly one position }
  {
    val lang: Lang = Lang(
      "ab#".toSet,
      w => {
        w.indexOf('#') match {
          case -1 => false
          case i =>
            val (x, y) = (w.take(i), w.drop(i + 1))
            !y.contains('#') && x.length == y.length && {
              x.reverse.zip(y).count { case (a, b) => a != b } == 1
            }
        }
      },
    )
    check(
      pda_hamming_one_final.mustValid.langByFinalStates.mustEqual(lang, TRIAL),
      weight = 10,
    )
  }

  // (Problem #5) L = { u u^R v v^R | u, v \in {a, b}+ }
  {
    val lang: Lang = Lang(
      "ab".toSet,
      w => {
        w.length >= 4 && w.length % 2 == 0 && {
          (2 to w.length - 2 by 2).exists { split =>
            val first = w.take(split)
            val second = w.drop(split)
            val u = first.take(first.length / 2)
            val v = second.take(second.length / 2)
            first == u + u.reverse && second == v + v.reverse
          }
        }
      },
    )
    check(
      pda_pal_concat_empty.mustValid.langByEmptyStacks.mustEqual(lang, TRIAL),
      weight = 15,
    )
  }

  // (Problem #6) L = { w \in {x, +, (, )}* | w is a valid arithmetic expression
  // with an odd number of x's }
  {
    val lang: Lang = Lang(
      "x+()".toSet, {
        type P = PackratParser[Unit]
        def unit[T](p: Parser[T]): P = p ^^^ ()
        lazy val expr: P = unit(prim ~ rep("+" ~ prim))
        lazy val prim: P = unit("x++" | "x" | ("(" ~ expr ~ ")"))
        w => parseAll(expr, w).successful && w.count(_ == 'x') % 2 == 1
      },
    )
    check(
      pda_expr_x_odd_final.mustValid.langByFinalStates.mustEqual(lang, TRIAL),
      weight = 15,
    )
  }

  // (Problem #7) L = { w \in {a, b}* | w is a concatenation of
  // palindromes, each of length >= 2 }
  {
    val lang: Lang = Lang(
      "ab".toSet,
      w => {
        val n = w.length
        val canFactor = Array.fill(n + 1)(false)
        canFactor(0) = true
        for (i <- 1 to n) {
          for (j <- 0 until i if canFactor(j) && !canFactor(i)) {
            val sub = w.substring(j, i)
            if (sub.length >= 2 && sub == sub.reverse) {
              canFactor(i) = true
            }
          }
        }
        canFactor(n)
      },
    )
    check(
      pda_pal_factor_empty.mustValid.langByEmptyStacks.mustEqual(lang, TRIAL),
      weight = 15,
    )
  }

  // (Problem #8) L = { x $ y | x, y \in {0, 1}* and
  // N(y^R) = 3 * N(x) } (binary, MSB-first)
  {
    val lang: Lang = Lang(
      "01$".toSet,
      w => {
        w.indexOf('$') match {
          case -1 => false
          case i =>
            val (x, y) = (w.take(i), w.drop(i + 1))
            !y.contains('$') &&
            x.forall(c => c == '0' || c == '1') &&
            y.forall(c => c == '0' || c == '1') && {
              val nx = if (x.isEmpty) BigInt(0) else BigInt(x, 2)
              val ny = if (y.isEmpty) BigInt(0) else BigInt(y.reverse, 2)
              ny == BigInt(3) * nx
            }
        }
      },
    )
    check(
      pda_triple_final.mustValid.langByFinalStates.mustEqual(lang, TRIAL),
      weight = 15,
    )
  }

  /* Write your own tests */
}
