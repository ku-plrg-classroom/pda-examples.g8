package kuplrg

import Implementation.*

import scala.util.parsing.combinator.*

class Spec extends SpecBase with RegexParsers with PackratParsers {

  // Check if there is no epsilon transition increasing the stack in the PDA
  def checkValid(pda: PDA): PDA =
    val incEpsTrans = pda.incEpsTrans
    if (incEpsTrans.nonEmpty)
      val str = (for {
        ((q, a, x), (p, seq)) <- incEpsTrans.sortBy(_._1)
        aStr = a.fold("ε")(_.toString)
        str = s"\n         * $q -> $p - $aStr [$x -> ${seq.mkString(" ")}]"
      } yield str).mkString
      error("The PDA has epsilon transitions increasing the stack: " + str)
    pda

  // L = { w w^R | w \in {0, 1}* }
  {
    val lang: Lang = Lang(
      "01".toSet,
      w => w.length % 2 == 0 && w == w.reverse,
    )
    check(
      checkValid(pda_even_pal_final).langByFinalStates.mustEqual(lang, 100),
      weight = 10,
    )
  }

  // L = { w \in {a, b}* | N_a(w) > N_b(w) }
  {
    val lang: Lang = Lang(
      "ab".toSet,
      w => w.count(_ == 'a') > w.count(_ == 'b'),
    )
    check(
      checkValid(pda_more_as_empty).langByEmptyStacks.mustEqual(lang, 100),
      weight = 10,
    )
  }

  // L = { a^i b^j c^k | i, j, k >= 0 and (i = j or i = k) }
  {
    val lang: Lang = Lang(
      "abc".toSet,
      w =>
        "a*b*c*".r.matches(w) && (
          w.count(_ == 'a') == w.count(_ == 'b') ||
          w.count(_ == 'a') == w.count(_ == 'c')
        ),
    )
    check(
      checkValid(pda_abc_ij_ik_final).langByFinalStates.mustEqual(lang, 1_000),
      weight = 10,
    )
  }

  // L = { a^i b^j | (i = 2n + 1 and j = 3n + 1) for n >= 0 }
  {
    val lang: Lang = Lang(
      "ab".toSet,
      w => {
        val a = w.count(_ == 'a')
        val b = w.count(_ == 'b')
        "a*b*".r.matches(w) && a % 2 == 1 && b == (a - 1) / 2 * 3 + 1
      },
    )
    check(
      checkValid(pda_a2n1_b3n1_empty).langByEmptyStacks.mustEqual(lang, 100),
      weight = 10,
    )
  }

  // L = { a^i b^j c^k | i, j, k >= 0 and j = i + 2k }
  {
    def lang: Lang = Lang(
      "abc".toSet,
      w => {
        val a = w.count(_ == 'a')
        val b = w.count(_ == 'b')
        val c = w.count(_ == 'c')
        "a*b*c*".r.matches(w) && b == a + 2 * c
      },
    )
    check(
      checkValid(pda_abc_j_i2k_final).langByFinalStates.mustEqual(lang, 1_000),
      weight = 15,
    )
  }

  // L = { w \in { '(', ')', '{', '}', '[', ']' }* | w is well-formed and
  // satisfies the order: '()' <= '{}' <= '[]' }
  {
    val lang: Lang = Lang(
      "(){}[]".toSet, {
        type P = PackratParser[Unit]
        def unit[T](p: Parser[T]): P = p ^^^ ()
        lazy val round: P = unit(("(" ~ round ~ ")" | "") ~ opt(round))
        lazy val curly: P = unit(("{" ~ curly ~ "}" | round) ~ opt(curly))
        lazy val square: P = unit(("[" ~ square ~ "]" | curly) ~ opt(square))
        w => parseAll(square, w).successful
      },
    )
    check(
      checkValid(pda_ord_brace_empty).langByEmptyStacks.mustEqual(lang, 50_000),
      weight = 15,
    )
  }

  // L = { w \in { '0', '1', '+', '*' }* | w is an arithmetic expression
  // evaluated to an even number }
  {
    val lang = Lang(
      "01+*".toSet, {
        type P = PackratParser[Int]
        lazy val base: P = "0" ^^^ 0 | "1" ^^^ 1 | "(" ~> add <~ ")"
        lazy val mul: P = base ~ rep("*" ~> base) ^^ {
          case x ~ xs => xs.foldLeft(x)(_ * _)
        }
        lazy val add: P = mul ~ rep("+" ~> mul) ^^ {
          case x ~ xs => xs.foldLeft(x)(_ + _ % 2)
        }
        w => parseAll(add, w).map(_ % 2 == 0).getOrElse(false)
      },
    )
    check(
      checkValid(pda_ae_even_final).langByFinalStates.mustEqual(lang, 5_000),
      weight = 15,
    )
  }

  // L = { a_1 a_2 ... a_2n \in {a, b}* | n >= 1 and a_i = a_{n+i} for some
  // 1 <= i <= n }
  {
    val lang = Lang(
      "ab".toSet,
      w => {
        w.length % 2 == 0 &&
        (w.take(w.length / 2) zip w.drop(w.length / 2)).exists(_ == _)
      },
    )
    check(
      checkValid(pda_eq_pair_empty).langByEmptyStacks.mustEqual(lang, 100),
      weight = 15,
    )
  }

  /* Write your own tests */
}
