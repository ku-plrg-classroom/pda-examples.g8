package kuplrg

trait Template {
  def pda_eq_a_c_empty: PDA
  def pda_excess_a_final: PDA
  def pda_ab_2c_empty: PDA
  def pda_hamming_one_final: PDA
  def pda_pal_concat_empty: PDA
  def pda_expr_x_odd_final: PDA
  def pda_pal_factor_empty: PDA
  def pda_triple_final: PDA
}
