package cats

import simulacrum.typeclass

/**
 * A type class abstracting over types that give rise to two independent [[cats.Foldable]]s.
 */
@typeclass trait Bifoldable[F[_, _]] { self =>

  /** Collapse the structure with a left-associative functions (first concatenate, second repeat)
   * {{{
   * scala> import cats.Bifoldable, cats.implicits._
   * scala> Bifoldable[Tuple2].bifoldLeft[String, Int, String](("hello ", 3), "aloha ")((s, t) => s"$t$s", (s, i) => s * i)
   * res0: String = "hello aloha hello aloha hello aloha "
   *
   * It can also be used to compute for example a hashcode:
   * scala> import cats.data.EitherT
   * scala> val list = List(Left(1), Right('A'), Left(2), Right('B'), Left(4))
   * scala> val eitherT = EitherT(list)
   * scala> type EitherTList[A, B] = EitherT[List, A, B]
   * defined type alias EitherTList
   * scala> Bifoldable[EitherTList].bifoldLeft[Int, Char, Int](eitherT, 3)((acc, l) => acc ^ (l * 17), (acc, r) => acc ^ ((r - 'A' + 10) * 37))
   * res0: Int = 145
   * scala> ((((3 ^ 17) ^ 10 * 37) ^ 2 * 17) ^ 11 * 37) ^ 4 * 17
   * res1: Int = 145
   * }}}
   */
  def bifoldLeft[A, B, C](fab: F[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C

  /** Collapse the structure with a right-associative function
   *
   * Example:
   * {{{
   * scala> import cats.data.EitherT, scala.math.pow, scala.math.sqrt
   * scala> val eitherT: EitherT[Stream, Double, Int] = EitherT(Stream(Right(4), Left(sqrt(3)), Left(2d)))
   * scala> type EitherTStream[A, B] = EitherT[Stream, A, B]
   * scala> Bifoldable[EitherTStream].bifoldRight[Double, Int, Double](eitherT, Eval.now(1d))
   *    | ((l, acc) => acc.map(v => pow(l, v)), (r, acc) => acc.map(v => pow(v, r))).value
   * res0: Double = 80.99999999999996
   * scala> pow(pow(sqrt(3), pow(2d, 1d)), 4)
   * res1: Double = 80.99999999999996
   * }}}
   * */
  def bifoldRight[A, B, C](fab: F[A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C]

  /** Collapse the structure by mapping each element to an element of a type that has a [[cats.Monoid]]
   *  Example:
   *  {{{
   *  scala> import cats.Bifoldable, cats.data.EitherT, cats.data.Validated, cats.implicits._
   *  scala> val eitherT = EitherT(List(Right(4), Left("error"), Left("x")))
   *  eitherT: cats.data.EitherT[List,String,Int] = EitherT(List(Right(4), Left(error), Left(x)))
   *  scala> Bifoldable[EitherT[List, *, *]].bifoldMap[String, Int, Validated[String, Int]](eitherT)(l => l.invalid[Int], r => r.valid[String])
   *  res0: cats.data.Validated[String,Int] = Invalid(errorx)
   *  scala> Bifoldable[EitherT[List, *, *]].bifoldMap(EitherT(List[Either[String, Int]](Right(3), Right(5))))
   *   | (l => l.invalid[Int], r => r.valid[String])
   *  res1: cats.data.Validated[String,Int] = Valid(8)
   *  }}}
   */
  def bifoldMap[A, B, C](fab: F[A, B])(f: A => C, g: B => C)(implicit C: Monoid[C]): C =
    bifoldLeft(fab, C.empty)(
      (c: C, a: A) => C.combine(c, f(a)),
      (c: C, b: B) => C.combine(c, g(b))
    )

  /** Examples:
   * {{{
   * scala> import cats.Bifoldable, cats.implicits._
   *
   * `Bifoldable[Tuple2].compose[Tuple2]` can collapse them by their position to a single `Tuple2`:
   *
   * scala> Bifoldable[Tuple2].compose[Tuple2].bifold[String, Int]((("Hello ", 2), ("world", 3)))
   * res0: (String, Int) = (Hello world,5)
   *
   * `Bifoldable[Either].compose[Either]` can extract the inner `Either`:
   *
   * scala> Bifoldable[Either].compose[Either].bifold[String, Int](Right[Either[String, Int], Either[String, Int]](Left("Hello")))
   * res1: (String, Int) = (Hello,0)
   *
   * `Bifoldable[Either].compose[Tuple2]` can also extract the inner `Tuple2`.
   *
   * scala> Bifoldable[Either].compose[Tuple2].bifold[String, Int](Right[(String, Int), (String, Int)](("", 3)))
   * res2: (String, Int) = ("",3)
   * scala> Bifoldable[Either].compose[Tuple2].bifold[String, Int](Left[(String, Int), (String, Int)](("", 3)))
   * res3: (String, Int) = ("",3)
   *
   * `Bifoldable[Tuple2].compose[Either]` can handle a tuple of `Either`s
   *
   * scala> Bifoldable[Tuple2].compose[Either].bifold[String, Int]((Left("Hello"), Right(2)))
   * res4: (String, Int) = (Hello,2)
   * scala> Bifoldable[Tuple2].compose[Either].bifold[String, Int]((Right(3), Right(2)))
   * res5: (String, Int) = ("",5)
   * }}}
   */
  def compose[G[_, _]](implicit ev: Bifoldable[G]): Bifoldable[λ[(α, β) => F[G[α, β], G[α, β]]]] =
    new ComposedBifoldable[F, G] {
      val F = self
      val G = ev
    }

  /** Collapse the structure using the corresponding monoids for `A` and `B` to a tuple.
   * Example:
   * {{{
   * scala> import cats.Bifoldable, cats.data.EitherT, cats.implicits._
   * scala> Bifoldable[EitherT[List, *, *]].bifold[String, Int](EitherT(List(Right(3), Left("Hello "), Left("world"), Right(5))))
   * res0: (String, Int) = (Hello world,8)
   * }}}
   */
  def bifold[A, B](fab: F[A, B])(implicit A: Monoid[A], B: Monoid[B]): (A, B) = {
    import cats.instances.tuple._
    bifoldMap(fab)((_, B.empty), (A.empty, _))
  }
}

private[cats] trait ComposedBifoldable[F[_, _], G[_, _]] extends Bifoldable[λ[(α, β) => F[G[α, β], G[α, β]]]] {
  implicit def F: Bifoldable[F]
  implicit def G: Bifoldable[G]

  override def bifoldLeft[A, B, C](fab: F[G[A, B], G[A, B]], c: C)(f: (C, A) => C, g: (C, B) => C): C =
    F.bifoldLeft(fab, c)(
      (c: C, gab: G[A, B]) => G.bifoldLeft(gab, c)(f, g),
      (c: C, gab: G[A, B]) => G.bifoldLeft(gab, c)(f, g)
    )

  override def bifoldRight[A, B, C](fab: F[G[A, B], G[A, B]], c: Eval[C])(f: (A, Eval[C]) => Eval[C],
                                                                          g: (B, Eval[C]) => Eval[C]): Eval[C] =
    F.bifoldRight(fab, c)(
      (gab: G[A, B], c: Eval[C]) => G.bifoldRight(gab, c)(f, g),
      (gab: G[A, B], c: Eval[C]) => G.bifoldRight(gab, c)(f, g)
    )
}
