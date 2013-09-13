object TaggedInterpreter {
  trait Expr
  case class Lit(s: String) extends Expr
  case class Pair(left: Expr, right: Expr) extends Expr
  case class Fst(e: Expr) extends Expr
  case class Snd(e: Expr) extends Expr

  trait Value
  case class VString(str: String) extends Value
  case class VPair(pair: (Value, Value)) extends Value

  def eval(expr: Expr): Option[Value]= expr match {
    case Lit(s) => Some(VString(s))
    case Pair(left, right) =>
      for (e1 <- eval(left); e2 <- eval(right)) yield VPair(e1, e2)
    case Fst(e) => eval(e) collect { case vp: VPair => vp.pair._1 }
    case Snd(e) => eval(e) collect { case vp: VPair => vp.pair._2 }
  }
}

object TaglessInterpreter {
  trait Expr[A]
  case class Lit(s: String) extends Expr[String]
  case class Pair[A, B](left: Expr[A], right: Expr[B]) extends Expr[Tuple2[A, B]]
  case class Fst[A, B](e: Expr[Tuple2[A, B]]) extends Expr[A]
  case class Snd[A, B](e: Expr[Tuple2[A, B]]) extends Expr[B]

  def eval[A](expr: Expr[A]): A = expr match {
    case Lit(s) => s
    case Pair(left, right) => (eval(left), eval(right))
    case Fst(e) => eval(e)._1
    case Snd(e) => eval(e)._2
  }
}

// Abbreviations for less typing...
val tag = TaggedInterpreter
val notag = TaglessInterpreter

// It's clearly wrong to try to take the "first" of a "literal"!
val fstRes1 = tag.Fst(tag.Lit("hello")) // compiles! Will fail (with None) during tag.eval!
val fstRes2 = notag.Fst(notag.Lit("hello")) // compile error! Exactly what we want :)
