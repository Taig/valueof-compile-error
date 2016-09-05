import shapeless.Witness

trait Rule {
    type T
}

object Rule {
    type Aux[T0] = Rule { type T = T0 }
}

// Type class stub
trait Validation[R <: Rule]

// The subtyping here seems to play a decisive role. If it's removed and
// all its occurences are replaced with `Rule`, the error disappears.
trait Applyable extends Rule

// Combination of two rules
class Operator extends Rule {
    type Left <: Rule

    type Right <: Rule.Aux[Left#T]

    override final type T = Left#T
}

object Operator {
    implicit def validation[O <: Operator](
        implicit
        l: Validation[O#Left],
        r: Validation[O#Right]
    ): Validation[O] = ???
}

class ~>[L <: Rule, R <: Rule.Aux[L#T]] extends Operator {
    final type Left = L

    final type Right = R
}

object trim extends Applyable {
    override type T = String
}
object nonEmpty extends Applyable {
    override type T = String
}

object required extends ( trim.type ~> nonEmpty.type )

object WitnessExample {
    implicit def validation[A <: Applyable](
        implicit
        w: Witness.Aux[A]
    ): Validation[A] = ???

    implicitly[Validation[trim.type]]
    implicitly[Validation[nonEmpty.type]]
    implicitly[Validation[required.type]]
}

object ValueOfExample {
    implicit def validation[A <: Applyable](
        implicit
        w: ValueOf[A]
    ): Validation[A] = ???

    implicitly[Validation[trim.type]]
    implicitly[Validation[nonEmpty.type]]

    // Compiler gives up here:
    // implicitly[Validation[required.type]]
}