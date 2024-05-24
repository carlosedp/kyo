package kyo

import kyo.*

type AIs = Fibers

class Schema[T]

object Schema:
    given [T]: Schema[T] = new Schema[T]

abstract class Skill[S]:
    import Skill.*

    type Input
    type Output

    case class Info(
        name: String,
        description: String
    )(using
        val input: Schema[Input],
        val output: Schema[Output]
    )

    def info: Info

    protected class InferDsl[T]:
        def apply[V1: Schema](v1: V1): T < AIs                                                             = ???
        def apply[V1: Schema, V2: Schema](v1: V1, v2: V2): T < AIs                                         = ???
        def apply[V1: Schema, V2: Schema, V3: Schema](v1: V1, v2: V2, v3: V3): T < AIs                     = ???
        def apply[V1: Schema, V2: Schema, V3: Schema, V4: Schema](v1: V1, v2: V2, v3: V3, v4: V4): T < AIs = ???
    end InferDsl

    protected def infer[T]: InferDsl[T] = new InferDsl[T]

    def apply(input: Input): Output < (S & AIs) =
        Skill.trace.update(Frame(this, input) :: _)(handle(input))

    protected def handle(input: Input): Output < (S & AIs)
end Skill

object Skill:
    case class Frame(skill: Skill[?], input: Any)
    private val trace = Locals.init(List.empty[Frame])

// object ReflectSkill extends Skill[Any]:
//     type Input  = String
//     type Output = String

//     def handle(input: String) =
//         infer[String]("is this correct?", input)
// end ReflectSkill
