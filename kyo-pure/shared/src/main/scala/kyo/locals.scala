package kyo

import core.*
import core.internal.*

abstract class Local[T]:

    import Locals.*

    def default: T

    val get: T < Defers =
        new Defer[T, Any]:
            def apply(v: Unit, s: Safepoint[Defers], l: State) =
                get(l)

    def let[U, S](f: T)(v: U < S): U < (S & Defers) =
        def letLoop(f: T, v: U < S): U < S =
            v match
                case kyo: Suspend[MX, Any, U, S] @unchecked =>
                    new Continue[MX, Any, U, S](kyo):
                        def apply(v2: Any < S, s: Safepoint[S], l: Locals.State) =
                            letLoop(f, kyo(v2, s, l.updated(Local.this, f)))
                case _ =>
                    v
        letLoop(f, v)
    end let

    inline def use[U, S](inline f: T => U < S): U < (S & Defers) =
        new Defer[U, S]:
            def apply(v: Unit, s: Safepoint[S & Defers], l: State) =
                f(get(l))

    def update[U, S](f: T => T)(v: U < S): U < (S & Defers) =
        def updateLoop(f: T => T, v: U < S): U < S =
            v match
                case kyo: Suspend[MX, Any, U, S] @unchecked =>
                    new Continue[MX, Any, U, S](kyo):
                        def apply(v2: Any < S, s: Safepoint[S], l: Locals.State) =
                            updateLoop(f, kyo(v2, s, l.updated(Local.this, f(get(l)))))
                case _ =>
                    v
        updateLoop(f, v)
    end update

    private def get(l: Locals.State) =
        l.getOrElse(Local.this, default).asInstanceOf[T]
end Local

object Locals:

    type State = Map[Local[?], Any]

    object State:
        inline def empty: State = Map.empty

    def init[T](defaultValue: => T): Local[T] =
        new Local[T]:
            def default = defaultValue

    val save: State < Defers =
        new Defer[State, Any]:
            def apply(v: Unit, s: Safepoint[Defers], l: Locals.State) =
                l

    inline def save[U, S](inline f: State => U < S): U < (Defers & S) =
        new Defer[U, S]:
            def apply(v: Unit, s: Safepoint[Defers & S], l: Locals.State) =
                f(l)

    def restore[T, S](st: State)(f: T < S): T < (Defers & S) =
        def loop(f: T < S): T < S =
            f match
                case kyo: Suspend[MX, Any, T, S] @unchecked =>
                    new Continue[MX, Any, T, S](kyo):
                        def apply(v2: Any < S, s: Safepoint[S], l: Locals.State) =
                            loop(kyo(v2, s, l ++ st))
                case _ =>
                    f
        loop(f)
    end restore
end Locals
