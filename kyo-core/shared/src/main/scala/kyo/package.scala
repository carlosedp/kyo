import kyo.internal.Trace
import scala.util.NotGiven

package object kyo:

    export core.<

    extension [T, S](v: T < S)(using NotGiven[Any => S])

        inline def flatMap[U, S2](inline f: T => U < S2)(using Trace): U < (S & S2) =
            if isNull(v) then
                throw new NullPointerException
            kyo.core.transform(v)(f)
        end flatMap

        inline def map[U, S2](inline f: T => U < S2)(using Trace): U < (S & S2) =
            flatMap(f)

        def unit(using Trace): Unit < S =
            map(_ => ())

        def withFilter(p: T => Boolean)(using Trace): T < S =
            map(v => if !p(v) then throw new MatchError(v) else v)

        def flatten[U, S2](using ev: T => U < S2)(using Trace): U < (S & S2) =
            flatMap(ev)

        inline def andThen[U, S2](inline f: => U < S2)(using ev: T => Unit, trace: Trace): U < (S & S2) =
            flatMap(_ => f)

        def repeat(i: Int)(using ev: T => Unit, trace: Trace): Unit < S =
            if i <= 0 then () else andThen(repeat(i - 1))

        private[kyo] def isPure: Boolean =
            !v.isInstanceOf[core.internal.Kyo[?, ?]]

    end extension

    extension [T: Flat](v: T < Any)
        def pure: T =
            v match
                case kyo: kyo.core.internal.Suspend[?, ?, ?, ?] =>
                    bug.failTag(kyo.tag)
                case v =>
                    v.asInstanceOf[T]
    end extension

    def zip[T1, T2, S](v1: T1 < S, v2: T2 < S)(using Trace): (T1, T2) < S =
        v1.map(t1 => v2.map(t2 => (t1, t2)))

    def zip[T1, T2, T3, S](v1: T1 < S, v2: T2 < S, v3: T3 < S)(using Trace): (T1, T2, T3) < S =
        v1.map(t1 => v2.map(t2 => v3.map(t3 => (t1, t2, t3))))

    def zip[T1, T2, T3, T4, S](
        v1: T1 < S,
        v2: T2 < S,
        v3: T3 < S,
        v4: T4 < S
    )(using Trace): (T1, T2, T3, T4) < S =
        v1.map(t1 => v2.map(t2 => v3.map(t3 => v4.map(t4 => (t1, t2, t3, t4)))))

    inline def discard[T](v: T): Unit =
        val _ = v
        ()

    private[kyo] inline def isNull[T](v: T): Boolean =
        v.asInstanceOf[AnyRef] eq null

    private[kyo] object bug:

        case class KyoBugException(msg: String) extends Exception(msg)

        inline def failTag[T, U](
            inline actual: Tag[U]
        ): Nothing =
            bug(s"Unexpected effect '${actual.show}' found in 'pure'.")

        inline def failTag[T, U](
            inline actual: Tag.Full[T],
            inline expected: Tag.Full[U]
        ): Nothing =
            bug(s"Unexpected effect '${actual.show}' found while handling '${expected.show}'.")

        inline def checkTag[T, U](
            inline actual: Tag.Full[U],
            inline expected: Tag.Full[T]
        ): Unit =
            if actual =!= expected then
                failTag(actual, expected)

        def when(cond: Boolean)(msg: String): Unit =
            if cond then bug(msg)

        def apply(msg: String): Nothing =
            throw KyoBugException(msg + " Please open an issue 🥹 https://github.com/getkyo/kyo/issues")
    end bug
end kyo
