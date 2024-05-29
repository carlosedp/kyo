package kyo2

import scala.annotation.switch
import scala.annotation.tailrec
import scala.collection.immutable
import scala.quoted.*

opaque type Tag[T] = String

object Tag:

    import internal.*

    inline given apply[T]: Tag[T] = ${ tagImpl[T] }

    extension [T](t1: Tag[T])

        def show: String =
            val decoded = t1.drop(2).takeWhile(_ != ';')
            fromCompact.getOrElse(decoded, decoded)

        infix def <:<[U](t2: Tag[U]): Boolean =
            t1 =:= t2 || isSubtype(t1, t2)

        infix def =:=[U](t2: Tag[U]): Boolean =
            (t1 eq t2) || t1 == t2

        infix def =!=[U](t2: Tag[U]): Boolean =
            !(t1 =:= t2)

        infix def >:>[U](t2: Tag[U]): Boolean =
            t2 <:< t1

        def erased: Tag[Any] = t1.asInstanceOf[Tag[Any]]

    end extension

    private object internal:

        ////////////////////
        // Sub-type check //
        ////////////////////

        opaque type Position = Long

        object Position:
            def apply(subIdx: Int, superIdx: Int): Position =
                (subIdx.toLong << 32) | superIdx.toLong

            extension (pos: Position)
                def subIdx: Int   = (pos >> 32).toInt
                def superIdx: Int = pos.toInt
        end Position

        def isSubtype(subTag: String, superTag: String): Boolean =
            checkType(subTag, superTag, 0, 0, false) >= 0

        def checkType(subTag: String, superTag: String, subIdx: Int, superIdx: Int, equality: Boolean): Int =
            val subKind   = subTag.charAt(subIdx)
            val superKind = superTag.charAt(superIdx)
            if subKind == 'u' then
                checkMultiple(subTag, superTag, subIdx + 1, superIdx, checkType(_, _, _, _, equality))
            else if superKind == 'i' then
                checkMultiple(superTag, subTag, superIdx + 1, subIdx, checkType(_, _, _, _, equality))
            else if subKind == 'i' then
                checkMultiple(
                    subTag,
                    superTag,
                    subIdx + 1,
                    superIdx,
                    (s1, s2, i1, i2) => if checkType(s1, s2, i1, i2, equality) >= 0 then i1 else -1
                )
            else if subKind == 't' && superKind == 't' then
                checkSingle(subTag, superTag, subIdx + 1, superIdx + 1, equality)
            else -1
            end if
        end checkType

        def checkMultiple(subTag: String, superTag: String, subIdx: Int, superIdx: Int, checkFn: (String, String, Int, Int) => Int): Int =
            val subSize = decodeInt(subTag.charAt(subIdx))
            @tailrec def loop(i: Int, idx: Int): Int =
                if i < subSize then
                    val nextIdx = checkFn(subTag, superTag, idx, superIdx)
                    if nextIdx >= 0 then loop(i + 1, nextIdx)
                    else -1
                else idx
            loop(0, subIdx + 1)
        end checkMultiple

        def checkSingle(subTag: String, superTag: String, subIdx: Int, superIdx: Int, equality: Boolean): Int =
            val subTotalBasesSize   = decodeInt(subTag.charAt(subIdx))
            val subParamsSize       = decodeInt(subTag.charAt(subIdx + 1))
            val superTotalBasesSize = decodeInt(superTag.charAt(superIdx))
            val superParamsSize     = decodeInt(superTag.charAt(superIdx + 1))
            val subBasesEnd         = subIdx + subTotalBasesSize + 2
            val superBasesEnd       = superIdx + superTotalBasesSize + 2
            @tailrec def checkBases(subIdx: Int, superIdx: Int): Int =
                if subIdx >= subBasesEnd then
                    -1
                else
                    val subHash   = subTag.charAt(subIdx)
                    val subSize   = decodeInt(subTag.charAt(subIdx + 1))
                    val superHash = superTag.charAt(superIdx)
                    if subHash == superHash && subTag.regionMatches(subIdx + 2, superTag, superIdx + 2, subSize) then
                        checkParams(subTag, superTag, subBasesEnd, superBasesEnd, subParamsSize, superParamsSize, equality)
                    else if !equality then
                        checkBases(subIdx + subSize + 2, superIdx)
                    else
                        -1
                    end if
            checkBases(subIdx + 2, superIdx + 2)
        end checkSingle

        def checkParams(
            subTag: String,
            superTag: String,
            subIdx: Int,
            superIdx: Int,
            subParamsSize: Int,
            superParamsSize: Int,
            equality: Boolean
        ): Int =
            @tailrec def loop(i: Int, j: Int, idx1: Int, idx2: Int): Int =
                if i >= subParamsSize || j >= superParamsSize then
                    if i >= subParamsSize && j >= superParamsSize then idx1 else -1
                else
                    subTag.charAt(idx1) match
                        case '+' =>
                            val nextIdx = checkType(subTag, superTag, idx1 + 1, idx2 + 1, equality)
                            if nextIdx >= 0 then loop(i + 1, j + 1, nextIdx, nextIdx) else -1
                        case '=' =>
                            val nextIdx = checkType(subTag, superTag, idx1 + 1, idx2 + 1, true)
                            if nextIdx >= 0 then loop(i + 1, j + 1, nextIdx, nextIdx) else -1
                        case '-' =>
                            val nextIdx = checkType(superTag, subTag, idx2 + 1, idx1 + 1, equality)
                            if nextIdx >= 0 then loop(i + 1, j + 1, nextIdx, nextIdx) else -1
                    end match
            loop(0, 0, subIdx, superIdx)
        end checkParams

        ///////////////////
        // Macro methods //
        ///////////////////

        def tagImpl[T: Type](using Quotes): Expr[Tag[T]] =
            import quotes.reflect.*
            encodeType(TypeRepr.of[T])
        end tagImpl

        def encodeType(using Quotes)(tpe: quotes.reflect.TypeRepr): Expr[String] =
            import quotes.reflect.*

            tpe.dealias match
                case tpe @ AndType(_, _) =>
                    val types =
                        flatten(tpe) {
                            case AndType(a, b) => List(a, b)
                        }.map(encodeType)
                    concat(Expr(s"i${encodeInt(types.size)}") :: types)
                case OrType(_, _) =>
                    val types =
                        flatten(tpe) {
                            case OrType(a, b) => List(a, b)
                        }.map(encodeType)
                    concat(Expr(s"u${encodeInt(types.size)}") :: types)
                case tpe if tpe.typeSymbol.isClassDef =>
                    val bases          = encodeBases(tpe)
                    val totalBasesSize = bases.map(_.size).sum

                    val variances  = encodeVariances(tpe)
                    val paramTypes = tpe.typeArgs.map(encodeType)
                    require(variances.size == paramTypes.size)
                    val params = variances.zip(paramTypes).flatMap((v, t) => Expr(v) :: t :: Nil)

                    val header = Expr(s"t${encodeInt(totalBasesSize)}${encodeInt(paramTypes.size)}")
                    concat(header :: bases.map(Expr(_)) ::: params)
                case _ =>
                    tpe.asType match
                        case '[tpe] =>
                            Expr.summon[Tag[tpe]] match
                                case None =>
                                    report.errorAndAbort(s"Please provide an implicit kyo.Tag[${tpe.show}] parameter.")
                                case Some(value) =>
                                    value
            end match
        end encodeType

        def encodeBases(using Quotes)(tpe: quotes.reflect.TypeRepr): List[String] =
            tpe.baseClasses.map { sym =>
                val name = toCompact.getOrElse(sym.fullName, sym.fullName)
                val size = encodeInt(name.length())
                val hash = encodeHash(name.hashCode())
                s"$hash$size$name"
            }

        def encodeVariances(using Quotes)(tpe: quotes.reflect.TypeRepr): List[String] =
            import quotes.reflect.*
            tpe.typeSymbol.typeMembers.flatMap { v =>
                if !v.isTypeParam then None
                else if v.flags.is(Flags.Contravariant) then Some("-")
                else if v.flags.is(Flags.Covariant) then Some("+")
                else Some("=")
            }
        end encodeVariances

        def flatten(using
            Quotes
        )(tpe: quotes.reflect.TypeRepr)(pf: PartialFunction[quotes.reflect.TypeRepr, List[quotes.reflect.TypeRepr]]) =
            import quotes.reflect.*
            def loop(tpe: TypeRepr): List[TypeRepr] =
                tpe match
                    case pf(l) => l.flatMap(loop)
                    case tpe   => List(tpe)

            loop(tpe)
        end flatten

        // Encodes ints using latin1Chars
        val latin1Chars = ('\u0000' to '\u00FF').toArray

        def encodeInt(using Quotes)(i: Int): Char =
            import quotes.reflect.*
            if i >= latin1Chars.length || i < 0 then
                report.errorAndAbort(s"Encoded tag 'Int($i)' exceeds supported limit: " + latin1Chars.length)
            latin1Chars(i)
        end encodeInt

        def decodeInt(c: Char): Int =
            latin1Chars.indexOf(c)

        def encodeHash(using Quotes)(hash: Int): Char =
            encodeInt(Math.abs(hash) % latin1Chars.length)

        def concat(l: List[Expr[String]])(using Quotes): Expr[String] =
            def loop(l: List[Expr[String]], acc: String, exprs: List[Expr[String]]): Expr[String] =
                l match
                    case Nil =>
                        (Expr(acc) :: exprs).reverse match
                            case Nil => Expr("")
                            case exprs =>
                                exprs.filter {
                                    case Expr("") => false
                                    case _        => true
                                }.reduce((a, b) => '{ $a + $b })
                    case Expr(s: String) :: next =>
                        loop(next, acc + s, exprs)
                    case head :: next =>
                        loop(next, "", head :: Expr(acc) :: exprs)
            loop(l, "", Nil)
        end concat

        val toCompact = Map[String, String](
            // "java.lang.Object"                              -> "0",
            // "scala.Matchable"                               -> "1",
            // "scala.Any"                                     -> "2",
            // "scala.AnyVal"                                  -> "3",
            // "java.lang.String"                              -> "4",
            // "scala.Int"                                     -> "5",
            // "scala.Long"                                    -> "6",
            // "scala.Float"                                   -> "7",
            // "scala.Double"                                  -> "8",
            // "scala.Boolean"                                 -> "9",
            // "scala.Unit"                                    -> "a",
            // "scala.Option"                                  -> "b",
            // "scala.Some"                                    -> "c",
            // "scala.None"                                    -> "d",
            // "scala.Left"                                    -> "e",
            // "scala.Right"                                   -> "f",
            // "scala.Tuple2"                                  -> "g",
            // "scala.collection.immutable.List"               -> "h",
            // "scala.collection.immutable.Nil"                -> "i",
            // "scala.collection.immutable.Map"                -> "j",
            // "scala.Nothing"                                 -> "k",
            // "java.lang.CharSequence"                        -> "l",
            // "java.lang.Comparable"                          -> "m",
            // "java.io.Serializable"                          -> "n",
            // "scala.Product"                                 -> "o",
            // "scala.Equals"                                  -> "p",
            // "kyo.core$.Effect"                              -> "q",
            // "kyo.fibersInternal$.FiberGets"                 -> "r",
            // "kyo.Streams"                                   -> "s",
            // "kyo.aborts$package$.Aborts$.internal$.DoAbort" -> "t",
            // "kyo.zios$package$.ZIOs$.internal$.Tasks"       -> "u",
            // "kyo.IOs"                                       -> "v",
            // "scala.Char"                                    -> "x",
            // "java.lang.Throwable"                           -> "y",
            // "java.lang.Exception"                           -> "z"
        )

        val fromCompact = toCompact.map(_.swap).toMap
    end internal

end Tag
