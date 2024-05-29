package kyoTest2

import izumi.reflect.Tag as ITag
import kyo2.*
import org.scalatest.NonImplicitAssertions
import org.scalatest.freespec.AsyncFreeSpec

class tagsTest extends AsyncFreeSpec with NonImplicitAssertions:

    inline def test[T1, T2](using k1: Tag[T1], i1: ITag[T1], k2: Tag[T2], i2: ITag[T2]): Unit =
        "T1 <:< T2" in {
            val kresult = k1 <:< k2
            val iresult = i1 <:< i2
            assert(
                kresult == iresult,
                s"Tag[T1] <:< Tag[T2] is $kresult but ITag[T1] <:< ITag[T2] is $iresult"
            )
        }
        "T2 <:< T1" in {
            val kresult = k2 <:< k1
            val iresult = i2 <:< i1
            assert(
                kresult == iresult,
                s"Tag[T2] <:< Tag[T1] is $kresult but ITag[T2] <:< ITag[T1] is $iresult"
            )
        }
        "T2 =:= T1" in {
            val kresult = k2 =:= k1
            val iresult = i2 =:= i1
            assert(
                kresult == iresult,
                s"Tag[T2] =:= Tag[T1] is $kresult but ITag[T2] =:= ITag[T1] is $iresult"
            )
        }
        "T2 =!= T1" in {
            val kresult = k2 =!= k1
            val iresult = !(i2 =:= i1)
            assert(
                kresult == iresult,
                s"Tag[T2] =!= Tag[T1] is $kresult but ITag[T2] =!= ITag[T1] is $iresult"
            )
        }
        ()
    end test

    "mixed variances" - {
        class Test[+T, -U]
        test[Test[String, Any], Test[Any, String]]
    }

    "without variance" - {
        "equal tags" - {
            class Test[T]
            test[Test[Int], Test[Int]]
        }

        "not equal tags (different type parameters)" - {
            class Test[T]
            test[Test[String], Test[Int]]
        }

        "not equal tags (different classes)" - {
            class Test1[T]
            class Test2[T]
            test[Test1[Int], Test2[Int]]
        }

        "not subtype (invariant)" - {
            class Test[T]
            test[Test[String], Test[Any]]
        }

        "not supertype (invariant)" - {
            class Test[T]
            test[Test[Any], Test[String]]
        }

        "not subtype or supertype (unrelated types)" - {
            class Test[T]
            test[Test[String], Test[Int]]
        }
    }

    "with variance" - {
        "contravariance" - {
            class Test[-T]
            test[Test[String], Test[Any]]
        }

        "covariance" - {
            class Test[+T]
            test[Test[String], Test[Any]]
        }

        "nested contravariance" - {
            class Test[-T]
            class NestedTest[-U]
            test[Test[NestedTest[Any]], Test[NestedTest[String]]]
        }

        "nested covariance" - {
            class Test[+T]
            class NestedTest[+U]
            test[Test[NestedTest[String]], Test[NestedTest[Any]]]
        }

        "mixed variances" - {
            class Test[+T, -U]
            test[Test[String, Any], Test[Any, String]]
        }

        "invariant type parameter" - {
            class Test[T, +U]
            test[Test[String, String], Test[String, Any]]
        }

        "complex variance scenario" - {
            class Test[-T, +U]
            class NestedTest[+V, -W]
            test[Test[NestedTest[String, Any], Any], Test[NestedTest[Any, String], String]]
        }
    }
end tagsTest
