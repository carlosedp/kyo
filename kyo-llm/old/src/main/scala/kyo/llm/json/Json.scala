package kyo.llm.json

import kyo.*
import zio.Chunk
import zio.schema.{Schema as ZSchema, *}
import zio.schema.codec.JsonCodec

trait Json[T]:
    def schema: Schema
    def zSchema: ZSchema[T]
    def encode(v: T): String < IOs
    def decode(s: String): T < IOs
end Json

object Json extends JsonDerive:

    def apply[T](using j: Json[T]): Json[T] =
        j

    def schema[T](using j: Json[T]): Schema =
        j.schema

    def encode[T](v: T)(using j: Json[T]): String < IOs =
        j.encode(v)

    def decode[T](s: String)(using j: Json[T]): T < IOs =
        j.decode(s)

    given primitive[T](using t: StandardType[T]): Json[T] =
        fromZio(ZSchema.Primitive(t, Chunk.empty))

    def fromZio[T](z: ZSchema[T]) =
        new Json[T]:
            val zSchema              = z
            lazy val schema: Schema  = Schema(z)
            private lazy val decoder = JsonCodec.jsonDecoder(z)
            private lazy val encoder = JsonCodec.jsonEncoder(z)

            def encode(v: T): String < IOs =
                IOs(encoder.encodeJson(v).toString)

            def decode(s: String): T < IOs =
                IOs {
                    decoder.decodeJson(s) match
                        case Left(fail) => IOs.fail(fail)
                        case Right(v)   => v
                }
end Json
