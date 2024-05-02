package kyoTest

import kyo.*

class localsTest extends KyoPureTest:

    "default" - {
        "method" in {
            val l = Locals.init(10)
            assert(l.default == 10)
        }
        "get" in {
            val l = Locals.init(10)
            assert(
                Defers.run(l.get).pure ==
                    10
            )
        }
        "effect + get" in {
            val l = Locals.init(10)
            assert(
                Defers.run(Options.run(Options(1).map(_ => l.get))).pure ==
                    Some(10)
            )
        }
        "effect + get + effect" in {
            val l = Locals.init(10)
            assert(
                Defers.run(Options.run(Options(1).map(_ => l.get).map(Options(_)))).pure ==
                    Some(10)
            )
        }
        "multiple" in {
            val l1 = Locals.init(10)
            val l2 = Locals.init(20)
            assert(
                Defers.run(zip(l1.get, l2.get)).pure ==
                    (10, 20)
            )
        }
    }

    "let" - {
        "get" in {
            val l = Locals.init(10)
            assert(
                Defers.run(l.let(20)(l.get)).pure ==
                    20
            )
        }
        "effect + get" in {
            val l = Locals.init(10)
            assert(
                Defers.run(Options.run(Options(1).map(_ => l.let(20)(l.get)))).pure ==
                    Some(20)
            )
        }
        "effect + get + effect" in {
            val l = Locals.init(10)
            assert(
                Defers.run(
                    Options.run(Options(1).map(_ => l.let(20)(l.get).map(Options(_))))
                ).pure ==
                    Some(20)
            )
        }
        "multiple" in {
            val l1 = Locals.init(10)
            val l2 = Locals.init(20)
            assert(
                Defers.run(zip(l1.let(30)(l1.get), l2.let(40)(l2.get))).pure ==
                    (30, 40)
            )
        }
    }

    "update" - {
        "get" in {
            val l = Locals.init(10)
            assert(
                Defers.run(l.update(_ + 10)(l.get)).pure ==
                    20
            )
        }
        "effect + get" in {
            val l = Locals.init(10)
            assert(
                Defers.run(Options.run(Options(1).map(_ => l.update(_ + 10)(l.get)))).pure ==
                    Some(20)
            )
        }
        "effect + get + effect" in {
            val l = Locals.init(10)
            assert(
                Defers.run(
                    Options.run(Options(1).map(_ => l.update(_ + 10)(l.get).map(Options(_))))
                ).pure ==
                    Some(20)
            )
        }
        "multiple" in {
            val l1 = Locals.init(10)
            val l2 = Locals.init(20)
            assert(
                Defers.run(zip(l1.update(_ + 10)(l1.get), l2.update(_ + 10)(l2.get))).pure ==
                    (20, 30)
            )
        }
    }

    given CanEqual[Locals.State, Map[Local[Int], Int]] = CanEqual.derived

    "save" - {
        "let + save" in {
            val l = Locals.init(10)
            assert(
                Defers.run(l.let(20)(Locals.save)).pure ==
                    Map(l -> 20)
            )
        }
        "let + effect + save" in {
            val l = Locals.init(10)
            assert(
                Defers.run(Options.run(l.let(20)(Options(1).map(_ =>
                    Locals.save
                )))).pure ==
                    Some(Map(l -> 20))
            )
        }
        "effect + let + save" in {
            val l = Locals.init(10)
            assert(
                Defers.run(Options.run(Options(1).map(_ =>
                    l.let(20)(Locals.save)
                ))).pure ==
                    Some(Map(l -> 20))
            )
        }
        "effect + let + save + effect" in {
            val l = Locals.init(10)
            assert(
                Defers.run(Options.run(Options(1).map(_ =>
                    l.let(20)(Locals.save).map(Options(_))
                ))).pure ==
                    Some(Map(l -> 20))
            )
        }
        "nested" in {
            val l1 = Locals.init(10)
            val l2 = Locals.init(20)
            assert(
                Defers.run(
                    l1.let(30)(
                        l2.let(40)(
                            Locals.save
                        )
                    )
                ).pure ==
                    Map(l1 -> 30, l2 -> 40)
            )
        }
        "nested + effect" in {
            val l1 = Locals.init(10)
            val l2 = Locals.init(20)
            assert(
                Defers.run(
                    Options.run(
                        l1.let(30)(
                            l2.let(40)(
                                Options(1).map(_ => Locals.save)
                            )
                        )
                    )
                ).pure ==
                    Some(Map(l1 -> 30, l2 -> 40))
            )
        }
        "nested + effects" in {
            val l1 = Locals.init(10)
            val l2 = Locals.init(20)
            assert(
                Defers.run(
                    Options.run(
                        l1.let(30)(
                            l2.let(40)(
                                Options(1).map(_ => Locals.save).map(Options(_))
                            ).map(Options(_))
                        ).map(Options(_))
                    )
                ).pure ==
                    Some(Map(l1 -> 30, l2 -> 40))
            )
        }
        "multiple" in {
            val l1 = Locals.init(0)
            val l2 = Locals.init(0)
            val l3 = Locals.init(0)
            assert(
                Defers.run(
                    l3.let(20) {
                        zip(
                            l1.let(30)(Locals.save),
                            l2.let(40)(Locals.save)
                        )
                    }
                ).pure ==
                    (Map(l3 -> 20, l1 -> 30), Map(l3 -> 20, l2 -> 40))
            )
        }
        "multiple + effect" in {
            val l1 = Locals.init(0)
            val l2 = Locals.init(0)
            val l3 = Locals.init(0)
            assert(
                Defers.run(
                    Options.run(
                        l3.let(20) {
                            Options(1).map(_ =>
                                zip(
                                    l1.let(30)(Locals.save).map(Options(_)),
                                    l2.let(40)(Locals.save)
                                )
                            )
                        }
                    )
                ).pure ==
                    Some((Map(l3 -> 20, l1 -> 30), Map(l3 -> 20, l2 -> 40)))
            )
        }
    }

    "restore" - {
        val l1 = Locals.init(0)
        val l2 = Locals.init(0)
        val l3 = Locals.init(0)
        val state: Locals.State =
            Defers.run {
                l1.let(10) {
                    l2.let(20) {
                        l3.let(30) {
                            Locals.save
                        }
                    }
                }
            }.pure
        "get" in {
            assert(
                Defers.run(Locals.restore(state)(l1.get)).pure ==
                    10
            )
        }
        "effect + get" in {
            assert(
                Defers.run(
                    Options.run(Locals.restore[Int, Options & Defers](state)(Options(1).map(_ =>
                        l1.get
                    )))
                ).pure ==
                    Some(10)
            )
        }
        "effect + get + effect" in {
            assert(
                Defers.run(
                    Options.run(Locals.restore[Int, Options & Defers](state)(
                        Options(1).map(_ => l1.get).map(Options(_))
                    ))
                ).pure ==
                    Some(10)
            )
        }
        "multiple" in {
            assert(
                Defers.run(Locals.restore(state)(zip(l1.get, l2.get))).pure ==
                    (10, 20)
            )
        }
        "multiple + effect" in {
            assert(
                Defers.run(
                    Options.run(Locals.restore[(Int, Int), Options & Defers](state)(Options(1).map(_ =>
                        zip(l1.get, l2.get)
                    )))
                ).pure ==
                    Some((10, 20))
            )
        }
        "nested" in {
            assert(
                Defers.run(
                    l1.let(30) {
                        Locals.restore(state)(zip(l1.get, l2.get))
                    }
                ).pure ==
                    (10, 20)
            )
        }
    }
end localsTest
