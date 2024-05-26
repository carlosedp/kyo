package kyo

import org.jctools.queues.MpmcUnboundedXaddArrayQueue
import scala.annotation.tailrec

abstract class Channel[T]:
    self =>

    def size: Int < IOs

    def offer(v: T): Boolean < IOs

    def offerUnit(v: T): Unit < IOs

    def poll: Option[T] < IOs

    def isEmpty: Boolean < IOs

    def isFull: Boolean < IOs

    def putFiber(v: T): Fiber[Unit] < IOs

    def takeFiber: Fiber[T] < IOs

    def put(v: T): Unit < Fibers =
        putFiber(v).map(_.get)

    def take: T < Fibers =
        takeFiber.map(_.get)

    def isClosed: Boolean < IOs

    def drain: Seq[T] < IOs

    def close: Option[Seq[T]] < IOs
end Channel

object Channels:

    private val closed = IOs.fail("Channel closed!")

    def init[T: Flat](
        capacity: Int,
        access: Access = kyo.Access.Mpmc
    ): Channel[T] < IOs =
        Queues.init[T](capacity, access).map { queue =>
            IOs {
                new Channel[T]:

                    def u     = queue.unsafe
                    val takes = new MpmcUnboundedXaddArrayQueue[Promise[T]](8)
                    val puts  = new MpmcUnboundedXaddArrayQueue[(T, Promise[Unit])](8)

                    def size    = op(u.size())
                    def isEmpty = op(u.isEmpty())
                    def isFull  = op(u.isFull())

                    def offer(v: T) =
                        op {
                            try u.offer(v)
                            finally flush()
                        }
                    def offerUnit(v: T) =
                        op {
                            try discard(u.offer(v))
                            finally flush()
                        }
                    val poll =
                        op {
                            try Option(u.poll())
                            finally flush()
                        }

                    def putFiber(v: T) =
                        op {
                            try
                                if u.offer(v) then
                                    Fiber.Unit
                                else
                                    val p = Fibers.unsafeInitPromise[Unit]
                                    puts.add((v, p))
                                    p
                            finally
                                flush()
                        }

                    val takeFiber =
                        op {
                            try
                                val v = u.poll()
                                if isNull(v) then
                                    val p = Fibers.unsafeInitPromise[T]
                                    takes.add(p)
                                    p
                                else
                                    Fiber.value(v)
                                end if
                            finally
                                flush()
                        }

                    inline def op[T](inline v: => T): T < IOs =
                        IOs {
                            if u.isClosed() then
                                closed
                            else
                                v
                        }

                    def isClosed = queue.isClosed

                    def drain = queue.drain

                    def close =
                        IOs {
                            u.close() match
                                case None =>
                                    None
                                case r: Some[Seq[T]] =>
                                    def dropTakes(): Unit < IOs =
                                        Loops.foreach {
                                            takes.poll() match
                                                case null => Loops.done
                                                case p    => p.interrupt.map(_ => Loops.continue)
                                        }
                                    def dropPuts(): Unit < IOs =
                                        Loops.foreach {
                                            puts.poll() match
                                                case null   => Loops.done
                                                case (_, p) => p.interrupt.map(_ => Loops.continue)
                                        }
                                    dropTakes()
                                        .andThen(dropPuts())
                                        .andThen(r)
                        }

                    @tailrec private def flush(): Unit =
                        // This method ensures that all values are processed
                        // and handles interrupted fibers by discarding them.
                        val queueSize  = u.size()
                        val takesEmpty = takes.isEmpty()
                        val putsEmpty  = puts.isEmpty()

                        if queueSize > 0 && !takesEmpty then
                            // Attempt to transfer a value from the queue to
                            // a waiting consumer (take).
                            val p = takes.poll()
                            if !isNull(p) then
                                val v = u.poll()
                                if isNull(v) then
                                    // If the queue has been emptied before the
                                    // transfer, requeue the consumer's promise.
                                    discard(takes.add(p))
                                else if !p.unsafeComplete(v) && !u.offer(v) then
                                    // If completing the take fails and the queue
                                    // cannot accept the value back, enqueue a
                                    // placeholder put operation to preserve the value.
                                    val placeholder = Fibers.unsafeInitPromise[Unit]
                                    discard(puts.add((v, placeholder)))
                                end if
                            end if
                            flush()
                        else if queueSize < capacity && !putsEmpty then
                            // Attempt to transfer a value from a waiting
                            // producer (put) to the queue.
                            val t = puts.poll()
                            if t != null then
                                val (v, p) = t
                                if u.offer(v) then
                                    // Complete the put's promise if the value is
                                    // successfully enqueued. If the fiber became
                                    // interrupted, the completion will be ignored.
                                    discard(p.unsafeComplete(()))
                                else
                                    // If the queue becomes full before the transfer,
                                    // requeue the producer's operation.
                                    discard(puts.add(t))
                                end if
                            end if
                            flush()
                        else if queueSize == 0 && !putsEmpty && !takesEmpty then
                            // Directly transfer a value from a producer to a
                            // consumer when the queue is empty.
                            val t = puts.poll()
                            if t != null then
                                val (v, p) = t
                                val p2     = takes.poll()
                                if p2 != null && p2.unsafeComplete(v) then
                                    // If the transfer is successful, complete
                                    // the put's promise. If the consumer's fiber
                                    // became interrupted, the completion will be
                                    // ignored.
                                    discard(p.unsafeComplete(()))
                                else
                                    // If the transfer to the consumer fails, requeue
                                    // the producer's operation.
                                    discard(puts.add(t))
                                end if
                            end if
                            flush()
                        end if
                    end flush
            }
        }
end Channels
