functor Make(M : sig
                 table slot : { Begin : time, End : time }
                 val initial : time
                 val final : time
                 val duration : int
             end) = struct
    open M

    fun addStartingAt here =
        if addSeconds here duration > final then
            return ()
        else
            dml (INSERT INTO slot(Begin, End)
                 VALUES ({[here]}, {[addSeconds here duration]}));
            addStartingAt (addSeconds here duration)

    task initialize = fn () =>
                         anyThere <- oneRowE1 (SELECT COUNT( * ) > 0
                                               FROM slot
                                               WHERE slot.Begin >= {[initial]}
                                                 AND slot.End <= {[final]});
                         if anyThere then
                             return ()
                         else
                             addStartingAt initial
end
