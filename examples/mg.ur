table h : { Title : string, Bogosity : int }
  PRIMARY KEY Title

table a : { Company : string, EmployeeId : int, Awesome : bool }
  PRIMARY KEY (Company, EmployeeId)

table time : { When : time, Description : string }
  PRIMARY KEY When

val show_time : show {When : time} = mkShow (fn r => timef "%H:%M" r.When)

structure S = MeetingGrid.Make(struct
                                   val home = h
                                   val away = a
                                   val time = time
                               end)

val main =
    fg <- S.FullGrid.create;
    return <xml><body>
      {S.FullGrid.render fg}
    </body></xml>
