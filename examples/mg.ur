table h : { Title : string, Bogosity : int }
  PRIMARY KEY Title

table a : { Company : string, EmployeeId : int, Awesome : bool }
  PRIMARY KEY (Company, EmployeeId)

structure S = MeetingGrid.Make(struct
                                   con homeKey = [Title = _]
                                   val home = h

                                   con awayKey = [Company = _, EmployeeId = _]
                                   val away = a
                               end)
