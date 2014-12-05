table h : { Title : string, Bogosity : int }
  PRIMARY KEY Title

table a : { Company : string, EmployeeId : int, Awesome : bool }
  PRIMARY KEY (Company, EmployeeId)

structure S = MeetingGrid.Make(struct
                                   val home = h
                                   val away = a
                               end)
