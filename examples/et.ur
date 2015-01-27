table h : { Title : string, Groovy : bool, Descr : string }
  PRIMARY KEY Title

open Widget

structure ET = EditableTable.Make(struct
                                      val tab = h
                                      val labels = {Title = "Title",
                                                    Groovy = "Groovy?",
                                                    Descr = "Description"}
                                      val permission = return {Add = True,
                                                               Delete = True,
                                                               Modify = True}

                                      val widgets = _
                                      val eqs = _
                                      val ords = _
                                      val injs = _
                                  end)

val main = Ui.simple "Main" ET.ui
