structure Theme = Ui.Make(Default)

table h : { Title : string, Groovy : bool, Descr : string, Url : string }
  PRIMARY KEY Title

open Widget

structure ET = EditableTable.Make(struct
                                      val tab = h
                                      val labels = {Title = "Title",
                                                    Groovy = "Groovy?",
                                                    Descr = "Description",
                                                    Url = "URL"}
                                      val permission = return {Add = True,
                                                               Delete = True,
                                                               Modify = True}

                                      val widgets = {Url = urlbox} ++ _

                                      fun onAdd _ = return ()
                                      fun onDelete _ = return ()
                                      fun onModify _ = return ()
                                  end)

val main = Theme.simple "Main" ET.ui
