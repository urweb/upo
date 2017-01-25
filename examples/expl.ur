table swamp : { SwampName : string,
                SmellinessLevel : float }
  PRIMARY KEY SwampName

table frog : { Nickname : string,
               Age : int,
               IsBullfrog : bool,
               Swamp : string }
  PRIMARY KEY Nickname,
  CONSTRAINT Swamp FOREIGN KEY Swamp REFERENCES swamp(SwampName)

table superPower : { Power : string,
                     Description : string }
  PRIMARY KEY Power

table hasPower : { Frog : string,
                   Power : string }
  PRIMARY KEY (Frog, Power),
  CONSTRAINT Frog FOREIGN KEY Frog REFERENCES frog(Nickname),
  CONSTRAINT Power FOREIGN KEY Power REFERENCES superPower(Power)

table protector : { SwampName : string,
                    Nickname : string}
  PRIMARY KEY (SwampName, Nickname),
  CONSTRAINT SwampName FOREIGN KEY SwampName REFERENCES swamp(SwampName),
  CONSTRAINT Nickname FOREIGN KEY Nickname REFERENCES frog(Nickname)

open Explorer
open Make(struct
              structure Theme = Default

              val title = "Froggyland"
              val t = none
                      |> one [#Frog] [#Nickname] frog "Frogs"
                      |> one [#Swamp] [#SwampName] swamp "Swamps"
                      |> one [#SuperPower] [#Power] superPower "Powers"

                      |> text [#Frog] [#Nickname] "Nickname"
                      |> text [#Frog] [#Age] "Age"
                      |> text [#Frog] [#IsBullfrog] "Bullfrog?"

                      |> text [#Swamp] [#SwampName] "Name"
                      |> text [#Swamp] [#SmellinessLevel] "Smelliness Level"
                      |> foreign [#Frog] [#Swamp] [#Swamp] [#SwampName] "Swamp" "Frogs"
                      |> manyToMany [#Frog] [#Nickname] [#Swamp] [#SwampName] protector "Protecting" "Protectors"

                      |> text [#SuperPower] [#Power] "Power"
                      |> text [#SuperPower] [#Description] "Description"
          end)

val main = index (make [#Frog] ())
