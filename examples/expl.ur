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

open Explorer
open Make(struct
              structure Theme = Cached

              val title = "Froggyland"
              val t = none
                      |> one [#Frog] [#Nickname] frog "Frogs"
                      |> one [#Swamp] [#SwampName] swamp "Swamps"
                      |> one [#SuperPower] [#Power] superPower "Powers"

                      |> text [#Frog] [#Nickname] "Nickname"
                      |> text [#Frog] [#Age] "Age"
                      |> text [#Frog] [#IsBullfrog] "Bullfrog?"
                      |> text [#Frog] [#Swamp] "Swamp"

                      |> text [#Swamp] [#SwampName] "Name"
                      |> text [#Swamp] [#SmellinessLevel] "Smelliness Level"

                      |> text [#SuperPower] [#Power] "Power"
                      |> text [#SuperPower] [#Description] "Description"
          end)

val main = index (make [#Frog] ())
