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
              structure Theme = Default

              val title = "Froggyland"
              val t = none
                          |> one [#Frog] [#Nickname] frog "Frogs"
                          |> one [#Swamp] [#SwampName] swamp "Swamps"
                          |> one [#SuperPower] [#Power] superPower "Powers"
          end)

val main = index (make [#Frog] ())
