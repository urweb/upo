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

table restaurantChain : { Chain : string,
                          Healthy : bool }
  PRIMARY KEY Chain

table favoriteFoodOptions : { SwampName : string,
                              Chain : string,
                              SeqNum : int }
  PRIMARY KEY (SwampName, Chain),
  CONSTRAINT SwampName FOREIGN KEY SwampName REFERENCES swamp(SwampName),
  CONSTRAINT Chain FOREIGN KEY Chain REFERENCES restaurantChain(Chain)

open Explorer
open Make(struct
              structure Theme = Default

              val title = "Froggyland"
              val t = none
                      |> one [#Frog] [#Nickname] frog "Frogs" <xml></xml>
                      |> one [#Swamp] [#SwampName] swamp "Swamps" <xml></xml>
                      |> one [#SuperPower] [#Power] superPower "Powers" <xml></xml>
                      |> one [#RestaurantChain] [#Chain] restaurantChain "Chains" <xml></xml>

                      |> text [#Frog] [#Nickname] "Nickname"
                      |> text [#Frog] [#Age] "Age"
                      |> checkbox [#Frog] [#IsBullfrog] "Bullfrog?"

                      |> text [#RestaurantChain] [#Chain] "Name"
                      |> checkbox [#RestaurantChain] [#Healthy] "Healthy?"

                      |> text [#Swamp] [#SwampName] "Name"
                      |> text [#Swamp] [#SmellinessLevel] "Smelliness Level"
                      |> foreign [#Frog] [#Swamp] [#Swamp] [#SwampName] "Swamp" "Frogs"
                      |> manyToMany [#Frog] [#Nickname] [#Swamp] [#SwampName] protector "Protecting" "Protectors"
                      |> manyToManyOrdered [#Swamp] [#SwampName] [#RestaurantChain] [#Chain] favoriteFoodOptions "Favorite Food Options" "Swamps"

                      |> text [#SuperPower] [#Power] "Power"
                      |> text [#SuperPower] [#Description] "Description"

              fun authorize _ = return True

              val preTabs = {Before = ("Before", fn mk => return <xml>Boring Before page; but consider <a href={mk (make [#After] ())}>After</a></xml>)}
              val postTabs = {After = ("After", fn mk => return <xml>Boring After page; but consider <a href={mk (make [#Before] ())}>Before</a></xml>)}
              val hiddenTabs = {}
          end)

val main = index (make [#Frog] ())
