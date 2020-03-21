table user : { Username : string,
               EmailAddress : string }
  PRIMARY KEY Username

table category : { Category : string }
  PRIMARY KEY Category

table paper : { Title : string }
  PRIMARY KEY Title

table author : { Paper : string,
                 User : string,
                 SeqNum : int }
  PRIMARY KEY (Paper, User),
  CONSTRAINT Paper FOREIGN KEY Paper REFERENCES paper(Title) ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT User FOREIGN KEY User REFERENCES user(Username) ON UPDATE CASCADE

table paperCategory : { Paper : string,
                        Category : string }
  PRIMARY KEY (Paper, Category),
  CONSTRAINT Paper FOREIGN KEY Paper REFERENCES paper(Title) ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT Category FOREIGN KEY Category REFERENCES category(Category) ON UPDATE CASCADE ON DELETE CASCADE
  
open Explorer

structure Exp =
Make(struct
         structure Theme = Default

         val title = "Papers"

         val t = none
                 |> one [#User] [#Username] user "Users" (return <xml></xml>) (Default (WHERE TRUE))
                 |> one [#Category] [#Category] category "Categories" (return <xml></xml>) (Default (WHERE TRUE))
                 |> one [#Paper] [#Title] paper "Papers" (return <xml></xml>) (Default (WHERE TRUE))

                 |> text [#User] [#Username] "Name"
                 |> text [#User] [#EmailAddress] "E-mail address"

                 |> text [#Category] [#Category] "Name"

                 |> text [#Paper] [#Title] "Title"
                 |> manyToManyOrdered [#Paper] [#Title] [#Paper] [#User] [#Username] [#User] author "Authors" "Papers" {}
                 |> manyToMany [#Paper] [#Title] [#Paper] [#Category] [#Category] [#Category] paperCategory "Categories" "Papers" {}

         fun authorize _ = return True

         val preTabs = {}
         val postTabs = {}
         val hiddenTabs = {}
     end)

val main = Exp.index (make [#Category] ())
