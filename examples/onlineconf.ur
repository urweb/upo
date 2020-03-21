table user : { Username : string,
               EmailAddress : string }
  PRIMARY KEY Username

table category : { Category : string }
  PRIMARY KEY Category

table paper : { Title : string,
                Category : string }
  PRIMARY KEY Title,
  CONSTRAINT Category FOREIGN KEY Category REFERENCES category(Category) ON UPDATE CASCADE

table author : { Paper : string,
                 User : string,
                 SeqNum : int }
  PRIMARY KEY (Paper, User),
  CONSTRAINT Paper FOREIGN KEY Paper REFERENCES paper(Title) ON UPDATE CASCADE,
  CONSTRAINT User FOREIGN KEY User REFERENCES user(Username) ON UPDATE CASCADE

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
                 |> foreign [#Paper] [#Category] [#Category] [#Category] "Category" "Papers"

         fun authorize _ = return True

         val preTabs = {}
         val postTabs = {}
         val hiddenTabs = {}
     end)

val main = Exp.index (make [#Category] ())
