
type dataset = {
    Label : string,
    Values : list float
}

(* Most graphs are a list of labels and a list of data sets.  Scatter and bubble
charts don't have labels. *)
datatype graph =
    Bar of list string * list dataset
  | StackedBar of list string * list dataset
  | Line of list string * list dataset
  | Pie of list string * list dataset
  | Doughnut of list string * list dataset
  | PolarArea of list string * list dataset
  | Radar of list string * list dataset
  | Scatter of list {Label : string, Values : list {X : float, Y : float}}
  | Bubble of list {Label : string, Values : list {X : float, Y : float, R : float}}

val graph : graph -> xbody
