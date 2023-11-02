
type dataset = {
    Label : string,
    Values : list float
}

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

fun graphToFFI (g : graph) : ChartjsFfi.graph = case g of
    Bar x => ChartjsFfi.Bar x
  | StackedBar x => ChartjsFfi.StackedBar x
  | Line x => ChartjsFfi.Line x
  | Pie x => ChartjsFfi.Pie x
  | Doughnut x => ChartjsFfi.Doughnut x
  | PolarArea x => ChartjsFfi.PolarArea x
  | Radar x => ChartjsFfi.Radar x
  | Scatter x => ChartjsFfi.Scatter x
  | Bubble x => ChartjsFfi.Bubble x


fun graph gr = <xml>
  <active code={id <- fresh;
                return <xml>
                  <span id={id}/>
                  <dyn signal={gr <- signal gr;
                               return <xml><active code={_ <- ChartjsFfi.setGraph id (graphToFFI gr);
                                                         return <xml/>}/></xml>}/>
                </xml>}/>
</xml>
