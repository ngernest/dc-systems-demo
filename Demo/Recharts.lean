import ProofWidgets.Component.Basic
import ProofWidgets.Component.Recharts

namespace ProofWidgets.Recharts
open Lean

inductive BarChartLayout where
  | horizontal
  | vertical
  deriving FromJson, ToJson

inductive BarChartSyncMethod where
  | index | value
  deriving FromJson, ToJson

structure BarChartMargin where
  top : Nat := 5
  right : Nat := 5
  bottom : Nat := 5
  left : Nat := 5
  deriving FromJson, ToJson

structure BarChartProps where
  layout : BarChartLayout := .horizontal
  syncId? : Option String := none
  syncMethod? : Option BarChartSyncMethod := some .index
  width : Nat
  height : Nat
  data : Array Json
  margin : BarChartMargin := {}
  deriving FromJson, ToJson

/-- See https://recharts.org/en-US/api/BarChart. -/
def BarChart : Component BarChartProps where
  javascript := Recharts.javascript
  «export» := "BarChart"

inductive BarType where
  | basis | basisClosed | basisOpen | linear | linearClosed | natural | monotoneX | monotoneY
  | monotone | step | stepBefore | stepAfter
  deriving FromJson, ToJson

structure BarProps where
  type : BarType := .linear
  dataKey : Json
  fill : String
  dot? : Option Bool := none
  -- TODO: There are many more props
  deriving FromJson, ToJson

/-- See https://recharts.org/en-US/api/Bar. -/
def Bar : Component BarProps where
  javascript := Recharts.javascript
  «export» := "Bar"

end ProofWidgets.Recharts
