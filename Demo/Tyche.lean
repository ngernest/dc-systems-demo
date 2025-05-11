import Lean
import ProofWidgets.Component.HtmlDisplay
import Plausible
import Demo.Recharts
import Demo.SliderTune

open Lean Widget ProofWidgets Recharts

def replicateM [Monad m] (n : Nat) (mx : m α) : m (List α) :=
  match n with
  | 0 => pure []
  | n + 1 => do
    let x ← mx
    let xs ← replicateM n mx
    pure (x :: xs)

open scoped ProofWidgets.Jsx in
def PlotFrequencies [BEq α] [Hashable α] [ToJson α] [ToString α] (data : List α) : Html :=
  let jsonData : Array Json :=
    data
      |> List.groupByKey id
      |> Std.HashMap.fold
          (λ rest label xs => json% {label: $(toString label), count: $(xs.length)} :: rest)
          []
      |> List.reverse
      |> List.toArray;
  <div>
    <BarChart width={400} height={400} data={jsonData}>
      <XAxis dataKey?="label" type={.category} />
      <YAxis />
      <Bar dataKey="count" fill="#8884d8" />
    </BarChart>
    <details>
      <SliderChanger />
    </details>
  </div>

open Lean Elab Command in
def tycheView
    [BEq α] [Hashable α] [ToJson α] [ToString α] [ToString β]
    (samples : Nat)
    (feature : β → α)
    (g : Plausible.Gen β) :
    CommandElabM Html := do
  let data ← replicateM samples <| g.run 100
  pure (PlotFrequencies (feature <$> data))

syntax (name := tycheCmd) "#tyche_bar " (atomic("(" &"samples" ":=" num ")"))? term (atomic(" with_feature ") term)? : command

open Lean Elab ProofWidgets Command in
@[command_elab tycheCmd]
def elabTycheCmd : CommandElab := fun
  | stx@`(#tyche_bar%$tk $t:term) => go tk stx t none none
  | stx@`(#tyche_bar%$tk (samples := $n:num) $t:term) => go tk stx t none (some n)
  | stx@`(#tyche_bar%$tk $t:term with_feature $f:term) => go tk stx t (some f) none
  | stx@`(#tyche_bar%$tk (samples := $n:num) $t:term with_feature $f:term) => go tk stx t (some f) (some n)
  | stx => throwError "Unexpected syntax {stx}."
  where
    go (tk : Syntax) (stx : Syntax) (gen : Term) (feature : Option Term) (samples : Option NumLit) : CommandElabM Unit := do
      let htX ← liftTermElabM
        <| HtmlCommand.evalCommandMHtml
        <| ← ``(HtmlEval.eval (tycheView $(samples.getD (Syntax.mkNatLit 1000)) $(feature.getD (← ``(id))) $gen))
      let ht ← htX
      liftCoreM <| Widget.savePanelWidgetInfo
        (hash HtmlDisplayPanel.javascript)
        (return json% { html: $(← Server.rpcEncode ht) })
        stx
      match samples with
      | some s => logInfoAt tk m!"Sampled {MessageData.ofSyntax s} values from {(MessageData.ofSyntax gen)}."
      | none => logInfoAt tk m!"Sampled 1000 values from {(MessageData.ofSyntax gen)}."
      for f in feature do
        logInfoAt tk m!"Plotting feature: {(MessageData.ofSyntax f)}."
