(* ::Package:: *)

BeginPackage["SciPlot`"]
SciPlot::usage = "SciPlot[{{\!\(\*SubscriptBox[\(f\), \(1\)]\),\!\(\*SubscriptBox[\(f\), \(2\)]\),...},{x,\!\(\*SubscriptBox[\(x\), \(min\)]\),\!\(\*SubscriptBox[\(x\), \(max\)]\)}},{{\!\(\*SubscriptBox[\(x\), \(1\)]\),\!\(\*SubscriptBox[\(y\), \(1\)]\)},{\!\(\*SubscriptBox[\(x\), \(2\)]\),\!\(\*SubscriptBox[\(y\), \(2\)]\)},...},...] plots all specified functions and lists"
Begin["`Private`"]
InternalPlotMarkers = {{Graphics[{Thin, Line[{{{-1, -1}, {1, 1}}, {{-1, 1}, {1, -1}}}]}], Medium}}
Options[InternalSinglePlot] = {
    PlotRange -> Automatic,
    Axes -> False,
    PlotStyle -> Black,
    PlotMarkers -> InternalPlotMarkers[[1]],
    PerformanceGoal -> "Quality",
    MaxPlotPoints -> Infinity,
    MaxRecursion -> Automatic}
Options[SciPlot] = {PlotRange -> Automatic}
InternalSinglePlot[{f_, {x_Symbol, xmin_?NumericQ, xmax_?NumericQ}}, OptionsPattern[]] :=
    Plot[f, {x, xmin, xmax}, PlotRange -> OptionValue[PlotRange], PerformanceGoal -> OptionValue[PerformanceGoal], MaxRecursion -> OptionValue[MaxRecursion]]
InternalSinglePlot[list_List, OptionsPattern[]] :=
    ListPlot[list, PlotRange -> OptionValue[PlotRange], PerformanceGoal -> OptionValue[PerformanceGoal], MaxPlotPoints -> OptionValue[MaxPlotPoints]]
SciPlot[x__?(Or[Head[#] == List, MatchQ[#, {_, {_Symbol, _?NumericQ, _?NumericQ}}]]&), OptionsPattern[]] :=
    Module[{plotRange = OptionValue[PlotRange]},
        If[plotRange == Automatic,
            plotRange = PlotRange[InternalSinglePlot[{x}[[1]], PerformanceGoal -> "Speed", MaxPlotPoints -> 25, MaxRecursion -> 5]]
        ];
        Show[
            (InternalSinglePlot[#, PlotRange -> plotRange]&) /@ {x},
            Axes -> True,
            TicksStyle -> Directive[Black, FontSize -> 12, FontFamily -> "Times"]
        ]
    ]
End[]
EndPackage[]
