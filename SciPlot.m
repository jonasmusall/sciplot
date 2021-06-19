(* ::Package:: *)

BeginPackage["SciPlot`"]
SciPlot::usage = "SciPlot[{{\!\(\*SubscriptBox[\(f\), \(1\)]\),\!\(\*SubscriptBox[\(f\), \(2\)]\),...},{x,\!\(\*SubscriptBox[\(x\), \(min\)]\),\!\(\*SubscriptBox[\(x\), \(max\)]\)}},{{\!\(\*SubscriptBox[\(x\), \(1\)]\),\!\(\*SubscriptBox[\(y\), \(1\)]\)},{\!\(\*SubscriptBox[\(x\), \(2\)]\),\!\(\*SubscriptBox[\(y\), \(2\)]\)},...},...] plots all specified functions and lists"
Begin["`Private`"]

InternalPlotMarkers = {{Graphics[{Thin, Line[{{{-1, -1}, {1, 1}}, {{-1, 1}, {1, -1}}}]}], Medium}}

Options[InternalSinglePlot] = {
    MaxPlotPoints -> Infinity,
    MaxRecursion -> Automatic,
    PerformanceGoal -> "Quality",
    PlotMarkers -> InternalPlotMarkers[[1]],
    PlotRange -> Automatic,
    PlotStyle -> Black
}
Options[SciPlot] = {
    AxesOrigin -> Automatic,
    PlotRange -> Automatic
}

InternalSinglePlot[{f_, {x_Symbol, xmin_?NumericQ, xmax_?NumericQ}}, OptionsPattern[]] :=
    Plot[f, {x, xmin, xmax},
        Axes -> False,
        MaxRecursion -> OptionValue[MaxRecursion],
        PerformanceGoal -> OptionValue[PerformanceGoal],
        PlotRange -> OptionValue[PlotRange],
        PlotStyle -> OptionValue[PlotStyle]
    ]
InternalSinglePlot[list_List, OptionsPattern[]] :=
    ListPlot[list,
        Axes -> False,
        MaxPlotPoints -> OptionValue[MaxPlotPoints],
        PerformanceGoal -> OptionValue[PerformanceGoal],
        PlotMarkers -> OptionValue[PlotMarkers],
        PlotRange -> OptionValue[PlotRange],
        PlotStyle -> OptionValue[PlotStyle]
    ]

SciPlot[x__?(Or[Head[#] == List, MatchQ[#, {_, {_Symbol, _?NumericQ, _?NumericQ}}]]&), OptionsPattern[]] :=
    Module[
        {
            defaultOptRules = Options[
                InternalSinglePlot[{x}[[1]], MaxPlotPoints -> 25, MaxRecursion -> 5, PerformanceGoal -> "Speed"],
                {AxesOrigin, PlotRange}
            ],
            optAxesOrigin = OptionValue[AxesOrigin],
            optPlotRange = OptionValue[PlotRange]
        },
        If[optAxesOrigin == Automatic,
            optAxesOrigin = AxesOrigin /. defaultOptRules
        ];
        If[optPlotRange == Automatic,
            optPlotRange = PlotRange /. defaultOptRules
        ];
        Show[
            (InternalSinglePlot[#,
                PlotRange -> optPlotRange
            ]&) /@ {x},
            Axes -> True,
            AxesOrigin -> optAxesOrigin,
            TicksStyle -> Directive[Black, FontSize -> 12, FontFamily -> "Times"]
        ]
    ]

End[]
EndPackage[]
