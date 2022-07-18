(* ::Package:: *)

BeginPackage["SciPlot`"]
SciPlot::usage = "SciPlot[{{\!\(\*SubscriptBox[\(f\), \(1\)]\),\!\(\*SubscriptBox[\(f\), \(2\)]\),...},{x,\!\(\*SubscriptBox[\(x\), \(min\)]\),\!\(\*SubscriptBox[\(x\), \(max\)]\)}},{{\!\(\*SubscriptBox[\(x\), \(1\)]\),\!\(\*SubscriptBox[\(y\), \(1\)]\)},{\!\(\*SubscriptBox[\(x\), \(2\)]\),\!\(\*SubscriptBox[\(y\), \(2\)]\)},...},...] plots all specified functions and lists"
Begin["`Private`"]

InternalPlotMarkers = {{Graphics[{Line[{{{-1, -1}, {1, 1}}, {{-1, 1}, {1, -1}}}]}], Medium}}

Options[InternalSinglePlot] = {
    AxesOrigin -> Automatic,
    ImageSize -> Large,
    MaxPlotPoints -> Infinity,
    MaxRecursion -> Automatic,
    PerformanceGoal -> "Quality",
    PlotMarkers -> InternalPlotMarkers[[1]],
    PlotRange -> Automatic,
    PlotRangePadding -> {{Scaled[0.02], Scaled[0.02]}, {Scaled[0.05], Scaled[0.05]}},
    PlotStyle -> Directive[Black, Thin]
}
Options[SciPlot] = {
    AxesLabel -> None,
    AxesOrigin -> Automatic,
    ImageSize -> Large,
    PlotRange -> Automatic,
    PlotStyle -> Directive[Black, Thin],
    Ticks -> Automatic
}

GetContentCount[{f_, {x_Symbol, xmin_?NumericQ, xmax_?NumericQ}}] := If[Head[f] === List, Length[f], 1]
GetContentCount[list_List] :=
    If[Length[list] > 0,
        If[MatchQ[Dimensions[list], {_}],
            If[Head[First[list]] === List,
                Length[list],
                1
            ],
            1
        ],
        0
    ]
InternalSinglePlot[{f_, {x_Symbol, xmin_?NumericQ, xmax_?NumericQ}}, OptionsPattern[]] :=
    Plot[f, {x, xmin, xmax},
        Axes -> False,
        AxesOrigin -> OptionValue[AxesOrigin],
        ImageSize -> OptionValue[ImageSize],
        MaxRecursion -> OptionValue[MaxRecursion],
        PerformanceGoal -> OptionValue[PerformanceGoal],
        PlotLabels -> None,
        PlotRange -> OptionValue[PlotRange],
        PlotRangePadding -> OptionValue[PlotRangePadding],
        PlotStyle -> OptionValue[PlotStyle]
    ]
InternalSinglePlot[list_List, OptionsPattern[]] :=
    ListPlot[list,
        Axes -> False,
        AxesOrigin -> OptionValue[AxesOrigin],
        ImageSize -> OptionValue[ImageSize],
        MaxPlotPoints -> OptionValue[MaxPlotPoints],
        PerformanceGoal -> OptionValue[PerformanceGoal],
        PlotLabels -> None,
        PlotMarkers -> OptionValue[PlotMarkers],
        PlotRange -> OptionValue[PlotRange],
        PlotRangePadding -> OptionValue[PlotRangePadding],
        PlotStyle -> OptionValue[PlotStyle]
    ]
ApplyDefaultStyle[x_] := If[x === None, None, Style[x, Black, FontSize -> 14, FontFamily -> "Times"]]
GetLabledPosition[{min_, max_}, axis_] :=
    Piecewise[{{"Min", axis <= 0.95 min + 0.05 max}, {"Max", axis >= 0.95 max + 0.05 min}}, "Axis"]

SciPlot[x__?(Or[Head[#] == List, MatchQ[#, {_, {_Symbol, _?NumericQ, _?NumericQ}}]]&), OptionsPattern[]] :=
    Module[
        {
            plot,
            styleAssoc = {#[[1]], #[[1]] + #[[2]] - 1}& /@ Transpose[{Drop[FoldList[Plus, 1, #], -1], #}]&[GetContentCount /@ {x}],
            labelPosX,
            labelPosY,
            defaultOptRules = Options[
                InternalSinglePlot[{x}[[1]],
                    AxesOrigin -> OptionValue[AxesOrigin],
                    MaxPlotPoints -> 25,
                    MaxRecursion -> 5,
                    PerformanceGoal -> "Speed",
                    PlotRange -> OptionValue[PlotRange]],
                {AxesOrigin, PlotRange}
            ],
            optAxesLabel = OptionValue[AxesLabel],
            optAxesOrigin = OptionValue[AxesOrigin],
            optPlotRange = OptionValue[PlotRange],
            optPlotStyle = OptionValue[PlotStyle]
        },
        If[!(Head[optAxesLabel] === List),
            optAxesLabel = {None, optAxesLabel}
        ];
        If[optAxesOrigin == Automatic,
            optAxesOrigin = AxesOrigin /. defaultOptRules
        ];
        If[optPlotRange == Automatic,
            optPlotRange = PlotRange /. defaultOptRules
        ];
        If[!(Head[optPlotStyle] === List),
            optPlotStyle = {optPlotStyle}
        ];
        optPlotStyle = PadRight[optPlotStyle, styleAssoc[[-1, 2]], optPlotStyle];
        labelPosX = GetLabledPosition[optPlotRange[[2]], optAxesOrigin[[2]]];
        labelPosY = GetLabledPosition[optPlotRange[[1]], optAxesOrigin[[1]]];
        plot = Show[
            MapIndexed[
                InternalSinglePlot[#1,
                    AxesOrigin -> optAxesOrigin,
                    ImageSize -> OptionValue[ImageSize],
                    PlotRange -> optPlotRange,
                    PlotStyle -> optPlotStyle[[styleAssoc[[#2[[1]], 1]];; styleAssoc[[#2[[1]], 2]]]]
                ]&,
                {x}
            ],
            Axes -> True,
            AxesLabel -> {
                If[labelPosX == "Axis" || (labelPosX == "Max" && labelPosY == "Axis"), ApplyDefaultStyle@optAxesLabel[[1]], None],
                If[labelPosY == "Axis", ApplyDefaultStyle@optAxesLabel[[2]], None]
            },
            AxesOrigin -> optAxesOrigin,
            PlotLabel -> None,
            PlotLabels -> None,
            Ticks -> OptionValue[Ticks],
            TicksStyle -> Directive[Black, FontSize -> 12, FontFamily -> "Times"]
        ];
        If[((labelPosX != "Axis" && !(optAxesLabel[[1]] === None)) || (labelPosY != "Axis" && !(optAxesLabel[[2]] === None))) && !(labelPosX == "Max" && labelPosY == "Axis"),
            Labeled[plot,
                {
                    If[labelPosX != "Axis", ApplyDefaultStyle@optAxesLabel[[1]], Nothing],
                    If[labelPosY != "Axis", Rotate[ApplyDefaultStyle@optAxesLabel[[2]], Pi / 2], Nothing]
                },
                {
                    If[labelPosX == "Min", Bottom, If[labelPosX == "Max", Top, Nothing]],
                    If[labelPosY == "Min", Left, If[labelPosY == "Max", Right, Nothing]]
                }
            ],
            plot
        ]
    ]

End[]
EndPackage[]
