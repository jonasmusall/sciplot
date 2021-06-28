# SciPlot
A Mathematica package to easily make good looking plots of functions and datasets. Under development. Feature set subject to rapid major changes. See [Tutorial.nb](Tutorial.nb) or [Tutorial.pdf](Tutorial.pdf) for in-depth usage and examples.

## Usage
First, import the latest version.

`Import["https://raw.githubusercontent.com/jonasmusall/sciplot/main/SciPlot.m"]`

To create a plot, pass functions and lists of points as you would pass them to Plot and ListPlot, respectively.

`plot = SciPlot[{f[x], {x, xmin, xmax}}, {{x1, y1}, {x2, y2}, ...}, ...]`

Use Export to save the plot to a file.

`Export[NotebookDirectory[] <> "Plot.pdf", plot]`

## Options
Syntax: `SciPlot[..., option -> value]`. More options will be added soon.

### AxesLabel
Labels which are automatically placed according to the position of the axes. Valid values are `yLabel` or `{xLabel, yLabel}` where the labels are any expression or `None` to display no label for that axis.

### AxesOrigin
Point where the axes cross. Valid values are points `{x, y}` or `Automatic`.

### ImageSize
Size of the produced image.

### PlotRange
Range to display in the plot. Valid values are ranges for both directions `{{xMin, xMax}, {yMin, yMax}}` or `Automatic`.

### PlotStyle
Style for the plots to be drawn in. Specify a single style to be applied to all plots or a list of styles.

### Ticks
Tick marks to be shown on each axes. A single tick mark is specified by either a single numerical
value or a list `{value,name,size,style}` where the size is a numerical value or a pair of sizes
`{s1,s2}` for the tick on either side of the axis and the style or style and size may be left out.
