# SciPlot
A Mathematica package to easily make good looking plots of functions and datasets. Under development. Feature set subject to rapid major changes.

## Usage
First, import the latest version.

`Import["https://raw.githubusercontent.com/jonasmusall/sciplot/main/SciPlot.m"]`

To create a plot, pass functions and lists of points as you would pass them to Plot and ListPlot, respectively.

`SciPlot[{f[x], {x, xmin, xmax}}, {{x1, y1}, {x2, y2}, ...}, ...]`

## Options
Syntax: `SciPlot[..., option -> value]`. More options will be added soon.

### AxesLabel
Labels which are automatically placed according to the position of the axes. Valid values are `yLabel` or `{xLabel, yLabel}` where the labels are any expression or `None` to display no label for that axis.

### AxesOrigin
Point where the axes cross. Valid values are points `{x, y}` or `Automatic`.

### PlotRange
Range to display in the plot. Valid values are ranges for both directions `{{xMin, xMax}, {yMin, yMax}}` or `Automatic`.

### PlotStyle
Style for the plots to be drawn in. Specify a single style to be applied to all plots or a list of styles.
