Xojo DrawSVG Module
===================

A module that extends the Graphics object with a DrawSVG method.

Installation
------------

Simply copy the DrawSVG module from the project in the [source folder](https://github.com/Zoclee/xojo-drawsvg/tree/master/source) to your project.

Example Code
------------

	Sub Paint(g As Graphics, areas() As REALbasic.Rect)
		g.DrawSVG "Some SVG XML", 100, 100
	End Sub
