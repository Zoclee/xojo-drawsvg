#tag Module
Protected Module DrawSVG
	#tag Method, Flags = &h21
		Private Sub ApplyValues(Extends Item As JSONItem, withItem As JSONItem)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim i As Integer
		  
		  i = 0
		  while i < withItem.Count
		    Item.Value(withItem.Name(i)) = withItem.Value(withItem.Name(i))
		    i = i + 1
		  wend
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function buildStyleItem(node As XmlNode) As JSONItem
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim result as new JSONItem("{}")
		  Dim i As Integer
		  Dim j As Integer
		  Dim xAttr As XmlAttribute
		  Dim styleArr() As String
		  Dim itemArr() As String
		  
		  result.EscapeSlashes = false
		  
		  i = 0
		  while i < node.AttributeCount
		    xAttr = node.GetAttributeNode(i)
		    
		    if xAttr.Name = "style" then
		      
		      // process style attribute
		      
		      styleArr = node.GetAttribute(xAttr.Name).Split(";")
		      j = 0
		      while j <= styleArr.Ubound
		        itemArr = styleArr(j).Split(":")
		        if itemArr.Ubound = 1 then
		          result.Value(itemArr(0).Lowercase) = itemArr(1)
		        end if
		        j = j + 1
		      wend
		      
		    else
		      result.Value(xAttr.Name.Lowercase) = node.GetAttribute(xAttr.Name)
		    end if
		    
		    i = i + 1
		  wend
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function buildTransformationMatrix(transform As String) As JSONItem
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim result as new JSONItem("[1,0,0,0,1,0,0,0,1]")
		  
		  
		  
		  return result
		  
		  'Dim pos As Integer
		  'Dim openBracket As Integer
		  'Dim closeBracket As Integer
		  'Dim functionName As String
		  'Dim parms As String
		  'Dim transItem As JSONItem
		  'Dim strArr() As String
		  '
		  'pos = 0
		  '
		  'do
		  'pos = pos + 1
		  'openBracket = Instr(pos, transform, "(")
		  'if openBracket > 0 then
		  '
		  'closeBracket = Instr(openBracket, transform, ")")
		  'if closeBracket > 0 then
		  '
		  'functionName = Lowercase(Trim(Mid(transform, pos, openBracket - pos)))
		  'parms = Mid(transform, openBracket + 1, closeBracket - openBracket - 1)
		  '
		  'select case functionName
		  'case "translate"
		  'transItem = new JSONItem("{}")
		  'transItem.Value("function") = "translate"
		  'strArr = parms.Split(",")
		  'if strArr.Ubound = 1 then
		  'transItem.Value("tx") = Val(strArr(0))
		  'transItem.Value("ty") = Val(strArr(1))
		  'result.Append transItem
		  'end if
		  '
		  'end select
		  '
		  'pos = closeBracket
		  'else
		  'pos = 0
		  'end if
		  '
		  'else
		  'pos = 0
		  'end if
		  '
		  'loop until (pos >= Len(transform)) or (pos = 0)
		  
		  'return resultMatrix
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function colorFromHex(s As String) As Color
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim result As Color
		  Dim colVariant As Variant
		  Dim tmpStr As String
		  
		  if Left(s, 1) = "#" then
		    tmpStr = "&c" + Right(s, Len(s) - 1)
		  else
		    tmpStr = "&c" + s
		  end if
		  
		  colVariant = tmpStr
		  result = colVariant.ColorValue
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function determineColor(s As String) As Color
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim col As Color
		  Static ColorTable As new Dictionary("aliceblue" : &cf0f8ff, "antiquewhite" : &cfaebd7, "aqua" : &c00ffff, "azure" : &c0fffff,  _
		  "beige": &cf5f5dc, "bisque" : &cffe4c4, "black" : &c000000, "blanchedalmond" : &cffebcd, "blue" : &c0000ff, _
		  "blueviolet" : &c8a2be2, "brown" : &ca52a2a, "burlywood" : &cdeb887, "cadetblue" : &c5f9ea0, "chartreuse" : &c7fff00, _
		  "chocolate" : &cd2691e, "coral" : &cff7f50, "cornflowerblue" : &c6495ed, "cornsilk" : &cfff8dc, "crimson" : &cdc143c, _
		  "cyan" : &c00ffff, "darkblue" : &c00008b, "darkcyan" : &c008b8b, "darkgoldenrod" : &cb8860b, "darkgray" : &ca9a9a9, _
		  "darkgreen" : &c006400, "darkgrey" : &ca9a9a9, "darkkhaki" : &cbdb76b, "darkmagenta" : &c8b008b, "darkolivegreen" : &c556b2f, _
		  "darkorange" : &cff8c00, "darkorchid" : &c9932cc, "darkred" : &c8b0000, "darksalmon" : &c39967a, "darkseagreen" : &c8fbc8f, _
		  "darkslateblue" : &c483d8b, "darkslategray" : &c2f4f4f, "darkslategrey" : &c2f4f4f, "darkturquoise" : &c00ced1, _
		  "darkviolet" : &c9400d3, "deeppink" : &cff1493, "deepskyblue" : &c00bfff, "dimgray" : &c696969, "dimgrey" : &c696969, _
		  "dodgerblue" : &c1e90ff, "firebrick" : &cb22222, "floralwhite" : &cfffaf0, "forestgreen" : &c228b22, "fuchsia" : &cff00ff, _
		  "gainsboro" : &cdcdcdc, "ghostwhite" : &cf8f8ff, "gold" : &cffd700, "goldenrod" : &cdaa520, "gray" : &c8080, "grey" : &c8080, _
		  "green" : &c008000, "greenyellow" : &cadff2f, "honeydew" : &cf0fff0, "hotpink" : &cff69b4, "indianred" : &ccd5c5c, _
		  "indigo" : &c4b0082, "ivory" : &cfffff0, "khaki" : &cf0e68c, "lavender" : &ce6e6fa, "lavenderblush" : &cfff0f5, _
		  "lawngreen" : &c7cfc00, "lemonchiffon" : &cfffacd, "lightblue" : &cadd8e6, "lightcoral" : &cf08080, "lightcyan" : &ce0ffff, _
		  "lightgoldenrodyellow" : &cfafad2, "lightgray" : &cd3d3d3, "lightgreen" : &c90ee90, "lightgrey" : &cd3d3d3, _
		  "lightpink" : &cffb6c1, "lightsalmon" : &cffa07a, "lightseagreen" : &c20b2aa, "lightskyblue" : &c87cefa, "lightslategray" : &c778899, _
		  "lightslategrey" : &c778899, "lightsteelblue" : &cb0c4de, "lightyellow" : &cffffe0, "lime" : &c00ff00, "limegreen" : &c32cd32, _
		  "linen" : &cfaf0e6, "magenta" : &cff00ff, "maroon" : &c800000, "mediumaquamarine" : &c66cdaa, "mediumblue" : &c0000cd, _
		  "mediumorchid" : &cba55d3, "mediumpurple" : &c9370db, "mediumseagreen" : &c3cb371, "mediumslateblue" : &c7b68ee, _
		  "mediumspringgreen" : &c00fa9a, "mediumturquoise" : &c48d1cc, "mediumvioletred" : &cc71585, "midnightblue" : &c191970, _
		  "mintcream" : &cf5fffa, "mistyrose" : &cffe4e1, "moccasin" : &cffe4b5, "navajowhite" : &cffdead, "navy" : &c000080, _
		  "oldlace" : &cfdf5e6, "olive" : &c808000, "olivedrab" : &c6b8e23, "orange" : &cffa500, "orangered" : &cff4500, "orchid" : &cda70d6, _
		  "palegoldenrod" : &ceee8aa, "palegreen" : &c98fb98, "paleturquoise" : &cafeeee, "palevioletred" : &cdb7093, _
		  "papayawhip" : &cffefd5, "peachpuff" : &cffdab9, "peru" : &ccd853f, "pink" : &cffc0cb, "plum" : &cdda0dd, _
		  "powderblue" : &cb0e0e6, "purple" : &c800080, "red" : &cff0000, "rosybrown" : &cbc8f8f, "royalblue" : &c4169e1, _
		  "saddlebrown" : &c8b4513, "salmon" : &cfa8072, "sandybrown" : &cf4a460, "seagreen" : &c2e8b57, "seashell" : &cfff5ee, _
		  "sienna" : &ca0522d, "silver" : &cc0c0c0, "skyblue" : &c87ceeb, "slateblue" : &c6a5acd, "slategray" : &c708090, _
		  "slategrey" : &c708090, "snow" : &cfffafa, "springgreen" : &c00ff7f, "steelblue" : &c4682b4, "tan" : &cd2b4bc, "teal" : &c008080, _
		  "thistle" : &cd8bfd8, "tomato" : &cff6347, "turquoise" : &c40e0d0, "violet" : &cee82ee, "wheat" : &cf5deb3, "white" : &cffffff, _
		  "whitesmoke" : &cf5f5f5, "yellow" : &cffff00, "yellowgreen" : &c9acd32)
		  
		  if ColorTable.HasKey(Lowercase(Trim(s))) then
		    col = ColorTable.Value(Lowercase(Trim(s)))
		  else
		    col = colorFromHex(s)
		  end if
		  
		  return col
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DrawSVG(Extends g As Graphics, svg As String, x As Integer, y As Integer)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim xdoc As XmlDocument
		  Dim i As Integer
		  
		  if Len(svg) > 0 then
		    
		    try
		      
		      xdoc = new XmlDocument(svg)
		      
		      i = 0
		      while (i < xdoc.ChildCount) 
		        if xdoc.Child(i).Name = "svg" then
		          renderNode(xdoc.Child(i), g, x, y)
		        end if
		        i = i + 1
		      wend
		      
		    catch
		      // invalid xml, so we won't be rendering anything
		      
		    end try
		    
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function LookupDouble(Extends Item As JSONItem, Name As String, DefaultValue As Double = 0) As Double
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim result As Double
		  
		  if Item.HasName(Name) then
		    result = Val(Item.Value(Name))
		  else
		    result = DefaultValue
		  end if
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function LookupString(Extends Item As JSONItem, Name As String, DefaultValue As String = "") As String
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim result As String
		  
		  if Item.HasName(Name) then
		    result = Item.Value(Name)
		  else
		    result = DefaultValue
		  end if
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function matrixMultiply(m1 As JSONItem, m2 As JSONItem) As JSONItem
		  Dim result as new JSONItem("[0,0,0,0,0,0,0,0,0]")
		  
		  result.Value(0) = m1.Value(0) * m2.Value(0) + m1.Value(1) * m2.Value(3) + m1.Value(2) * m2.Value(6)
		  result.Value(1) = m1.Value(0) * m2.Value(1) + m1.Value(1) * m2.Value(4) + m1.Value(2) * m2.Value(7)
		  result.Value(2) = m1.Value(0) * m2.Value(2) + m1.Value(1) * m2.Value(5) + m1.Value(2) * m2.Value(8)
		  
		  result.Value(3) = m1.Value(3) * m2.Value(0) + m1.Value(4) * m2.Value(3) + m1.Value(5) * m2.Value(6)
		  result.Value(4) = m1.Value(3) * m2.Value(1) + m1.Value(4) * m2.Value(4) + m1.Value(5) * m2.Value(7)
		  result.Value(5) = m1.Value(3) * m2.Value(2) + m1.Value(4) * m2.Value(5) + m1.Value(5) * m2.Value(8)
		  
		  result.Value(6) = m1.Value(6) * m2.Value(0) + m1.Value(7) * m2.Value(3) + m1.Value(8) * m2.Value(6)
		  result.Value(7) = m1.Value(6) * m2.Value(1) + m1.Value(7) * m2.Value(4) + m1.Value(8) * m2.Value(7)
		  result.Value(8) = m1.Value(6) * m2.Value(2) + m1.Value(7) * m2.Value(5) + m1.Value(8) * m2.Value(8)
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub renderNode(node As XmlNode, g As Graphics, xOffset As Double, yOffset As Double)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim foundNode As Boolean
		  
		  foundNode = true
		  
		  if node.Name.Left(9) = "sodipodi:" then
		    // we ignore sodipodi tags
		    
		  else
		    
		    select case node.Name
		      
		    case "#comment"
		      // we ignore xml comments
		      
		    case "circle"
		      render_circle(node, g, xOffset, yOffset)
		      
		    case "desc"
		      // we ignore these tags
		      
		    case "ellipse"
		      render_ellipse(node, g, xOffset, yOffset)
		      
		    case "g"
		      render_g(node, g, xOffset, yOffset)
		      
		    case "line"
		      render_line(node, g, xOffset, yOffset)
		      
		    case "metadata"
		      // we ignore these tags
		      
		    case "polygon"
		      render_polygon(node, g, xOffset, yOffset)
		      
		    case "polyline"
		      render_polyline(node, g, xOffset, yOffset)
		      
		    case "rect"
		      render_rect(node, g, xOffset, yOffset)
		      
		    case "svg"
		      render_svg(node, g, xOffset, yOffset)
		      
		    case "text"
		      render_text(node, g, xOffset, yOffset)
		      
		    case else
		      foundNode = false
		      
		    end select
		    
		  end if
		  
		  // we only want to display error popups when debugging
		  
		  #if DebugBuild then
		    if not foundNode then
		      MsgBox "Unknown element: " + node.Name
		    end if
		  #endif
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_circle(node As XmlNode, g As Graphics, xOffset As Double, yOffset As Double)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim style As JSONItem
		  Dim matrix As JSONItem
		  Dim element As Picture
		  Dim eg As Graphics
		  Dim cx As Double
		  Dim cy As Double
		  Dim r As Double
		  Dim fill As String
		  Dim stroke As String
		  Dim strokeWidth As Double
		  Dim strokeStep As Integer
		  
		  style = buildStyleItem(node)
		  matrix = buildTransformationMatrix(style.Lookup("transform", ""))
		  
		  r = style.LookupDouble("r")
		  
		  if r > 0 then
		    
		    cx = style.LookupDouble("cx")
		    cy = style.LookupDouble("cy")
		    fill = style.LookupString("fill", "#000000")
		    stroke = style.LookupString("stroke", "")
		    strokeWidth = style.LookupDouble("stroke-width", 1)
		    
		    strokeStep = Floor(strokeWidth \ 2)
		    
		    element = new Picture(r * 2 + strokeWidth * 2, r * 2 + strokeWidth * 2)
		    eg = element.Graphics
		    
		    // fill
		    
		    if fill <> "none" then
		      eg.ForeColor = determineColor(fill)
		      eg.FillOval strokeStep, _
		      strokeStep, _
		      r * 2, _
		      r * 2
		    end if
		    
		    // stroke
		    
		    if (stroke <> "none") and (stroke <> "") and (strokeWidth > 0) then
		      eg.ForeColor = determineColor(stroke)
		      eg.PenWidth = strokeWidth
		      eg.PenHeight = eg.PenWidth
		      eg.DrawOval strokeStep, _
		      strokeStep, _
		      r * 2, _
		      r * 2
		    end if
		    
		    g.DrawPicture element, xOffset + cx - r - strokeStep, yOffset + cy - r - strokeStep
		    
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_ellipse(node As XmlNode, g As Graphics, xOffset As Double, yOffset As Double)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim style As JSONItem
		  Dim matrix As JSONItem
		  Dim element As Picture
		  Dim eg As Graphics
		  Dim cx As Double
		  Dim cy As Double
		  Dim rx As Double
		  Dim ry As Double
		  Dim fill As String
		  Dim stroke As String
		  Dim strokeWidth As Double
		  Dim strokeStep As Integer
		  
		  style = buildStyleItem(node)
		  matrix = buildTransformationMatrix(style.Lookup("transform", ""))
		  
		  cx = style.LookupDouble("cx")
		  cy = style.LookupDouble("cy")
		  rx = style.LookupDouble("rx")
		  ry = style.LookupDouble("ry")
		  fill = style.LookupString("fill", "#000000")
		  stroke = style.LookupString("stroke", "")
		  strokeWidth = style.LookupDouble("stroke-width", 1)
		  
		  if (rx > 0) and (ry > 0) then
		    
		    strokeStep = Floor(strokeWidth \ 2)
		    
		    element = new Picture(rx * 2 + strokeWidth * 2, ry * 2 + strokeWidth * 2)
		    eg = element.Graphics
		    
		    // fill
		    
		    if fill <> "none" then
		      eg.ForeColor = determineColor(fill)
		      eg.FillOval strokeStep, _
		      strokeStep, _
		      rx * 2, _
		      ry * 2
		    end if
		    
		    // stroke
		    
		    if (stroke <> "none") and (stroke <> "") and (strokeWidth > 0) then
		      eg.ForeColor = determineColor(stroke)
		      eg.PenWidth = strokeWidth
		      eg.PenHeight = eg.PenWidth
		      eg.DrawOval strokeStep, _
		      strokeStep, _
		      rx * 2, _
		      ry * 2
		    end if
		    
		    g.DrawPicture element, xOffset + cx - rx - strokeStep, yOffset + cy - ry - strokeStep
		    
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_g(node As XmlNode, g As Graphics, xOffset As Double, yOffset As Double)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim style As JSONItem
		  Dim matrix As JSONItem
		  Dim i As Integer
		  
		  style = buildStyleItem(node)
		  matrix = buildTransformationMatrix(style.Lookup("transform", ""))
		  
		  i = 0
		  while i < node.ChildCount
		    renderNode node.Child(i), g, xOffset, yOffset
		    i = i + 1
		  wend
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_line(node As XmlNode, g As Graphics, xOffset As Double, yOffset As Double)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim style As JSONItem
		  Dim matrix As JSONItem
		  Dim element As Picture
		  Dim eg As Graphics
		  Dim x1 As Double
		  Dim y1 As Double
		  Dim x2 As Double
		  Dim y2 As Double
		  Dim stroke As String
		  Dim strokeWidth As Double
		  Dim strokeStep As Integer
		  Dim width As Integer
		  Dim height As Integer
		  Dim minX As Integer
		  Dim minY As Integer
		  
		  style = buildStyleItem(node)
		  matrix = buildTransformationMatrix(style.Lookup("transform", ""))
		  
		  stroke = style.LookupString("stroke", "")
		  strokeWidth = style.LookupDouble("stroke-width", 1)
		  
		  strokeStep = (strokeWidth / 2)
		  
		  x1 = style.LookupDouble("x1") - strokeStep
		  y1 = style.LookupDouble("y1") - strokeStep
		  x2 = style.LookupDouble("x2") - strokeStep
		  y2 = style.LookupDouble("y2") - strokeStep
		  
		  if (stroke <> "none") and (stroke <> "") and (strokeWidth > 0) then
		    
		    width = Abs(x2- x1) + 1
		    height = Abs(y2- y1) + 1
		    if x2 < x1 then
		      minX = x2
		    else
		      minX = x1
		    end if
		    if y2 < y1 then
		      minY = y2
		    else
		      minY = y1
		    end if
		    
		    element = new Picture(width + strokeWidth * 2, height + strokeWidth * 2)
		    eg = element.Graphics
		    
		    eg.ForeColor = determineColor(stroke)
		    eg.PenWidth = strokeWidth
		    eg.PenHeight = eg.PenWidth
		    eg.DrawLine x1 - minX + strokeWidth, _
		    y1 - minY + strokeWidth, _
		    x2 - minX + strokeWidth, _
		    y2 - minY + strokeWidth
		    
		    g.DrawPicture element, xOffset + minX - strokeWidth, yOffset + minY - strokeWidth
		    
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_polygon(node As XmlNode, g As Graphics, xOffset As Double, yOffset As Double)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim style As JSONItem
		  Dim matrix As JSONItem
		  Dim element As Picture
		  Dim eg As Graphics
		  Dim fill As String
		  Dim stroke As String
		  Dim strokeWidth As Double
		  Dim points() As Integer
		  Dim tmpArr() As String
		  Dim coord() As String
		  Dim i As Integer
		  Dim minX As Integer
		  Dim maxX As Integer
		  Dim minY As Integer
		  Dim maxY As Integer
		  Dim width As Integer
		  Dim height As Integer
		  
		  style = buildStyleItem(node)
		  matrix = buildTransformationMatrix(style.Lookup("transform", ""))
		  
		  fill = style.LookupString("fill", "#000000")
		  stroke = style.LookupString("stroke", "")
		  strokeWidth = style.LookupDouble("stroke-width", 1)
		  
		  points.Append 1 // sentinal value
		  
		  tmpArr = style.LookupString("points", "").Split(" ")
		  i = 0
		  while i <= tmpArr.Ubound
		    coord = tmpArr(i).Split(",")
		    if coord.Ubound = 1 then
		      
		      points.Append Val(coord(0))
		      points.Append Val(coord(1))
		      
		      if i = 0 then
		        minX = Val(coord(0))
		        maxX = Val(coord(0))
		        minY = Val(coord(1))
		        maxY = Val(coord(1))
		      else
		        if Val(coord(0)) < minX then
		          minX = Val(coord(0))
		        end if
		        if Val(coord(0)) > maxX then
		          maxX = Val(coord(0))
		        end if
		        if Val(coord(1)) < minY then
		          minY = Val(coord(1))
		        end if
		        if Val(coord(1)) > maxY then
		          maxY = Val(coord(1))
		        end if
		      end if
		      
		    end if
		    i = i + 1
		  wend
		  
		  width = maxX - minX + 1
		  height = maxY - minY + 1
		  
		  // adjust polygon values for seperate layer rendering
		  
		  i = 1
		  while i < points.Ubound
		    points(i) = points(i) - minX + strokeWidth * 2
		    points(i + 1) = points(i + 1) - minY + strokeWidth * 2 
		    i = i + 2
		  wend
		  
		  element = new Picture(width + strokeWidth * 4, height + strokeWidth * 4)
		  eg = element.Graphics
		  
		  // fill
		  
		  if fill <> "none" then
		    eg.ForeColor = determineColor(fill)
		    eg.FillPolygon points
		  end if
		  
		  // stroke
		  
		  if (stroke <> "none") and (stroke <> "") and (strokeWidth > 0) then
		    eg.ForeColor = determineColor(stroke)
		    eg.PenWidth = strokeWidth
		    eg.PenHeight = eg.PenWidth
		    
		    eg.DrawPolygon points
		  end if
		  
		  g.DrawPicture element, xOffset + minX - strokeWidth * 2, yOffset + minY - strokeWidth * 2
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_polyline(node As XmlNode, g As Graphics, xOffset As Double, yOffset As Double)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim style As JSONItem
		  Dim matrix As JSONItem
		  Dim element As Picture
		  Dim eg As Graphics
		  Dim fill As String
		  Dim stroke As String
		  Dim strokeWidth As Double
		  Dim points() As Integer
		  Dim tmpArr() As String
		  Dim coord() As String
		  Dim i As Integer
		  Dim minX As Integer
		  Dim maxX As Integer
		  Dim minY As Integer
		  Dim maxY As Integer
		  Dim width As Integer
		  Dim height As Integer
		  
		  style = buildStyleItem(node)
		  matrix = buildTransformationMatrix(style.Lookup("transform", ""))
		  
		  fill = style.LookupString("fill", "#000000")
		  stroke = style.LookupString("stroke", "")
		  strokeWidth = style.LookupDouble("stroke-width", 1)
		  
		  points.Append 1 // sentinal value
		  
		  tmpArr = style.LookupString("points", "").Split(" ")
		  i = 0
		  while i <= tmpArr.Ubound
		    coord = tmpArr(i).Split(",")
		    if coord.Ubound = 1 then
		      
		      points.Append Val(coord(0))
		      points.Append Val(coord(1))
		      
		      if i = 0 then
		        minX = Val(coord(0))
		        maxX = Val(coord(0))
		        minY = Val(coord(1))
		        maxY = Val(coord(1))
		      else
		        if Val(coord(0)) < minX then
		          minX = Val(coord(0))
		        end if
		        if Val(coord(0)) > maxX then
		          maxX = Val(coord(0))
		        end if
		        if Val(coord(1)) < minY then
		          minY = Val(coord(1))
		        end if
		        if Val(coord(1)) > maxY then
		          maxY = Val(coord(1))
		        end if
		      end if
		      
		    end if
		    i = i + 1
		  wend
		  
		  width = maxX - minX + 1
		  height = maxY - minY + 1
		  
		  // adjust polygon values for seperate layer rendering
		  
		  i = 1
		  while i < points.Ubound
		    points(i) = points(i) - minX + strokeWidth * 2
		    points(i + 1) = points(i + 1) - minY + strokeWidth * 2
		    i = i + 2
		  wend
		  
		  // add reverse polygons to prevent closing line from drawing
		  
		  for i = points.Ubound - 3 downto 3 step 2
		    points.Append points(i)
		    points.Append points(i+1)
		  next i
		  
		  element = new Picture(width + strokeWidth * 4, height + strokeWidth * 4)
		  eg = element.Graphics
		  
		  // fill
		  
		  if fill <> "none" then
		    eg.ForeColor = determineColor(fill)
		    eg.FillPolygon points
		  end if
		  
		  // stroke
		  
		  if (stroke <> "none") and (stroke <> "") and (strokeWidth > 0) then
		    eg.ForeColor = determineColor(stroke)
		    eg.PenWidth = strokeWidth
		    eg.PenHeight = eg.PenWidth
		    
		    eg.DrawPolygon points
		  end if
		  
		  g.DrawPicture element, xOffset + minX - strokeWidth * 2, yOffset + minY - strokeWidth * 2
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_rect(node As XmlNode, g As Graphics, xOffset As Double, yOffset As Double)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim style As JSONItem
		  Dim matrix As JSONItem
		  Dim element As Picture
		  Dim eg As Graphics
		  Dim x As Double
		  Dim y As Double
		  Dim width As Double
		  Dim height As Double
		  Dim fill As String
		  Dim stroke As String
		  Dim strokeWidth As Double
		  Dim strokeStep As Integer
		  
		  style = buildStyleItem(node)
		  matrix = buildTransformationMatrix(style.Lookup("transform", ""))
		  
		  x = style.LookupDouble("x")
		  y = style.LookupDouble("y")
		  width = style.LookupDouble("width")
		  height = style.LookupDouble("height")
		  fill = style.LookupString("fill", "#000000")
		  stroke = style.LookupString("stroke", "")
		  strokeWidth = style.LookupDouble("stroke-width", 1)
		  
		  if (width > 0) and (height > 0) then
		    
		    strokeStep = Floor(strokeWidth \ 2)
		    
		    element = new Picture(width + strokeWidth * 2, height + strokeWidth * 2)
		    eg = element.Graphics
		    
		    // fill
		    
		    if fill <> "none" then
		      eg.ForeColor = determineColor(fill)
		      eg.FillRect strokeStep, _
		      strokeStep, _
		      width + strokeStep, _
		      height + strokeStep
		    end if
		    
		    // stroke
		    
		    if (stroke <> "none") and (stroke <> "") and (strokeWidth > 0) then
		      eg.ForeColor = determineColor(stroke)
		      eg.PenWidth = strokeWidth
		      eg.PenHeight = eg.PenWidth
		      eg.DrawRect strokeStep, _
		      strokeStep, _
		      width + strokeStep, _
		      height + strokeStep
		    end if
		    
		    g.DrawPicture element, xOffset + x - strokeStep, yOffset + y - strokeStep
		    
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_svg(node As XmlNode, g As Graphics, x As Double, y As Double)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim style As JSONItem
		  Dim matrix As JSONItem
		  Dim i As Integer
		  Dim drawG As Graphics
		  
		  style = buildStyleItem(node)
		  matrix = buildTransformationMatrix(style.Lookup("transform", ""))
		  
		  if (node.GetAttribute("width") <> "") and (node.GetAttribute("height") <> "") then
		    drawG = g.Clip(x, y, Val(node.GetAttribute("width")), Val(node.GetAttribute("height")))
		  else
		    drawG = g.Clip(x, y, g.Width - x, g.Height - y)
		  end if
		  
		  i = 0
		  while i < node.ChildCount
		    renderNode node.Child(i), drawG, 0, 0
		    i = i + 1
		  wend
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_text(node As XmlNode, g As Graphics, xOffset As Double, yOffset As Double)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim style As JSONItem
		  Dim matrix As JSONItem
		  Dim element As Picture
		  Dim eg As Graphics
		  Dim tspanStyle As JSONItem
		  Dim textStr As String
		  Dim x As Double
		  Dim y As Double
		  Dim fill As String
		  Dim strShape as new StringShape
		  
		  style = buildStyleItem(node)
		  matrix = buildTransformationMatrix(style.Lookup("transform", ""))
		  
		  x = style.LookupDouble("x")
		  y = style.LookupDouble("y")
		  fill = style.LookupString("fill", "#000000")
		  
		  // fill
		  
		  if fill <> "none" then
		    
		    textStr = ""
		    if node.FirstChild <> nil then
		      if node.FirstChild.Name = "#text" then
		        textStr = Trim(node.FirstChild.Value)
		      elseif node.FirstChild.Name = "tspan" then
		        
		        tspanStyle = buildStyleItem(node.FirstChild)
		        style.ApplyValues(tspanStyle)
		        if node.FirstChild.FirstChild <> nil then
		          if node.FirstChild.FirstChild.Name = "#text" then
		            textStr = Trim(node.FirstChild.FirstChild.Value)
		          end if
		        end if
		        
		      end if
		    end if
		    
		    g.TextFont = style.LookupString("font-family", "Arial")
		    g.TextUnit = FontUnits.Pixel
		    g.TextSize = style.LookupDouble("font-size", 16)
		    
		    if textStr <> "" then
		      
		      element = new Picture(g.StringWidth(textStr), g.TextHeight)
		      eg = element.Graphics
		      
		      strShape.FillColor = determineColor(fill)
		      strShape.TextFont = g.TextFont
		      strShape.TextUnit = g.TextUnit
		      strShape.TextSize = g.TextSize
		      strShape.HorizontalAlignment = StringShape.Alignment.Left
		      strShape.VerticalAlignment = StringShape.Alignment.Top
		      strShape.Text = textStr
		      
		      eg.DrawObject strShape, _
		      0, _
		      0
		      
		      g.DrawPicture element, xOffset + x, yOffset + y - g.TextAscent
		      
		    end if
		    
		  end if
		End Sub
	#tag EndMethod


	#tag ViewBehavior
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			Type="String"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			Type="String"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
		#tag EndViewProperty
	#tag EndViewBehavior
End Module
#tag EndModule
