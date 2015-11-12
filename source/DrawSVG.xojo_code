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
		Private Function buildTransformationMatrix(transform As String) As Double()
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim result() As Double = Array( _
		  1.0, 0.0, 0.0, _
		  0.0, 1.0, 0.0, _
		  0.0, 0.0, 1.0)
		  Dim mulMatrix() As Double = Array( _
		  0.0, 0.0, 0.0, _
		  0.0, 0.0, 0.0, _
		  0.0, 0.0, 0.0)
		  Dim pos As Integer
		  Dim openBracket As Integer
		  Dim closeBracket As Integer
		  Dim functionName As String
		  Dim parms As String
		  Dim strArr() As String
		  
		  pos = 0
		  
		  do
		    pos = pos + 1
		    openBracket = Instr(pos, transform, "(")
		    if openBracket > 0 then
		      
		      closeBracket = Instr(openBracket, transform, ")")
		      if closeBracket > 0 then
		        
		        functionName = Lowercase(Trim(Mid(transform, pos, openBracket - pos)))
		        parms = Mid(transform, openBracket + 1, closeBracket - openBracket - 1)
		        strArr = parms.Split(",")
		        
		        select case functionName
		          
		        case "matrix"
		          if strArr.Ubound = 5 then
		            mulMatrix = initMatrix(val(strArr(0)), _ ' a
		            val(strArr(1)), _ ' b
		            val(strArr(2)), _ ' c
		            val(strArr(3)), _ ' d
		            val(strArr(4)), _ ' e
		            val(strArr(5)) ) ' f
		            result = matrixMultiply(result, mulMatrix)
		          end if
		          
		        case "rotate"
		          if strArr.Ubound = 0 then // around origin
		            mulMatrix = initRotateMatrix(val(strArr(0)))
		            result = matrixMultiply(result, mulMatrix)
		          elseif strArr.Ubound = 2 then // around point
		            mulMatrix = initTranslationMatrix(val(strArr(1)), val(strArr(2)))
		            result = matrixMultiply(result, mulMatrix)
		            mulMatrix = initRotateMatrix(val(strArr(0)))
		            result = matrixMultiply(result, mulMatrix)
		            mulMatrix = initTranslationMatrix(-val(strArr(1)), -val(strArr(2)))
		            result = matrixMultiply(result, mulMatrix)
		          end if
		          
		        case "scale"
		          if strArr.Ubound >= 1 then
		            mulMatrix = initScaleMatrix(val(strArr(0)), val(strArr(1)))
		          else
		            mulMatrix = initScaleMatrix(val(strArr(0)), val(strArr(0)))
		          end if
		          result = matrixMultiply(result, mulMatrix)
		          
		        case "skewx"
		          if strArr.Ubound >= 0 then
		            mulMatrix = initSkewXMatrix(val(strArr(0)))
		            result = matrixMultiply(result, mulMatrix)
		          end if
		          
		        case "skewy"
		          if strArr.Ubound >= 0 then
		            mulMatrix = initSkewYMatrix(val(strArr(0)))
		            result = matrixMultiply(result, mulMatrix)
		          end if
		          
		        case "translate"
		          if strArr.Ubound >= 1 then
		            mulMatrix = initTranslationMatrix(val(strArr(0)), val(strArr(1)))
		          else
		            mulMatrix = initTranslationMatrix(val(strArr(0)), 0)
		          end if
		          result = matrixMultiply(result, mulMatrix)
		          
		        end select
		        
		        pos = closeBracket
		      else
		        pos = 0
		      end if
		      
		    else
		      pos = 0
		    end if
		    
		  loop until (pos >= Len(transform)) or (pos = 0)
		  
		  
		  return result
		  
		  
		  
		  
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
		  Dim matrix() As Double
		  Dim drawG As Graphics
		  
		  if Len(svg) > 0 then
		    
		    try
		      
		      xdoc = new XmlDocument(svg)
		      
		      drawG = g.Clip(x, y, g.Width - x, g.Height - y)
		      
		      matrix = initIdentityMatrix()
		      
		      i = 0
		      while (i < xdoc.ChildCount) 
		        if xdoc.Child(i).Name = "svg" then
		          renderNode(xdoc.Child(i), drawG, matrix)
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
		Private Function initIdentityMatrix() As Double()
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim result() As Double = Array( _
		  1.0, 0.0, 0.0, _
		  0.0, 1.0, 0.0, _
		  0.0, 0.0, 1.0)
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function initMatrix(a As Double, b As Double, c As Double, d As Double, e As Double, f As Double) As Double()
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim result() As Double = Array( _
		  a, c, e, _
		  b, d, f, _
		  0.0, 0.0, 1.0)
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function initRotateMatrix(angle As Double) As Double()
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim result() As Double = Array( _
		  cos(angle * DegToRad), -sin(angle * DegToRad), 0.0, _
		  sin(angle * DegToRad), cos(angle * DegToRad), 0.0, _
		  0.0, 0.0, 1.0)
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function initScaleMatrix(sx As Double, sy As Double) As Double()
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim result() As Double = Array( _
		  sx, 0.0, 0.0, _
		  0.0, sy, 0.0, _
		  0.0, 0.0, 1.0)
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function initSkewXMatrix(angle As Double) As Double()
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim result() As Double = Array( _
		  1.0, tan(angle * DegToRad), 0.0, _
		  0.0, 1.0, 0.0, _
		  0.0, 0.0, 1.0)
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function initSkewYMatrix(angle As Double) As Double()
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim result() As Double = Array( _
		  1.0, 0.0, 0.0, _
		  tan(angle * DegToRad), 1.0, 0.0, _
		  0.0, 0.0, 1.0)
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function initTranslationMatrix(tx As Double, ty As Double) As Double()
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim result() As Double = Array( _
		  1.0, 0.0, tx, _
		  0.0, 1.0, ty, _
		  0.0, 0.0, 1.0)
		  
		  return result
		  
		End Function
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
		Private Function matrixMultiply(m1() As Double, m2() As Double) As Double()
		  Dim result() As Double = Array( _
		  0.0, 0.0, 0.0, _
		  0.0, 0.0, 0.0, _
		  0.0, 0.0, 0.0)
		  
		  result(0) = m1(0) * m2(0) + m1(1) * m2(3) + m1(2) * m2(6)
		  result(1) = m1(0) * m2(1) + m1(1) * m2(4) + m1(2) * m2(7)
		  result(2) = m1(0) * m2(2) + m1(1) * m2(5) + m1(2) * m2(8)
		  
		  result(3) = m1(3) * m2(0) + m1(4) * m2(3) + m1(5) * m2(6)
		  result(4) = m1(3) * m2(1) + m1(4) * m2(4) + m1(5) * m2(7)
		  result(5) = m1(3) * m2(2) + m1(4) * m2(5) + m1(5) * m2(8)
		  
		  result(6) = m1(6) * m2(0) + m1(7) * m2(3) + m1(8) * m2(6)
		  result(7) = m1(6) * m2(1) + m1(7) * m2(4) + m1(8) * m2(7)
		  result(8) = m1(6) * m2(2) + m1(7) * m2(5) + m1(8) * m2(8)
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub renderNode(node As XmlNode, g As Graphics, parentMatrix() As Double)
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
		      render_circle(node, g, parentMatrix)
		      
		    case "defs"
		      // we ignore these tags
		      
		    case "desc"
		      // we ignore these tags
		      
		    case "ellipse"
		      render_ellipse(node, g, parentMatrix)
		      
		    case "g"
		      render_g(node, g, parentMatrix)
		      
		    case "line"
		      render_line(node, g, parentMatrix)
		      
		    case "metadata"
		      // we ignore these tags
		      
		    case "polygon"
		      render_polygon(node, g, parentMatrix)
		      
		    case "polyline"
		      render_polyline(node, g, parentMatrix)
		      
		    case "rect"
		      render_rect(node, g, parentMatrix)
		      
		    case "svg"
		      render_svg(node, g, parentMatrix)
		      
		    case "text"
		      render_text(node, g, parentMatrix)
		      
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
		Private Sub render_circle(node As XmlNode, g As Graphics, parentMatrix() As Double)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim style As JSONItem
		  Dim matrix() As Double
		  Dim points() As Integer
		  Dim pointsDbl() As Double
		  Dim i As Integer
		  Dim tmpX As Double
		  Dim tmpY As Double
		  Dim cx As Double
		  Dim cy As Double
		  Dim r As Double
		  Dim fill As String
		  Dim stroke As String
		  Dim strokeWidth As Double
		  Dim pointCount As Integer
		  Dim theta As Double
		  
		  style = buildStyleItem(node)
		  matrix = buildTransformationMatrix(style.Lookup("transform", ""))
		  matrix = matrixMultiply(matrix, parentMatrix)
		  
		  cx = style.LookupDouble("cx")
		  cy = style.LookupDouble("cy")
		  fill = style.LookupString("fill", "#000000")
		  stroke = style.LookupString("stroke", "")
		  strokeWidth = style.LookupDouble("stroke-width", 1)
		  r = style.LookupDouble("r")
		  
		  if (r > 0) then
		    
		    // build polygon
		    
		    pointsDbl.Append 0
		    
		    pointCount = 128
		    i = 0
		    while i <= pointCount 
		      theta = Pi * (i / (pointCount / 2))
		      pointsDbl.Append cx + r * cos(theta) // center a + radius x * cos(theta)
		      pointsDbl.Append cy + r * sin(theta) // center b + radius y * sin(theta)
		      i = i + 1
		    wend
		    
		    // transform polygon
		    
		    i = 1
		    while i < pointsDbl.Ubound
		      tmpX = pointsDbl(i)
		      tmpY = pointsDbl(i + 1)
		      transformPoint tmpX, tmpY, matrix
		      pointsDbl(i) = tmpX
		      pointsDbl(i + 1) = tmpY
		      i = i + 2
		    wend
		    
		    // convert circle points to integers
		    
		    Redim points(-1)
		    i = 0
		    while i <= pointsDbl.Ubound
		      points.Append Round(pointsDbl(i))
		      i = i + 1
		    wend
		    
		    // fill
		    
		    if fill <> "none" then
		      g.ForeColor = determineColor(fill)
		      g.FillPolygon points
		    end if
		    
		    // stroke
		    
		    if (stroke <> "none") and (stroke <> "") and (strokeWidth > 0) then
		      g.ForeColor = determineColor(stroke)
		      g.PenWidth = strokeWidth
		      g.PenHeight = strokeWidth
		      g.DrawPolygon points
		    end if
		    
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_ellipse(node As XmlNode, g As Graphics, parentMatrix() As Double)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim style As JSONItem
		  Dim matrix() As Double
		  Dim points() As Integer
		  Dim pointsDbl() As Double
		  Dim i As Integer
		  Dim tmpX As Double
		  Dim tmpY As Double
		  Dim cx As Double
		  Dim cy As Double
		  Dim rx As Double
		  Dim ry As Double
		  Dim fill As String
		  Dim stroke As String
		  Dim strokeWidth As Double
		  Dim pointCount As Integer
		  Dim theta As Double
		  
		  style = buildStyleItem(node)
		  matrix = buildTransformationMatrix(style.Lookup("transform", ""))
		  matrix = matrixMultiply(matrix, parentMatrix)
		  
		  cx = style.LookupDouble("cx")
		  cy = style.LookupDouble("cy")
		  fill = style.LookupString("fill", "#000000")
		  stroke = style.LookupString("stroke", "")
		  strokeWidth = style.LookupDouble("stroke-width", 1)
		  rx = style.LookupDouble("rx")
		  ry = style.LookupDouble("ry")
		  
		  if (rx > 0) and (ry > 0) then
		    
		    // build polygon
		    
		    pointsDbl.Append 0
		    
		    pointCount = 128
		    i = 0
		    while i <= pointCount 
		      theta = Pi * (i / (pointCount / 2))
		      pointsDbl.Append cx + rx * cos(theta) // center a + radius x * cos(theta)
		      pointsDbl.Append cy + ry * sin(theta) // center b + radius y * sin(theta)
		      i = i + 1
		    wend
		    
		    // transform polygon
		    
		    i = 1
		    while i < pointsDbl.Ubound
		      tmpX = pointsDbl(i)
		      tmpY = pointsDbl(i + 1)
		      transformPoint tmpX, tmpY, matrix
		      pointsDbl(i) = tmpX
		      pointsDbl(i + 1) = tmpY
		      i = i + 2
		    wend
		    
		    // convert circle points to integers
		    
		    Redim points(-1)
		    i = 0
		    while i <= pointsDbl.Ubound
		      points.Append Round(pointsDbl(i))
		      i = i + 1
		    wend
		    
		    // fill
		    
		    if fill <> "none" then
		      g.ForeColor = determineColor(fill)
		      g.FillPolygon points
		    end if
		    
		    // stroke
		    
		    if (stroke <> "none") and (stroke <> "") and (strokeWidth > 0) then
		      g.ForeColor = determineColor(stroke)
		      g.PenWidth = strokeWidth
		      g.PenHeight = strokeWidth
		      g.DrawPolygon points
		    end if
		    
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_g(node As XmlNode, g As Graphics, parentMatrix() As Double)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim style As JSONItem
		  Dim matrix() As Double
		  Dim i As Integer
		  
		  style = buildStyleItem(node)
		  matrix = buildTransformationMatrix(style.Lookup("transform", ""))
		  matrix = matrixMultiply(matrix, parentMatrix)
		  
		  i = 0
		  while i < node.ChildCount
		    renderNode node.Child(i), g, matrix
		    i = i + 1
		  wend
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_line(node As XmlNode, g As Graphics, parentMatrix() As Double)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim style As JSONItem
		  Dim matrix() As Double
		  Dim points() As Integer
		  Dim i As Integer
		  Dim tmpX As Integer
		  Dim tmpY As Integer
		  Dim x1 As Double
		  Dim y1 As Double
		  Dim x2 As Double
		  Dim y2 As Double
		  Dim stroke As String
		  Dim strokeWidth As Double
		  
		  style = buildStyleItem(node)
		  matrix = buildTransformationMatrix(style.Lookup("transform", ""))
		  matrix = matrixMultiply(matrix, parentMatrix)
		  
		  x1 = style.LookupDouble("x1")
		  y1 = style.LookupDouble("y1")
		  x2 = style.LookupDouble("x2")
		  y2 = style.LookupDouble("y2")
		  stroke = style.LookupString("stroke", "")
		  strokeWidth = style.LookupDouble("stroke-width", 1)
		  
		  // build polygon
		  
		  points.Append 0
		  points.Append x1
		  points.Append y1
		  points.Append x2
		  points.Append y2
		  
		  // transform polygon
		  
		  i = 1
		  while i < points.Ubound
		    tmpX = points(i)
		    tmpY = points(i + 1)
		    transformPoint tmpX, tmpY, matrix
		    points(i) = tmpX
		    points(i + 1) = tmpY
		    i = i + 2
		  wend
		  
		  // stroke
		  
		  if (stroke <> "none") and (stroke <> "") and (strokeWidth > 0) then
		    g.ForeColor = determineColor(stroke)
		    g.PenWidth = strokeWidth
		    g.PenHeight = strokeWidth
		    g.DrawPolygon points
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_polygon(node As XmlNode, g As Graphics, parentMatrix() As Double)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim style As JSONItem
		  Dim matrix() As Double
		  Dim i As Integer
		  Dim tmpX As Integer
		  Dim tmpY As Integer
		  Dim fill As String
		  Dim stroke As String
		  Dim strokeWidth As Double
		  Dim points() As Integer
		  Dim tmpArr() As String
		  Dim coord() As String
		  
		  style = buildStyleItem(node)
		  matrix = buildTransformationMatrix(style.Lookup("transform", ""))
		  matrix = matrixMultiply(matrix, parentMatrix)
		  
		  fill = style.LookupString("fill", "#000000")
		  stroke = style.LookupString("stroke", "")
		  strokeWidth = style.LookupDouble("stroke-width", 1)
		  
		  // build polygon
		  
		  points.Append 1 // sentinal value
		  
		  tmpArr = style.LookupString("points", "").Split(" ")
		  i = 0
		  while i <= tmpArr.Ubound
		    coord = tmpArr(i).Split(",")
		    if coord.Ubound = 1 then
		      points.Append Val(coord(0))
		      points.Append Val(coord(1))
		    end if
		    i = i + 1
		  wend
		  
		  // transform polygon
		  
		  i = 1
		  while i < points.Ubound
		    tmpX = points(i)
		    tmpY = points(i + 1)
		    transformPoint tmpX, tmpY, matrix
		    points(i) = tmpX
		    points(i + 1) = tmpY
		    i = i + 2
		  wend
		  
		  // fill
		  
		  if fill <> "none" then
		    g.ForeColor = determineColor(fill)
		    g.FillPolygon points
		  end if
		  
		  // stroke
		  
		  if (stroke <> "none") and (stroke <> "") and (strokeWidth > 0) then
		    g.ForeColor = determineColor(stroke)
		    g.PenWidth = strokeWidth
		    g.PenHeight = strokeWidth
		    g.DrawPolygon points
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_polyline(node As XmlNode, g As Graphics, parentMatrix() As Double)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim style As JSONItem
		  Dim matrix() As Double
		  Dim i As Integer
		  Dim tmpX As Integer
		  Dim tmpY As Integer
		  Dim fill As String
		  Dim stroke As String
		  Dim strokeWidth As Double
		  Dim points() As Integer
		  Dim tmpArr() As String
		  Dim coord() As String
		  
		  style = buildStyleItem(node)
		  matrix = buildTransformationMatrix(style.Lookup("transform", ""))
		  matrix = matrixMultiply(matrix, parentMatrix)
		  
		  fill = style.LookupString("fill", "#000000")
		  stroke = style.LookupString("stroke", "")
		  strokeWidth = style.LookupDouble("stroke-width", 1)
		  
		  // build polygon
		  
		  points.Append 1 // sentinal value
		  
		  tmpArr = style.LookupString("points", "").Split(" ")
		  i = 0
		  while i <= tmpArr.Ubound
		    coord = tmpArr(i).Split(",")
		    if coord.Ubound = 1 then
		      points.Append Val(coord(0))
		      points.Append Val(coord(1))
		    end if
		    i = i + 1
		  wend
		  
		  // transform polygon
		  
		  i = 1
		  while i < points.Ubound
		    tmpX = points(i)
		    tmpY = points(i + 1)
		    transformPoint tmpX, tmpY, matrix
		    points(i) = tmpX
		    points(i + 1) = tmpY
		    i = i + 2
		  wend
		  
		  // add reverse polygons to prevent closing line from drawing
		  
		  for i = points.Ubound - 3 downto 3 step 2
		    points.Append points(i)
		    points.Append points(i+1)
		  next i
		  
		  // fill
		  
		  if fill <> "none" then
		    g.ForeColor = determineColor(fill)
		    g.FillPolygon points
		  end if
		  
		  // stroke
		  
		  if (stroke <> "none") and (stroke <> "") and (strokeWidth > 0) then
		    g.ForeColor = determineColor(stroke)
		    g.PenWidth = strokeWidth
		    g.PenHeight = strokeWidth
		    g.DrawPolygon points
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_rect(node As XmlNode, g As Graphics, parentMatrix() As Double)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim style As JSONItem
		  Dim matrix() As Double
		  Dim points() As Integer
		  Dim i As Integer
		  Dim tmpX As Integer
		  Dim tmpY As Integer
		  Dim x As Double
		  Dim y As Double
		  Dim width As Double
		  Dim height As Double
		  Dim fill As String
		  Dim stroke As String
		  Dim strokeWidth As Double
		  
		  style = buildStyleItem(node)
		  matrix = buildTransformationMatrix(style.Lookup("transform", ""))
		  matrix = matrixMultiply(matrix, parentMatrix)
		  
		  x = style.LookupDouble("x")
		  y = style.LookupDouble("y")
		  width = style.LookupDouble("width")
		  height = style.LookupDouble("height")
		  fill = style.LookupString("fill", "#000000")
		  stroke = style.LookupString("stroke", "")
		  strokeWidth = style.LookupDouble("stroke-width", 1)
		  
		  if (width > 0) and (height > 0) then
		    
		    // build polygon
		    
		    points.Append 0
		    points.Append x
		    points.Append y
		    points.Append x
		    points.Append y + height - 1
		    points.Append x + width - 1
		    points.Append y + height - 1
		    points.Append x + width - 1
		    points.Append y
		    
		    // transform polygon
		    
		    i = 1
		    while i < points.Ubound
		      tmpX = points(i)
		      tmpY = points(i + 1)
		      transformPoint tmpX, tmpY, matrix
		      points(i) = tmpX
		      points(i + 1) = tmpY
		      i = i + 2
		    wend
		    
		    // fill
		    
		    if fill <> "none" then
		      g.ForeColor = determineColor(fill)
		      g.FillPolygon points
		    end if
		    
		    // stroke
		    
		    if (stroke <> "none") and (stroke <> "") and (strokeWidth > 0) then
		      g.ForeColor = determineColor(stroke)
		      g.PenWidth = strokeWidth
		      g.PenHeight = strokeWidth
		      g.DrawPolygon points
		    end if
		    
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_svg(node As XmlNode, g As Graphics, parentMatrix() As Double)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim style As JSONItem
		  Dim matrix() As Double
		  Dim i As Integer
		  Dim drawG As Graphics
		  
		  style = buildStyleItem(node)
		  matrix = buildTransformationMatrix(style.Lookup("transform", ""))
		  matrix = matrixMultiply(matrix, parentMatrix)
		  
		  if (node.GetAttribute("width") <> "") and (node.GetAttribute("height") <> "") then
		    drawG = g.Clip(0, 0, Val(node.GetAttribute("width")), Val(node.GetAttribute("height")))
		  else
		    drawG = g.Clip(0, 0, g.Width, g.Height)
		  end if
		  
		  i = 0
		  while i < node.ChildCount
		    renderNode node.Child(i), drawG, matrix
		    i = i + 1
		  wend
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_text(node As XmlNode, g As Graphics, parentMatrix() As Double)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim style As JSONItem
		  Dim matrix() As Double
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
		  matrix = matrixMultiply(matrix, parentMatrix)
		  
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
		      
		      'drawTransformedPicture g, element, matrix
		      g.DrawPicture element, x, y - g.TextAscent
		      
		    end if
		    
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub transformPoint(ByRef x As Double, ByRef y As Double, matrix() As Double)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim cx As Double
		  Dim cy As Double
		  Dim cw As Double
		  
		  cx = matrix(0) * x + matrix(1) * y + matrix(2)
		  cy = matrix(3) * x + matrix(4) * y + matrix(5)
		  cw = matrix(6) * x + matrix(7) * y + matrix(8)
		  
		  x = (cx / cw)
		  y = (cy / cw)
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub transformPoint(ByRef x As Integer, ByRef y As Integer, matrix() As Double)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim cx As Double
		  Dim cy As Double
		  Dim cw As Double
		  
		  cx = matrix(0) * x + matrix(1) * y + matrix(2)
		  cy = matrix(3) * x + matrix(4) * y + matrix(5)
		  cw = matrix(6) * x + matrix(7) * y + matrix(8)
		  
		  x = (cx / cw)
		  y = (cy / cw)
		  
		  
		End Sub
	#tag EndMethod


	#tag Constant, Name = DegToRad, Type = Double, Dynamic = False, Default = \"0.0174533", Scope = Private
	#tag EndConstant

	#tag Constant, Name = Pi, Type = Double, Dynamic = False, Default = \"3.1415927", Scope = Private
	#tag EndConstant


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
