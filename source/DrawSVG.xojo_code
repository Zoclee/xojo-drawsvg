#tag Module
Protected Module DrawSVG
	#tag Method, Flags = &h21
		Private Function buildStyleItem(node As XmlNode) As JSONItem
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim i As Integer
		  Dim j As Integer
		  Dim xAttr As XmlAttribute
		  Dim result as new JSONItem("{}")
		  Dim styleArr() As String
		  Dim itemArr() As String
		  
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
		  
		  if Len(svg) > 0 then
		    
		    try
		      
		      xdoc = new XmlDocument(svg)
		      renderNode(xdoc.FirstChild, g, x, y)
		      
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
		Private Sub renderNode(node As XmlNode, g As Graphics, xOffset As Integer, yOffset As Integer)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim foundNode As Boolean
		  
		  foundNode = true
		  
		  select case node.Name
		    
		  case "#comment"
		    // we ignore xml comments
		    
		  case "circle"
		    render_circle(node, g, xOffset, yOffset)
		    
		  case "desc"
		    // we ignore these tags
		    
		  case "rect"
		    render_rect(node, g, xOffset, yOffset)
		    
		  case "svg"
		    render_svg(node, g, xOffset, yOffset)
		    
		  case "text"
		    render_text(node, g, xOffset, yOffset)
		    
		  case else
		    foundNode = false
		    
		  end select
		  
		  // we only want to display error popups when debugging
		  
		  #if DebugBuild then
		    if not foundNode then
		      MsgBox "Unknown element: " + node.Name
		    end if
		  #endif
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_circle(node As XmlNode, g As Graphics, xOffset As Integer, yOffset As Integer)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim style As JSONItem
		  Dim cx As Double
		  Dim cy As Double
		  Dim r As Double
		  
		  style = buildStyleItem(node)
		  
		  cx = style.LookupDouble("cx")
		  cy = style.LookupDouble("cy")
		  r = style.LookupDouble("r")
		  
		  if r > 0 then
		    
		    // fill circle
		    
		    if style.HasName("fill") then
		      g.ForeColor = determineColor(style.Value("fill"))
		      g.FillOval xOffset + cx - r, _
		      yOffset + cy - r, _
		      r * 2, _
		      r * 2
		    end if
		    
		    // stroke circle
		    
		    if style.HasName("stroke") then
		      g.ForeColor = determineColor(style.Value("stroke"))
		      g.PenWidth = 1
		      g.PenHeight =g.PenWidth
		      g.DrawOval xOffset + cx - r, _
		      yOffset + cy - r, _
		      r * 2, _
		      r * 2
		    end if
		    
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_rect(node As XmlNode, g As Graphics, xOffset As Integer, yOffset As Integer)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim style As JSONItem
		  
		  style = buildStyleItem(node)
		  
		  if style.HasName("fill") then
		    if style.Value("fill") <> "none" then
		      g.ForeColor = determineColor(style.Value("fill"))
		      g.FillRect xOffset + Val(style.Lookup("x", "")), _
		      yOffset + Val(style.Lookup("y", "")), _
		      Val(style.Lookup("width", "")), _
		      Val(style.Lookup("height", ""))
		    end if
		  end if
		  
		  if style.HasName("stroke") then
		    g.ForeColor = determineColor(style.Value("stroke"))
		    g.PenWidth = Val(style.Lookup("stroke-width", "1"))
		    g.PenHeight =g.PenWidth
		    g.DrawRect xOffset + Val(style.Lookup("x", "")), _
		    yOffset + Val(style.Lookup("y", "")), _
		    Val(style.Lookup("width", "")), _
		    Val(style.Lookup("height", ""))
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_svg(node As XmlNode, g As Graphics, x As Integer, y As Integer)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim i As Integer
		  
		  i = 0
		  while i < node.ChildCount
		    renderNode node.Child(i), g, x, y
		    i = i + 1
		  wend
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_text(node As XmlNode, g As Graphics, xOffset As Integer, yOffset As Integer)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim style As JSONItem
		  Dim textStr As String
		  
		  style = buildStyleItem(node)
		  
		  g.ForeColor = determineColor(style.Lookup("fill", "000000"))
		  g.TextFont = style.Lookup("font-family", "Arial")
		  g.TextSize = style.Lookup("font-size", "")
		  
		  textStr = ""
		  if node.FirstChild <> nil then
		    if node.FirstChild.Name = "#text" then
		      textStr = Trim(node.FirstChild.Value)
		    end if
		  end if
		  
		  g.DrawString textStr, _
		  xOffset + Val(style.Lookup("x", "")), _
		  yOffset + Val(style.Lookup("y", "")) 
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
