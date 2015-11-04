#tag Module
Protected Module DrawSVG
	#tag Method, Flags = &h0
		Sub DrawSVG(Extends g As Graphics, svg As String, x As Integer, y As Integer)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim xdoc As XmlDocument
		  
		  try
		    
		    xdoc = new XmlDocument(svg)
		    renderNode(xdoc.FirstChild, g, x, y)
		    
		  catch
		    // invalid xml, so we won't be rendering anything
		    
		  end try
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub renderNode(node As XmlNode, g As Graphics, x As Integer, y As Integer)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim foundNode As Boolean
		  
		  foundNode = true
		  
		  select case node.Name
		    
		  case "circle"
		    render_circle(node, g, x, y)
		    
		  case "svg"
		    render_svg(node, g, x, y)
		    
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
		Private Sub render_circle(node As XmlNode, g As Graphics, x As Integer, y As Integer)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim cx As Double
		  Dim cy As Double
		  Dim r As Double
		  Dim fill As String
		  Dim stroke As String
		  
		  cx = Val(node.GetAttribute("cx"))
		  cy = Val(node.GetAttribute("cy"))
		  r = Val(node.GetAttribute("r"))
		  fill = node.GetAttribute("fill")
		  stroke = node.GetAttribute("fill")
		  
		  if r > 0 then
		    
		    // fill circle
		    
		    if fill <> "" then
		      g.ForeColor = &cff0000
		      g.FillOval (cx - r), (cy - r), r * 2, r * 2
		    end if
		    
		    // stroke circle
		    
		    if stroke <> "" then
		      g.ForeColor = &c0000ff
		      g.DrawOval (cx - r), (cy - r), r * 2, r * 2
		    end if
		    
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_svg(node As XmlNode, g As Graphics, x As Integer, y As Integer)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim i As Integer
		  
		  i = 0
		  while i <= node.ChildCount
		    renderNode node.Child(i), g, x, y
		    i = i + 1
		  wend
		  
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
