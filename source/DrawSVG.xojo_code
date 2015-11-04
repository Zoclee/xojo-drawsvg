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
		Private Sub render_svg(node As XmlNode, g As Graphics, x As Integer, y As Integer)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  // todo
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
