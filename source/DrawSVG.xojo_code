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
		  Dim className As String
		  Dim classProperties As JSONItem
		  
		  result.EscapeSlashes = false
		  
		  i = 0
		  while i < node.AttributeCount
		    xAttr = node.GetAttributeNode(i)
		    
		    if xAttr.Name = "class" then
		      
		      className = Trim(Lowercase(node.GetCIAttribute(xAttr.Name)))
		      if mClasses.HasName(className) then
		        classProperties = mClasses.Value(className)
		        result.ApplyValues classProperties
		      end if
		      
		    elseif xAttr.Name = "style" then
		      
		      // process style attribute
		      
		      styleArr = node.GetCIAttribute(xAttr.Name).Split(";")
		      j = 0
		      while j <= styleArr.Ubound
		        itemArr = styleArr(j).Split(":")
		        if itemArr.Ubound = 1 then
		          result.Value(itemArr(0).Trim.Lowercase) = itemArr(1)
		        end if
		        j = j + 1
		      wend
		      
		    else
		      result.Value(xAttr.Name.Lowercase) = node.GetCIAttribute(xAttr.Name)
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
		        parms = parms.ReplaceAll(",", " ")
		        while parms.InStr(0, "  ") > 0 
		          parms = parms.ReplaceAll("  ", " ")
		        wend
		        strArr = parms.Split(" ")
		        
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
		  Dim colStr As String
		  Dim startPos As Integer
		  Dim endPos As Integer
		  Dim tmpStr As String
		  Dim tmpArr() As String
		  
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
		  
		  colStr = Lowercase(Trim(s))
		  
		  if ColorTable.HasKey(colStr) then
		    col = ColorTable.Value(colStr)
		  elseif left(colStr, 3) = "rgb" then
		    startPos = Instr(0, colStr, "(")
		    endPos = Instr(startPos + 1, colStr, ")")
		    if (startPos > 0) and (endPos > 0) then
		      tmpStr = Mid(colStr, startPos + 1, endPos - startPos - 1)
		      tmpArr = tmpStr.Split(",")
		      if tmpArr.Ubound = 2 then
		        col = RGB(Val(tmpArr(0)), Val(tmpArr(1)), Val(tmpArr(2)))
		      end if
		    end if
		  else
		    col = colorFromHex(colStr)
		  end if
		  
		  return col
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DrawSVG(Extends g As Graphics, svg As String, x As Integer, y As Integer, w1 As Integer = -10000, h1 As Integer = -10000)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim xdoc As XmlDocument
		  Dim i As Integer
		  Dim matrix() As Double
		  Dim mulMatrix() As Double
		  Dim drawG As Graphics
		  Dim w As Integer
		  Dim h As Integer
		  Dim wStr As String
		  Dim hStr As String
		  Dim viewbox As String
		  Dim viewboxArr() As String
		  Dim xScale As Double
		  Dim yScale As Double
		  Dim xOffset As Double
		  Dim yOffset As Double
		  Dim scale As Double
		  
		  if Len(svg) > 0 then
		    
		    try
		      
		      xdoc = new XmlDocument(svg)
		      
		      // clear classes
		      
		      mClasses = new JSONItem("{}")
		      
		      matrix = initIdentityMatrix()
		      
		      i = 0
		      while (i < xdoc.ChildCount) 
		        if xdoc.Child(i).Name = "svg" then
		          
		          // determine graphics context width and height
		          
		          w = 0
		          h = 0
		          
		          wStr = Trim(xdoc.Child(i).GetCIAttribute("width"))
		          if wStr <> "" then
		            if IsNumeric(wStr) then
		              w = Val(wStr)
		            elseif Right(wStr, 1) = "%" then
		              w = g.Width * (Val(Left(wStr, Len(wStr) - 1)) / 100)
		            end if
		          end if
		          
		          hStr = Trim(xdoc.Child(i).GetCIAttribute("height"))
		          if hStr <> "" then
		            if IsNumeric(hStr) then
		              h = Val(hStr)
		            elseif Right(hStr, 1) = "%" then
		              h = g.Height * (Val(Left(hStr, Len(hStr) - 1)) / 100)
		            end if
		          end if
		          
		          if w = 0 then
		            w = g.Width
		          end if
		          if h = 0 then
		            h = g.Height
		          end if
		          
		          // apply viewbox if there is one
		          
		          viewbox = Trim(xdoc.Child(i).GetCIAttribute("viewbox"))
		          if viewbox <> "" then
		            while viewbox.InStr(0, "  ") > 0 
		              viewbox = viewbox.ReplaceAll("  ", " ")
		            wend
		            viewboxArr = viewbox.Split(" ")
		            if viewboxArr.Ubound = 3 then
		              xScale = w / Val(viewboxArr(2)) 
		              yScale = h / Val(viewboxArr(3)) 
		              if xScale < yScale then
		                scale = xScale
		                xOffset = 0
		                yOffset = (h - (Val(viewboxArr(3))  * scale)) / 2
		              else
		                scale = yScale
		                xOffset = (w - (Val(viewboxArr(2))  * scale)) / 2
		                yOffset = 0
		              end if
		              mulMatrix = initTranslationMatrix(xOffset, yOffset)
		              matrix = matrixMultiply(matrix, mulMatrix)
		              mulMatrix = initScaleMatrix(scale, scale)
		              matrix = matrixMultiply(matrix, mulMatrix)
		              
		            end if
		          end if
		          
		          if (w1 > 0) and (h1 > 0) then
		            mulMatrix = initScaleMatrix(w1 / w, h1 / h)
		            matrix = matrixMultiply(matrix, mulMatrix)
		            drawG = g.Clip(x, y, w1, h1)
		          else
		            drawG = g.Clip(x, y, w, h)
		          end if
		          
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
		Private Sub DrawTransformedPicture(Extends g As Graphics, image As Picture, matrix() As Double)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  ' This routine is based on code written by Alain Bailleul.
		  ' www.alwaysbusycorner.com
		  
		  Dim srcWidth as Integer
		  Dim srcHeight as Integer 
		  Dim destinationQuadrilateral() as REALbasic.Point
		  Dim tmpX As Integer
		  Dim tmpY As Integer
		  Dim startX As Integer
		  Dim startY As Integer
		  Dim stopX As Integer
		  Dim stopY As Integer
		  Dim minXY as REALbasic.Point
		  Dim maxXY as REALbasic.Point
		  Dim srcRect(3) as REALbasic.Point
		  Dim transMatrix(8) As Double
		  Dim x As Integer
		  Dim y As Integer
		  Dim  factor, srcX, srcY as Double
		  Dim tgtPic as Picture
		  Dim srcRGB as RGBSurface
		  Dim tgtRGB as RGBSurface
		  Dim srcWidthM1 As Integer
		  Dim srcHeightM1 As Integer
		  Dim dx1, dy1, dx2, dy2 as Double 'coordinates of source points
		  Dim sx1, sy1, sx2, sy2 as Integer
		  Dim p1,p2,p3, p4 as Color ' temporary pixels
		  Dim r, gp , b, a as Integer
		  
		  srcWidth = image.Width
		  srcHeight = image.Height
		  
		  // determine destination quadrilateral using transformation matrix
		  
		  tmpX = 0
		  tmpY = 0
		  transformPoint(tmpX, tmpY, matrix)
		  destinationQuadrilateral.Append new REALbasic.Point(tmpX, tmpY)
		  
		  tmpX = srcWidth -1
		  tmpY = 0
		  transformPoint(tmpX, tmpY, matrix)
		  destinationQuadrilateral.Append new REALbasic.Point(tmpX, tmpY)
		  
		  tmpX = srcWidth -1
		  tmpY = srcHeight - 1
		  transformPoint(tmpX, tmpY, matrix)
		  destinationQuadrilateral.Append new REALbasic.Point(tmpX, tmpY)
		  
		  tmpX = 0
		  tmpY = srcHeight - 1
		  transformPoint(tmpX, tmpY, matrix)
		  destinationQuadrilateral.Append new REALbasic.Point(tmpX, tmpY)
		  
		  'get bounding rectangle of the quadrilateral
		  
		  GetBoundingRectangle destinationQuadrilateral, minXY, maxXY
		  
		  startX = minXY.X
		  startY = minXY.Y
		  stopX = maxXY.X
		  stopY = maxXY.Y
		  
		  'calculate tranformation matrix
		  
		  srcRect(0) = new REALbasic.Point(0,0)
		  srcRect(1) = new REALbasic.Point(srcWidth -1 ,0)
		  srcRect(2) = new REALbasic.Point(srcWidth - 1, srcHeight - 1)
		  srcRect(3) = new REALbasic.Point(0, srcHeight - 1)
		  transMatrix = MapQuadToQuad(destinationQuadrilateral, srcRect)
		  
		  tgtPic = new Picture(g.Width, g.Height)
		  srcRGB = image.RGBSurface
		  tgtRGB = tgtPic.RGBSurface
		  
		  srcWidthM1 = srcWidth - 1
		  srcHeightM1 = srcHeight - 1
		  
		  ' for each row
		  for y = startY to stopY
		    'for each pixel
		    for x = startX to stopX
		      factor = transMatrix(2) * x + transMatrix(5) * y + transMatrix(8)
		      srcX = ( transMatrix(0) * x + transMatrix(3) * y + transMatrix(6) ) / factor
		      srcY = ( transMatrix(1) * x + transMatrix(4) * y + transMatrix(7) ) / factor
		      if srcX >= 0 and srcY >= 0 and srcX< srcWidth and srcY < srcHeight then
		        sx1 = srcX
		        if sx1 = srcWidthM1 then
		          sx2 = sx1
		        else
		          sx2 = sx1 + 1
		        end if
		        dx1 = srcX - sx1
		        dx2 = 1.0 - dx1
		        
		        sy1 = srcY
		        if sy1 = srcHeightM1 then
		          sy2 = sy1
		        else
		          sy2 = sy1 + 1
		        end if
		        dy1 = srcY - sy1
		        dy2 = 1.0 - dy1
		        
		        ' copy the pixel from the source to the target using interpolation of 4 points
		        p1 = srcRGB.Pixel(sx1, sy1)
		        p2 = srcRGB.Pixel(sx2, sy1)
		        p3 = srcRGB.Pixel(sx1, sy2)
		        p4 = srcRGB.Pixel(sx2, sy2)
		        
		        r = dy2 * ( dx2 * ( p1.red ) + dx1 * ( p2.red ) ) + dy1 * ( dx2 * ( p3.red ) + dx1 * ( p4.red ) )
		        gp = dy2 * ( dx2 * ( p1.green ) + dx1 * ( p2.green ) ) + dy1 * ( dx2 * ( p3.green ) + dx1 * ( p4.green ) )
		        b = dy2 * ( dx2 * ( p1.blue ) + dx1 * ( p2.blue ) ) + dy1 * ( dx2 * ( p3.blue ) + dx1 * ( p4.blue ) )
		        a = dy2 * ( dx2 * ( p1.Alpha ) + dx1 * ( p2.Alpha ) ) + dy1 * ( dx2 * ( p3.Alpha ) + dx1 * ( p4.Alpha ) )
		        
		        tgtRGB.Pixel(x,y) = RGB(r,gp,b, a)
		      end if
		    next
		  next
		  
		  g.DrawPicture tgtPic, 0, 0
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub getBoundingRectangle(cloud() as REALbasic.Point, byref minXY as REALbasic.Point, byref maxXY as REALbasic.Point)
		  ' This routine is based on code written by Alain Bailleul.
		  ' www.alwaysbusycorner.com
		  
		  dim minX as integer = 10e6
		  dim maxX as integer = -10e6
		  dim minY as integer = 10e6
		  dim maxY as integer = -10e6
		  
		  dim i as integer
		  for i = 0 to UBound(cloud)
		    if cloud(i).x < minX then minX = cloud(i).x
		    if cloud(i).x > maxX then maxX = cloud(i).x
		    if cloud(i).y < minY then minY = cloud(i).y
		    if cloud(i).y > maxY then maxY = cloud(i).y
		  next
		  
		  minXY = new REALbasic.Point(minX, minY)
		  maxXY = new REALbasic.Point(maxX, maxY)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function GetCIAttribute(extends node As XmlNode, name As String) As String
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim i As Integer
		  Dim foundAttr As Boolean
		  Dim attr As XmlAttribute
		  Dim result As String
		  
		  result = ""
		  
		  i = 0
		  foundAttr = false
		  while (i <node.AttributeCount) and not foundAttr
		    attr = node.GetAttributeNode(i)
		    if attr.name = name then
		      result = attr.Value
		      foundAttr = true
		    end if
		    i = i + 1
		  wend
		  
		  return result
		End Function
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
		Private Function isScaleMatrix(matrix() As Double) As Boolean
		  Dim result As Boolean
		  
		  result = (matrix(1) = 0) and (matrix(2) = 0) and (matrix(3) = 0) and (matrix(5) = 0) and _
		  (matrix(6) = 0) and (matrix(7) = 0) and (matrix(8) = 1)
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function isTranslationMatrix(matrix() As Double) As Boolean
		  Dim result As Boolean
		  
		  result = (matrix(0) = 1) and (matrix(1) = 0) and (matrix(3) = 0) and (matrix(4) = 1) and _
		  (matrix(6) = 0) and (matrix(7) = 0) and (matrix(8) = 1)
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub loadCSS(styleData As String)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim className As String
		  Dim i As Integer
		  Dim ch As String
		  Dim dataLen As Integer
		  Dim state As Integer // 0 = next class, 1 = class name, 2 = property name, 3 = property value
		  Dim classProperties As JSONItem
		  Dim propName As String
		  Dim propValue As String
		  
		  dataLen = len(styleData)
		  state = 0 // next class
		  i = 1
		  while i <= dataLen
		    ch = Mid(styleData, i, 1)
		    
		    if Asc(ch) <= 32 then
		      // do nothing
		      
		    elseif ch = "{" then
		      classProperties = new JSONItem("{}")
		      propName = ""
		      state = 2 // property name
		      
		    elseif ch = "}" then
		      if propName <> "" then
		        classProperties.Value(Lowercase(propName)) = propValue
		      end if
		      mClasses.Value(Lowercase(Trim(className))) = classProperties
		      state = 0 // next class
		      
		    elseif ch = "." then
		      className = ""
		      state = 1 // class name
		      
		    elseif ch = ";" then
		      classProperties.Value(Lowercase(propName)) = propValue
		      propName = ""
		      propValue = ""
		      state = 2 // property name
		      
		    elseif ch = ":" then
		      propValue = ""
		      state = 3 // property value
		      
		    else
		      select case state
		      case 1 // class name
		        className = className + ch
		      case 2 // property name
		        propName = propName + ch
		      case 3 // property value
		        propValue = propValue + ch
		      end select
		    end if
		    
		    i = i + 1
		  wend
		  
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
		Private Function mapQuadToQuad(Quad() as REALbasic.Point) As Double()
		  ' This routine is based on code written by Alain Bailleul.
		  ' www.alwaysbusycorner.com
		  
		  dim sq(8) as Double
		  dim px, py as Double
		  
		  dim TOLERANCE as double = 1e-13
		  
		  px = quad(0).X - quad(1).X + quad(2).X - quad(3).X
		  py = quad(0).Y - quad(1).Y + quad(2).Y - quad(3).Y
		  
		  if ( ( px < TOLERANCE ) And ( px > -TOLERANCE ) And ( py < TOLERANCE ) And ( py > -TOLERANCE ) ) then
		    sq(0) = quad(1).X - quad(0).X
		    sq(3) = quad(2).X - quad(1).X
		    sq(6) = quad(0).X
		    
		    sq(1) = quad(1).Y - quad(0).Y
		    sq(4) = quad(2).Y - quad(1).Y
		    sq(7) = quad(0).Y
		    
		    sq(2) = 0.0
		    sq(5) = 0.0
		    sq(8) = 1.0
		  else
		    
		    dim dx1, dx2, dy1, dy2, del as Double
		    
		    dx1 = quad(1).X - quad(2).X
		    dx2 = quad(3).X - quad(2).X
		    dy1 = quad(1).Y - quad(2).Y
		    dy2 = quad(3).Y - quad(2).Y
		    
		    del = matrixDeterminant2x2( dx1, dx2, dy1, dy2 )
		    
		    if ( del = 0 ) then
		      return sq
		    end if
		    
		    sq(2) = matrixDeterminant2x2( px, dx2, py, dy2 ) / del
		    sq(5) = matrixDeterminant2x2( dx1, px, dy1, py ) / del
		    sq(8) = 1.0
		    
		    sq(0) = quad(1).X - quad(0).X + sq(2) * quad(1).X
		    sq(3) = quad(3).X - quad(0).X + sq(5) * quad(3).X
		    sq(6) = quad(0).X
		    
		    sq(1) = quad(1).Y - quad(0).Y + sq(2) * quad(1).Y
		    sq(4) = quad(3).Y - quad(0).Y + sq(5) * quad(3).Y
		    sq(7) = quad(0).Y
		  end if
		  
		  return sq
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function mapQuadToQuad(input() as REALbasic.Point, output() as REALbasic.Point) As Double()
		  ' This routine is based on code written by Alain Bailleul.
		  ' www.alwaysbusycorner.com
		  
		  Dim squareToInput(8) as Double = MapQuadToQuad(input)
		  dim squareToOutput(8) as Double = MapQuadToQuad(output)
		  
		  Return matrixMultiply(matrixAdjugate(squareToInput), squareToOutput)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function matrixAdjugate(a() as double) As double()
		  ' Calculates adjugate 3x3 matrix
		  
		  Dim b(8) as double
		  
		  b(0) = matrixDeterminant2x2( a(4), a(7), a(5), a(8) )
		  b(1) = matrixDeterminant2x2( a(7), a(1), a(8), a(2) )
		  b(2) = matrixDeterminant2x2( a(1), a(4), a(2), a(5) )
		  b(3) = matrixDeterminant2x2( a(5), a(8), a(3), a(6) )
		  b(4) = matrixDeterminant2x2( a(8), a(2), a(6), a(0) )
		  b(5) = matrixDeterminant2x2( a(2), a(5), a(0), a(3) )
		  b(6) = matrixDeterminant2x2( a(3), a(6), a(4), a(7) )
		  b(7) = matrixDeterminant2x2( a(6), a(0), a(7), a(1) )
		  b(8) = matrixDeterminant2x2( a(0), a(3), a(1), a(4) )
		  
		  return b
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function matrixDeterminant2x2(a as double, b as double, c as double, d as double) As double
		  ' Caclculates determinant of a 2x2 matrix
		  return ( a * d - b * c )
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function matrixMultiply(m1() As Double, m2() As Double) As Double()
		  Dim result(8) As Double
		  
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
		Private Sub process_style(node As XmlNode)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  Dim styleData As String
		  
		  select case node.GetCIAttribute("type")
		    
		  case "text/css"
		    styleData = node.FirstChild.Value
		    loadCSS(styleData)
		    
		  case else
		    break
		    
		  end select
		End Sub
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
		      
		    case "path"
		      render_path(node, g, parentMatrix)
		      
		    case "polygon"
		      render_polygon(node, g, parentMatrix)
		      
		    case "polyline"
		      render_polyline(node, g, parentMatrix)
		      
		    case "rect"
		      render_rect(node, g, parentMatrix)
		      
		    case "style"
		      process_style(node)
		      
		    case "svg"
		      render_svg(node, g, parentMatrix)
		      
		    case "text"
		      render_text(node, g, parentMatrix)
		      
		    case "title"
		      // we ignore these tags
		      
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
		  matrix = matrixMultiply(parentMatrix, matrix)
		  
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
		  matrix = matrixMultiply(parentMatrix, matrix)
		  
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
		  matrix = matrixMultiply(parentMatrix, matrix)
		  
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
		  matrix = matrixMultiply(parentMatrix, matrix)
		  
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
		Private Sub render_path(node As XmlNode, g As Graphics, parentMatrix() As Double)
		  ' This project is a {Zoclee}™ open source initiative.
		  ' www.zoclee.com
		  
		  ' This routine is based on code written by Dr Brian R Gaines .
		  
		  Dim style As JSONItem
		  Dim matrix() As Double
		  Dim i As Integer
		  Dim fill As String
		  Dim stroke As String
		  Dim strokeWidth As Double
		  Dim fs as new FigureShape
		  Dim cs As CurveShape
		  Dim d As String
		  Dim dLen As Integer
		  Dim ch As String
		  Dim penX As Double
		  Dim penY As Double
		  Dim tmpX As Double
		  Dim tmpY As Double
		  Dim path() As String
		  Dim continueLineto As Boolean
		  Dim prevControlX As Double
		  Dim prevControlY As Double
		  Dim grp As Group2D
		  Dim itemFill As Double
		  Dim itemFillColor As Color
		  Dim itemStroke As Double
		  Dim itemStrokeColor As Color
		  Dim prevClosed As Boolean
		  Dim currentStart As Integer
		  Dim currentCommand As String
		  Dim additionalPath() As String
		  
		  style = buildStyleItem(node)
		  matrix = buildTransformationMatrix(style.Lookup("transform", ""))
		  matrix = matrixMultiply(parentMatrix, matrix)
		  
		  fill = style.LookupString("fill", "#000000")
		  stroke = style.LookupString("stroke", "")
		  strokeWidth = style.LookupDouble("stroke-width", 1)
		  
		  // fill
		  
		  if fill <> "none" then
		    itemFill = 100
		    itemFillColor = determineColor(fill)
		  else
		    itemFill = 0
		  end if
		  
		  // stroke
		  
		  if (stroke <> "none") and (stroke <> "") and (strokeWidth > 0) then
		    itemStroke = 100
		    itemStrokeColor = determineColor(stroke)
		  else
		    itemStroke = 0
		  end if
		  
		  // build figure shape
		  
		  penX = 0
		  penY = 0
		  prevClosed = false
		  
		  d = Trim(style.LookupString("d", ""))
		  d = d.ReplaceAll(",", " ")
		  dLen = Len(d)
		  
		  Redim path(-1)
		  path.Append ""
		  i = 1
		  while i <= dLen
		    ch = Mid(d, i, 1)
		    
		    if ch = " " then
		      
		      if path(path.Ubound) <> "" then
		        path.Append ""
		      end if
		      
		    elseif ch = "-" then
		      
		      if path(path.Ubound) <> "" then
		        path.Append "-"
		      else
		        path(path.Ubound) = ch
		      end if
		      
		    elseif not IsNumeric(ch) and (ch <> ".") and (ch <> "-") then
		      
		      if path(path.Ubound) <> "" then
		        path.Append ch
		      else
		        path(path.Ubound) = ch
		      end if
		      path.Append ""
		      
		    else
		      
		      path(path.Ubound) = path(path.Ubound) + ch
		      
		    end if
		    i = i + 1
		  wend
		  
		  if path(path.Ubound) = "" then
		    path.Remove(path.Ubound)
		  end if
		  
		  // prep path to hide any possible artifacts created by multiple closed paths in a single path instruction
		  
		  i = 0
		  currentCommand = ""
		  while i <= path.Ubound
		    select case path(i)
		    case "M", "m"
		      if currentCommand <> path(i) then
		        additionalPath.Append path(i)
		      end if
		      additionalPath.Append path(i + 1)
		      additionalPath.Append path(i + 2)
		      currentCommand = path(i)
		      while (i <= path.Ubound) and path(i) <> "z"
		        i = i + 1
		      wend
		      if i <= path.Ubound then
		        path.Insert i, "L"
		        path.Insert i + 1, additionalPath(additionalPath.Ubound - 1)
		        path.Insert i + 2, additionalPath(additionalPath.Ubound)
		      end if
		      i = i + 1
		    case else 
		      i = i + 1
		    end select
		    
		  wend
		  
		  if additionalPath.Ubound > 3 then
		    additionalPath.Append "z"
		    i = 0
		    while i <= additionalPath.Ubound
		      path.Insert(i, additionalPath(i))
		      i = i + 1
		    wend
		  end if
		  
		  // process (draw) path
		  
		  i = 0
		  while i <= path.Ubound
		    
		    if StrComp(path(i), "A", 0) = 0 then // absolute elliptical arc
		      // todo
		      
		    elseif StrComp(path(i), "a", 0) = 0 then // relative elliptical arc
		      // todo
		      
		    elseif StrComp(path(i), "C", 0) = 0 then // absolute curveto
		      cs = new CurveShape
		      fs.Append cs
		      tmpX = penX
		      tmpY = penY
		      transformPoint tmpX, tmpY, matrix
		      cs.X = tmpX
		      cs.Y = tmpY
		      cs.Order = 2
		      i = i + 1
		      tmpX = Val(path(i))
		      i = i + 1
		      tmpY = Val(path(i))
		      transformPoint tmpX, tmpY, matrix
		      cs.ControlX(0) = tmpX
		      cs.ControlY(0) = tmpY
		      i = i + 1
		      tmpX = Val(path(i))
		      i = i + 1
		      tmpY = Val(path(i))
		      transformPoint tmpX, tmpY, matrix
		      cs.ControlX(1) = tmpX
		      cs.ControlY(1) = tmpY
		      prevControlX = tmpX
		      prevControlY = tmpY
		      i = i + 1
		      tmpX = Val(path(i))
		      i = i + 1
		      tmpY = Val(path(i))
		      penX = tmpX
		      penY = tmpY
		      transformPoint tmpX, tmpY, matrix
		      cs.X2 = tmpX
		      cs.Y2 = tmpY
		      
		    elseif StrComp(path(i), "c", 0) = 0 then // relative curveto
		      cs = new CurveShape
		      fs.Append cs
		      tmpX = penX
		      tmpY = penY
		      transformPoint tmpX, tmpY, matrix
		      cs.X = tmpX
		      cs.Y = tmpY
		      cs.Order = 2
		      i = i + 1
		      tmpX = penX + Val(path(i))
		      i = i + 1
		      tmpY = penY + Val(path(i))
		      transformPoint tmpX, tmpY, matrix
		      cs.ControlX(0) = tmpX
		      cs.ControlY(0) = tmpY
		      i = i + 1
		      tmpX = penX + Val(path(i))
		      i = i + 1
		      tmpY = penY + Val(path(i))
		      transformPoint tmpX, tmpY, matrix
		      cs.ControlX(1) = tmpX
		      cs.ControlY(1) = tmpY
		      prevControlX = tmpX
		      prevControlY = tmpY
		      i = i + 1
		      tmpX = penX + Val(path(i))
		      i = i + 1
		      tmpY = penY + Val(path(i))
		      penX = tmpX
		      penY = tmpY
		      transformPoint tmpX, tmpY, matrix
		      cs.X2 = tmpX
		      cs.Y2 = tmpY
		      
		    elseif StrComp(path(i), "H", 0) = 0 then // absolute horizontal lineto
		      cs =new CurveShape
		      fs.Append cs
		      tmpX = penX
		      tmpY = penY
		      transformPoint tmpX, tmpY, matrix
		      cs.X = tmpX
		      cs.Y = tmpY
		      i = i + 1
		      tmpX = Val(path(i))
		      penX = tmpX
		      tmpY = penY
		      transformPoint tmpX, tmpY, matrix
		      cs.X2 = tmpX
		      cs.Y2 = tmpY
		      
		    elseif StrComp(path(i), "h", 0) = 0 then // relative horizontal lineto
		      cs =new CurveShape
		      fs.Append cs
		      tmpX = penX
		      tmpY = penY
		      transformPoint tmpX, tmpY, matrix
		      cs.X = tmpX
		      cs.Y = tmpY
		      i = i + 1
		      tmpX = penX + Val(path(i))
		      penX = tmpX
		      tmpY = penY
		      transformPoint tmpX, tmpY, matrix
		      cs.X2 = tmpX
		      cs.Y2 = tmpY
		      
		    elseif StrComp(path(i), "Q", 0) = 0 then // absolute quadratic Bézier curveto
		      // todo
		      
		    elseif StrComp(path(i), "q", 0) = 0 then // relative quadratic Bézier curveto
		      // todo
		      
		    elseif StrComp(path(i), "L", 0) = 0 then // absolute lineto
		      cs =new CurveShape
		      fs.Append cs
		      tmpX = penX
		      tmpY = penY
		      transformPoint tmpX, tmpY, matrix
		      cs.X = tmpX
		      cs.Y = tmpY
		      i = i + 1
		      tmpX = Val(path(i))
		      i = i + 1
		      tmpY = Val(path(i))
		      penX = tmpX
		      penY = tmpY
		      transformPoint tmpX, tmpY, matrix
		      cs.X2 = tmpX
		      cs.Y2 = tmpY
		      
		    elseif StrComp(path(i), "l", 0) = 0 then // relative lineto
		      cs =new CurveShape
		      fs.Append cs
		      tmpX = penX
		      tmpY = penY
		      transformPoint tmpX, tmpY, matrix
		      cs.X = tmpX
		      cs.Y = tmpY
		      i = i + 1
		      tmpX = penX + Val(path(i))
		      i = i + 1
		      tmpY = penY + Val(path(i))
		      penX = tmpX
		      penY = tmpY
		      transformPoint tmpX, tmpY, matrix
		      cs.X2 = tmpX
		      cs.Y2 = tmpY
		      
		    elseif StrComp(path(i), "M", 0) = 0 then // absolute move
		      if (fs.Count > 0) and not prevclosed then
		        fs.Fill = itemFill
		        fs.FillColor = itemFillColor
		        fs.Border = itemStroke
		        fs.BorderColor = itemStrokeColor
		        fs.BorderWidth = strokeWidth
		        g.DrawObject fs
		        fs = new FigureShape()
		      end if
		      i = i + 1
		      tmpX = Val(path(i))
		      i = i + 1
		      tmpY = Val(path(i))
		      
		      penX = tmpX
		      penY = tmpY
		      
		      // apply  implicit lineto commands
		      
		      do
		        continueLineto = false
		        if i < (path.Ubound - 1) then
		          if IsNumeric(path(i + 1)) then
		            cs =new CurveShape
		            fs.Append cs
		            tmpX = penX
		            tmpY = penY
		            transformPoint tmpX, tmpY, matrix
		            cs.X = tmpX
		            cs.Y = tmpY
		            i = i + 1
		            tmpX = Val(path(i))
		            i = i + 1
		            tmpY = Val(path(i))
		            penX = tmpX
		            penY = tmpY
		            transformPoint tmpX, tmpY, matrix
		            cs.X2 = tmpX
		            cs.Y2 = tmpY
		            continueLineto = true
		          end if
		        end if
		      loop until (i > path.Ubound) or not continueLineto
		      
		    elseif StrComp(path(i), "m", 0) = 0 then // relative move
		      
		      if (fs.Count > 0) and not prevClosed then
		        fs.Fill = itemFill
		        fs.FillColor = itemFillColor
		        fs.Border = itemStroke
		        fs.BorderColor = itemStrokeColor
		        fs.BorderWidth = strokeWidth
		        g.DrawObject fs
		        fs = new FigureShape()
		      end if
		      
		      i = i + 1
		      tmpX = Val(path(i))
		      i = i + 1
		      tmpY = Val(path(i))
		      
		      penX = penX + tmpX
		      penY = penY + tmpY
		      
		      // apply  implicit lineto commands
		      
		      do
		        continueLineto = false
		        if i < (path.Ubound - 1) then
		          if IsNumeric(path(i + 1)) then
		            cs =new CurveShape
		            fs.Append cs
		            tmpX = penX
		            tmpY = penY
		            transformPoint tmpX, tmpY, matrix
		            cs.X = tmpX
		            cs.Y = tmpY
		            i = i + 1
		            tmpX = Val(path(i))
		            i = i + 1
		            tmpY = Val(path(i))
		            penX = penX + tmpX
		            penY = penY +tmpY
		            tmpX = penX
		            tmpY = penY
		            transformPoint tmpX, tmpY, matrix
		            cs.X2 = tmpX
		            cs.Y2 = tmpY
		            continueLineto = true
		          end if
		        end if
		      loop until (i > path.Ubound) or not continueLineto
		      
		    elseif StrComp(path(i), "S", 0) = 0 then // absolute smooth curveto
		      cs = new CurveShape
		      fs.Append cs
		      tmpX = penX
		      tmpY = penY
		      transformPoint tmpX, tmpY, matrix
		      cs.X = tmpX
		      cs.Y = tmpY
		      cs.Order = 2
		      cs.ControlX(0) = (tmpX - prevControlX)  + tmpX
		      cs.ControlY(0) = (tmpY - prevControlY)  + tmpY
		      i = i + 1
		      tmpX = Val(path(i))
		      i = i + 1
		      tmpY = Val(path(i))
		      transformPoint tmpX, tmpY, matrix
		      cs.ControlX(1) = tmpX
		      cs.ControlY(1) = tmpY
		      i = i + 1
		      tmpX = Val(path(i))
		      i = i + 1
		      tmpY = Val(path(i))
		      penX = tmpX
		      penY = tmpY
		      transformPoint tmpX, tmpY, matrix
		      cs.X2 = tmpX
		      cs.Y2 = tmpY
		      
		    elseif StrComp(path(i), "s", 0) = 0 then // relative smooth curveto
		      cs = new CurveShape
		      fs.Append cs
		      tmpX = penX
		      tmpY = penY
		      transformPoint tmpX, tmpY, matrix
		      cs.X = tmpX
		      cs.Y = tmpY
		      cs.Order = 2
		      cs.ControlX(0) = (tmpX - prevControlX)  + tmpX
		      cs.ControlY(0) = (tmpY - prevControlY)  + tmpY
		      i = i + 1
		      tmpX = penX + Val(path(i))
		      i = i + 1
		      tmpY = penY + Val(path(i))
		      transformPoint tmpX, tmpY, matrix
		      cs.ControlX(1) = tmpX
		      cs.ControlY(1) = tmpY
		      i = i + 1
		      tmpX = penX + Val(path(i))
		      i = i + 1
		      tmpY = penY + Val(path(i))
		      penX = tmpX
		      penY = tmpY
		      transformPoint tmpX, tmpY, matrix
		      cs.X2 = tmpX
		      cs.Y2 = tmpY
		      
		    elseif StrComp(path(i), "T", 0) = 0 then // absolute smooth quadratic Bézier curveto
		      // todo
		      
		    elseif StrComp(path(i), "t", 0) = 0 then // relative smooth quadratic Bézier curveto
		      // todo
		      
		    elseif StrComp(path(i), "V", 0) = 0 then // absolute vertical lineto
		      cs =new CurveShape
		      fs.Append cs
		      tmpX = penX
		      tmpY = penY
		      transformPoint tmpX, tmpY, matrix
		      cs.X = tmpX
		      cs.Y = tmpY
		      tmpX = penX
		      i = i + 1
		      tmpY = Val(path(i))
		      penY = tmpY
		      transformPoint tmpX, tmpY, matrix
		      cs.X2 = tmpX
		      cs.Y2 = tmpY
		      
		    elseif StrComp(path(i), "v", 0) = 0 then // relative vertical lineto
		      cs =new CurveShape
		      fs.Append cs
		      tmpX = penX
		      tmpY = penY
		      transformPoint tmpX, tmpY, matrix
		      cs.X = tmpX
		      cs.Y = tmpY
		      tmpX = penX
		      i = i + 1
		      tmpY = penY + Val(path(i))
		      penY = tmpY
		      transformPoint tmpX, tmpY, matrix
		      cs.X2 = tmpX
		      cs.Y2 = tmpY
		      
		    elseif path(i) = "z" then // close path
		      prevClosed = true
		      
		    else
		      // todo
		      
		    end if
		    
		    if path(i) <> "z" then
		      prevClosed = false
		    end if
		    
		    i = i + 1
		  wend
		  
		  if fs.Count > 0 then
		    
		    if (itemFill = 100) and (fs.Count > 1) then
		      
		      fs.Fill = itemFill
		      fs.FillColor = itemFillColor
		      fs.Border = itemStroke
		      fs.BorderColor = itemStrokeColor
		      fs.BorderWidth = strokeWidth
		      g.DrawObject fs
		      
		    else
		      
		      grp = new Group2D
		      
		      i = 0
		      while i < fs.Count
		        grp.Append fs.Item(i)
		        
		        grp.Item(i).Fill = itemFill
		        grp.Item(i).FillColor = itemFillColor
		        grp.Item(i).Border = itemStroke
		        grp.Item(i).BorderColor = itemStrokeColor
		        grp.Item(i).BorderWidth = strokeWidth
		        
		        i = i + 1
		      wend
		      
		      g.DrawObject grp
		      
		    end if
		    
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
		  matrix = matrixMultiply(parentMatrix, matrix)
		  
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
		  matrix = matrixMultiply(parentMatrix, matrix)
		  
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
		  matrix = matrixMultiply(parentMatrix, matrix)
		  
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
		  
		  style = buildStyleItem(node)
		  matrix = buildTransformationMatrix(style.Lookup("transform", ""))
		  matrix = matrixMultiply(parentMatrix, matrix)
		  
		  i = 0
		  while i < node.ChildCount
		    renderNode node.Child(i), g, matrix
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
		  Dim mulMatrix() As Double
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
		  matrix = matrixMultiply(parentMatrix, matrix)
		  
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
		      
		      mulMatrix = initTranslationMatrix(x, y - g.TextAscent)
		      matrix = matrixMultiply(matrix, mulMatrix)
		      
		      strShape.FillColor = determineColor(fill)
		      strShape.TextFont = g.TextFont
		      strShape.TextUnit = g.TextUnit
		      strShape.TextSize = g.TextSize
		      select case style.Lookup("text-anchor", "start")
		      case "end"
		        strShape.HorizontalAlignment = StringShape.Alignment.Right
		      case "middle"
		        strShape.HorizontalAlignment = StringShape.Alignment.Center
		      case else
		        strShape.HorizontalAlignment = StringShape.Alignment.Left
		      end select
		      strShape.VerticalAlignment = StringShape.Alignment.Top
		      strShape.Text = textStr
		      
		      // to speed up rendering, we only use DrawTransformedPicture when needed
		      
		      if isTranslationMatrix(matrix) then
		        g.DrawObject strShape, matrix(2), matrix(5)
		      else
		        element = new Picture(g.StringWidth(textStr), g.TextHeight)
		        eg = element.Graphics
		        
		        eg.DrawObject strShape, _
		        0, _
		        0
		        
		        g.DrawTransformedPicture element, matrix
		      end if
		      
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


	#tag Property, Flags = &h21
		Private mClasses As JSONItem
	#tag EndProperty


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
