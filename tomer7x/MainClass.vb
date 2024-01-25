Imports System.Windows.Forms
Imports Autodesk.AutoCAD.ApplicationServices
Imports Autodesk.AutoCAD.DatabaseServices
Imports Autodesk.AutoCAD.EditorInput
Imports Autodesk.AutoCAD.Geometry
Imports Autodesk.AutoCAD.Runtime
Imports netDxf
Imports System.IO



Namespace MyNamespace

    Public Class MainClass
        <CommandMethod("matrix_help")>
        Public Shared Sub MatrixHelp()
            Dim TextString As String = $"{vbLf}MatrixToLayout{vbLf}================{vbLf}{vbLf}Requirments{vbLf}-------------------{vbLf}1. Layers names must be: ""P_Names"" ""Quantity"" ""Length"" ""Width""{vbLf}2. The user must use the command ""DLI"" (DIMLINEAR) to dim the ""Length"" and ""Width"".{vbLf}3. The matrix should be a block created by 10 columns and each rectangle must be 6000x3000.{vbLf}{vbLf}Installation{vbLf}-------------------{vbLf}{vbLf}1. Browse to your autocad folder. (something like: C:\program files\Autodesk\AUTOCAD2023){vbLf}2. click install.{vbLf}3. add the files: ""PAGENUMBERS.LSP"" and ""AUTO_LOAD_DLL.lsp"" (from the path: C:\program files\Autodesk\YOUR_AUTOCAD_VERSION\support) to your Startup suite using ""appload"".{vbLf}4. restart your autocad and enjoy!{vbLf}{vbLf}Usage{vbLf}-------------------{vbLf}{vbLf}1. ""MAT-TO-LAYOUTS"" - Select the matrix, and press enter, layouts will be created.{vbLf}2. ""REO"" - Reorganize the (#num) pages of the layouts.{vbLf}3. ""PAGESETUP"" - If the layouts sheets are not well-organized, use the command and select 'TH_A3 PDF' and then click 'modify', ensuring that 'Center the Plot' and 'Fit to Paper' options are checked.{vbLf}"
            Dim doc As Document = Autodesk.AutoCAD.ApplicationServices.Application.DocumentManager.MdiActiveDocument
            Dim ed As Editor = doc.Editor
            ed.WriteMessage(TextString)
        End Sub
        <CommandMethod("MAT-TO-LAYOUT")>
        Public Shared Sub MatrixToLayout()
            'Dim endDate As New DateTime(2023, 8, 15)
            'Dim currentDate As DateTime = DateTime.Now.Date
            'If currentDate > endDate Then
            'MessageBox.Show("it looks like you need to call tomer.")
            'Return
            'End If
            Const VPCenterX As Double = 210.6893
            Const VPCenterY As Double = 172.6984
            Const VPCenterZ As Double = 0
            Const VPHeight As Double = 234
            Const VPWidth As Double = 396
            Dim savei As Integer = 0
            Dim LayersMissing As String = ""
            Dim VPCenter As Point3d = New Point3d(VPCenterX, VPCenterY, VPCenterZ)
            Dim LayoutCount As Integer = 0
            Dim PageNumber As Integer = 0
            Dim BlocksList As List(Of String) = BlocksClass.GetBlockNames
            Dim VerifyFlag As Boolean = True
            Dim blkname As String = "TH-Template"
            Dim doc As Document = Autodesk.AutoCAD.ApplicationServices.Application.DocumentManager.MdiActiveDocument
            Dim db As Database = doc.Database
            Dim ed As Editor = doc.Editor
            Dim LISPPath As String
            Dim LISPName As String = "PAGENUMBERS"
            Dim DrawingName As String = Left(db.Filename, Len(db.Filename) - 4)
            Dim DrawingPath As String = $"{Path.GetDirectoryName(db.Filename)}\"
            Dim csvFile As String = $"{DrawingName}.csv"

            Try
                LISPPath = HostApplicationServices.Current.FindFile(LISPName & Convert.ToString(".lsp"), db, FindFileHint.[Default])
                LISPPath = LISPPath.Replace("\", "/")
            Catch ex As Exception
                'MessageBox.Show(ex.Message)  
            End Try

            Dim FileName = "TH-Template.dwg"
            If BlocksList.Contains(blkname) = False Then
                ReplaceBlock(blkname)
            End If

            BlocksList = BlocksClass.GetBlockNames

            If BlocksList.Contains(blkname) = False Then
                Autodesk.AutoCAD.ApplicationServices.Application.ShowAlertDialog($"File ""{FileName}"" not found.{vbLf}Aborting.")
                Return
            End If

            'End While
            'End If
            Dim vals As TypedValue() = New TypedValue() {New TypedValue(CInt(DxfCode.Start), "INSERT")}
            Dim pso As PromptSelectionOptions = New PromptSelectionOptions
            pso.MessageForAdding = vbLf & "Select block matrix : "
            pso.SinglePickInSpace = True

            Dim res As PromptSelectionResult = ed.GetSelection(pso, New SelectionFilter(vals))
            If res.Status <> PromptStatus.OK Then Return

            Dim LayoutList As List(Of String) = LayoutsClass.GetLayoutList()
            Dim EndCell As Integer

            Dim PKO As PromptKeywordOptions = New PromptKeywordOptions($"{vbLf}Delete all layouts? [Yes/No] ", "Yes No")
            PKO.Keywords.Default = "Yes"

            Dim KeywordResult = ed.GetKeywords(PKO)

            If KeywordResult.Status <> PromptStatus.OK Then Return

            If KeywordResult.StringResult = "Yes" Then
                For Each layout In LayoutList
                    If layout <> "Model" Then
                        LayoutsClass.DeleteLayout(layout)
                    End If
                Next
            End If
            Dim writelineflag As Boolean
            Dim header As String = $"Page,Panel Name,Length,Width,Quantity"
            If System.IO.File.Exists(csvFile) Then
                If KeywordResult.StringResult = "Yes" Then
                    writelineflag = False
                    System.IO.File.Delete(csvFile)
                Else
                    writelineflag = True
                    header = ""
                    PageNumber = LayoutsClass.GetLayoutList.Count - 1
                End If
            Else
                writelineflag = False
            End If
            Dim Writer As New System.IO.StreamWriter(csvFile, True)
            Using trans As Transaction = doc.TransactionManager.StartTransaction()
                Try
                    'if flag is true it's mean that we want to add more lines, pages must start from the last one

                    If writelineflag = False Then Writer.WriteLine(header)


                    For Each id As ObjectId In res.Value.GetObjectIds()

                        Dim mblock As BlockReference = CType(trans.GetObject(id, OpenMode.ForRead), BlockReference)
                        'Dim x = mblock.DynamicBlockTableRecord.GetObject(OpenMode.ForRead)
                        Dim bname As String = mblock.Name
                        Dim inspt As Point3d = mblock.Position
                        'If bname = "NET" Then
                        'EndCell = 110
                        'ElseIf bname = "GRID" Then
                        'EndCell = 120
                        'Else
                        'ed.WriteMessage($"{vbLf}Selected matrix is not valid. Aborting.")
                        ''trans.Abort()
                        'Return
                        'End If
                        EndCell = 1000

                        For i = 1 To EndCell
                            savei = i
                            Dim currentboundary = GetCurrentMatrixBoundary(inspt, i)
                            Dim StringValues As List(Of String) = New List(Of String)
                            Dim LayerValues As List(Of String) = New List(Of String)
                            ZoomToWindow(currentboundary)
                            Dim dxfDoc As New DxfDocument()

                            Dim psop As TypedValue() = New TypedValue() {New TypedValue(CInt(DxfCode.Start), "TEXT,DIMENSION"), New TypedValue(CInt(DxfCode.LayerName), "P_Names,Quantity,Length,Width")}
                            Dim psr As PromptSelectionResult = ed.SelectCrossingPolygon(currentboundary, New SelectionFilter(psop))
                            If psr.Status <> PromptStatus.OK Then Exit For

                            Dim ids = psr.Value.GetObjectIds()
                            For Each id1 In ids
                                Dim dbobj As DBObject = trans.GetObject(id1, OpenMode.ForRead)
                                'Dim str As String = obj.GetType().Name
                                'Dim dbText = TryCast(obj, DBText)

                                Select Case dbobj.GetType()

                                    Case GetType(DBText)
                                        Dim Text As DBText = DirectCast(dbobj, DBText)
                                        Dim ObjString As String = Text.TextString
                                        Dim LayerName As String = Text.Layer



                                        LayerValues.Add(LayerName)
                                        StringValues.Add(ObjString)
                                        'ed.WriteMessage(vbLf & i.ToString & " " & ObjString & " " & LayerName)
                                        Dim textEntity As New netDxf.Entities.Text(ObjString, New Vector2(0, 0), 12)
                                        dxfDoc.Entities.Add(textEntity)



                                        If LayerName = "P_Names" And LayerValues.Contains(ObjString) Then
                                            ed.WriteMessage(vbLf & "Error! " & ObjString & " has duplicate value! Aborting.")
                                            Exit For
                                        End If

                                        'If LayerName <> "Quantity" Then
                                        'Dim dxfDoc As New DxfDocument()
                                        'dxfDoc.Save(ObjString & ".dxf".ToString)
                                        'dxfDoc.Save($"{DrawingPath}{ObjString}.dxf")
                                        'End If


                                    Case GetType(RotatedDimension)
                                        Dim RotDim As RotatedDimension = DirectCast(dbobj, RotatedDimension)
                                        Dim ObjValue As String = Math.Round(RotDim.Measurement).ToString
                                        Dim layername As String = RotDim.Layer
                                        LayerValues.Add(layername)
                                        StringValues.Add(ObjValue)
                                        'Dim dimensionEntity As New Dimension(RotDim.Measurement, New Vector2(0, 0))
                                        'dxfDoc.Entities.Add(dimensionEntity)
                                        'ed.WriteMessage(vbLf & i.ToString & " " & ObjValue & " " & layername)

                                End Select

                                'dbobj.Dispose()
                            Next
                            If LayerValues.Contains("P_Names") = False Or LayerValues.Contains("Quantity") = False Or LayerValues.Contains("Width") = False Or LayerValues.Contains("Length") = False Then
                                If LayerValues.Contains("P_Names") = True Or LayerValues.Contains("Quantity") = True Or LayerValues.Contains("Width") = True Or LayerValues.Contains("Length") = True Then
                                    If LayerValues.Contains("P_Names") = False Then
                                        LayersMissing &= "'P_Names' "
                                    End If
                                    If LayerValues.Contains("Quantity") = False Then
                                        LayersMissing &= "'Quantity' "
                                    End If
                                    If LayerValues.Contains("Width") = False Then
                                        LayersMissing &= "'Width' "
                                    End If
                                    If LayerValues.Contains("Length") = False Then
                                        LayersMissing &= "'Length'"
                                    End If
                                End If
                                Exit For
                            End If
                            Dim NameValue As String = ""
                            Dim QuantityValue As String = ""
                            Dim WidthValue As String = ""
                            Dim LengthValue As String = ""

                            For j = 0 To LayerValues.Count - 1
                                Select Case LayerValues(j)
                                    Case "P_Names"
                                        NameValue = StringValues(0)
                                        StringValues.RemoveAt(0)
                                    Case "Quantity"
                                        QuantityValue = StringValues(0)
                                        StringValues.RemoveAt(0)
                                    Case "Width"
                                        WidthValue = StringValues(0)
                                        StringValues.RemoveAt(0)
                                    Case "Length"
                                        LengthValue = StringValues(0)
                                        StringValues.RemoveAt(0)
                                End Select
                            Next

                            Dim AttributeTags As List(Of String) = New List(Of String)
                            Dim AttributeValues As List(Of String) = New List(Of String)

                            With AttributeTags
                                .Add("PANEL_NAME")
                                .Add("QUANTITY")
                                .Add("WIDTH")
                                .Add("LENGTH")
                                .Add("PAGE")
                            End With

                            PageNumber += 1
                            With AttributeValues
                                .Add(NameValue)
                                .Add(QuantityValue)
                                .Add(WidthValue)
                                .Add(LengthValue)
                                .Add(PageNumber)
                            End With

                            LayoutList = LayoutsClass.GetLayoutList()

                            If LayoutList.Contains(NameValue) Then
                                Throw New Exception(ErrorStatus.AlreadyInGroup, $"Layout {NameValue} already exists!")
                                Exit For
                            End If
                            ZoomToWindow(currentboundary)
                            Dim psr2 As PromptSelectionResult = ed.SelectWindowPolygon(currentboundary)

                            'MinimumEnclosingBoundary(psr)

                            If psr2.Status <> PromptStatus.OK Then trans.Abort()

                            Dim SelectionBoundary = SelectionExtents(psr2)
                            Dim ViewCenter = GetCentroidfromPoints(SelectionBoundary)

                            Dim ViewHeight = Math.Abs(SelectionBoundary(0).Y - SelectionBoundary(1).Y)
                            Dim ViewWidth = Math.Abs(SelectionBoundary(0).X - SelectionBoundary(3).X)

                            LayoutsClass.AddLayout(NameValue)
                            If LayoutsClass.GetLayoutList.Contains("Layout1") Then
                                LayoutsClass.DeleteLayout("Layout1")
                            End If
                            LayoutsClass.DeleteCurrentLayoutEntities(NameValue)
                            Dim TitleBlock As ObjectId = BlocksClass.InsertBlock(blkname, New Point3d(4.75, 4.65, 0), True, NameValue)

                            BlocksClass.SetBlockAttributes(TitleBlock, AttributeTags, AttributeValues, NameValue)
                            'AvailableAttributes = BlocksClass.CheckBlockAttributes(TitleBlock, AttributeTags)

                            'For Each tag As String In AvailableAttributes
                            '    If AttributeTags.Contains(tag) Then
                            '        AttributeTags.Remove(tag)
                            '    End If
                            'Next
                            LayoutManager.Current.CurrentLayout = NameValue
                            LayoutsClass.SetPageSetupToLayout()
                            If VPHeight / ViewHeight < VPWidth / ViewWidth Then
                                LayoutsClass.CreateViewport(VPCenter, VPWidth, VPHeight, New Point2d(ViewCenter.X, ViewCenter.Y), VPHeight / (ViewHeight + 30))
                            Else
                                LayoutsClass.CreateViewport(VPCenter, VPWidth, VPHeight, New Point2d(ViewCenter.X, ViewCenter.Y), VPWidth / (ViewWidth + 30))
                            End If
                            LayoutManager.Current.CurrentLayout = "Model"
                            LayoutCount += 1
                            Writer.WriteLine($"{PageNumber},{NameValue},{LengthValue},{WidthValue},{QuantityValue}")
                            dxfDoc.Save($"{DrawingPath}{NameValue}.dxf")
                        Next
                    Next

                    Writer.Close()

                    If LayersMissing <> "" Then
                        MessageBox.Show("Error! " & LayoutCount & " layouts created, #" & savei & " rectangle missing the layers: " & LayersMissing)
                        Return
                    End If

                    If LISPPath <> "" Then
                        doc.SendStringToExecute($"(load ""{LISPPath}"")(c:reo)(princ){vbLf}", True, False, False)
                        'ed.WriteMessage($"(load {LISPPath})(c:reo)(princ){vbLf}")
                    End If

                    doc.SendStringToExecute($"(command ""._BEDIT"" ""{blkname}"")(princ){vbLf}", True, False, False)
                    ed.WriteMessage($"{vbLf}{LayoutCount.ToString} layout{If(LayoutCount = 1, "", "s")} created.")
                    trans.Commit()
                    trans.Dispose()
                Catch ex As Exception
                    MsgBox(vbLf & "Error encountered : " & ex.Message)
                    Writer.Close()
                    trans.Abort()
                End Try
            End Using

        End Sub



        Public Shared Function GetCurrentMatrixBoundary(inspt As Point3d, CurrentPosition As Integer) As Point3dCollection
            Dim ReturnValue As Point3dCollection = New Point3dCollection
            Dim colCount As Integer = 10
            Dim column As Integer = (CurrentPosition - 1) Mod colCount
            Dim row As Integer = (CurrentPosition - 1) \ colCount

            Dim Width As Double = 6000
            Dim Height As Double = 3000

            Dim LeftX As Double = inspt.X + Width * column
            Dim BottomY As Double = inspt.Y - Height * row

            Dim RightX As Double = LeftX + Width
            Dim TopY As Double = BottomY - Height

            ReturnValue.Add(New Point3d(LeftX, BottomY, 0))
            ReturnValue.Add(New Point3d(LeftX, TopY, 0))
            ReturnValue.Add(New Point3d(RightX, TopY, 0))
            ReturnValue.Add(New Point3d(RightX, BottomY, 0))

            Return ReturnValue
        End Function

        Public Function Compute2DCentroid(Points As Point2dCollection) As Point2d
            Dim centroid As Point2d = New Point2d(0, 0)
            Dim signedArea As Double = 0
            Dim x0 As Double = 0
            Dim y0 As Double = 0
            Dim x1 As Double = 0
            Dim y1 As Double = 0
            Dim a As Double = 0
            Dim idx As Integer = 0

            While idx < Points.Count - 1
                x0 = Points(idx).X
                y0 = Points(idx).Y
                x1 = Points(idx + 1).X
                y1 = Points(idx + 1).Y
                a = x0 * y1 - x1 * y0
                signedArea += a
                centroid = New Point2d(centroid.X + (x0 + x1) * a, centroid.Y + (y0 + y1) * a)
                idx += 1
            End While

            x0 = Points(idx).X
            y0 = Points(idx).Y
            x1 = Points(0).X
            y1 = Points(0).Y
            a = x0 * y1 - x1 * y0
            signedArea += a
            centroid = New Point2d(centroid.X + (x0 + x1) * a, centroid.Y + (y0 + y1) * a)

            signedArea *= 0.5
            centroid = New Point2d(centroid.X / (6 * signedArea), centroid.Y / (6 * signedArea))

            Return centroid
        End Function

        Public Shared Sub ExtractBounds(ByVal txt As DBText, ByVal pts As Point3dCollection)
            ' We have a special approach for DBText and
            ' AttributeReference objects, as we want to get
            ' all four corners of the bounding box, even
            ' when the text or the containing block reference
            ' is rotated
            If txt.Bounds.HasValue AndAlso txt.Visible Then
                ' Create a straight version of the text object
                ' and copy across all the relevant properties
                ' (stopped copying AlignmentPoint, as it would
                ' sometimes cause an eNotApplicable error)
                ' We'll create the text at the WCS origin
                ' with no rotation, so it's easier to use its
                ' extents
                Dim txt2 As DBText = New DBText()
                txt2.Normal = Vector3d.ZAxis
                txt2.Position = Point3d.Origin
                ' Other properties are copied from the original
                txt2.TextString = txt.TextString
                txt2.TextStyleId = txt.TextStyleId
                txt2.LineWeight = txt.LineWeight
                txt2.Thickness = txt2.Thickness
                txt2.HorizontalMode = txt.HorizontalMode
                txt2.VerticalMode = txt.VerticalMode
                txt2.WidthFactor = txt.WidthFactor
                txt2.Height = txt.Height
                txt2.IsMirroredInX = txt2.IsMirroredInX
                txt2.IsMirroredInY = txt2.IsMirroredInY
                txt2.Oblique = txt.Oblique
                ' Get its bounds if it has them defined
                ' (which it should, as the original did)
                If txt2.Bounds.HasValue Then
                    Dim maxPt As Point3d = txt2.Bounds.Value.MaxPoint
                    ' Place all four corners of the bounding box
                    ' in an array
                    Dim bounds As Point2d() = New Point2d() {Point2d.Origin, New Point2d(0.0, maxPt.Y), New Point2d(maxPt.X, maxPt.Y), New Point2d(maxPt.X, 0.0)}
                    ' We're going to get each point's WCS coordinates
                    ' using the plane the text is on
                    Dim pl As Plane = New Plane(txt.Position, txt.Normal)
                    ' Rotate each point and add its WCS location to the
                    ' collection
                    For Each pt As Point2d In bounds
                        pts.Add(pl.EvaluatePoint(pt.RotateBy(txt.Rotation, Point2d.Origin)))
                    Next
                End If
            End If
        End Sub

        Public Shared Function CollectPoints(tr As Transaction, ent As Entity) As Point3dCollection

            ' The collection of points to populate and return
            Dim pts As Point3dCollection = New Point3dCollection()
            ' We'll start by checking a block reference for
            ' attributes, getting their bounds and adding
            ' them to the point list. We'll still explode
            ' the BlockReference later, to gather points
            ' from other geometry, it's just that approach
            ' doesn't work for attributes (we only get the
            ' AttributeDefinitions, which don't have bounds)
            Dim br As BlockReference = TryCast(ent, BlockReference)
            If br IsNot Nothing Then
                For Each arId As ObjectId In br.AttributeCollection
                    Dim obj As DBObject = tr.GetObject(arId, OpenMode.ForRead)
                    If TypeOf obj Is AttributeReference Then
                        Dim ar As AttributeReference = CType(obj, AttributeReference)
                        ExtractBounds(ar, pts)
                    End If
                Next
            End If
            ' If we have a curve - other than a polyline, which
            ' we will want to explode - we'll get points along
            ' its length
            Dim cur As Curve = TryCast(ent, Curve)
            If cur IsNot Nothing AndAlso Not (TypeOf cur Is Polyline OrElse TypeOf cur Is Polyline2d OrElse TypeOf cur Is Polyline3d) Then
                ' Two points are enough for a line, we'll go with
                ' a higher number for other curves
                Dim segs = If(TypeOf ent Is Line, 2, 20)
                Dim param As Double = cur.EndParam - cur.StartParam
                For i = 0 To segs - 1
                    Try
                        Dim pt As Point3d = cur.GetPointAtParameter(cur.StartParam + i * param / (segs - 1))
                        pts.Add(pt)
                    Catch
                    End Try
                Next
            ElseIf TypeOf ent Is DBPoint Then
                ' Points are easy
                pts.Add(CType(ent, DBPoint).Position)
            ElseIf TypeOf ent Is DBText Then
                ' For DBText we use the same approach as
                ' for AttributeReferences
                ExtractBounds(CType(ent, DBText), pts)
            ElseIf TypeOf ent Is MText Then
                ' MText is also easy - you get all four corners
                ' returned by a function. That said, the points
                ' are of the MText's box, so may well be different
                ' from the bounds of the actual contents
                Dim txt As MText = CType(ent, MText)
                Dim pts2 As Point3dCollection = txt.GetBoundingPoints()
                For Each pt As Point3d In pts2
                    pts.Add(pt)
                Next
            ElseIf TypeOf ent Is Face Then
                Dim f As Face = CType(ent, Face)
                Try
                    For i As Short = 0 To 3
                        pts.Add(f.GetVertexAt(i))
                    Next
                Catch
                End Try
            ElseIf TypeOf ent Is Solid Then
                Dim sol As Solid = CType(ent, Solid)
                Try
                    For i As Short = 0 To 3
                        pts.Add(sol.GetPointAt(i))
                    Next
                Catch
                End Try
            Else
                ' Here's where we attempt to explode other types
                ' of object
                Dim oc As DBObjectCollection = New DBObjectCollection()
                Try
                    ent.Explode(oc)
                    If oc.Count > 0 Then
                        For Each obj As DBObject In oc
                            Dim ent2 As Entity = TryCast(obj, Entity)
                            If ent2 IsNot Nothing AndAlso ent2.Visible Then
                                For Each pt As Point3d In CollectPoints(tr, ent2)
                                    pts.Add(pt)
                                Next
                            End If
                            obj.Dispose()
                        Next
                    End If
                Catch
                End Try
            End If
            Return pts
        End Function

        Public Shared Sub MinimumEnclosingBoundary(psr As PromptSelectionResult)
            Dim doc As Document = Autodesk.AutoCAD.ApplicationServices.Application.DocumentManager.MdiActiveDocument
            Dim db As Database = doc.Database
            Dim ed As Editor = doc.Editor
            ' Ask user to select entities
            'Dim pso As PromptSelectionOptions = New PromptSelectionOptions()
            'pso.MessageForAdding = vbLf & "Select objects to enclose: "
            'pso.AllowDuplicates = False
            'pso.AllowSubSelections = True
            'pso.RejectObjectsFromNonCurrentSpace = True
            'pso.RejectObjectsOnLockedLayers = False
            'Dim psr As PromptSelectionResult = ed.GetSelection(pso)
            If psr.Status <> PromptStatus.OK Then Return
            Dim oneBoundPerEnt = False
            'If psr.Value.Count > 1 Then
            '    Dim pko As PromptKeywordOptions = New PromptKeywordOptions(vbLf & "Multiple objects selected: create " & "individual boundaries around each one?")
            '    pko.AllowNone = True
            '    pko.Keywords.Add("Yes")
            '    pko.Keywords.Add("No")
            '    pko.Keywords.[Default] = "No"
            '    Dim pkr As PromptResult = ed.GetKeywords(pko)
            '    If pkr.Status <> PromptStatus.OK Then Return
            '    oneBoundPerEnt = pkr.StringResult Is "Yes"
            'End If

            ' There may be a SysVar defining the buffer
            ' to add to our radius

            Dim buffer = 0.0
            Try
                Dim bufvar As Object = Autodesk.AutoCAD.ApplicationServices.Application.GetSystemVariable("ENCLOSINGBOUNDARYBUFFER")
                If bufvar IsNot Nothing Then
                    Dim bufval As Short = bufvar
                    buffer = bufval / 100.0
                End If
            Catch
                Dim bufvar As Object = Autodesk.AutoCAD.ApplicationServices.Application.GetSystemVariable("USERI1")
                If bufvar IsNot Nothing Then
                    Dim bufval As Short = bufvar
                    buffer = bufval / 100.0
                End If
            End Try

            ' Get the current UCS
            Dim ucs As CoordinateSystem3d = ed.CurrentUserCoordinateSystem.CoordinateSystem3d
            ' Collect points on the component entities
            Dim pts As Point3dCollection = New Point3dCollection()
            Dim tr As Transaction = db.TransactionManager.StartTransaction()
            Using tr
                Dim btr As BlockTableRecord = CType(tr.GetObject(db.CurrentSpaceId, OpenMode.ForWrite), BlockTableRecord)
                For i As Integer = 0 To psr.Value.Count - 1
                    Dim ent As Entity = CType(tr.GetObject(psr.Value(i).ObjectId, OpenMode.ForRead), Entity)
                    ' Collect the points for each selected entity
                    Dim entPts As Point3dCollection = CollectPoints(tr, ent)
                    For Each pt As Point3d In entPts
                        '  Create a DBPoint, for testing purposes
                        ' DBPoint dbp = new DBPoint(pt);
                        ' btr.AppendEntity(dbp);
                        ' tr.AddNewlyCreatedDBObject(dbp, true);
                        ' 

                        pts.Add(pt)
                    Next
                    ' Create a boundary for each entity (if so chosen) or
                    ' just once after collecting all the points
                    If oneBoundPerEnt OrElse i = psr.Value.Count - 1 Then
                        Try
                            Dim bnd As Entity = RectangleFromPoints(pts, ucs, buffer)
                            btr.AppendEntity(bnd)
                            tr.AddNewlyCreatedDBObject(bnd, True)
                        Catch
                            ed.WriteMessage(vbLf & "Unable to calculate enclosing boundary.")
                        End Try
                        pts.Clear()
                    End If
                Next
                tr.Commit()
            End Using
        End Sub

        Public Shared Function SelectionExtents(psr As PromptSelectionResult) As Point3dCollection
            Dim p As Point3dCollection = New Point3dCollection
            Dim doc As Document = Autodesk.AutoCAD.ApplicationServices.Application.DocumentManager.MdiActiveDocument
            Dim db As Database = doc.Database
            Dim ed As Editor = doc.Editor
            ' Ask user to select entities

            ' There may be a SysVar defining the buffer
            ' to add to our radius

            Dim buffer = 0.0
            Try
                Dim bufvar As Object = Autodesk.AutoCAD.ApplicationServices.Application.GetSystemVariable("ENCLOSINGBOUNDARYBUFFER")
                If bufvar IsNot Nothing Then
                    Dim bufval As Short = bufvar
                    buffer = bufval / 100.0
                End If
            Catch
                Dim bufvar As Object = Autodesk.AutoCAD.ApplicationServices.Application.GetSystemVariable("USERI1")
                If bufvar IsNot Nothing Then
                    Dim bufval As Short = bufvar
                    buffer = bufval / 100.0
                End If
            End Try

            ' Get the current UCS
            Dim ucs As CoordinateSystem3d = ed.CurrentUserCoordinateSystem.CoordinateSystem3d
            ' Collect points on the component entities
            Dim pts As Point3dCollection = New Point3dCollection()
            Dim tr As Transaction = db.TransactionManager.StartTransaction()
            Using tr
                Dim btr As BlockTableRecord = CType(tr.GetObject(db.CurrentSpaceId, OpenMode.ForWrite), BlockTableRecord)
                For i As Integer = 0 To psr.Value.Count - 1
                    Dim ent As Entity = CType(tr.GetObject(psr.Value(i).ObjectId, OpenMode.ForRead), Entity)
                    ' Collect the points for each selected entity
                    Dim entPts As Point3dCollection = CollectPoints(tr, ent)
                    For Each pt As Point3d In entPts
                        '  Create a DBPoint, for testing purposes
                        ' DBPoint dbp = new DBPoint(pt);
                        ' btr.AppendEntity(dbp);
                        ' tr.AddNewlyCreatedDBObject(dbp, true);
                        ' 
                        pts.Add(pt)
                    Next
                    ' Create a boundary for each entity (if so chosen) or
                    ' just once after collecting all the points
                    If i = psr.Value.Count - 1 Then
                        Try
                            p = TotalBoundingBox(pts, ucs, buffer)
                        Catch
                            ed.WriteMessage(vbLf & "Unable to calculate enclosing boundary.")
                        End Try
                        pts.Clear()
                    End If
                Next
                tr.Commit()
            End Using
            Return p
        End Function

        Public Shared Function RectangleFromPoints(ByVal pts As Point3dCollection, ByVal ucs As CoordinateSystem3d, ByVal buffer As Double) As Entity
            ' Get the plane of the UCS
            Dim pl As Plane = New Plane(ucs.Origin, ucs.Zaxis)
            ' We will project these (possibly 3D) points onto
            ' the plane of the current UCS, as that's where
            ' we will create our circle
            ' Project the points onto it
            Dim pts2d As List(Of Point2d) = New List(Of Point2d)(pts.Count)
            For i As Integer = 0 To pts.Count - 1
                pts2d.Add(pl.ParameterOf(pts(i)))
            Next
            ' Assuming we have some points in our list...
            If pts.Count > 0 Then
                ' Set the initial min and max values from the first entry
                Dim minX As Double = pts2d(0).X, maxX = minX, minY As Double = pts2d(0).Y, maxY = minY
                ' Perform a single iteration to extract the min/max X and Y
                For i = 1 To pts2d.Count - 1
                    Dim pt As Point2d = pts2d(i)
                    If pt.X < minX Then minX = pt.X
                    If pt.X > maxX Then maxX = pt.X
                    If pt.Y < minY Then minY = pt.Y
                    If pt.Y > maxY Then maxY = pt.Y
                Next
                ' Our final buffer amount will be the percentage of the
                ' smallest of the dimensions

                Dim buf = Math.Min(maxX - minX, maxY - minY) * buffer
                ' Apply the buffer to our point ordinates

                minX -= buf
                minY -= buf
                maxX += buf
                maxY += buf

                ' Create the boundary points

                Dim pt0 As Point2d = New Point2d(minX, minY), pt1 As Point2d = New Point2d(minX, maxY), pt2 As Point2d = New Point2d(maxX, maxY), pt3 As Point2d = New Point2d(maxX, minY)
                ' Finally we create the polyline
                Dim p = New Polyline(4)
                p.Normal = pl.Normal
                p.AddVertexAt(0, pt0, 0, 0, 0)
                p.AddVertexAt(1, pt1, 0, 0, 0)
                p.AddVertexAt(2, pt2, 0, 0, 0)
                p.AddVertexAt(3, pt3, 0, 0, 0)
                p.Closed = True
                Return p
            End If
            Return Nothing
        End Function

        Private Shared Function TotalBoundingBox(ByVal pts As Point3dCollection, ByVal ucs As CoordinateSystem3d, ByVal buffer As Double) As Point3dCollection
            ' Get the plane of the UCS
            Dim p As Point3dCollection = New Point3dCollection
            Dim pl As Plane = New Plane(ucs.Origin, ucs.Zaxis)
            ' We will project these (possibly 3D) points onto
            ' the plane of the current UCS, as that's where
            ' we will create our circle
            ' Project the points onto it
            Dim pts2d As List(Of Point2d) = New List(Of Point2d)(pts.Count)
            For i As Integer = 0 To pts.Count - 1
                pts2d.Add(pl.ParameterOf(pts(i)))
            Next
            ' Assuming we have some points in our list...
            If pts.Count > 0 Then
                ' Set the initial min and max values from the first entry
                Dim minX As Double = pts2d(0).X, maxX = minX, minY As Double = pts2d(0).Y, maxY = minY
                ' Perform a single iteration to extract the min/max X and Y
                For i = 1 To pts2d.Count - 1
                    Dim pt As Point2d = pts2d(i)
                    If pt.X < minX Then minX = pt.X
                    If pt.X > maxX Then maxX = pt.X
                    If pt.Y < minY Then minY = pt.Y
                    If pt.Y > maxY Then maxY = pt.Y
                Next
                ' Our final buffer amount will be the percentage of the
                ' smallest of the dimensions

                Dim buf = Math.Min(maxX - minX, maxY - minY) * buffer
                ' Apply the buffer to our point ordinates

                minX -= buf
                minY -= buf
                maxX += buf
                maxY += buf

                ' Create the boundary points

                Dim pt0 As Point3d = New Point3d(minX, minY, 0), pt1 As Point3d = New Point3d(minX, maxY, 0), pt2 As Point3d = New Point3d(maxX, maxY, 0), pt3 As Point3d = New Point3d(maxX, minY, 0)
                ' Finally we create the points
                p.Add(pt0)
                p.Add(pt1)
                p.Add(pt2)
                p.Add(pt3)

                Return p
            End If
            Return Nothing
        End Function

        Private Shared Sub ZoomToWindow(ByVal boundaryInModelSpace As Point3dCollection)
            Dim ext As Extents3d = MainClass.GetViewportBoundaryExtentsInModelSpace(boundaryInModelSpace)

            Dim p1 = New Double() {ext.MinPoint.X, ext.MinPoint.Y, 0.00}
            Dim p2 = New Double() {ext.MaxPoint.X, ext.MaxPoint.Y, 0.00}

            Dim acadApp = Autodesk.AutoCAD.ApplicationServices.Application.AcadApplication
            acadApp.ZoomWindow(p1, p2)
            'Autodesk.AutoCAD.ApplicationServices.Application.DocumentManager.MdiActiveDocument.Editor.Regen()
        End Sub

        Private Shared Function GetViewportBoundaryExtentsInModelSpace(ByVal points As Point3dCollection) As Extents3d
            Dim ext As Extents3d = New Extents3d()
            For Each p As Point3d In points
                ext.AddPoint(p)
            Next

            Return ext
        End Function


        Private Function GetCentroid(ByVal pl As Polyline) As Point3d
            Dim p0 As Point2d = pl.GetPoint2dAt(0)
            Dim cen As Point2d = New Point2d(0.0, 0.0)
            Dim area = 0.0
            Dim last As Integer = pl.NumberOfVertices - 1
            Dim tmpArea As Double
            Dim tmpPoint As Point2d

            If pl.GetSegmentType(0) = SegmentType.Arc Then
                Dim datas = GetArcGeom(pl, pl.GetBulgeAt(0), 0, 1)
                area = datas(0)
                cen = New Point2d(datas(1), datas(2)) * datas(0)
            End If
            For i = 1 To last - 1
                tmpArea = TriangleAlgebricArea(p0, pl.GetPoint2dAt(i), pl.GetPoint2dAt(i + 1))
                tmpPoint = TriangleCentroid(p0, pl.GetPoint2dAt(i), pl.GetPoint2dAt(i + 1))
                cen += (tmpPoint * tmpArea).GetAsVector()
                area += tmpArea
                If pl.GetSegmentType(i) = SegmentType.Arc Then
                    Dim datas = GetArcGeom(pl, pl.GetBulgeAt(i), i, i + 1)
                    area += datas(0)
                    cen += New Vector2d(datas(1), datas(2)) * datas(0)
                End If
            Next
            If pl.GetSegmentType(last) = SegmentType.Arc Then
                Dim datas = GetArcGeom(pl, pl.GetBulgeAt(last), last, 0)
                area += datas(0)
                cen += New Vector2d(datas(1), datas(2)) * datas(0)
            End If
            cen = cen.DivideBy(area)
            Dim result As Point3d = New Point3d(cen.X, cen.Y, pl.Elevation)
            Return result.TransformBy(Matrix3d.PlaneToWorld(pl.Normal))
        End Function

        Private Shared Function GetCentroidfromPoints(pts As Point3dCollection) As Point3d
            Dim point2ds As Point2dCollection = New Point2dCollection
            For Each pt As Point3d In pts
                point2ds.Add(Convert3dto2d(pt))
            Next
            Dim p0 As Point2d = point2ds(0)
            Dim cen As Point2d = New Point2d(0.0, 0.0)
            Dim area = 0.0
            Dim last As Integer = point2ds.Count - 1
            Dim tmpArea As Double
            Dim tmpPoint As Point2d

            For i = 1 To last - 1
                tmpArea = TriangleAlgebricArea(p0, point2ds(i), point2ds(i + 1))
                tmpPoint = TriangleCentroid(p0, point2ds(i), point2ds(i + 1))
                cen += (tmpPoint * tmpArea).GetAsVector()
                area += tmpArea
            Next
            cen = cen.DivideBy(area)
            Dim result As Point3d = New Point3d(cen.X, cen.Y, 0)
            Return result
        End Function

        Private Shared Function GetArcGeom(ByVal pl As Polyline, ByVal bulge As Double, ByVal index1 As Integer, ByVal index2 As Integer) As Double()
            Dim arc As CircularArc2d = pl.GetArcSegment2dAt(index1)
            Dim arcRadius As Double = arc.Radius
            Dim arcCenter As Point2d = arc.Center
            Dim arcAngle = 4.0 * Math.Atan(bulge)
            Dim tmpArea = ArcAlgebricArea(arcRadius, arcAngle)
            Dim tmpPoint As Point2d = ArcCentroid(pl.GetPoint2dAt(index1), pl.GetPoint2dAt(index2), arcCenter, tmpArea)
            Return New Double(2) {tmpArea, tmpPoint.X, tmpPoint.Y}
        End Function

        Private Shared Function TriangleCentroid(ByVal p0 As Point2d, ByVal p1 As Point2d, ByVal p2 As Point2d) As Point2d
            Return (p0 + p1.GetAsVector() + p2.GetAsVector()) / 3.0
        End Function

        Private Shared Function TriangleAlgebricArea(ByVal p0 As Point2d, ByVal p1 As Point2d, ByVal p2 As Point2d) As Double
            Return ((p1.X - p0.X) * (p2.Y - p0.Y) - (p2.X - p0.X) * (p1.Y - p0.Y)) / 2.0
        End Function

        Private Shared Function ArcCentroid(ByVal start As Point2d, ByVal [end] As Point2d, ByVal cen As Point2d, ByVal tmpArea As Double) As Point2d
            Dim chord As Double = start.GetDistanceTo([end])
            Dim angle As Double = ([end] - start).Angle
            Return Polar2d(cen, angle - Math.PI / 2.0, chord * chord * chord / (12.0 * tmpArea))
        End Function

        Private Shared Function ArcAlgebricArea(ByVal rad As Double, ByVal ang As Double) As Double
            Return rad * rad * (ang - Math.Sin(ang)) / 2.0
        End Function

        Private Shared Function Polar2d(ByVal org As Point2d, ByVal angle As Double, ByVal distance As Double) As Point2d
            Return org + New Vector2d(distance * Math.Cos(angle), distance * Math.Sin(angle))
        End Function

        Private Shared Function Convert3dto2d(point As Point3d) As Point2d
            Convert3dto2d = New Point2d(point.X, point.Y)
        End Function

        Public Shared Function ReplaceBlock(blockName As String)
            Dim doc As Document = Autodesk.AutoCAD.ApplicationServices.Application.DocumentManager.MdiActiveDocument
            Dim db As Database = doc.Database
            Dim blockpath As String
            Dim ObjId As ObjectId = New ObjectId
            'Dim blockName As String = "TEST"

            Try
                blockpath = HostApplicationServices.Current.FindFile(blockName & Convert.ToString(".dwg"), db, FindFileHint.[Default])
            Catch ex As Exception
                'MessageBox.Show(ex.Message)
                Return ObjectId.Null
            End Try

            Dim blkDb As Database = New Database(False, True)
            blkDb.ReadDwgFile(blockpath, System.IO.FileShare.Read, True, "")

            Using trans As Transaction = db.TransactionManager.StartTransaction()
                Dim bt As BlockTable = trans.GetObject(db.BlockTableId, OpenMode.ForRead, False, True)
                Dim btrId As ObjectId = db.Insert(blockName, blkDb, True)
                If btrId <> ObjectId.Null Then
                    Dim btr As BlockTableRecord = trans.GetObject(btrId, OpenMode.ForRead, False, True)
                    Dim brefIds As ObjectIdCollection = btr.GetBlockReferenceIds(False, True)
                    For Each id As ObjectId In brefIds
                        Dim bref As BlockReference = trans.GetObject(id, OpenMode.ForWrite, False, True)
                        bref.RecordGraphicsModified(True)
                    Next
                End If
                trans.Commit()
            End Using
            blkDb.Dispose()

            Return ObjId
        End Function
    End Class
End Namespace

