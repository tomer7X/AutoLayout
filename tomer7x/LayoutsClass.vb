Imports System.Windows.Forms
Imports Autodesk.AutoCAD.ApplicationServices
Imports Autodesk.AutoCAD.DatabaseServices
Imports Autodesk.AutoCAD.Runtime
Imports Autodesk.AutoCAD.EditorInput
Imports Autodesk.AutoCAD.Geometry
Imports Autodesk.AutoCAD.PlottingServices
Namespace MyNamespace
    Public Class LayoutsClass
        Public Shared Function GetLayoutList() As List(Of String)
            Dim dwgLayouts As New List(Of String)
            Try

                Dim doc As Document = Core.Application.DocumentManager.MdiActiveDocument
                Dim db As Database = doc.Database
                Dim ed As Editor = doc.Editor

                Dim layAndTab As SortedDictionary(Of Integer, String) = New SortedDictionary(Of Integer, String)

                Using tr As Transaction = db.TransactionManager.StartTransaction()
                    Dim lm As LayoutManager = LayoutManager.Current

                    Dim layoutDic As DBDictionary = TryCast(tr.GetObject(db.LayoutDictionaryId, OpenMode.ForRead, False), DBDictionary)

                    For Each entry As DBDictionaryEntry In layoutDic
                        Dim layoutid As ObjectId = entry.Value
                        Dim layout As Layout = TryCast(tr.GetObject(layoutid, OpenMode.ForRead), Layout)

                        'If layout.TabOrder > 0 Then
                        layAndTab.Add(layout.TabOrder, layout.LayoutName)
                        'dwgLayouts.Add(layout.LayoutName)

                        'End If
                    Next
                    tr.Abort()
                End Using

                For Each v In layAndTab.Values
                    dwgLayouts.Add(v)
                Next

                Return dwgLayouts
            Catch ex As Exception
                dwgLayouts.Add("")
                Return dwgLayouts
            End Try

        End Function

        Public Shared Sub SetCurrentLayoutTab(tab As String)
            Dim doc = Core.Application.DocumentManager.MdiActiveDocument
            LayoutManager.Current.CurrentLayout = tab
        End Sub

        Public Shared Function GetCurrentLayoutTab()
            Dim tab As String = LayoutManager.Current.CurrentLayout
            Return tab
        End Function

        Public Shared Sub AddLayout(LayoutName As String)
            Dim doc = Core.Application.DocumentManager.MdiActiveDocument
            Dim LayoutList As List(Of String) = GetLayoutList()
            Using trans As Transaction = doc.TransactionManager.StartTransaction()
                If LayoutList.Contains(LayoutName) = False Then
                    'Using doc.LockDocument()
                    LayoutManager.Current.CreateLayout(LayoutName)

                    'End Using
                Else
                    MessageBox.Show("Layout " & LayoutName & " already exists.")
                End If
                trans.Commit()
            End Using

        End Sub
        Public Shared Sub DeleteLayout(LayoutName As String)
            Dim doc = Core.Application.DocumentManager.MdiActiveDocument
            Using trans As Transaction = doc.TransactionManager.StartTransaction()
                If LayoutName = "Model" Then
                    doc.Editor.WriteMessage("Modelspace could not be deleted.")
                Else
                    LayoutManager.Current.DeleteLayout(LayoutName)
                End If
                trans.Commit()
            End Using
        End Sub

        Public Shared Sub DeleteCurrentLayoutEntities(LayoutName As String)
            Dim doc As Document = Core.Application.DocumentManager.MdiActiveDocument
            Dim db As Database = doc.Database
            Dim ed As Editor = doc.Editor
            'Dim tabName As String = LayoutManager.Current.CurrentLayout
            Using trans As Transaction = doc.TransactionManager.StartTransaction()
                Try
                    Dim tv As TypedValue()
                    tv = New TypedValue(0) {}
                    tv.SetValue(New TypedValue(CInt(DxfCode.LayoutName), LayoutName), 0)

                    Dim filter As New SelectionFilter(tv)
                    Dim ssPrompt As PromptSelectionResult = ed.SelectAll(filter)
                    Dim ss As SelectionSet = ssPrompt.Value

                    If ssPrompt.Status = PromptStatus.OK Then

                        Dim blks = ss.GetObjectIds()
                        For Each sObj As ObjectId In blks

                            Dim ent As Entity = TryCast(trans.GetObject(sObj, OpenMode.ForWrite), Entity)
                            ent.Erase()

                        Next
                        trans.Commit()

                    Else
                        ed.WriteMessage("No object selected.")
                        trans.Abort()
                    End If

                Catch ex As Exception
                    ed.WriteMessage("Error encountered : " & ex.Message)
                    trans.Abort()
                End Try
            End Using
        End Sub

        Public Shared Sub CreateViewport(CenterPoint As Point3d, Width As Double, Height As Double, ViewCenter As Point2d, Scale As Double, Optional LayerName As String = "")
            Dim doc As Document = Core.Application.DocumentManager.MdiActiveDocument
            Dim db As Database = doc.Database
            Dim ed As Editor = doc.Editor

            Dim idCol As ObjectIdCollection = New ObjectIdCollection



            If LayerName = "" Then
                LayerName = "Defpoints"
            End If

            Using trans As Transaction = db.TransactionManager.StartTransaction
                Dim idLay As ObjectId = db.LayerTableId
                Dim lt As LayerTable = trans.GetObject(idLay, OpenMode.ForRead)


                If lt.Has("P_Names") Then
                    idCol.Add(lt.Item("P_Names"))
                End If
                If lt.Has("Quantity") Then
                    idCol.Add(lt.Item("Quantity"))
                End If
                If lt.Has("Defpoints") Then
                    idCol.Add(lt.Item("Defpoints"))
                End If

                Dim bt As BlockTable = trans.GetObject(db.BlockTableId, OpenMode.ForRead)
                Dim btr As BlockTableRecord = trans.GetObject(bt(BlockTableRecord.PaperSpace), OpenMode.ForWrite)
                ed.SwitchToPaperSpace()

                Dim vp As Viewport = New Viewport()
                vp.Layer = LayerName
                vp.SetDatabaseDefaults()
                vp.CenterPoint = _ 'New Point3d(125, 217.75, 0)
                CenterPoint
                vp.Width = _ '200
                Width
                vp.Height = _ '128.5
                Height
                vp.ViewCenter = ViewCenter
                vp.ViewHeight = Height / Scale
                vp.FreezeLayersInViewport(idCol.GetEnumerator)

                btr.AppendEntity(vp)
                trans.AddNewlyCreatedDBObject(vp, True)

                vp.On = True
                vp.Locked = True

                trans.Commit()

            End Using

        End Sub
        Public Shared Sub CreateOrEditPageSetup()
            ' Get the current document and database, and start a transaction
            Dim doc As Document = Core.Application.DocumentManager.MdiActiveDocument
            Dim db As Database = doc.Database
            Dim ed As Editor = doc.Editor

            'Using doc.LockDocument
            Using trans As Transaction = db.TransactionManager.StartTransaction()

                Dim plSets As DBDictionary =
                trans.GetObject(db.PlotSettingsDictionaryId, OpenMode.ForRead)
                Dim vStyles As DBDictionary =
                trans.GetObject(db.VisualStyleDictionaryId, OpenMode.ForRead)

                Dim plSet As PlotSettings
                Dim createNew As Boolean = False

                ' Reference the Layout Manager
                Dim acLayoutMgr As LayoutManager = LayoutManager.Current

                ' Get the current layout and output its name in the Command Line window
                Dim acLayout As Layout =
                trans.GetObject(acLayoutMgr.GetLayoutId(acLayoutMgr.CurrentLayout),
                                  OpenMode.ForRead)

                ' Check to see if the page setup exists
                If plSets.Contains("TH_A3 PDF") = False Then
                    createNew = True

                    ' Create a new PlotSettings object: 
                    '    True - model space, False - named layout
                    plSet = New PlotSettings(acLayout.ModelType)
                    plSet.CopyFrom(acLayout)

                    plSet.PlotSettingsName = "TH_A3 PDF"
                    plSet.AddToPlotSettingsDictionary(db)
                    trans.AddNewlyCreatedDBObject(plSet, True)
                Else
                    plSet = plSets.GetAt("TH_A3 PDF").GetObject(OpenMode.ForWrite)
                    'Return
                End If

                ' Update the PlotSettings object
                Try
                    Dim acPlSetVdr As PlotSettingsValidator = PlotSettingsValidator.Current
                    ' Set the Plotter and page size
                    acPlSetVdr.SetPlotConfigurationName(plSet,
                                                        "DWG To PDF.pc3",
                                                        "ISO_full_bleed_A3_(420.00_x_297.00_MM)")

                    ' Set to plot to the current display
                    'If acLayout.ModelType = False Then
                    acPlSetVdr.SetPlotType(plSet,
                                           Autodesk.AutoCAD.DatabaseServices.PlotType.Extents)
                    'Else
                    'acPlSetVdr.SetPlotType(plSet, Autodesk.AutoCAD.DatabaseServices.PlotType.Layout)

                    'End If



                    ' Use SetPlotWindowArea with PlotType.Window
                    'acPlSetVdr.SetPlotWindowArea(plSet, _
                    '                             New Extents2d(New Point2d(0.0, 0.0), _
                    '                             New Point2d(9.0, 12.0)))

                    ' Use SetPlotViewName with PlotType.View
                    'acPlSetVdr.SetPlotViewName(plSet, "MyView")

                    ' Set the plot offset
                    acPlSetVdr.SetPlotCentered(plSet, True)

                    acPlSetVdr.SetPlotOrigin(plSet, New Point2d(0, 0))


                    ' Set the plot scale
                    acPlSetVdr.SetUseStandardScale(plSet, True)
                    acPlSetVdr.SetPlotPaperUnits(plSet, PlotPaperUnit.Millimeters)
                    acPlSetVdr.SetStdScaleType(plSet, StdScaleType.ScaleToFit)
                    plSet.ScaleLineweights = False

                    ' Specify if plot styles should be displayed on the layout
                    plSet.ShowPlotStyles = True

                    ' Rebuild plotter, plot style, and canonical media lists 
                    ' (must be called before setting the plot style)
                    acPlSetVdr.RefreshLists(plSet)


                    ' Specify the shaded viewport options
                    plSet.ShadePlot = PlotSettingsShadePlotType.AsDisplayed

                    plSet.ShadePlotResLevel = ShadePlotResLevel.Normal

                    ' Specify the plot options
                    plSet.PrintLineweights = True
                    plSet.PlotTransparency = False
                    plSet.PlotPlotStyles = True
                    plSet.DrawViewportsFirst = True

                    ' Use only on named layouts - Hide paperspace objects option
                    ' plSet.PlotHidden = True

                    ' Specify the plot orientation
                    acPlSetVdr.SetPlotRotation(plSet, PlotRotation.Degrees000)

                    ' Set the plot style
                    If db.PlotStyleMode = True Then
                        acPlSetVdr.SetCurrentStyleSheet(plSet, "monochrome.ctb")
                    Else
                        acPlSetVdr.SetCurrentStyleSheet(plSet, "monochrome.stb")
                    End If

                    ' Zoom to show the whole paper
                    acPlSetVdr.SetZoomToPaperOnUpdate(plSet, True)


                    'acLayout.CopyFrom(plSet)
                Catch es As Autodesk.AutoCAD.Runtime.Exception
                    'MsgBox(es.Message)
                    ed.WriteMessage("Error encountered : " & es.Message)
                    'trans.Abort()
                End Try

                ' Save the changes made
                'plSet.UpgradeOpen()
                plSet.Dispose()
                plSets.Dispose()
                vStyles.Dispose()
                trans.Commit()
                trans.Dispose()
                'If createNew = True Then
                '    acPlSet.Dispose()
                'End If
            End Using
            'End Using
        End Sub

        Public Shared Sub AssignPageSetupToLayout()
            ' Get the current document and database, and start a transaction
            Dim doc As Document = Core.Application.DocumentManager.MdiActiveDocument
            Dim db As Database = doc.Database

            'Using doc.LockDocument
            Using trans As Transaction = db.TransactionManager.StartTransaction()
                ' Reference the Layout Manager
                Dim lm As LayoutManager = LayoutManager.Current

                ' Get the current layout and output its name in the Command Line window
                Dim acLayout As Layout =
                        trans.GetObject(lm.GetLayoutId(lm.CurrentLayout),
                                          OpenMode.ForRead)

                Dim acPlSet As DBDictionary =
                        trans.GetObject(db.PlotSettingsDictionaryId, OpenMode.ForRead)

                ' Check to see if the page setup exists
                If acPlSet.Contains("TH_A3 PDF") = True Then
                    Dim plSet As PlotSettings =
                            acPlSet.GetAt("TH_A3 PDF").GetObject(OpenMode.ForRead)

                    ' Update the layout
                    trans.GetObject(lm.GetLayoutId(lm.CurrentLayout), OpenMode.ForWrite)
                    acLayout.CopyFrom(plSet)

                    ' Save the new objects to the database
                    trans.Commit()
                Else
                    ' Ignore the changes made
                    trans.Abort()
                End If
            End Using

            'End Using

            ' Update the display
            doc.Editor.Regen()
        End Sub

        Public Shared Sub SetPageSetupToLayout()
        ' Get the current document and database, and start a transaction
        Dim doc As Document = Core.Application.DocumentManager.MdiActiveDocument
        Dim db As Database = doc.Database

            'Using doc.LockDocument

            Using trans As Transaction = db.TransactionManager.StartTransaction()
                ' Reference the Layout Manager
                Dim lm As LayoutManager = LayoutManager.Current

                ' Get the current layout and output its name in the Command Line window
                Dim acLayout As Layout =
                    trans.GetObject(lm.GetLayoutId(lm.CurrentLayout),
                                      OpenMode.ForRead)

                Dim acPlSet As DBDictionary =
                    trans.GetObject(db.PlotSettingsDictionaryId, OpenMode.ForRead)

                ' Check to see if the page setup exists
                If acPlSet.Contains("TH_A3 PDF") = False Then CreateOrEditPageSetup()

                Dim plSet As PlotSettings =
                        acPlSet.GetAt("TH_A3 PDF").GetObject(OpenMode.ForRead)

                ' Update the layout
                trans.GetObject(lm.GetLayoutId(lm.CurrentLayout), OpenMode.ForWrite)
                acLayout.CopyFrom(plSet)

                ' Save the new objects to the database
                lm.Dispose()
                plSet.Dispose()
                acPlSet.Dispose()
                acLayout.Dispose()
                trans.Commit()
                trans.Dispose()
            End Using
            'End Using

            ' Update the display
            doc.Editor.Regen()
    End Sub

    End Class
End Namespace
