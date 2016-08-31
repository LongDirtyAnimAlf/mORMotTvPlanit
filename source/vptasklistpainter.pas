unit VpTasklistPainter;

{$I vp.inc}

interface

uses
  SysUtils, LCLType, LCLIntf,
  //SysUtils, LCLType, LCLIntf, Types,
  Classes, Graphics, Types,
  //VpConst,
  VPBase, //VpData,
  VpTaskList, VpBasePainter;

type
  TVpTaskListPainter = class(TVpBasePainter)
  private
    FTaskList: TVpTaskList;

    // local parameters of the old TVpTaskList method
    HeadRect: TRect;
    Bmp: Graphics.TBitmap;
    RowHeight: Integer;
    RealColor: TColor;
    BackgroundSelHighlight: TColor;
    ForegroundSelHighlight: TColor;
    BevelShadow: TColor;
    BevelHighlight: TColor;
    BevelDarkShadow: TColor;
    BevelFace: TColor;
    RealLineColor: TColor;
    RealCheckBgColor: TColor;
    RealCheckBoxColor: TColor;
    RealCheckColor: TColor;
    RealCompleteColor: TColor;
    RealOverdueColor: TColor;
    RealNormalColor: TColor;
    TaskHeadAttrColor: TColor;

  protected
    procedure Clear;
    function DrawCheck(Rec: TRect; Checked: Boolean): TRect;
    procedure DrawBorders;
    procedure DrawHeader;
    procedure DrawLines;
    procedure DrawTasks;
    procedure FixFontHeights;
    procedure InitColors;
    procedure MeasureRowHeight;

  public
    constructor Create(ATaskList: TVpTaskList; ARenderCanvas: TCanvas);
    procedure RenderToCanvas(ARenderIn: TRect; AAngle: TVpRotationAngle;
      AScale: Extended; ARenderDate: TDateTime; AStartLine, AStopLine: Integer;
      AUseGran: TVpGranularity; ADisplayOnly: Boolean); override;
  end;

implementation

uses
  VpConst, VpData, VpMisc, VpCanvasUtils, VpSR;

type
  TVpTaskListOpener = class(TVpTaskList);

constructor TVpTaskListPainter.Create(ATaskList: TVpTaskList; ARenderCanvas: TCanvas);
begin
  inherited Create(ARenderCanvas);
  FTaskList := ATaskList;
end;

procedure TVpTaskListPainter.Clear;
var
  I: Integer;
begin
  RenderCanvas.Brush.Color := RealColor;
  RenderCanvas.FillRect(RenderIn);

  { Clear the LineRect }
  with TVpTasklistOpener(FTaskList) do
    for I := 0 to pred(Length(tlVisibleTaskArray)) do begin
      tlVisibleTaskArray[I].Task := nil;
      tlVisibleTaskArray[I].LineRect := Rect(0, 0, 0, 0);
    end;
end;

{ draws the check box and returns it's rectangle }
function TVpTaskListPainter.DrawCheck(Rec: TRect; Checked: Boolean): TRect;
var
  CR: TRect;     // checbox rectangle
  W: Integer;    // width of the checkbox
  X, Y: Integer; // Coordinates
  dx, dy: Integer;
  tm: Integer;   // Scaled text margin;
  d2: Integer;   // 2*Scale
begin
  tm := Round(Textmargin * Scale);

  X := Rec.Left + tm;
  Y := Rec.Top + tm;
  W := RowHeight - tm * 2;    // correct: The checkbox is square, its width is determined by the row height

  { draw check box }
  case FTaskList.DrawingStyle of
    dsFlat:
      begin
        RenderCanvas.Brush.Color := RealCheckBgColor;
        RenderCanvas.Pen.Color := RealCheckBoxColor;
        TPSRectangle(RenderCanvas, Angle, RenderIn, Rect(X, Y, X + W, Y + W));
      end;
    ds3d:
      begin
        // complete box, rather bright
        RenderCanvas.Pen.Color := RGB(192, 204, 216);
        RenderCanvas.Brush.Color := RealCheckBgColor;
        TPSRectangle(RenderCanvas, Angle, RenderIn, Rect(X, Y, X + W, Y + W));
        // left and top lines
        RenderCanvas.Pen.Color := RGB(80, 100, 128);
        TPSPolyLine(RenderCanvas, Angle, RenderIn, [
          Point(X, Y + W - 2),
          Point(X, Y),
          Point(X + W - 1, Y)
        ]);
        // left and top lines
        RenderCanvas.Pen.Color := RealCheckBoxColor;
        TPSPolyLine(RenderCanvas, Angle, RenderIn, [
          Point(X + 1,     Y + W - 3),
          Point(X + 1,     Y + 1),
          Point(X + W - 2, Y + 1)
        ]);
        // right and bottom lines
        RenderCanvas.Pen.Color := RGB(128, 152, 176);
        TPSPolyLine(RenderCanvas, Angle, RenderIn, [
          Point(X + 1,     Y + W - 2),
          Point(X + W - 2, Y + W - 2),
          Point(X + W - 2, Y)
        ]);
      end;
  end;

  { build check rect }
  d2 := Round(2*Scale);
  if Scale > 1 then
    CR := Rect(X + d2, Y + d2, X + W - d2, Y + W - d2)
  else
    CR := Rect(X + 3, Y + 3, X + W - 3, Y + W - 3);
  if Checked then begin
    RenderCanvas.Pen.Color := RealCheckColor;
    // Instead of using Pen.Width = 3 we paint 3x - looks better
    case FTaskList.DisplayOptions.CheckStyle of
      csX: {X}
        with RenderCanvas do begin
          { \ }
          TPSMoveTo(RenderCanvas, Angle, RenderIn, CR.Left, CR.Top);        // center
          TPSLineTo(RenderCanvas, Angle, RenderIn, CR.Right, CR.Bottom);
          TPSMoveTo(RenderCanvas, Angle, RenderIn, CR.Left+1, CR.Top);      // upper
          TPSLineTo(RenderCanvas, Angle, RenderIn, CR.Right, CR.Bottom-1);
          TPSMoveTo(RenderCanvas, Angle, RenderIn, CR.Left, CR.Top+1);      // lower
          TPSLineTo(RenderCanvas, Angle, RenderIn, CR.Right-1, CR.Bottom);
          { / }
          TPSMoveTo(RenderCanvas, Angle, RenderIn, CR.Left, CR.Bottom-1);   // center
          TPSLineTo(RenderCanvas, Angle, RenderIn, CR.Right, CR.Top-1);
          TPSMoveTo(RenderCanvas, Angle, RenderIn, CR.Left, CR.Bottom-2);   // upper
          TPSLineTo(RenderCanvas, Angle, RenderIn, CR.Right-1, CR.Top-1);
          TPSMoveTo(RenderCanvas, Angle, RenderIn, CR.Left+1, CR.Bottom-1); // lower
          TPSLineTo(RenderCanvas, Angle, RenderIn, CR.Right, CR.Top);
        end;
      csCheck: {check}
        begin
          dx := WidthOf(CR) div 3;
          dy := HeightOf(CR) div 3;
          with RenderCanvas do begin
            TPSMoveTo(RenderCanvas, Angle, RenderIn, CR.Left, CR.Bottom-dy);
            TPSLineTo(RenderCanvas, Angle, RenderIn, CR.Left+dx, CR.Bottom);
            TPSLineTo(RenderCanvas, Angle, RenderIn, CR.Right, CR.Top-1);

            TPSMoveTo(RenderCanvas, Angle, RenderIn, CR.Left+1, CR.Bottom-dy);
            TPSLineTo(RenderCanvas, Angle, RenderIn, CR.Left+ dx, CR.Bottom-1);
            TPSLineTo(RenderCanvas, Angle, RenderIn, CR.Right-1, CR.Top-1);

            TPSMoveTo(RenderCanvas, Angle, RenderIn, CR.Left, CR.Bottom-dy+1);
            TPSLineTo(RenderCanvas, Angle, RenderIn, CR.Left+dx, CR.Bottom+1);
            TPSLineTo(RenderCanvas, Angle, RenderIn, CR.Right, CR.Top);
          end;
        end;
    end;
  end; {if checked}
  result := Rect(X, Y, X + W, Y + W);  //CR;
end;

procedure TVpTaskListPainter.DrawBorders;
var
  R: TRect;
begin
  R := Rect(RenderIn.Left, RenderIn.Top, RenderIn.Right - 1, RenderIn.Bottom - 1);
  case FTasklist.DrawingStyle of
    dsFlat:
      begin
        DrawBevelRect(RenderCanvas, R, BevelShadow, BevelShadow);
        { wp: above line replaces the following code, no bevel in flat mode
        DrawBevelRect(RenderCanvas, R, BevelShadow, BevelHighlight);
        InflateRect(R, -1, -1);
        DrawBevelRect(RenderCanvas, R, BevelHighlight, BevelShadow); }
      end;
    ds3d:
      begin
        DrawBevelRect(RenderCanvas, R, BevelShadow, BevelHighlight);
        InflateRect(R, -1, -1);
        DrawBevelRect(RenderCanvas, R, BevelDarkShadow, BevelFace);
      end;
  end;
  (*
  if FDrawingStyle = dsFlat then begin
    { draw an outer and inner bevel }
    DrawBevelRect(
      RenderCanvas,
      Rect(RenderIn.Left, RenderIn.Top, RenderIn.Right - 1, RenderIn.Bottom - 1),
      BevelShadow,
      BevelHighlight
    );
    DrawBevelRect (RenderCanvas,
                   Rect (RenderIn.Left + 1,
                         RenderIn.Top + 1,
                         RenderIn.Right - 2,
                         RenderIn.Bottom - 2),
                   BevelHighlight,
                   BevelShadow);
  end else if FDrawingStyle = ds3d then begin
  { draw a 3d bevel }
    DrawBevelRect (RenderCanvas,
                   Rect (RenderIn.Left, RenderIn.Top,
                         RenderIn.Right - 1, RenderIn.Bottom - 1),
                   BevelShadow,
                   BevelHighlight);
    DrawBevelRect (RenderCanvas,
                   Rect (RenderIn.Left + 1,
                         RenderIn.Top + 1,
                         RenderIn.Right - 2,
                         RenderIn.Bottom - 2),
                   BevelDarkShadow,
                   BevelFace);
  end;
  *)
end;

procedure TVpTaskListPainter.DrawHeader;
var
  GlyphRect: TRect;
  HeadStr: string;
  delta: Integer;
  w, h: Integer;
begin
  RenderCanvas.Brush.Color := TaskHeadAttrColor;
  RenderCanvas.Font.Assign(FTaskList.TaskHeadAttributes.Font);
  RenderCanvas.Font.Size := ScaleY(RenderCanvas.Font.Size, DesignTimeDPI);

  if FTaskList.DrawingStyle = dsFlat then delta := 1 else delta := 2;
  HeadRect.Left := RealLeft + delta;
  HeadRect.Left := RealLeft + delta;
  HeadRect.Top := RealTop + delta;
  HeadRect.Right := RealRight - delta;
  HeadRect.Bottom := RealTop + RenderCanvas.TextHeight('YyGg0') + TextMargin * 2;
  TPSFillRect(RenderCanvas, Angle, RenderIn, HeadRect);

  { draw the header cell borders }
  case FTaskList.DrawingStyle of
    dsFlat:
      begin { draw an outer and inner bevel }
        { wp: no bevel in flat style!
        HeadRect.Left := HeadRect.Left - 1;
        HeadRect.Top := HeadRect.Top - 1;
        DrawBevelRect(RenderCanvas, TPSRotateRectangle(Angle, RenderIn, HeadRect), BevelShadow, BevelShadow);
        }
      end;
    ds3d:
      begin  { draw a 3d bevel }
        HeadRect.Right := HeadRect.Right - 1;
        DrawBevelRect(RenderCanvas, TPSRotateRectangle(Angle, RenderIn, HeadRect), BevelHighlight, BevelDarkShadow);
      end;
  end;

  if FTaskList.ShowIcon then begin
    { Draw the glyph }
    Bmp := Graphics.TBitmap.Create;
    try
      Bmp.LoadFromResourceName(HINSTANCE, 'VPCHECKPAD'); //soner changed: Bmp.Handle := LoadBaseBitmap('VPCHECKPAD');
      if Bmp.Height > 0 then begin
        w := Round(Bmp.Width * Scale);
        h := Round(Bmp.Height * Scale);

        GlyphRect.TopLeft := Point(HeadRect.Left + TextMargin, HeadRect.Top + TextMargin);
        GlyphRect.BottomRight := Point(GlyphRect.Left + w, GlyphRect.Top + h);

        {$IFDEF FPC}
        RotateBitmap(Bmp, Angle);
        {$ENDIF}

        TPSStretchDraw(RenderCanvas, Angle, RenderIn, GlyphRect, Bmp);
        {
        RenderCanvas.BrushCopy(
          TPSRotateRectangle(Angle, RenderIn, GlyphRect),
          Bmp,
          Rect(0, 0, Bmp.Width, Bmp.Height),
          Bmp.Canvas.Pixels[0, Bmp.Height-1]
        );
         }
//TODO:          RenderCanvas.BrushCopy (TPSRotateRectangle (Angle, RenderIn, GlyphRect),
//                                  Bmp, Rect(0, 0, Bmp.Width, Bmp.Height),
//            Bmp.Canvas.Pixels[0, Bmp.Height - 1]);
       // RenderCanvas.Draw(GlyphRect.TopLeft.x, GlyphRect.TopLeft.y, Bmp); //soner added
        HeadRect.Left := HeadRect.Left + w + TextMargin;
      end;
    finally
      Bmp.Free;
    end;
  end;

  { draw the text }
  with FTaskList do begin
    if ShowResourceName and (DataStore <> nil) and (DataStore.Resource <> nil) then
      HeadStr := RSTaskTitleResource + DataStore.Resource.Description
    else
      HeadStr := RSTaskTitleNoResource;
    RenderCanvas.Font.Assign(TaskHeadAttributes.Font);
    RenderCanvas.Font.Size := ScaleY(RenderCanvas.Font.Size, DesignTimeDPI);
    TPSTextOut(
      RenderCanvas,
      Angle,
      RenderIn,
      HeadRect.Left + TextMargin,
      HeadRect.Top + TextMargin,
      HeadStr
    );
  end;
end;

procedure TVpTasklistPainter.DrawLines;
var
  LinePos: Integer;
begin
  RenderCanvas.Pen.Color := RealLineColor;
  RenderCanvas.Pen.Style := psSolid;
  LinePos := HeadRect.Bottom + RowHeight;
  while LinePos < RealBottom do begin
    TPSMoveTo(RenderCanvas, Angle, RenderIn, RealLeft, LinePos);
    TPSLineTo(RenderCanvas, Angle, RenderIn, RealRight - 2, LinePos);
    Inc(LinePos, RowHeight);
  end;
end;

procedure TVpTaskListPainter.DrawTasks;
var
  I: Integer;
  Task: TVpTask;
  LineRect: TRect;
  CheckRect: TRect;
  DisplayStr: string;
begin
  with TVpTaskListOpener(FTaskList) do begin
    if (DataStore = nil) or
       (DataStore.Resource = nil) or
       (DataStore.Resource.Tasks.Count = 0)
    then begin
      if Focused then begin
        LineRect.TopLeft := Point(RealLeft + 2, HeadRect.Bottom);
        LineRect.BottomRight := Point(LineRect.Left + RealWidth - 4, LineRect.Top + RowHeight);
        RenderCanvas.Brush.Color := BackgroundSelHighlight;
        RenderCanvas.FillRect(LineRect);
        RenderCanvas.Brush.Color := RealColor;
      end;
      Exit;
    end;

    LineRect.TopLeft := Point(RealLeft + 2, HeadRect.Bottom);
    LineRect.BottomRight := Point(LineRect.Left + RealWidth - 4, LineRect.Top + RowHeight);

    tlVisibleItems := 0;
    RenderCanvas.Brush.Color := RealColor;

    tlAllTaskList.Clear;

    { Make sure the tasks are properly sorted }
    DataStore.Resource.Tasks.Sort;

    for I := 0 to pred(DataStore.Resource.Tasks.Count) do begin
      if DisplayOptions.ShowAll then
        { Get all tasks regardless of their status and due date }
        tlAllTaskList.Add(DataStore.Resource.Tasks.GetTask(I))
      else begin
        { get all tasks which are incomplete, or were just completed today.}
        Task := DataStore.Resource.Tasks.GetTask(I);
        if not Task.Complete then
          tlAllTaskList.Add(Task)
        else
        if FDisplayOptions.ShowCompletedTasks and SameDate(Task.CompletedOn, now) then
          tlAllTaskList.Add(Task);
      end;
    end;

    RenderCanvas.Font.Assign(Font);
    RenderCanvas.Font.Size := ScaleY(RenderCanvas.Font.Size, DesignTimeDPI);
    for I := StartLine to pred(tlAllTaskList.Count) do begin
      Task := tlAllTaskList[I];
      if (LineRect.Top + Trunc(RowHeight * 0.5) <= RealBottom) then begin
        { if this is the selected task and we are not in edit mode, }
        { then set background selection                             }
        if (Task = FActiveTask) and
           ((tlInPlaceEditor = nil) or not tlInplaceEditor.Visible) and
           (not DisplayOnly) and Focused
        then begin
          RenderCanvas.Brush.Color := BackgroundSelHighlight;
          RenderCanvas.FillRect(LineRect);
          RenderCanvas.Brush.Color := RealColor;
        end;

        { draw the checkbox }
        CheckRect := DrawCheck(LineRect, Task.Complete);

        if Task.Complete then begin
          { complete task }
          RenderCanvas.Font.Style := RenderCanvas.Font.Style + [fsStrikeout];
          RenderCanvas.Font.Color := RealCompleteColor;
        end else begin
          { incomplete task }
          RenderCanvas.Font.Style := RenderCanvas.Font.Style - [fsStrikeout];
          if (Trunc(Task.DueDate) < Trunc(Now)) and (Trunc(Task.DueDate) <> 0) then
            { overdue task }
            RenderCanvas.Font.Color := RealOverdueColor
          else
            RenderCanvas.Font.Color := RealNormalColor;
        end;

        { if this is the selected task, set highlight text color }
        if (Task = FActiveTask) and
           ((tlInPlaceEditor = nil) or not tlInplaceEditor.Visible) and
           (not DisplayOnly) and Focused
        then
          RenderCanvas.Font.Color := ForegroundSelHighlight;

        { build display string }
        DisplayStr := '';
        if (FDisplayOptions.ShowDueDate) and (Trunc(Task.DueDate) <> 0) then
          DisplayStr := FormatDateTime(FDisplayOptions.DueDateFormat, Task.DueDate) + ' - ';
        DisplayStr := DisplayStr + Task.Description;

        { Adjust display string - If the string is too long for the available }
        { space, Chop the end off and replace it with an ellipses.            }
        DisplayStr := GetDisplayString(RenderCanvas, DisplayStr, 3, WidthOf(LineRect) - CheckRect.Right - TextMargin);

        { paint the text }
        TPSTextOut(RenderCanvas, Angle, RenderIn, CheckRect.Right + TextMargin * 2, LineRect.Top + TextMargin, DisplayStr);

        { store the tasks drawing details }
        tlVisibleTaskArray[tlVisibleItems].Task := Task;
        tlVisibleTaskArray[tlVisibleItems].LineRect := Rect(
          CheckRect.Right + TextMargin,
          LineRect.Top,
          LineRect.Right,
          LineRect.Bottom
        );
        tlVisibleTaskArray[tlVisibleItems].CheckRect := CheckRect;
        LineRect.Top := LineRect.Bottom;
        LineRect.Bottom := LineRect.Top + RowHeight;
        Inc(tlVisibleItems);
      end else
      if (LineRect.Bottom - TextMargin > RealBottom) then begin
        FLastPrintLine := I;
        Break;
      end;
    end;
    if tlVisibleItems + tlItemsBefore = tlAllTaskList.Count then begin
      FLastPrintLine := -2;
      tlItemsAfter := 0;
    end else begin
      tlItemsAfter := tlAllTaskList.Count - tlItemsBefore - tlVisibleItems;
    end;

    { these are for the syncing the scrollbar }
    if StartLine < 0 then
      tlItemsBefore := 0
    else
      tlItemsBefore := StartLine;
  end;  // with TVpTaskListOpener(FTaskList)...
end;

procedure TVpTaskListPainter.FixFontHeights;
begin
  with FTaskList do begin
    Font.Height := GetRealFontHeight(Font);
    TaskHeadAttributes.Font.Height := GetRealFontHeight(TaskHeadAttributes.Font);
  end;
end;

procedure TVpTaskListPainter.InitColors;
begin
  if DisplayOnly then begin
    RealColor := clWhite;
    BackgroundSelHighlight := clBlack;
    ForegroundSelHighlight := clWhite;
    BevelShadow := clBlack;
    BevelHighlight := clBlack;
    BevelDarkShadow := clBlack;
    BevelFace := clBlack;
    RealLineColor := clBlack;
    RealCheckBgColor := clWhite;
    RealCheckBoxColor := clBlack;
    RealCheckColor := clBlack;
    RealCompleteColor := clBlack;
    RealOverdueColor := clBlack;
    RealNormalColor := clBlack;
    TaskHeadAttrColor := clSilver;
  end else begin
    RealColor := FTaskList.Color;
    BackgroundSelHighlight := clHighlight;
    ForegroundSelHighlight := clHighlightText;
    BevelShadow := clBtnShadow;
    BevelHighlight := clBtnHighlight;
    BevelDarkShadow := cl3DDkShadow;
    BevelFace := clBtnFace;
    RealLineColor := FTaskList.LineColor;
    RealCheckBgColor := FTaskList.DisplayOptions.CheckBGColor;
    RealCheckBoxColor := FTaskList.DisplayOptions.CheckColor;
    RealCheckColor := FTaskList.DisplayOptions.CheckColor;
    RealCompleteColor := FTaskList.DisplayOptions.CompletedColor;
    RealOverdueColor := FTaskList.DisplayOptions.OverdueColor;
    RealNormalColor := FTaskList.DisplayOptions.NormalColor;
    TaskHeadAttrColor := FTaskList.TaskHeadAttributes.Color;
  end;
end;

procedure TVpTaskListPainter.MeasureRowHeight;
begin
  RenderCanvas.Font.Assign(FTaskList.Font);
  RenderCanvas.Font.Size := ScaleY(RenderCanvas.Font.Size, DesignTimeDPI);
  RowHeight := RenderCanvas.TextHeight(VpProductName) + TextMargin * 2;
end;

procedure TVpTaskListPainter.RenderToCanvas(ARenderIn: TRect;
  AAngle: TVpRotationAngle; AScale: Extended; ARenderDate: TDateTime;
  AStartLine, AStopLine: Integer; AUseGran: TVpGranularity; ADisplayOnly: Boolean);
begin
  inherited;

  InitColors;
  SavePenBrush;
  InitPenBrush;
  if ADisplayOnly then FixFontHeights;

  Rgn := CreateRectRgn(RenderIn.Left, RenderIn.Top, RenderIn.Right, RenderIn.Bottom);
  try
    SelectClipRgn(RenderCanvas.Handle, Rgn);

    if StartLine < 0 then
      StartLine := 0;

    { clear client area }
    Clear;

    SetMeasurements;

    { measure the row height }
    MeasureRowHeight;

    { draw header }
    DrawHeader;

    { draw lines }
    DrawLines;

    { draw the tasks }
    DrawTasks;

    { draw the borders }
    DrawBorders;

    TVpTaskListOpener(FTaskList).tlSetVScrollPos;

  finally
    SelectClipRgn(RenderCanvas.Handle, 0);
    DeleteObject(Rgn);
    RestorePenBrush;
  end;
end;

end.
