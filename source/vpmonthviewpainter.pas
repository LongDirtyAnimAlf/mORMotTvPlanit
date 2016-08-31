{$I vp.inc}

unit VpMonthViewPainter;

interface

uses
  SysUtils, LCLType, LCLIntf, Types, Classes, Graphics,
  VpConst, VPBase, VpData, VpMonthView, VpBasePainter;

type
  TVpMonthViewPainter = class(TVpBasePainter)
  private
    FMonthView: TVpMonthView;
    // local parameters of the old TVpMonthView method
//    HeadRect: TRect;
    DisplayDate: TDateTime;
    RealColor: TColor;
    BevelHighlight: TColor;
    BevelShadow: TColor;
    BevelDarkShadow: TColor;
    BevelFace: TColor;
    DayHeadAttrColor: TColor;
    HeadAttrColor: TColor;
    RealLineColor: TColor;
    RealOffDayColor: TColor;
    RealSelDayColor: TColor;
    EventFontColor: TColor;
    TodayFontColor: TColor;
    TodayAttrColor: TColor;
    DotDotDotColor: TColor;

    // protected variables of the original monthview needed only for painting
    mvEventTextHeight: Integer;
    mvDayNumberHeight: Integer;
    mvRowHeight: Integer;
    mvColWidth: Integer;
    mvLineHeight: Integer;
    mvVisibleEvents: Integer;

  protected
    procedure Clear;
    procedure DrawBorders;
    procedure DrawDayHead;
    procedure DrawDays;
    procedure DrawFocusRect(ARect: TRect; FixRight: Boolean = false);
    procedure DrawHeader;
    procedure FixFontHeights;
    procedure InitColors;
    procedure SetMeasurements; override;

  public
    constructor Create(AMonthView: TVpMonthView; ARenderCanvas: TCanvas);
    procedure RenderToCanvas(ARenderIn: TRect; AAngle: TVpRotationAngle;
      AScale: Extended; ARenderDate: TDateTime; AStartLine, AStopLine: Integer;
      AUseGran: TVpGranularity; ADisplayOnly: Boolean); override;
  end;

implementation

uses
  LazUtf8,
  VpCanvasUtils, VpMisc;

type
  TVpMonthViewOpener = class(TVpMonthView);

constructor TVpMonthViewPainter.Create(AMonthView: TVpMonthView;
  ARenderCanvas: TCanvas);
begin
  inherited Create(ARenderCanvas);
  FMonthView := AMonthView;
end;

procedure TVpMonthViewPainter.Clear;
begin
  RenderCanvas.Brush.Color := RealColor;
  RenderCanvas.FillRect(RenderIn);
end;

procedure TVpMonthViewPainter.DrawBorders;
var
  R: TRect;
begin
  R := Rect(RealLeft, RealTop, RealRight - 1, RealBottom - 1);
  if FMonthView.DrawingStyle = dsFlat then begin
    { draw a simple rectangular border }
    DrawBevelRect(
      RenderCanvas,
      TPSRotateRectangle(Angle, RenderIn, R),
      BevelShadow,
      BevelShadow
    );
  end else
  if FMonthView.DrawingStyle = ds3d then begin
    { draw a 3d bevel }
    DrawBevelRect(
      RenderCanvas,
      TPSRotateRectangle(Angle, RenderIn, R),
      BevelShadow,
      BevelHighlight
    );
    InflateRect(R, -1, -1);
    DrawBevelRect(
      RenderCanvas,
      TPSRotateRectangle(Angle, RenderIn, R),
      BevelDarkShadow,
      BevelFace
    );
  end;
end;

procedure TVpMonthViewPainter.DrawDayHead;
var
  dhRect, R: TRect;
  I: Integer;
  DayTag: Integer;
  Str: string;
  StrL: Integer;
begin
  { clear day head area }
  RenderCanvas.Font.Assign(FMonthView.DayHeadAttributes.Font);
  RenderCanvas.Font.Size := ScaleY(RenderCanvas.Font.Size, DesignTimeDPI);
  RenderCanvas.Brush.Color := DayHeadAttrColor;

  { build rect }
  dhRect.Left := RealLeft;
  dhRect.Top := RealTop + TVpMonthViewOpener(FMonthView).mvDayHeadHeight;
  dhRect.Right := RealRight;
  dhRect.Bottom := dhRect.Top + TVpMonthViewOpener(FMonthView).mvDayHeadHeight;
  if FMonthView.DrawingStyle = ds3d then begin
    inc(dhRect.Left, 2);
    inc(dhRect.Top, 3);
    dec(dhRect.Right, 3);
    dhRect.Bottom := dhRect.Top + TVpMonthViewOpener(FMonthView).mvDayHeadHeight;
    TPSFillRect(RenderCanvas, Angle, RenderIn, dhRect);
    R := TPSRotateRectangle(Angle, RenderIn, dhRect);
    DrawBevelRect(RenderCanvas, R, BevelHighlight, BevelDarkShadow);
  end else
  if FMonthView.DrawingStyle = dsFlat then begin
    dhRect.Left := RealLeft;
    dhRect.Top := RealTop + TVpMonthViewOpener(FMonthView).mvDayHeadHeight;
    dhRect.Right := RealRight;
    dhRect.Bottom := dhRect.Top + TVpMonthViewOpener(FMonthView).mvDayHeadHeight;
    TPSFillRect(RenderCanvas, Angle, RenderIn, dhRect);
    R := TPSRotateRectangle(Angle, RenderIn, dhRect);
    DrawBevelRect(RenderCanvas, R, BevelShadow, BevelShadow);
  end else
  begin
    dhRect.Left := RealLeft;
    TPSFillRect(RenderCanvas, Angle, RenderIn, dhRect);
  end;

  DayTag := Ord(FMonthView.WeekStartsOn);
  dhRect.Right := dhRect.Left + mvColWidth;

  for I := 0 to 6 do begin
    { draw the little vertical lines between each day }
    if I < 6 then begin
      if FMonthView.DrawingStyle = ds3d then
      begin
        R := Rect(dhRect.Right-1, dhRect.Top + 3, dhRect.Right, dhRect.Bottom - 3);
        R := TPSRotateRectangle(Angle, RenderIn, R);
        DrawBevelRect(RenderCanvas, TPSRotateRectangle(Angle, RenderIn, R), BevelShadow, BevelHighlight);
      end else
      begin
        TPSMoveTo(RenderCanvas, Angle, RenderIn, dhRect.Right + 1, dhRect.Top + 4);
        TPSLineTo(RenderCanvas, Angle, RenderIn, dhRect.Right + 1, dhRect.Bottom - 3);
      end;
    end;

   {$IFDEF LCL}
    case FMonthView.DayNameStyle of
      dsLong :  { Draw each day's full caption... }
        str := FormatSettings.LongDayNames[DayTag+1];
      dsShort:  { Draw each day's abbreviated caption... }
        str := FormatSettings.ShortDayNames[DayTag+1];
      dsLetter:  { Draw each day's first letter only }
        str := FormatSettings.ShortDayNames[DayTag+1, 1];
    end;
   {$ELSE}
    case FMontheView.DayNameStyle of
      dsLong:   { Draw each day's full caption... }
        case DayTag of
          0: str := RSSunday;
          1: str := RSMonday;
          2: str := RSTuesday;
          3: str := RSWednesday;
          4: str := RSThursday;
          5: str := RSFriday;
          6: str := RSSaturday;
        end
      dsShort:  { Draw each day's abbreviated caption... }
        case DayTag of
          0: str := RSASunday;
          1: str := RSAMonday;
          2: str := RSATuesday;
          3: str := RSAWednesday;
          4: str := RSAThursday;
          5: str := RSAFriday;
          6: str := RSASaturday;
        end
      dsLetter: { Draw each day's first letter only }
        case DayTag of
          0: str := RSLSunday;
          1: str := RSLMonday;
          2: str := RSLTuesday;
          3: str := RSLWednesday;
          4: str := RSLThursday;
          5: str := RSLFriday;
          6: str := RSLSaturday;
        end;
    end;
   {$ENDIF}

    { Fix Header String }
    StrL := RenderCanvas.TextWidth(Str);
    if (StrL > mvColWidth - TextMargin * 2) then
      Str := GetDisplayString(RenderCanvas, Str, 0, mvColWidth - TextMargin * 2);
    StrL := RenderCanvas.TextWidth(Str);

    TPSTextOut(
      RenderCanvas,
      Angle,
      RenderIn,
      (dhRect.Left + dhRect.Right - StrL) div 2,
      dhRect.Top + TextMargin - 1,
      Str
    );

    if DayTag = 6 then
      DayTag := 0
    else
      Inc(DayTag);

    dhRect.Left := dhRect.Right;
    dhRect.Right := dhRect.Left + mvColWidth;
  end;
end;

procedure TVpMonthViewPainter.DrawDays;
var
  TextRect: TRect;
  Col, Row: Integer;
  DayNumber: Integer;
  M, D, Y, Tmp: Word;
  MonthStartsOn: Integer;
  DayTag: Integer;
  DayOffset: Integer;
  StartingDate: TDateTime;
  ThisDate: TDateTime;
  Str: string;
  StrLn: Integer;
  I, J: Integer;
  EventList: TList;
  Drawn: Boolean;
  TextAdjust: Integer;
  TextH: Integer;
  FontStyle: TFontStyles;
  OldBrush: TBrush;
  OldPen: TPen;
  OldFont: TFont;
  todayDate: TDate;
  tmpRect: TRect;
  hDayHead: Integer;
begin
  todayDate := Date();

  { initialize the MonthDayArray }
  with TVpMonthViewOpener(FMonthView) do begin
    for I := 0 to Pred(Length(mvMonthDayArray)) do begin
      mvMonthDayArray[I].Rec := Rect(-1, -1, -1, -1);
      mvMonthDayArray[I].Date := 0.0;
    end;
    hDayHead := mvDayHeadHeight;
  end;

  RenderCanvas.Pen.Color := RealLineColor;
  RenderCanvas.Brush.Color := RealColor;

  if FMonthView.DrawingStyle = ds3d then
  begin
    mvRowHeight := (RealHeight - hDayHead * 2 - 4) div 6;
    TextRect.TopLeft := Point(RealLeft + 1, RealTop + hDayHead * 2 + 4);
  end else
  begin
    mvRowHeight := (RealHeight - hDayHead * 2) div 6;
    TextRect.TopLeft := Point(RealLeft + 1, RealTop + hDayHead * 2);
  end;
  TextRect.BottomRight := Point(TextRect.Left +  mvColWidth, TextRect.Top + mvRowHeight);

  { Determine the starting date and offset }
  DecodeDate(DisplayDate, Y, M, D);
  StartingDate := EncodeDate(Y, M, 1);
  MonthStartsOn := DayOfWeek(StartingDate);
  DayTag := Ord(FMonthView.WeekStartsOn);
  DayOffset := DayTag - MonthStartsOn;

  I := 0;
  DayNumber := DayOffset + 1;

  // iterate through each column row by row, drawing each day in numerical order.
  OldBrush := TBrush.Create;
  try
    OldPen := TPen.Create;
    try
      OldFont := TFont.Create;
      try
        for Row := 0 to 5 do begin
          for Col := 0 to 6 do begin
            ThisDate := Trunc(StartingDate + DayNumber);
            DecodeDate(ThisDate, Y, Tmp, D);

            { Allow the user to draw the day }
            Drawn  := false;
            if Assigned(FMonthView.OwnerDrawCells) then begin
              { wp: Using Canvas here does not look correct...
              OldBrush.Assign (Canvas.Brush);
              OldPen.Assign (Canvas.Pen);
              OldFont.Assign (Canvas.Font); }
              OldBrush.Assign(RenderCanvas.Brush);
              OldPen.Assign(RenderCanvas.Pen);
              OldFont.Assign(RenderCanvas.Font);
              try
                FMonthView.OwnerDrawCells(self, RenderCanvas, TextRect, D, Drawn);
                if Drawn then continue;
              finally
                { wp: Using Canvas here does not look correct...
                Canvas.Brush.Assign (OldBrush);
                Canvas.Pen.Assign (OldPen);
                Canvas.Font.Assign (OldFont); }
                RenderCanvas.Brush.Assign(OldBrush);
                RenderCanvas.Pen.Assign(OldPen);
                RenderCanvas.Font.Assign(OldFont);
              end;
            end;

            { draws the far right day for this week }
            if (Col = 6) then begin
              TextRect.Right := TextRect.Right + 8;
              tmpRect := TextRect;
              if TextRect.Bottom > RealBottom then
                tmpRect.Bottom := RealBottom;
              if Tmp <> M then begin
                RenderCanvas.Brush.Color := RealOffDayColor;
                TPSFillRect(RenderCanvas, Angle, RenderIn, tmpRect);
              end else
              {
              if ThisDate = todayDate then begin
                RenderCanvas.Brush.Color := TodayAttrColor;
                TPSFillRect(RenderCanvas, Angle, RenderIn, tmpRect);
              end else
              }
                RenderCanvas.Brush.Color := RealColor;

              { draw bottom line }
              TPSMoveTo(RenderCanvas, Angle, RenderIn, TextRect.Left, TextRect.Bottom);
              TPSLineTo(RenderCanvas, Angle, RenderIn, RealRight - 2, TextRect.Bottom);

              { Paint the day number }
              Str := FormatDateTime('d', ThisDate);

              { set the proper font and style }
              if ThisDate = todayDate then
                RenderCanvas.Font.Assign(FMonthView.TodayAttributes.Font)
              else
                RenderCanvas.Font.Assign(FMonthView.DayNumberFont);
              RenderCanvas.Font.Size := ScaleY(RenderCanvas.Font.Size, DesignTimeDPI);
              fontstyle := RenderCanvas.Font.style;

              if (DisplayDate = ThisDate) then begin
                if FMonthView.Focused then begin
                  tmpRect := TextRect;
                  dec(tmpRect.Right, 4);
                  DrawFocusRect(tmpRect, true);
                end;
                RenderCanvas.Font.Color := RealSelDayColor;
                RenderCanvas.Font.Style := FMonthView.DayNumberFont.Style + [fsBold];
                if (FMonthView.EventDayStyle <> []) and (FMonthView.DataStore.Resource <> nil) and
                   (FMonthView.DataStore.Resource.Schedule.EventCountByDay(ThisDate) > 0)
                then
                  RenderCanvas.Font.Style := RenderCanvas.Font.Style + FMonthView.EventDayStyle;
              end else begin
                { Set the font style for days which have events. }
                if (FMonthView.EventDayStyle <> []) and (FMonthView.DataStore.Resource <> nil) and
                   (FMonthView.DataStore.Resource.Schedule.EventCountByDay(ThisDate) > 0)
                then
                  RenderCanvas.Font.Style := RenderCanvas.Font.Style + FMonthView.EventDayStyle
                else begin
                  RenderCanvas.Font.Color := EventFontColor;
                  RenderCanvas.Font.Style := FMonthView.DayNumberFont.Style;
                end;
              end;

              FontStyle := RenderCanvas.Font.Style;
              RenderCanvas.Font.Style := [fsBold, fsItalic];
              TextAdjust := RenderCanvas.TextWidth(Str);
              TextH := RenderCanvas.TextHeight(Str);
              RenderCanvas.Font.Style := FontStyle;
              if Tmp <> M then
                RenderCanvas.Font.Color := FMonthView.OffDayFontColor;

              { Calculate size of rect for the day number at the top of the TextRect. }
              tmpRect.Left := TextRect.Left + mvColWidth - TextAdjust - TextMargin;
              if fsItalic in RenderCanvas.Font.Style then
                dec(tmpRect.Left, 2);
              tmpRect.Top := TextRect.Top + TextMargin div 2;
              tmpRect.Right := tmpRect.Left + TextAdjust;
              tmpRect.Bottom := tmpRect.Top + RenderCanvas.TextHeight(Str);

              { Highlight today by a border }
              if ThisDate = todayDate then begin
                InflateRect(tmpRect, 3, 3);
                RenderCanvas.Pen.Assign(FMonthView.TodayAttributes.BorderPen);
                RenderCanvas.Brush.Color := FMonthView.TodayAttributes.Color;
                RenderCanvas.Brush.Style := bsSolid; //bsClear;
                RenderCanvas.Rectangle(tmpRect);
                RenderCanvas.Pen.Assign(OldPen);
                RenderCanvas.Brush.Assign(OldBrush);
                InflateRect(tmpRect, -3, -3);
                RenderCanvas.Font.Color := FMonthView.TodayAttributes.Font.Color;
              end;

              { Write the day number at the top of the TextRect. }
              TPSTextOut(RenderCanvas, Angle, RenderIn, tmpRect.Left, tmpRect.Top, Str);

              { Update MonthDayArray }
              with TVpMonthViewOpener(FMonthView) do begin
                mvMonthDayArray[I].Rec := TextRect;
                mvMonthDayArray[I].Date := ThisDate;
                mvMonthDayArray[I].OffDay := Tmp <> M;
              end;
              Inc(DayNumber);
              Inc(I);

              { drop rect down one row and all the way to the left }
              TextRect.TopLeft := Point(
                RealLeft + 1,
                TextRect.Bottom + 1
              );
              TextRect.BottomRight := Point(
                TextRect.Left + mvColWidth,
                TextRect.Top + mvRowHeight
              );
            end  // if Col = 6 ...
            else
            { draws all days for the week, except the far right one }
            begin
              if Tmp <> M then begin
                RenderCanvas.Brush.Color := RealOffDayColor;
                TPSFillRect(RenderCanvas, Angle, RenderIn, TextRect);
              end else
              {
              if ThisDate = todayDate then begin
                RenderCanvas.Brush.Color := TodayAttrColor;
                TPSFillRect(RenderCanvas, Angle, RenderIn, TextRect);
              end else
              }
                RenderCanvas.Brush.Color := RealColor;

              { draw right side and bottom lines }
              TPSMoveTo(RenderCanvas, Angle, RenderIn, TextRect.Right, TextRect.top);
              if TextRect.Bottom > RealBottom then begin
                TPSLineTo(RenderCanvas, Angle, RenderIn, TextRect.Right, RealBottom);
                TPSLineTo(RenderCanvas, Angle, RenderIn, TextRect.Left - 1, RealBottom);
              end else begin
                TPSLineTo(RenderCanvas, Angle, RenderIn, TextRect.Right, TextRect.Bottom);
                TPSLineTo(RenderCanvas, Angle, RenderIn, TextRect.Left - 1, TextRect.Bottom);
              end;

              { paint the day number }
              Str := FormatDateTime('d', ThisDate);

              { set the proper font and style }
              if ThisDate = todayDate then
                RenderCanvas.Font.Assign(FMonthView.TodayAttributes.Font)
              else
                RenderCanvas.Font.Assign(FMonthView.DayNumberFont);
              RenderCanvas.Font.Size := ScaleY(RenderCanvas.Font.Size, DesignTimeDPI);
              fontstyle := Rendercanvas.Font.Style;

              if (DisplayDate = ThisDate) then begin
                if FMonthView.Focused then
                  DrawFocusRect(TextRect);
                RenderCanvas.Font.Color := RealSelDayColor;
                RenderCanvas.Font.Style := FMonthView.DayNumberFont.Style + [fsBold];
                if (FMonthView.EventDayStyle <> []) and (FMonthView.DataStore.Resource <> nil) and
                   (FMonthView.DataStore.Resource.Schedule.EventCountByDay(ThisDate) > 0)
                then
                  RenderCanvas.Font.Style := RenderCanvas.Font.Style + FMonthView.EventDayStyle;
              end else begin
                { Set the font style for days which have events. }
                if (FMonthView.EventDayStyle <> []) and (FMonthView.DataStore.Resource <> nil) and
                   (FMonthView.DataStore.Resource.Schedule.EventCountByDay(ThisDate) > 0)
                then
                  RenderCanvas.Font.Style := RenderCanvas.Font.Style + FMonthView.EventDayStyle
                else begin
                  RenderCanvas.Font.Color := EventFontColor;
                  RenderCanvas.Font.Style := FMonthView.DayNumberFont.Style;
                end;
              end;

              FontStyle := RenderCanvas.Font.Style;
              RenderCanvas.Font.Style := [fsBold, fsItalic];
              TextAdjust := RenderCanvas.TextWidth(Str);
              TextH := RenderCanvas.TextHeight(Str);
              RenderCanvas.Font.Style := FontStyle;
              if Tmp <> M then
                RenderCanvas.Font.Color := FMonthView.OffdayFontColor;

              { Calculate rectangle for day number at the top of the TextRect. }
              tmpRect.Left := TextRect.Right - TextAdjust - TextMargin;
              if fsItalic in RenderCanvas.Font.Style then
                dec(tmpRect.Left, 2);
              tmpRect.Top := TextRect.Top + TextMargin div 2;
              tmpRect.Right := tmpRect.Left + TextAdjust;
              tmpRect.Bottom := tmpRect.Top + TextH;

              { Highlight today by a border }
              if ThisDate = todayDate then begin
                InflateRect(tmpRect, 3, 3);
                RenderCanvas.Pen.Assign(FMonthView.TodayAttributes.BorderPen);
                RenderCanvas.Brush.Color := FMonthView.TodayAttributes.Color;
                RenderCanvas.Brush.Style := bsSolid; //bsClear;
                RenderCanvas.Rectangle(tmpRect);
                RenderCanvas.Pen.Assign(OldPen);
                RenderCanvas.Brush.Assign(OldBrush);
                InflateRect(tmpRect, -3, -3);
                RenderCanvas.Font.Color := FMonthView.TodayAttributes.Font.Color;
              end;

              { Write the day number at the top of the TextRect. }
              TPSTextOut(RenderCanvas, Angle, RenderIn, tmpRect.Left, tmpRect.Top, Str);


              { Update Array }
              with TVpMonthViewOpener(FMonthView) do begin
                mvMonthDayArray[I].Rec := TextRect;
                mvMonthDayArray[I].Date := ThisDate;
                mvMonthDayArray[I].OffDay := Tmp <> M;
              end;
              Inc(DayNumber);
              Inc(I);

              { slide rect one column to the right }
              TextRect.Left := TextRect.Right + 1;
              TextRect.Right := TextRect.Right + mvColWidth;
            end;
          end;
        end;

      finally
        OldFont.Free;
      end;
    finally
      OldPen.Free;
    end;
  finally
    OldBrush.Free;
  end;

  RenderCanvas.Pen.Color := RealLineColor;
  RenderCanvas.Pen.Style := psSolid;
  RenderCanvas.Brush.Color := RealColor;

  { write the events }
  if (FMonthView.DataStore <> nil) and FMonthVIew.ShowEvents and
     (FMonthView.DataStore.Resource <> nil) and
     (FMonthView.DataStore.Resource.Schedule.EventCount <> 0)
  then begin
    EventList := TList.Create;
    try
      for I := 0 to 43 do begin
        EventList.Clear;
        FMonthVIew.DataStore.Resource.Schedule.EventsByDate(TVpMonthViewOpener(FMonthView).mvMonthDayArray[I].Date, EventList);
        if EventList.Count > 0 then begin
          { there are events scheduled for this day }

          { initialize TextRect for this day }
          TextRect.TopLeft := Point(
            TVpMonthViewOpener(FMonthView).mvMonthDayArray[I].Rec.Left,
            TVpMonthViewOpener(FMonthView).mvMonthDayArray[I].Rec.Top
          );
          TextRect.BottomRight := Point(
            TextRect.Left + mvColWidth,
            TextRect.Top + mvEventTextHeight + TextMargin div 2
          );

          { set canvas color }
          if TVpMonthViewOpener(FMonthView).mvMonthDayArray[I].OffDay
            then RenderCanvas.Brush.Color := RealOffDayColor
            else RenderCanvas.Brush.Color := RealColor;

          { spin through the events and paint them }
          for J := 0 to Pred(EventList.Count) do begin
            if (TextRect.Bottom > TVpMonthViewOpener(FMonthView).mvMonthDayArray[I].Rec.Bottom) and
               (J <= Pred(EventList.Count))
            then begin
              { draw a little red square with a (...) at the bottom right }
              { corner of the day to indicate that there are more events  }
              { than can be listed in the available space.                }
              RenderCanvas.Brush.Color := DotDotDotColor;

              { draw dot dot dot }
              TPSFillRect(
                RenderCanvas,
                Angle,
                RenderIn,
                Rect(
                  TVpMonthViewOpener(FMonthView).mvMonthDayArray[I].Rec.Right - 20,
                  TVpMonthViewOpener(FMonthView).mvMonthDayArray[I].Rec.Bottom - 7,
                  TVpMonthViewOpener(FMonthView).mvMonthDayArray[I].Rec.Right - 17,
                  TVpMonthViewOpener(FMonthView).mvMonthDayArray[I].Rec.Bottom - 4
                )
              );
              TPSFillRect(
                RenderCanvas,
                Angle,
                RenderIn,
                Rect(
                  TVpMonthViewOpener(FMonthView).mvMonthDayArray[I].Rec.Right - 13,
                  TVpMonthViewOpener(FMonthView).mvMonthDayArray[I].Rec.Bottom - 7,
                  TVpMonthViewOpener(FMonthView).mvMonthDayArray[I].Rec.Right - 10,
                  TVpMonthViewOpener(FMonthView).mvMonthDayArray[I].Rec.Bottom - 4
                )
              );
              TPSFillRect(
                RenderCanvas,
                Angle,
                RenderIn,
                Rect(
                  TVpMonthViewOpener(FMonthView).mvMonthDayArray[I].Rec.Right -  6,
                  TVpMonthViewOpener(FMonthView).mvMonthDayArray[I].Rec.Bottom - 7,
                  TVpMonthViewOpener(FMonthView).mvMonthDayArray[I].Rec.Right -  3,
                  TVpMonthViewOpener(FMonthView).mvMonthDayArray[I].Rec.Bottom - 4
                )
              );
              Break;
            end;

            { shorten events that are next to the day number, in order }
            { to give the day number enough room }
            if (TextRect.Top < TVpMonthViewOpener(FMonthView).mvMonthDayArray[I].Rec.Top + mvDayNumberHeight + TextMargin div 2)
            then
              TextRect.Right := TextRect.Left + mvColWidth - mvDayNumberHeight - TextMargin
            else
              TextRect.Right := TextRect.Left + mvColWidth;

            { format the display text }
            if FMonthView.ShowEventTime then begin
              if (FMonthView.TimeFormat = tf24Hour) then
                Str := FormatDateTime('hh:nn',
                  TVpEvent(EventList.List^[j]).StartTime)
              else
                Str := FormatDateTime('hh:nn AM/PM',
                  TVpEvent(EventList.List^[j]).StartTime);
              Str := Str + ' - ' + TVpEvent(EventList.List^[j]).Description;
            end else
              Str := TVpEvent(EventList.List^[j]).Description;

            { set the event font }
            RenderCanvas.Font.Assign(FMonthView.EventFont);
            RenderCanvas.Font.Size := ScaleY(RenderCanvas.Font.Size, DesignTimeDPI);
            if TVpMonthViewOpener(FMonthView).mvMonthDayArray[I].OffDay then
              RenderCanvas.Font.Color := FMonthView.OffDayFontColor;

            StrLn := RenderCanvas.TextWidth(Str);
            if (StrLn > TextRect.Right - TextRect.Left - TextMargin * 2) then
            begin
              Str := GetDisplayString(
                RenderCanvas,
                Str,
                0,
                TextRect.Right - TextRect.Left - TextMargin * 2
              );
            end;

            { write the event text }
            TPSTextOut(
              RenderCanvas,
              Angle,
              RenderIn,
              TextRect.Left + TextMargin div 2,
              TextRect.Top + TextMargin div 2,
              Str
            );

            { - begin block}
            with TVpMonthViewOpener(FMonthView) do begin
              Inc(mvVisibleEvents);
              mvEventArray[mvVisibleEvents - 1].Rec := TextRect;
              mvEventArray[mvVisibleEvents - 1].Event := TVpEvent(EventList.List^[j]);
            end;
            { - end block}

            { Move TextRect down one line for the next item... }
            TextRect.Top := TextRect.Bottom + 1;
            TextRect.Bottom := TextRect.Top + mvLineHeight;
          end;
        end;
      end;
    finally
      EventList.Free;
    end;
  end;
end;

procedure TVpMonthViewPainter.DrawFocusRect(ARect: TRect; FixRight: Boolean = false);
var
  tmpRect: TRect;
begin
  tmpRect := ARect;
  InflateRect(tmpRect, 2, 2);
  TPSDrawFocusRect(RenderCanvas, Angle, RenderIn, tmpRect);

  tmpRect := ARect;
  InflateRect(tmpRect, -2, -2);
  if FixRight then
    inc(tmpRect.Right);
  TPSDrawFocusRect(RenderCanvas, Angle, RenderIn, tmpRect);
end;

procedure TVpMonthViewPainter.DrawHeader;
var
  HeadRect: TRect;
  HeadTextRect: TRect;
  HeadStr: string;
  HeadStrLen : Integer;
  dayHeadHeight: Integer;
  R: TRect;
begin
  RenderCanvas.Brush.Color := HeadAttrColor;
  dayHeadHeight := TVpMonthViewOpener(FMonthView).mvDayHeadHeight;

  HeadRect := Rect(RealLeft, RealTop, RealRight, RealTop + dayHeadHeight);

  { Draw the header cell and borders }
  if FMonthView.DrawingStyle = dsFlat then begin
    // draw a flat rectanbular border
    TPSFillRect(RenderCanvas, Angle, RenderIn, HeadRect);
    R := TPSRotateRectangle(Angle, RenderIn, HeadRect);
    DrawBevelRect(RenderCanvas, R, BevelShadow, BevelShadow);
  end else
  if FMonthView.DrawingStyle = ds3d then begin
    // draw a 3d bevel
    InflateRect(HeadRect, -2, -2);
    dec(HeadRect.Right);
    HeadRect.Bottom := HeadRect.Top + dayHeadHeight;
    TPSFillRect(RenderCanvas, Angle, RenderIn, HeadRect);
    R := TPSRotateRectangle(Angle, RenderIn, HeadRect);
    DrawBevelRect(RenderCanvas, R, BevelHighlight, BevelDarkShadow);
  end else
    TPSFillRect(RenderCanvas, Angle, RenderIn, HeadRect);

  { Position the spinner }
  with TVpMonthViewOpener(FMonthView) do begin
    mvSpinButtons.Height := dayHeadHeight - 3;
    mvSpinButtons.Width := mvSpinButtons.Height * 2;
    mvSpinButtons.Left := TextMargin;
    mvSpinButtons.Top := HeadRect.Top + (dayHeadHeight - mvSpinButtons.Height) div 2 + 1;
  end;

  { Acquire startdate and end date }
  HeadStr := FormatDateTime(FMonthView.DateLabelFormat, DisplayDate);
  {$IFDEF FPC}{$IF FPC_FULLVERSION < 30000}
  HeadStr := SysToUTF8(HeadStr);
  {$ENDIF}{$ENDIF}

  { Calculate the text rectangle }
  RenderCanvas.Font.Assign(FMonthView.HeadAttributes.Font);
  RenderCanvas.Font.Size := ScaleY(RenderCanvas.Font.Size, DesignTimeDPI);
  if DisplayOnly and (RenderCanvas.TextWidth(HeadStr) >= RealWidth) then
    HeadTextRect.Left:= RealLeft + TextMargin * 2
  else
  if DisplayOnly then
    HeadTextRect.Left := RealLeft + (RealWidth - RenderCanvas.TextWidth(HeadStr)) div 2
  else
    HeadTextRect.Left := RealLeft + 30 + TextMargin * 2;
  HeadTextRect.Top := (HeadRect.Top + HeadRect.Bottom - RenderCanvas.TextHeight('Tg')) div 2;
  HeadTextRect.BottomRight := HeadRect.BottomRight;

  { Fix Header String }
  HeadStrLen := RenderCanvas.TextWidth(HeadStr);

  if HeadStrLen > HeadTextRect.Right - HeadTextRect.Left then begin
    HeadStr := GetDisplayString(
      RenderCanvas,
      HeadStr,
      0,
      HeadTextRect.Right - HeadTextRect.Left - TextMargin
    );
  end;

  // Draw the text
  RenderCanvas.Font.Assign(FMonthView.HeadAttributes.Font);
  RenderCanvas.Font.Size := ScaleY(RenderCanvas.Font.Size, DesignTimeDPI);
  TPSTextOut(
    RenderCanvas,
    Angle,
    RenderIn,
    RealLeft + TVpMonthViewOpener(FMonthView).mvSpinButtons.Width + TextMargin * 2,
    HeadTextRect.Top, // this vertical position is already centered
    HeadStr
  );
end;

procedure TVpMonthViewPainter.FixFontHeights;
begin
  with FMonthView do begin
    HeadAttributes.Font.Height := GetRealFontHeight(HeadAttributes.Font);
    DayHeadAttributes.Font.Height := GetRealFontHeight(DayHeadAttributes.Font);
    DayNumberFont.Height := GetRealFontHeight(DayNumberFont);
    EventFont.Height := GetRealFontHeight(EventFont);
    Font.Height := GetRealFontHeight(Font);
  end;
end;

procedure TVpMonthViewPainter.InitColors;
begin
  if DisplayOnly then begin
    BevelHighlight := clBlack;
    BevelShadow := clBlack;
    BevelDarkShadow := clBlack;
    BevelFace := clBlack;
    RealColor := clWhite;
    DayHeadAttrColor := clSilver;
    HeadAttrColor := clSilver;
    RealLineColor := clBlack;
    RealOffDayColor := clSilver;
    RealSelDayColor := clWhite;
    EventFontColor := clBlack;
    TodayFontColor := clBlack;
    TodayAttrColor := clWhite;
  end else begin
    BevelHighlight := clBtnHighlight;
    BevelShadow := clBtnShadow;
    BevelDarkShadow := cl3DDkShadow;
    BevelFace := clBtnFace;
    RealColor := FMonthView.Color;
    HeadAttrColor := FMonthView.HeadAttributes.Color;
    DayHeadAttrColor := FMonthView.DayHeadAttributes.Color;
    RealLineColor := FMonthView.LineColor;
    RealOffDayColor := FMonthView.OffDayColor;
    RealSelDayColor := FMonthView.SelectedDayColor;
    EventFontColor := FMonthView.DayNumberFont.Color;
    TodayFontColor := FMonthView.TodayAttributes.Font.Color;
    TodayAttrColor := FMonthView.TodayAttributes.Color;
  end;
  DotDotDotColor := clBlack;
end;

procedure TVpMonthViewPainter.RenderToCanvas(ARenderIn: TRect;
  AAngle: TVpRotationAngle; AScale: Extended; ARenderDate: TDateTime;
  AStartLine, AStopLine: Integer; AUseGran: TVpGranularity; ADisplayOnly: Boolean);
begin
  inherited;

  InitColors;
  SavePenBrush;
  InitPenBrush;
  if ADisplayOnly then
    FixFontHeights;

  Rgn := CreateRectRgn(RenderIn.Left, RenderIn.Top, RenderIn.Right, RenderIn.Bottom);
  try
    SelectClipRgn(RenderCanvas.Handle, Rgn);

    { clear client area }
    Clear;

    { measure the row heights }
    SetMeasurements;

    { draw headers }
    DrawHeader;
    DrawDayHead;

    { draw days }
    mvVisibleEvents := 0;
    DrawDays;

    { draw the borders }
    DrawBorders;

  finally
    SelectClipRgn(RenderCanvas.Handle, 0);
    DeleteObject(Rgn);
  end;

  { reinstate canvas settings}
  RestorePenBrush;
end;

procedure TVpMonthViewPainter.SetMeasurements;
begin
  inherited;

  if RenderDate = 0 then
    DisplayDate := Date
  else
    DisplayDate := RenderDate;

  { we use the VpProductName because is is a good representation of some }
  { generic text }
  RenderCanvas.Font.Assign(FMonthView.DayHeadAttributes.Font);
  RenderCanvas.Font.Size := ScaleY(RenderCanvas.Font.Size, DesignTimeDPI);
  with TVpMonthViewOpener(FMonthView) do
    mvDayHeadHeight := RenderCanvas.TextHeight(VpProductName) + TextMargin + 2;

  RenderCanvas.Font.Assign(FMonthView.DayNumberFont);
  RenderCanvas.Font.Size := ScaleY(RenderCanvas.Font.Size, DesignTimeDPI);
  mvDayNumberHeight := RenderCanvas.TextHeight('00');

  RenderCanvas.Font.Assign(FMonthView.EventFont);
  RenderCanvas.Font.Size := ScaleY(RenderCanvas.Font.Size, DesignTimeDPI);
  mvEventTextHeight := RenderCanvas.TextHeight(VpProductName);

  RenderCanvas.Font.Assign(FMonthView.Font);
  RenderCanvas.Font.Size := ScaleY(RenderCanvas.Font.Size, DesignTimeDPI);
  mvLineHeight := RenderCanvas.TextHeight(VpProductName) + 2;
  mvColWidth := (RealWidth - 4) div 7;
end;

end.
