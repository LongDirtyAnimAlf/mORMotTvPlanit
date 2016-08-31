{$I vp.inc}

unit VpWeekViewPainter;

interface

uses
  SysUtils, LCLType, LCLIntf, Types,
  Classes, Graphics, VpConst, VPBase, VpData, VpBasePainter, VpWeekView;

type
  TVpWeekViewPainter = class(TVpBasePainter)
  private
    FWeekView: TVpWeekView;
    FDayHeadHeight: Integer;

    // local parameters of the old TVpWeekView method
//    HeadRect: TRect;
    DayRectHeight: Integer;
//    StrLn: Integer;
    StartDate: TDateTime;
    ADEventsRect: TRect;
    DotDotDotColor: TColor;
    BevelHighlightColor: TColor;
    BevelShadowColor: TColor;
    BevelDarkShadow: TColor;
    BevelButtonFace: TColor;
    RealLineColor: TColor;
    RealDayHeadAttrColor: TColor;
    RealColor: TColor;
    RealHeadAttrColor: TColor;
    ADBackgroundColor: TColor;
    ADEventBackgroundColor: TColor;
    ADEventBorderColor: TColor;

  protected
    procedure Clear;
    function DrawAllDayEvents(ADate: TDateTime; DayRect: TRect; var EAIndex: Integer): Boolean;
    procedure DrawBorders;
    procedure DrawFocusRect(ADayIndex: Integer; DayRect: TRect);
    procedure DrawDay(ADayIndex: Integer; var DayRect: TRect; var EAIndex: Integer);
    procedure DrawDayHeader(ADayIndex: Integer; var TextRect: TRect);
    procedure DrawDays;
    procedure DrawEvent(AEvent: TVpEvent; TextRect: TRect; ADayIndex: Integer);
    procedure DrawHeader;
    procedure FixFontHeights;
    procedure InitColors;
    procedure SetMeasurements; override;

  public
    constructor Create(AWeekView: TVpWeekView; ARenderCanvas: TCanvas);
    procedure RenderToCanvas(ARenderIn: TRect; AAngle: TVpRotationAngle;
      AScale: Extended; ARenderDate: TDateTime; AStartLine, AStopLine: Integer;
      AUseGran: TVpGranularity; ADisplayOnly: Boolean); override;
  end;


implementation

uses
  StrUtils, Math,
 {$IFDEF LCL}
  LazUtf8, DateUtils,
 {$ENDIF}
  VpCanvasUtils, VpMisc, VpSR;

type
  TVpWeekViewOpener = class(TVpWeekView);

constructor TVpWeekViewPainter.Create(AWeekView: TVpWeekView;
  ARenderCanvas: TCanvas);
begin
  inherited Create(ARenderCanvas);
  FWeekView := AWeekView;
end;

procedure TVpWeekViewPainter.Clear;
begin
  RenderCanvas.Brush.Color := RealColor;
  RenderCanvas.FillRect(RenderIn);
end;

function TVpWeekViewPainter.DrawAllDayEvents(ADate: TDateTime; DayRect: TRect;
  var EAIndex: Integer): Boolean;
var
  ADEventsList: TList;
  TempList: TList;
  I, J, K: Integer;
  Event: TVpEvent;
  ADEvRect: TRect;
  StartsBeforeRange: Boolean;
  NumADEvents: Integer;
  Skip: Boolean;
  ADTextHeight: Integer;
  EventStr: string;
  txtDist: Integer;
begin
  Result := False;

  { initialize the All Day Events area... }
  ADEventsRect := DayRect;

  if (FWeekView.DataStore = nil) or (FWeekView.DataStore.Resource = nil) then
    Exit;

  { Collect all of the events for this range and determine the maximum     }
  { number of all day events for the range of days covered by the control. }
  NumADEvents := 0;

  ADEventsList := TList.Create;
  try
    TempList := TList.Create;
    try
      { get the all day events for the day specified by ADate + I }
      FWeekView.DataStore.Resource.Schedule.AllDayEventsByDate(ADate, TempList);

      { Iterate through these events and place them in ADEventsList }
      Skip := false;
      for J := 0 to pred(TempList.Count) do begin
        if AdEventsList.Count > 0 then begin
          for K := 0 to pred(AdEventsList.Count) do begin
            if TVpEvent(AdEventsList[K]) = TVpEvent(TempList[J]) then begin
              Skip := true;
              Break;
            end;
          end;
          if not Skip then
            AdEventsList.Add(TempList[J]);
        end else
          AdEventsList.Add(TempList[J]);
      end;

      if TempList.Count > NumADEvents then
        NumADEvents := TempList.Count;
    finally
      TempList.Free;
    end;

    if NumADEvents > 0 then begin
      { Set attributes }
      RenderCanvas.Brush.Color := ADBackgroundColor;

      { Measure the AllDayEvent TextHeight }
      txtDist := TextMargin div 2;
      RenderCanvas.Font.Assign(FWeekView.AllDayEventAttributes.Font);
      RenderCanvas.Font.Size := ScaleY(RenderCanvas.Font.Size, DesignTimeDPI);
      ADTextHeight := RenderCanvas.TextHeight(VpProductName) + TextMargin + txtDist;

      { Build the AllDayEvent rect based on the value of NumADEvents }
      if AdEventsRect.Top + (NumADEvents * ADTextHeight) + TextMargin * 2 > DayRect.Bottom
      then
        ADEventsRect.Bottom := DayRect.Bottom
      else
        ADEventsRect.Bottom := AdEventsRect.Top + NumADEvents * ADTextHeight + TextMargin * 2;

      // Clear the AllDayEvents area
      TpsFillRect(RenderCanvas, Angle, RenderIn, ADEventsRect);

      StartsBeforeRange := false;

      // Cycle through the all day events and draw them appropriately
      for I := 0 to pred(ADEventsList.Count) do begin
        Event := ADEventsList[I];

        // Draw "..."
        if ADEventsRect.Top + ((I + 1) * ADTextHeight) > DayRect.Bottom then
        begin
          DrawDotDotDot(DayRect, DotDotDotColor);

        {
          RenderCanvas.Brush.Color := DotDotDotColor;
          TPSFillRect(RenderCanvas, Angle, RenderIn,
            Rect(DayRect.Right - 20,  DayRect.Bottom - 7, DayRect.Right - 17,  DayRect.Bottom - 4));
          TPSFillRect(RenderCanvas, Angle, RenderIn,
            Rect(DayRect.Right - 13,  DayRect.Bottom - 7, DayRect.Right - 10,  DayRect.Bottom - 4));
          TPSFillRect(RenderCanvas, Angle, RenderIn,
            Rect(DayRect.Right - 6,  DayRect.Bottom - 7, DayRect.Right -  3,  DayRect.Bottom - 4));
            }
          break;
        end;

        // See if the event began before the start of the range
        if (Event.StartTime < DayOf(RenderDate)) then
          StartsBeforeRange := true;

        // Set the event's rect
        ADEvRect.Top := ADEventsRect.Top + TextMargin + I * ADTextHeight;
        ADEvRect.Bottom := ADEvRect.Top + ADTextHeight;
        ADEvRect.Left := AdEventsRect.Left + txtDist;
        ADEvRect.Right := DayRect.Right;

        // Paint the background of the event rect
        RenderCanvas.Brush.Color := ADEventBackgroundColor;
        RenderCanvas.Pen.Color := ADEventBorderColor;
        TPSRectangle(RenderCanvas, Angle, RenderIn,
          ADEvRect.Left + TextMargin,
          ADEvRect.Top + txtDist,
          ADEvRect.Right - TextMargin,
          ADEvRect.Top + ADTextHeight + txtDist
        );

        // Paint the event string
        EventStr := IfThen(StartsBeforeRange, '>> ', '') + Event.Description;
        EventStr := GetDisplayString(RenderCanvas, EventStr, 0, WidthOf(ADEvRect) - 3*TextMargin);

        TPSTextOut(RenderCanvas,Angle, RenderIn,
          ADEvRect.Left + TextMargin * 2 + txtDist,
          ADEvRect.Top + TextMargin,
          EventStr
        );

        Result := True;

        TVpWeekViewOpener(FWeekView).wvEventArray[EAIndex].Rec := Rect(
          ADEvRect.Left + TextMargin,
          ADEvRect.Top + TextMargin,
          ADEvRect.Right - TextMargin,
          ADEvRect.Bottom
        );
        TVpWeekViewOpener(FWeekView).wvEventArray[EAIndex].Event := Event;
        Inc(EAIndex);
      end; { for I := 0 to pred(ADEventsList.Count) do ... }

    end;   { if NumADEvents > 0 }

  finally
    ADEventsList.Free;
  end;
end;

procedure TVpWeekViewPainter.DrawBorders;
begin
  if FWeekView.DrawingStyle = dsFlat then begin
    {
    DrawBevelRect(RenderCanvas,
      TPSRotateRectangle(Angle, RenderIn, Rect(RealLeft, RealTop, RealRight - 1, RealBottom - 1)),
      BevelShadowColor,
      BevelShadowColor
    );
    }
    DrawBevelRect(RenderCanvas,
      TPSRotateRectangle(Angle, RenderIn, Rect(RealLeft + 1, RealTop + 1, RealRight - 2, RealBottom - 2)),
      BevelShadowColor,
      BevelShadowColor    // use the same color --> no bevel in flat mode!
    );
  end else
  if FWeekView.DrawingStyle = ds3d then begin
    { draw a 3d bevel }
    DrawBevelRect(RenderCanvas,
      TPSRotateRectangle(Angle, RenderIn, Rect(RealLeft, RealTop, RealRight - 1, RealBottom - 1)),
      BevelShadowColor,
      BevelShadowColor
    );
    DrawBevelRect(RenderCanvas,
      TPSRotateRectangle(Angle, RenderIn, Rect(RealLeft + 1, RealTop + 1, RealRight - 2, RealBottom - 2)),
      BevelDarkShadow,
      BevelButtonFace
    );
  end;
end;

procedure TVpWeekViewPainter.DrawFocusRect(ADayIndex: Integer; DayRect: TRect);
var
  tmpRect: TRect;
begin
  if (not DisplayOnly) and SameDate(StartDate + ADayIndex, FWeekView.Date) and FWeekView.Focused
  then begin
    tmpRect := DayRect;
    InflateRect(tmpRect, -2, -2);
    tmpRect.Top := tmpRect.Top + FDayHeadHeight;
    TPSDrawFocusRect(RenderCanvas, Angle, RenderIn, tmpRect);
  end;
end;

procedure TVpWeekViewPainter.DrawDay(ADayIndex: Integer; var DayRect: TRect;
  var EAIndex: Integer);
var
  TextRect: TRect;
  J: Integer;
  EventList: TList;
//  dayHeadHeight: Integer;
  rowHeight: Integer;
  headerHeight: Integer;
  tmpRect: TRect;
begin
  // Abbreviations
//  dayHeadHeight := TVpWeekviewOpener(FWeekView).wvDayHeadHeight;
  rowHeight := TVpWeekViewOpener(FWeekView).wvRowHeight;
  headerHeight := TVpWeekViewOpener(FWeekView).wvHeaderHeight;

  // Get header rectangle
  TextRect := DayRect;
  TextRect.Bottom := DayRect.Top + FDayHeadHeight;

  // Draw day header
  tmpRect := TextRect;
  inc(tmpRect.Right);
  RenderCanvas.Font.Assign(FWeekView.DayHeadAttributes.Font);
  RenderCanvas.Font.Size := ScaleY(RenderCanvas.Font.Size, DesignTimeDPI);
  RenderCanvas.Brush.Color := RealDayHeadAttrColor;
  TPSFillRect(RenderCanvas, Angle, RenderIn, tmpRect);
  if FWeekView.DayHeadAttributes.Bordered and (FWeekView.DrawingStyle <> dsNoBorder) then
    TPSRectangle(RenderCanvas, Angle, RenderIn, tmpRect);

  // Fix header string
  DrawDayHeader(ADayIndex, TextRect);

  if (FWeekView.DataStore <> nil) and (FWeekView.DataStore.Resource <> nil) and
     (FWeekView.DataStore.Resource.Schedule.EventCountByDay(StartDate + ADayIndex) > 0) and
     (HeightOf(DayRect) >= TextMargin * 2 + FDayHeadHeight)
  then begin
    // Events exist for this day
    EventList := TList.Create;
    try
      // Populate the event list with events for this day
      FWeekView.DataStore.Resource.Schedule.EventsByDate(StartDate + ADayIndex, EventList);

      { Now sort times in ascending order. This must be done because the event
        list can contain recurring events which have the wrong date part }
      EventList.Sort(CompareEventsByTimeOnly);

      // Initialize TextRect for this day
      TextRect := DayRect;
      TextRect.Top := DayRect.Top + FDayHeadHeight;
      TextRect.Bottom := TextRect.Top + rowHeight;

      // Handle all-day events
      tmpRect := TextRect;
      tmpRect.Bottom := DayRect.Bottom;
      if DrawAllDayEvents(StartDate + ADayIndex, tmpRect, EAIndex) then
      begin
        TextRect.Bottom := TextRect.Bottom + ADEventsRect.Bottom - TextRect.Top;
        TextRect.Top := ADEventsRect.Bottom;
      end;

      // Discard AllDayEvents, because they are drawn above.
      for J := pred(EventList.Count) downto 0 do
        if TVpEvent(EventList[J]).AllDayEvent then
          EventList.Delete(J);

      // Iterate the events, painting them one by one
      for J := 0 to pred(EventList.Count) do begin
        { if the TextRect extends below the available space then draw a   }
        { dot dot dot to indicate there are more events than can be drawn }
        { in the available space }
        if TextRect.Bottom - TextMargin > DayRect.Bottom then begin
          { Draw ". . ." }
          DrawDotDotDot(DayRect, DotDotDotColor);
          break;
        end;

        // Write the event text
        DrawEvent(TVpEvent(EventList.List^[J]), TextRect, ADayIndex);

        // Update the EventArray
        with TVpWeekViewOpener(FWeekView).wvEventArray[EAIndex] do begin
          Rec := TextRect;
          Event := TVpEvent(EventList[J]);
        end;
        Inc(EAIndex);

        TextRect.Top := TextRect.Bottom;
        TextRect.Bottom := TextRect.Top + rowHeight;
      end; { for loop }

    finally
      EventList.Free;
    end;
  end;

  { Draw focus rect if this is the current day }
  DrawFocusRect(ADayIndex, DayRect);

  { update WeekdayArray }
  with TVpWeekViewOpener(FWeekView).wvWeekdayArray[ADayIndex] do begin
    Rec := DayRect;
    Day := StartDate + ADayIndex;
  end;

  { adjust the DayRect for the next day }
  if (ADayIndex = 2) then begin
    { move the dayrect to the top of the next column }
    DayRect := Rect(
      RealLeft + (RealRight - RealLeft) div 2,
      RealTop + headerHeight + 2,
      RealRight - 2,
      RealTop + headerHeight + DayRectHeight
    );
    if FWeekView.DrawingStyle = ds3D then begin
      inc(DayRect.Top);
      dec(DayRect.Right);
    end;
  end
  else
  if (ADayIndex = 4 {Friday}) then begin
    { shrink DayRect for weekend days }
    DayRectHeight := DayRectHeight div 2;
    DayRect.Top := DayRect.Bottom;
    DayRect.Bottom := DayRect.Top + DayRectHeight;
  end
  else begin
    DayRect.Top := DayRect.Bottom;
    DayRect.Bottom := DayRect.Top + DayRectHeight;
  end;
end;

procedure TVpWeekViewPainter.DrawDayHeader(ADayIndex: Integer; var TextRect: TRect);
var
  dayStr: String;
  strWid: Integer;
  strH: Integer;
begin
  dayStr := FormatDateTime(FWeekView.DayHeadAttributes.DateFormat, StartDate + ADayIndex);
  {$IFDEF LCL}
  {$IF FPC_FULLVERSION < 30000}DayStr := SysToUTF8(DayStr); {$ENDIF}
  {$ENDIF}

  strWid := RenderCanvas.TextWidth(dayStr);
  if strWid > WidthOf(TextRect) then
    dayStr := GetDisplayString(RenderCanvas, dayStr, 0, WidthOf(TextRect) - TextMargin);
  strWid := RenderCanvas.TextWidth(dayStr);
  strH := RenderCanvas.TextHeight(dayStr);

  TextRect.Left := TextRect.Right - strWid - TextMargin;
  TPSTextOut(
    RenderCanvas,
    Angle,
    RenderIn,
    TextRect.Left,
    (TextRect.Top + TextRect.Bottom - strH) div 2,
    dayStr
  );
end;

procedure TVpWeekViewPainter.DrawDays;
var
  DayRect: TRect;
  EAIndex: Integer;   // Index of last-used item in wvEventArray
  I: Integer;
  headerHeight: Integer;
  realCenter: Integer;
begin
  with TVpWeekViewOpener(FWeekView) do begin

    { Initialize weekday array }
    for I := 0 to pred(Length(wvWeekdayArray)) do begin
      wvWeekdayArray[I].Rec.TopLeft := Point(-1, -1);
      wvWeekdayArray[I].Rec.BottomRight := Point(-1, -1);
      wvWeekdayArray[I].Day := 0;
    end;

    { initialize event array }
    EAIndex := 0;
    for I := 0 to pred(Length(wvEventArray)) do begin
      wvEventArray[I].Rec.TopLeft := Point(-1, -1);
      wvEventArray[I].Rec.BottomRight := Point(-1, -1);
      wvEventArray[I].Event := nil;
    end;
  end;

  RenderCanvas.Pen.Color := RealLineColor;
  RenderCanvas.Pen.Style := psSolid;

  { build the first day rect }
  headerHeight := TVpWeekViewOpener(FWeekView).wvHeaderHeight;
  DayRectHeight := (RealBottom - RealTop - headerHeight) div 3;
  DayRect := Rect(
    RealLeft + 1,
    RealTop + headerHeight + 2,
    RealLeft + (RealRight - RealLeft) div 2 + 1,
    Realtop + headerHeight + DayRectHeight
  );
  if FWeekView.DrawingStyle = ds3D then
    inc(DayRect.Top, 1);

  { Draw the day frames and texts }
  for I := 0 to 6 do
    DrawDay(I, DayRect, EAIndex);

  { Draw the center vertical line }
  RenderCanvas.Pen.Color := RealLineColor;
  realCenter := RealLeft + (RealRight - RealLeft) div 2;
  TPSMoveTo(RenderCanvas, Angle, RenderIn, realCenter, RealTop + headerHeight + 2);
  TPSLineTo(RenderCanvas, Angle, RenderIn, realCenter, RealBottom - 1);
end;

procedure TVpWeekViewPainter.DrawEvent(AEvent: TVpEvent; TextRect: TRect;
  ADayIndex: Integer);
var
  dayStr: String;
  todayStartTime: TDateTime;
  todayEndTime: TDateTime;
  strLen: Integer;
  timefmt: String;
begin
  { format the display text }
  todayStartTime := AEvent.StartTime;
  todayEndTime := AEvent.EndTime;

  // Event reaches into the next day(s)
  if trunc(todayEndTime) > trunc(todayStartTime) then begin
    if trunc(todayStartTime) < trunc(StartDate + ADayIndex) then  // first event
      todayStartTime := 0;
    if trunc(TodayEndTime) > trunc(StartDate + ADayIndex) then    // last event
      todayEndTime := 0.9999;
  end;

  { set the event font }
  RenderCanvas.Font.Assign(FWeekView.EventFont);
  RenderCanvas.Font.Size := ScaleY(RenderCanvas.Font.Size, DesignTimeDPI);
  RenderCanvas.Brush.Color := RealColor;

  { Build the event text }
  if FWeekView.ShowEventTime then
  begin
    timefmt := IfThen(FWeekView.TimeFormat = tf24Hour, 'hh:nn', 'hh:nn AM/PM');
    dayStr := Format('%s - %s: ', [
      FormatDateTime(timeFmt, todayStartTime),
      FormatDateTime(timeFmt, todayEndTime)
    ]);
  end else
    dayStr := '';
  dayStr := IfThen(dayStr = '', AEvent.Description, dayStr + ' ' + AEvent.Description);

  strLen := RenderCanvas.TextWidth(dayStr);
  if (strLen > WidthOf(TextRect) - TextMargin) then                                           // wp: shouldn't this be 2*TextMargin ?
    dayStr := GetDisplayString(RenderCanvas, dayStr, 0, WidthOf(TextRect) - TextMargin * 2);

  { Write the event text }
  TPSTextOut(RenderCanvas, Angle, RenderIn,
    TextRect.Left + TextMargin, TextRect.Top + TextMargin div 2,
    dayStr
  );
end;

procedure TVpWeekViewPainter.DrawHeader;
var
  HeadRect: TRect;
  HeadTextRect: TRect;
  HeadStr: string;
  HeadStrLen: Integer;
  weekNo: Integer;
begin
  RenderCanvas.Brush.Color := RealHeadAttrColor;
  RenderCanvas.Font.Assign(TFont(FWeekView.HeadAttributes.Font));
  RenderCanvas.Font.Size := ScaleY(RenderCanvas.Font.Size, DesignTimeDPI);

  { draw the header cell and borders }
  if FWeekView.DrawingStyle = ds3d then begin
    { draw a 3d bevel }
    HeadRect.Left := RealLeft + 2;
    HeadRect.Top := RealTop + 2;
    HeadRect.Right := RealRight - 3;
    HeadRect.Bottom := RealTop + TVpWeekViewOpener(FWeekView).wvHeaderHeight + 2;
    TPSFillRect(RenderCanvas, Angle, RenderIn, HeadRect);
    DrawBevelRect(
      RenderCanvas,
      TPSRotateRectangle(Angle, RenderIn, HeadRect),
      BevelHighlightColor,
      BevelDarkShadow
    );
  end else begin
//  if FWeekView.DrawingStyle = dsFlat then begin
//    { draw simple border rectangle }
    HeadRect := Rect(RealLeft, RealTop, RealRight, RealTop + TVpWeekViewOpener(FWeekView).wvHeaderHeight + 2);
    TPSFillRect(RenderCanvas, Angle, RenderIn, HeadRect);
  end;

  { build header caption }
  weekNo := GetWeekOfYear(StartDate);
  HeadStr := HeadStr + Format('%s %s (%s %d)', [
    RSWeekOf, FormatDateTime(FWeekView.DateLabelFormat, StartDate), RSCalendarWeekAbbr, weekNo
  ]);

  { draw the text }
  if DisplayOnly and (RenderCanvas.TextWidth(HeadStr) >= WidthOf(RenderIn)) then
    HeadTextRect.TopLeft:= Point(RealLeft + TextMargin * 2, HeadRect.Top)
  else
  if DisplayOnly then
    HeadTextRect.TopLeft := Point(
      RealLeft + (RealRight - RealLeft - RenderCanvas.TextWidth(HeadStr)) div 2,
      HeadRect.Top
    )
  else
    HeadTextRect.TopLeft := Point(
      RealLeft + Trunc(TVpWeekViewOpener(FWeekView).wvHeaderHeight * 0.8) * 2 + TextMargin * 2,
      HeadRect.Top
    );
  HeadTextRect.BottomRight := HeadRect.BottomRight;

  { Fix Header String }
  HeadStrLen := RenderCanvas.TextWidth(HeadStr);
  if HeadStrLen > HeadTextRect.Right - HeadTextRect.Left - TextMargin then
  begin
    HeadStr := GetDisplayString(RenderCanvas, HeadStr, 0,
      HeadTextRect.Right - HeadTextRect.Left - TextMargin );
  end;

  { position the spinner }
  with TVpWeekViewOpener(FWeekView) do begin
    wvSpinButtons.Height := Trunc(wvHeaderHeight * 0.8);
    wvSpinButtons.Width := wvSpinButtons.Height * 2;
    wvSpinButtons.Left := TextMargin;
    wvSpinButtons.Top := (wvHeaderHeight - wvSpinButtons.Height) div 2 + 2;
  end;
  TPSTextOut(RenderCanvas, Angle, RenderIn,
    HeadTextRect.Left + TextMargin,
    (HeadTextRect.Top + HeadTextRect.Bottom - RenderCanvas.TextHeight('Tg')) div 2, //HeadTextRect.Top + TextMargin,
    HeadStr
  );
end;

procedure TVpWeekViewPainter.FixFontHeights;
begin
  with FWeekView do begin
    AllDayEventAttributes.Font.Height := GetRealFontHeight(AllDayEventAttributes.Font);
    DayHeadAttributes.Font.Height := GetRealFontHeight(DayHeadAttributes.Font);
    EventFont.Height := GetRealFontHeight(EventFont);
    Font.Height := GetRealFontHeight(Font);
    HeadAttributes.Font.Height := GetRealFontHeight(HeadAttributes.Font);
  end;
end;

procedure TVpWeekViewPainter.InitColors;
begin
  if DisplayOnly then begin
    BevelHighlightColor := clBlack;
    BevelShadowColor := clBlack;
    BevelDarkShadow := clBlack;
    BevelButtonFace := clBlack;
    RealLineColor := clBlack;
    RealColor := clWhite;
    RealDayHeadAttrColor := clSilver;
    RealHeadAttrColor := clSilver;
    ADBackgroundColor := clWhite;
    ADEventBackgroundColor := clWhite;
    ADEventBorderColor := clSilver;
  end else begin
    BevelHighlightColor := clBtnHighlight;
    BevelShadowColor := clBtnShadow;
    BevelDarkShadow := cl3DDkShadow;
    BevelButtonFace := clBtnFace;
    RealLineColor := FWeekView.LineColor;
    RealColor := FWeekView.Color;
    RealDayHeadAttrColor := FWeekView.DayHeadAttributes.Color;
    RealHeadAttrColor := FWeekView.HeadAttributes.Color;
    ADBackgroundColor := FWeekView.AllDayEventAttributes.BackgroundColor;
    ADEventBackgroundColor := FWeekView.AllDayEventAttributes.EventBackgroundColor;
    ADEventBorderColor := FWeekView.AllDayEventAttributes.EventBorderColor;
  end;
  DotDotDotColor := clBlack;
end;

procedure TVpWeekViewPainter.RenderToCanvas(ARenderIn: TRect;
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

    { clear client area }
    Clear;

    { measure the row heights }
    SetMeasurements;

    { draw header }
    DrawHeader;

    { draw days }
    DrawDays;

    { draw the borders }
    DrawBorders;

  finally
    { reinstate canvas settings}
    SelectClipRgn(RenderCanvas.Handle, 0);
    DeleteObject(Rgn);
  end;

  RestorePenBrush;
end;

procedure TVpWeekViewPainter.SetMeasurements;
begin
  inherited;

  with TVpWeekViewOpener(FWeekView) do
    if RenderDate = 0 then
      StartDate := GetStartOfWeek(wvStartDate, WeekStartsOn)
    else
      StartDate := GetStartOfWeek(RenderDate, WeekStartsOn);

  RenderCanvas.Font.Assign(FWeekView.DayHeadAttributes.Font);
  RenderCanvas.Font.Size := ScaleY(RenderCanvas.Font.Size, DesignTimeDPI);
  FDayHeadHeight := RenderCanvas.TextHeight(VpProductName) + TextMargin + 2 ;

  RenderCanvas.Font.Assign(FWeekView.EventFont);
  RenderCanvas.Font.Size := ScaleY(RenderCanvas.Font.Size, DesignTimeDPI);
  with TVpWeekViewOpener(FWeekView) do
    wvRowHeight := RenderCanvas.TextHeight(VpProductName) + TextMargin div 2;

  RenderCanvas.Font.Assign(TFont(FWeekView.HeadAttributes.Font));
  RenderCanvas.Font.Size := ScaleY(RenderCanvas.Font.Size, DesignTimeDPI);
  with TVpWeekViewOpener(FWeekView) do
    wvHeaderHeight := RenderCanvas.TextHeight(VpProductName) + TextMargin * 2;
end;

end.
