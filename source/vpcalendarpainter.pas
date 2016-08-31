unit VpCalendarPainter;

{$I vp.inc}

interface

uses
  SysUtils, Classes, Graphics,
  VpBase, VpMisc, VpBasePainter, VpCalendar;

type
  TVpCalendarPainter = class(TVpBasePainter)
  private
    FCalendar: TVpCustomCalendar;
    // local variables of the old RenderToCanvas method of TVpCalendar
//    R, C: Integer;
//    I: Integer;
    SatCol: Integer;
    SunCol: Integer;
    DOW: TVpDayType;
    Y, M, D: Word;
    lBadDate: Boolean;
    lDate: TDateTime;
    BevelHighlight: TColor;
    BevelShadow: TColor;
    InactiveDayColor: TColor;
    MonthYearColor: TColor;
    DayNameColor: TColor;
    LineColor: TColor;
    EventDayColor: TColor;
    DayColor: TColor;
    RealColor: TColor;
    WeekendColor: TColor;

  protected
    procedure DrawAllDays;
    procedure DrawDate;
    procedure DrawDay(R, C, I: Integer; Grayed: Boolean);
    procedure DrawDayNames;
    procedure DrawFocusBox;
    procedure DrawLine;
    procedure FixFontHeights;
    procedure InitColors;
    procedure SetMeasurements; override;

  public
    constructor Create(ACalendar: TVpCustomCalendar; ARenderCanvas: TCanvas);
    procedure RenderToCanvas(ARenderIn: TRect; AAngle: TVpRotationAngle;
      AScale: Extended; ARenderDate: TDateTime; AStartLine, AStopLine: Integer;
      AUseGran: TVpGranularity; ADisplayOnly: Boolean); override;
  end;

implementation

uses
  LCLProc, LazUtf8,
  VpCanvasUtils;

type
  TVpCalendarOpener = class(TVpCustomCalendar);

constructor TVpCalendarPainter.Create(ACalendar: TVpCustomCalendar;
  ARenderCanvas: TCanvas);
begin
  inherited Create(ARenderCanvas);
  FCalendar := ACalendar;
end;

procedure TVpCalendarPainter.DrawAllDays;
var
  I, R, C: Integer;
begin
  I := 1;
  for R := 2 to 8 do
    for C := 0 to 6 do begin
      if ((C = SatCol) and (cdoHighlightSat in FCalendar.Options)) or
         ((C = SunCol) and (cdoHighlightSun in Fcalendar.Options))
      then
        RenderCanvas.Font.Color := WeekendColor
      else
        RenderCanvas.Font.Color := DayColor;

      { set highlight color and font style for days with events }
      RenderCanvas.Font.Style := RenderCanvas.Font.Style - [fsBold];
      lBadDate := false;
      if (FCalendar.DataStore <> nil) and (FCalendar.DataStore.Resource <> nil) then
      begin
        DecodeDate(RenderDate, Y, M, D);
        try begin
          {$IFDEF VERSION6}
          if not TryEncodeDate(Y, M, TVpCalendarOpener(FCalendar).clCalendar[I], lDate) then
            lBadDate := True;
          {$ELSE}
          if TVpCalendarOpener(FCalendar).clCalendar[I] > DaysInMonth(Y, M) then
            lDate := EncodeDate(Y, M, DaysInMonth(Y, M))
          else
            lDate := EncodeDate(Y, M, TVpCalendarOpener(FCalendar).clCalendar[I]);
          {$ENDIF}
        end;
        except
          lBadDate := true;
        end;

        if (not lBadDate) and (FCalendar.DataStore.Resource.Schedule.EventCountByDay(lDate) > 0)
        then begin
          RenderCanvas.Font.Style := RenderCanvas.Font.Style + [fsBold, fsUnderline];
          RenderCanvas.Font.Color := EventDayColor;
        end else
          RenderCanvas.Font.Style := RenderCanvas.Font.Style - [fsBold, fsUnderline];
      end;
      with TVpCalendarOpener(FCalendar) do
        DrawDay(R, C, I, (I < clFirst) or (I > clLast));
      Inc(I);
    end;
end;

procedure TVpCalendarPainter.DrawDate;
var
  R: TRect;
  S: string;
begin
  // Calculate size of available rectangle
  with TVpCalendarOpener(FCalendar) do begin
    R := Rect(clRowCol[0, 1].Left + RealLeft,
              clRowCol[0, 1].Top + RealTop,
              clRowCol[0, 1].Right + RealLeft,
              clRowCol[0, 1].Bottom + RealTop
    );
    R.Right := clRowCol[0, 6].Left + RealLeft;
  end;

  // Calculate string to be displayed
  if FCalendar.DateFormat = dfLong then
    if cdoShowYear in FCalendar.Options then
      S := FormatDateTime('mmmm yyyy', RenderDate)
    else
      S := FormatDateTime('mmmm', RenderDate)
  else
    if cdoShowYear in FCalendar.Options then
      S := FormatDateTime('mmm yyyy', RenderDate)
    else
      S := FormatDateTime('mmm', RenderDate);

  // switch to short date format if string won't fit
  if FCalendar.DateFormat = dfLong then
    if RenderCanvas.TextWidth(S) > R.Right - R.Left then
      S := FormatDateTime('mmm yyyy', RenderDate);

  {$IF FPC_FULLVERSION < 30000}
  S := SysToUTF8(S);
  {$ENDIF}

  RenderCanvas.Font.Color := MonthYearColor;
  if Assigned(FCalendar.OnDrawDate) then
    FCalendar.OnDrawDate(Self, RenderDate, R)
  else
    TPSCenteredTextOut(RenderCanvas, Angle, RenderIn, R, S);
end;

procedure TVpCalendarPainter.DrawDay(R, C, I: Integer; Grayed: Boolean);
var
  Cl: TColor;
  OldIdx: Integer;
  NewIdx: Integer;
  S: string[10];
  DrawRect: TRect;
  TH: Integer;
begin
  {avoid painting day number under buttons}
  if cdoShowRevert in FCalendar.Options then
    if (R = 8) and (C >= 3) then
      Exit;
  if cdoShowToday in FCalendar.Options then
    if (R = 8) and (C >= 5) then
      Exit;

  {convert to a string and draw it centered in its rectangle}
  S := IntToStr(TVpCalendarOpener(FCalendar).clCalendar[I]);

  if Grayed then
    RenderCanvas.Font.Color := InactiveDayColor;

  if not Grayed or (cdoShowInactive in FCalendar.Options) then begin
    NewIdx := ((R-2) * 7) + Succ(C);
    with TVpCalendarOpener(FCalendar) do
      OldIdx := clFirst + Pred(clDay);
    if Assigned(FCalendar.OnGetHighlight) then begin
      Cl := RenderCanvas.Font.Color;
      FCalendar.OnGetHighlight(Self, RenderDate + NewIdx - OldIdx , Cl);
      RenderCanvas.Font.Color := Cl;
    end;
    with TVpCalendarOpener(FCalendar) do
      if Assigned(OnDrawItem) then
        OnDrawItem(Self, RenderDate + NewIdx - OldIdx, clRowCol[R,C])
      else
      if clRowCol[R, C].Top <> 0 then begin
        DrawRect := clRowCol[R, C];
        OffsetRect(DrawRect, RealLeft, RealTop);
        TH := RenderCanvas.TextHeight(S);
        if TH < DrawRect.Bottom - DrawRect.Top then
          DrawRect.Top := DrawRect.Top + ((DrawRect.Bottom - DrawRect.Top) - TH) div 2;
        TPSCenteredTextOut(RenderCanvas, Angle, RenderIn, DrawRect, S);
      end;
  end;
end;

procedure TVpCalendarPainter.DrawDayNames;
var
  I: Integer;
  S: string;
  DrawRect: TRect;
begin
  {draw the day name column labels}
  RenderCanvas.Font.Color := DayNameColor;
  I := 0;
  DOW := FCalendar.WeekStarts;
  repeat
    {record columns for weekends}
    if DOW = dtSaturday then
      SatCol := I;
    if DOW = dtSunday then
      SunCol := I;

    {get the day name}
    if cdoShortNames in FCalendar.Options then begin
      if FCalendar.DayNameWidth < 1 then
        S := DefaultFormatSettings.ShortDayNames[Ord(DOW)+1]
      else
        S := Copy(DefaultFormatSettings.ShortDayNames[Ord(DOW)+1], 1, FCalendar.DayNameWidth)
    end else begin
      if FCalendar.DayNameWidth < 1 then
        S := DefaultFormatSettings.LongDayNames[Ord(DOW)+1]
      else
        S := Copy(DefaultFormatSettings.LongDayNames[Ord(DOW)+1], 1, FCalendar.DayNameWidth)
    end;
   {$IF FPC_FULLVERSION < 30000}
    S := SysToUTF8(S);
   {$ENDIF}

    {draw the day name above each column}
    DrawRect := TVpCalendarOpener(FCalendar).clRowCol[1, I];
    OffsetRect(DrawRect, RealLeft, Realtop);
    TPSCenteredTextOut(RenderCanvas, Angle, RenderIn, DrawRect, S);
    Inc(I);
    if DOW < High(DOW) then
      Inc(DOW)
    else
      DOW := Low(DOW);
  until DOW = FCalendar.WeekStarts;
end;

procedure TVpCalendarPainter.FixFontHeights;
begin
  with TVpCalendarOpener(FCalendar) do begin
    Font.Height := GetRealFontHeight(Font);
    calRebuildCalArray(RenderDate);
  end;
end;

procedure TVpCalendarPainter.InitColors;
begin
  if DisplayOnly then begin
    BevelHighlight := clBlack;
    BevelShadow := clBlack;
    InactiveDayColor := clSilver;
    MonthYearColor := clBlack;
    DayNameColor := clBlack;
    LineColor := clBlack;
    EventDayColor := clBlack;
    DayColor := clBlack;
    RealColor := clWhite;
    WeekendColor := $5f5f5f;
  end else begin
    BevelHighlight := clBtnHighlight;
    BevelShadow := clBtnShadow;
    InactiveDayColor := FCalendar.Colors.InactiveDays;
    MonthYearColor := FCalendar.Colors.MonthAndYear;
    DayNameColor := FCalendar.Colors.DayNames;
    LineColor := FCalendar.Font.Color;
    EventDayColor := FCalendar.Colors.EventDays;
    DayColor := FCalendar.Colors.Days;
    RealColor := FCalendar.Color;
    WeekendColor := FCalendar.Colors.WeekEnd;
  end;
end;

procedure TVpCalendarPainter.DrawFocusBox;
var
  R: TRect;
  S: string[10];
begin
  S := IntToStr(TVpCalendarOpener(FCalendar).clDay);

  { set highlight color and font style for days with events }
  RenderCanvas.Font.Style := RenderCanvas.Font.Style - [fsBold];
  lBadDate := false;

  if (FCalendar.DataStore <> nil) and (FCalendar.DataStore.Resource <> nil) then begin
    DecodeDate(RenderDate, Y, M, D);
    try
      {$IFDEF VERSION6}
      if not TryEncodeDate (Y, M, TVpCalendarOpener(FCalendar).clDay, lDate) then
        lBadDate := true;
      {$ELSE}
      lDate := EncodeDate(Y, M, TVpCalendarOpener(FCalendar).clDay);
      {$ENDIF}
    except
      lBadDate := true;
    end;

    if (not lBadDate) and (FCalendar.DataStore.Resource.Schedule.EventCountByDay(lDate) > 0)
    then begin
      RenderCanvas.Font.Style := RenderCanvas.Font.Style + [fsBold, fsUnderline];
      RenderCanvas.Font.Color := EventDayColor;
    end else
      RenderCanvas.Font.Style := RenderCanvas.Font.Style - [fsBold, fsUnderline];
  end;

  R := TVpCalendarOpener(FCalendar).calGetCurrentRectangle;
  R.Left := R.Left + RealLeft;
  R.Top := R.Top + RealTop;
  R.Right := R.Right + RealLeft;
  R.Bottom := R.Bottom + RealTop;

  R := TPSRotateRectangle (Angle, RenderIn, R);
  if not DisplayOnly then begin
  {$IFNDEF LCL}
    if Focused then
      DrawButtonFace (RenderCanvas, R, 1, bsNew, True, True, False)
    else
      DrawButtonFace (RenderCanvas, R, 1, bsNew, True, False, False);
  {$ENDIF}
    R := TVpCalendarOpener(FCalendar).calGetCurrentRectangle;
    R.Left := R.Left + RealLeft;
    R.Top := R.Top + RealTop;
    R.Right := R.Right + RealLeft;
    R.Bottom := R.Bottom + RealTop;
    TPSCenteredTextOut (RenderCanvas, Angle, RenderIn, R, S);
  end;
end;

procedure TVpCalendarPainter.DrawLine;
begin
//    if (not Ctl3D) then begin
    with TVpCalendarOpener(FCalendar) do begin
      RenderCanvas.Pen.Color := LineColor;
      TPSMoveTo(RenderCanvas, Angle, RenderIn, RealLeft, clRowCol[1,0].Bottom-3 + RealTop);
      TPSLineTo(RenderCanvas, Angle, RenderIn, RealRight, clRowCol[1,0].Bottom-3 + RealTop);
    end;
{    end else if Ctl3D then begin
    RenderCanvas.Pen.Color := BevelHighlight;
    TPSMoveTo (RenderCanvas, Angle, RenderIn,
               RealLeft, clRowCol[1,0].Bottom-3 + RealTop);
    TPSLineTo (RenderCanvas, Angle, RenderIn,
               RealRight, clRowCol[1,0].Bottom-3 + RealTop);
    RenderCanvas.Pen.Color := BevelShadow;
    TPSMoveTo (RenderCanvas, Angle, RenderIn,
               RealLeft,  clRowCol[1,0].Bottom-2 + RealTop);
    TPSLineTo (RenderCanvas, Angle, RenderIn,
               RealRight, clRowCol[1,0].Bottom-2 + RealTop);
  end;  }
end;

procedure TVpCalendarPainter.RenderToCanvas(ARenderIn: TRect;
  AAngle: TVpRotationAngle; AScale: Extended; ARenderDate: TDateTime;
  AStartLine, AStopLine: Integer; AUseGran: TVpGranularity; ADisplayOnly: Boolean);
var
  Row: TRowArray;
  Col: TColArray;
begin
  inherited;

  InitColors;
  SavePenBrush;
  InitPenBrush;
  if ADisplayOnly then FixFontHeights;

  RenderCanvas.Lock;
  try
    SetMeasurements;

    RenderCanvas.Font.Assign(FCalendar.Font);
    RenderCanvas.Font.Size := ScaleY(RenderCanvas.Font.Size, DesignTimeDPI);

    with TVpCalendarOpener(FCalendar) do
      if (RealRight - RealLeft <> FLastRenderX) or
         (RealBottom - RealTop <> FLastRenderY)
      then begin
        FLastRenderX := RealRight - RealLeft;
        FLastRenderY := RealBottom - RealTop;
        CalculateSizes(RenderCanvas, Angle, RenderIn, Row, Col, DisplayOnly);
      end;
    RenderCanvas.Brush.Color := RealColor;
    RenderCanvas.FillRect(RenderIn);

    {draw the month and year at the top of the calendar}
    DrawDate;

    {draw the days of the week}
    DrawDayNames;

    {draw line under day names}
    DrawLine;

    {draw each day}
    DrawAllDays;

    RenderCanvas.Font.Color := DayColor;
    if not Assigned(FCalendar.OnDrawItem) then
      if not (cdoHideActive in FCalendar.Options) then
        DrawFocusBox;

  finally
    RenderCanvas.Unlock;
  end;
end;

procedure TVpCalendarPainter.SetMeasurements;
begin
  inherited;
  if RenderDate = 0 then
    RenderDate := FCalendar.Date;
end;

end.
