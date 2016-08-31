{*********************************************************}
{*                VPMONTHVIEW.PAS 1.03                   *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{$I vp.inc}

unit VpMonthView;

interface

uses
  {$IFDEF LCL}
  LMessages, LCLProc, LCLType, LCLIntf, FileUtil,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  Classes, Graphics, Controls, ComCtrls, ExtCtrls,
  VpBase, VpBaseDS, VpMisc, VpData, VpSR, VpConst, VpCanvasUtils, Menus;

type
  TVpMonthdayRec = packed record
    Rec     : TRect;
    Date    : TDateTime;
    OffDay  : Boolean;
  end;

type
  TVpMonthdayArray = array of TVpMonthdayRec;

  { Forward Declarations }
  TVpMonthView = class;

  TVpMVDayNameStyle = (dsLong, dsShort, dsLetter);

  TVpOnEventClick = procedure(Sender: TObject; Event: TVpEvent) of object;

  TVpMvHeadAttr = class(TPersistent)
  protected{ private }
    FOwner: TVpMonthView;
    FColor: TColor;
    FFont: TVpFont;
    procedure SetColor(const Value: TColor);
    procedure SetFont(Value: TVpFont);
  public
    constructor Create(AOwner: TVpMonthView);
    destructor Destroy; override;
    property Owner: TVpMonthView read FOwner;
  published
    property Font: TVpFont read FFont write SetFont;
    property Color: TColor read FColor write SetColor;
  end;

  TVpDayHeadAttr = class(TPersistent)
  protected{private}
    FMonthView: TVpMonthView;
    FFont: TVpFont;
    FColor: TColor;
    procedure SetColor(Value: TColor);
    procedure SetFont(Value: TVpFont);
  public
    constructor Create(AOwner: TVpMonthView);
    destructor Destroy; override;
    property MonthView: TVpMonthView read FMonthView;
  published
    property Color: TColor read FColor write SetColor;
    property Font: TVpFont read FFont write SetFont;
  end;

  TVpMvTodayAttr = class(TPersistent)
  protected
    FMonthView: TVpMonthView;
    FFont: TVpFont;
    FColor: TColor;
    FBorderPen: TPen;
    procedure SetColor(Value: TColor);
    procedure SetFont(Value: TVpFont);
    procedure SetBorderPen(Value: TPen);
  public
    constructor Create(AOwner: TVpMonthView);
    destructor Destroy; override;
    property MonthView: TVpMonthView read FMonthView;
  published
    property Color: TColor read FColor write SetColor;
    property Font: TVpFont read FFont write FFont;
    property BorderPen: TPen read FBorderPen write SetBorderPen;
  end;

  { TVpMonthView }

  TVpMonthView = class(TVpLinkableControl)
  protected{ private }
    FKBNavigate: Boolean;
    FColumnWidth: Integer;
    FColor: TColor;
    FLineColor: TColor;
    FLineCount: Integer;
    FVisibleLines: Integer;
    FDayNameStyle: TVpMVDayNameStyle;
    FOffDayColor: TColor;
    FOffDayFontColor: TColor;
    FSelectedDayColor: TColor;
    FWeekStartsOn: TVpDayType;
    FShowEvents: Boolean;
    FEventDayStyle: TFontStyles;
    FDateLabelFormat: string;
    FShowEventTime: Boolean;
    FTopLine: Integer;
    FDayHeadAttr: TVpDayHeadAttr;
    FHeadAttr: TVpMvHeadAttr;
    FTodayAttr: TVpMvTodayAttr;
    FDayNumberFont: TVpFont;
    FEventFont: TVpFont;
    FTimeFormat: TVpTimeFormat;
    FDrawingStyle: TVpDrawingStyle;
    FDate: TDateTime;
    FDefaultPopup: TPopupMenu;
    FRightClickChangeDate: Boolean;
    { event variables }
    FOwnerDrawCells: TVpOwnerDrawDayEvent;
    FOnEventClick: TVpOnEventClick;
    FOnEventDblClick: TVpOnEventClick;
    { internal variables }
//    mvDayNumberHeight  : Integer;
//    mvEventTextHeight  : Integer;
    mvLoaded: Boolean;
//    mvInLinkHandler    : Boolean;
//    mvRowHeight        : Integer;
//    mvLineHeight       : Integer;
//    mvColWidth         : Integer;
    mvDayHeadHeight: Integer;
    mvSpinButtons: TUpDown;
    mvEventArray: TVpEventArray;
    mvMonthDayArray: TVpMonthdayArray;
    mvActiveEvent: TVpEvent;
    mvActiveEventRec: TRect;
//    mvEventList        : TList;
//    mvCreatingEditor   : Boolean;
//    mvPainting         : Boolean;
//    mvVScrollDelta     : Integer;
//    mvHotPoint         : TPoint;
//    mvVisibleEvents    : Integer;                                        

    { property methods }
    procedure SetDrawingStyle(Value: TVpDrawingStyle);
    procedure SetColor(Value: TColor);
    procedure SetLineColor(Value: TColor);
    procedure SetOffDayColor(Value: TColor);
    procedure SetOffDayFontColor(Value: TColor);
    procedure SetDateLabelFormat(Value: string);
    procedure SetShowEvents(Value: Boolean);
    procedure SetEventDayStyle(Value: TFontStyles);
    procedure SetDayNameStyle(Value: TVpMVDayNameStyle);
    procedure SetDayNumberFont(Value: TVpFont);
    procedure SetEventFont(Value: TVpFont);
    procedure SetSelectedDayColor(Value: TColor);
    procedure SetShowEventTime(Value: Boolean);
    procedure SetTimeFormat(Value: TVpTimeFormat);
    procedure SetDate(Value: TDateTime);
    procedure SetRightClickChangeDate(const v: Boolean);
    procedure SetWeekStartsOn(Value: TVpDayType);
    { internal methods }
    procedure mvHookUp;
    procedure mvPenChanged(Sender: TObject);
//    procedure mvFontChanged(Sender: TObject);

    procedure Paint; override;
    procedure Loaded; override;
    procedure InitializeDefaultPopup;
    procedure mvPopulate;
    procedure mvSpinButtonClick(Sender: TObject; Button: TUDBtnType);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    {$IFNDEF LCL}
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClick(var Msg: TWMLButtonDblClk);message WM_LBUTTONDBLCLK;
    {$ELSE}
    procedure WMLButtonDown(var Msg: TLMLButtonDown); message LM_LBUTTONDOWN;
    procedure WMLButtonDblClick(var Msg: TLMLButtonDblClk); message LM_LBUTTONDBLCLK;
    {$ENDIF}
    { - renamed from EditEventAtCoord and re-written}
    function  SelectEventAtCoord(Point: TPoint): Boolean;
    procedure mvSetDateByCoord(Point: TPoint);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    { message handlers }
    {$IFNDEF LCL}
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMRButtonDown(var Msg: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey);
      message CM_WANTSPECIALKEY;
    {$ELSE}
    procedure WMSize(var Msg: TLMSize); message LM_SIZE;
    procedure WMSetFocus(var Msg: TLMSetFocus); message LM_SETFOCUS;
    procedure WMRButtonDown(var Msg: TLMRButtonDown); message LM_RBUTTONDOWN;
    {$ENDIF}
    procedure PopupToday(Sender: TObject);
    procedure PopupNextMonth(Sender: TObject);
    procedure PopupPrevMonth(Sender: TObject);
    procedure PopupNextYear(Sender: TObject);
    procedure PopupPrevYear(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadLanguage;
    procedure Invalidate; override;
    procedure LinkHandler(Sender: TComponent;
      NotificationType: TVpNotificationType; const Value: Variant); override;
    function GetControlType: TVpItemType; override;
    procedure PaintToCanvas(ACanvas: TCanvas; ARect: TRect; Angle: TVpRotationAngle;
      ADate: TDateTime);
    procedure RenderToCanvas(RenderCanvas: TCanvas; RenderIn: TRect;
      Angle: TVpRotationAngle; Scale: Extended; RenderDate: TDateTime;
      StartLine, StopLine: Integer; UseGran: TVpGranularity;
      DisplayOnly: Boolean); override;

    property Date: TDateTime read FDate write SetDate;

  published
    { inherited properties }
    property Align;
    property Anchors;
    property TabStop;
    property TabOrder;
    property KBNavigation: Boolean read FKBNavigate write FKBNavigate;
    property Color: TColor read FColor write SetColor;
    property DateLabelFormat: string read FDateLabelFormat write SetDateLabelFormat;
    property DayHeadAttributes: TVpDayHeadAttr read FDayHeadAttr write FDayHeadAttr;
    property DayNameStyle: TVpMVDayNameStyle read FDayNameStyle write SetDayNameStyle;
    property DayNumberFont: TVpFont read FDayNumberFont write SetDayNumberFont;
    property DrawingStyle: TVpDrawingStyle read FDrawingStyle write SetDrawingStyle stored True;
    property EventDayStyle: TFontStyles read FEventDayStyle write SetEventDayStyle;
    property EventFont: TVpFont read FEventFont write SetEventFont;
    property HeadAttributes: TVpMvHeadAttr read FHeadAttr write FHeadAttr;
    property LineColor: TColor read FLineColor write SetLineColor;
    property TimeFormat: TVpTimeFormat read FTimeFormat write SetTimeFormat;
    property TodayAttributes: TVpMvTodayAttr read FTodayAttr write FTodayAttr;
    property OffDayColor: TColor read FOffDayColor write SetOffDayColor;
    property OffDayFontColor: TColor read FOffDayFontColor write SetOffDayFontColor default clGray;
    property OwnerDrawCells: TVpOwnerDrawDayEvent read FOwnerDrawCells write FOwnerDrawCells;
    property RightClickChangeDate: Boolean
      read FRightClickChangeDate write SetRightClickChangeDate default vpDefWVRClickChangeDate;
    property SelectedDayColor: TColor read FSelectedDayColor write SetSelectedDayColor;
    property ShowEvents: Boolean read FShowEvents write SetShowEvents;
    property ShowEventTime: Boolean read FShowEventTime write SetShowEventTime;
    property WeekStartsOn: TVpDayType read FWeekStartsOn write SetWeekStartsOn;
    {events}
    property OnEventClick: TVpOnEventClick read FOnEventClick write FOnEventClick;
    property OnEventDblClick: TVpOnEventClick read FOnEventDblClick write FOnEventDblClick;
  end;


implementation

uses
  SysUtils, LazUTF8, Forms, Dialogs, VpMonthViewPainter;


(*****************************************************************************)
{ TVpMvHeadAttr }

constructor TVpMvHeadAttr.Create(AOwner: TVpMonthView);
begin
  inherited Create;
  FOwner := AOwner;
  FColor := clBtnFace;
  FFont := TVpFont.Create(AOwner);
end;

destructor TVpMvHeadAttr.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TVpMvHeadAttr.SetColor(const Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    FOwner.Invalidate;
  end;
end;

procedure TVpMvHeadAttr.SetFont(Value: TVpFont);
begin
  FFont.Assign(Value);
end;


(*****************************************************************************)
{ TVpContactHeadAttr }
constructor TVpDayHeadAttr.Create(AOwner: TVpMonthView);
begin
  inherited Create;
  FMonthView := AOwner;
  FFont := TVpFont.Create(AOwner);
  FFont.Assign(FMonthView.Font);
  FColor := clSilver;
end;
{=====}

destructor TVpDayHeadAttr.Destroy;
begin
  FFont.Free;
  inherited;
end;
{=====}

procedure TVpDayHeadAttr.SetColor(Value: TColor);
begin
  if Value <> FColor then begin
    FColor := Value;
    MonthView.Invalidate;
  end;
end;
{=====}

procedure TVpDayHeadAttr.SetFont(Value: TVpFont);
begin
  if Value <> FFont then begin
    FFont.Assign(Value);
    MonthView.Invalidate;
  end;
end;


(*****************************************************************************)
{ TVpMvTodayAttr }

constructor TVpMvTodayAttr.Create(AOwner: TVpMonthView);
begin
  inherited Create;
  FMonthView := AOwner;
  FFont := TVpFont.Create(AOwner);
  FFont.Assign(FMonthView.Font);
  FColor := clSilver;
  FBorderPen := TPen.Create;
  FBorderPen.Color := clRed;
  FBorderPen.Width := 3;
  FBorderPen.OnChange := FMonthView.mvPenChanged;
end;

destructor TVpMvTodayAttr.Destroy;
begin
  FBorderPen.Free;
  FFont.Free;
  inherited;
end;

procedure TVpMvTodayAttr.SetColor(Value: TColor);
begin
  if Value <> FColor then begin
    FColor := Value;
    MonthView.Invalidate;
  end;
end;

procedure TVpMvTodayAttr.SetFont(Value: TVpFont);
begin
  if Value <> FFont then begin
    FFont.Assign(Value);
    MonthView.Invalidate;
  end;
end;

procedure TVpMvTodayAttr.SetBorderPen(Value: TPen);
begin
  if Value <> FBorderPen then begin
    FBorderPen.Assign(Value);
    MonthView.Invalidate;
  end;
end;


(*****************************************************************************)
{ TVpMonthView }

constructor TVpMonthView.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csCaptureMouse, csOpaque, csDoubleClicks];

  { Create internal classes and stuff }
  FHeadAttr := TVpMvHeadAttr.Create(self);
  FDayHeadAttr := TVpDayHeadAttr.Create(self);
  FTodayAttr := TVpMvTodayAttr.Create(self);
//  mvEventList := TList.Create;
  mvSpinButtons := TUpDown.Create(self);

  { Set styles and initialize internal variables }
  {$IFDEF VERSION4}
  DoubleBuffered := true;
  {$ENDIF}
  FShowEvents := true;
  FEventDayStyle := [];
  FShowEventTime := false;
  FDayNameStyle :=dsShort;
  FKBNavigate := true;
//  mvInLinkHandler := false;
  mvSpinButtons.OnClick := mvSpinButtonClick;
  mvSpinButtons.Orientation := udHorizontal;
  mvSpinButtons.Min := -32768;
  mvSpinButtons.Max := 32767;
//  mvCreatingEditor := false;
  FSelectedDayColor := clRed;
  FDrawingStyle := ds3d;
//  mvPainting := false;
  FColor := clWindow;
  FOffDayColor := clSilver;
  FLineColor := clGray;
  FDate := Trunc(Now);
  FTimeFormat := tf12Hour;
  FDateLabelFormat := 'mmmm yyyy';
  FColumnWidth := 200;
  FRightClickChangeDate := vpDefWVRClickChangeDate;                      
//  mvVisibleEvents := 0;                                                  

  { set up fonts and colors }
//  FDayHeadAttributes.Font.Name := 'Tahoma';   wp: better use defaults
//  FDayHeadAttributes.Font.Size := 10;
//  FDayHeadAttributes.Font.Style := [];
  FDayHeadAttr.Color := clBtnFace;

  { Assign default font to DayNumberFont and EventFont }
  FDayNumberFont := TVpFont.Create(AOwner);
  FDayNumberFont.Assign(Font);
  FEventFont := TVpFont.Create(AOwner);
  FEventFont.Assign(Font);
  FOffDayFontColor := clGray;

  SetLength(mvEventArray, MaxVisibleEvents);
  SetLength(mvMonthdayArray, 45);

  { size }
  Height := 225;
  Width := 300;

  FDefaultPopup := TPopupMenu.Create(Self);
  Self.PopupMenu := FDefaultPopup;
  LoadLanguage;

  mvHookUp;
end;
{=====}

destructor TVpMonthView.Destroy;
begin
  FHeadAttr.Free;
  FTodayAttr.Free;
  FDayHeadAttr.Free;
  FDayNumberFont.Free;
  FEventFont.Free;
  mvSpinButtons.Free;
//  mvEventList.Free;
  FDefaultPopup.Free;
  inherited;
end;

procedure TVpMonthView.LoadLanguage;
begin
  FDefaultPopup.Items.Clear;
  InitializeDefaultPopup;
end;

{=====}

procedure TVpMonthView.Invalidate;
begin
  inherited;
end;
{=====}

procedure TVpMonthView.LinkHandler(Sender: TComponent;
  NotificationType: TVpNotificationType; const Value: Variant);
begin
//  mvInLinkHandler := true;
//  try
    case NotificationType of
      neDateChange      : Date := Value;
      neDataStoreChange : Invalidate;
      neInvalidate      : Invalidate;
    end;
//  finally
//    mvInLinkHandler := false;
//  end;
end;
{=====}

procedure TVpMonthView.mvHookUp;
var
  I: Integer;
begin
  { If the component is being dropped on a form at designtime, then }
  { automatically hook up to the first datastore component found    }
  if csDesigning in ComponentState then
    for I := 0 to pred(Owner.ComponentCount) do begin
      if (Owner.Components[I] is TVpCustomDataStore) then begin
        DataStore := TVpCustomDataStore(Owner.Components[I]);
        Exit;
      end;
    end;
end;

procedure TVpMonthView.mvPenChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TVpMonthView.Loaded;
begin
  inherited;
  mvLoaded := true;
  mvPopulate;
end;
{=====}

function TVpMonthView.GetControlType: TVpItemType;
begin
  Result := itMonthView;
end;

procedure TVpMonthView.Paint;
begin
  RenderToCanvas(Canvas, Rect (0, 0, Width, Height), ra0, 1, Self.Date,
    -1, -1, gr30Min, False);
end;

procedure TVpMonthView.PaintToCanvas(ACanvas: TCanvas; ARect: TRect;
  Angle: TVpRotationAngle; ADate: TDateTime);
begin
  RenderToCanvas(ACanvas, ARect, Angle, 1, ADate, -1, -1, gr30Min, True);
end;

procedure TVpMonthView.RenderToCanvas(RenderCanvas: TCanvas; RenderIn: TRect;
  Angle: TVpRotationAngle; Scale: Extended; RenderDate: TDateTime;
  StartLine, StopLine: Integer; UseGran: TVpGranularity; DisplayOnly: Boolean);
var
  painter: TVpMonthViewPainter;
begin
//  mvPainting := true;
  painter := TVpMonthViewPainter.Create(Self, RenderCanvas);
  try
    painter.RenderToCanvas(RenderIn, Angle, Scale, RenderDate, StartLine,
      StopLine, UseGran, DisplayOnly);
  finally
    painter.Free;
//    mvPainting := false;
  end;
end;

procedure TVpMonthView.mvPopulate;
begin
  if DataStore <> nil then
    DataStore.Date := FDate;
end;
{=====}

procedure TVpMonthView.mvSpinButtonClick(Sender: TObject; Button: TUDBtnType);
var
  M, D, Y: Word;
begin
  DecodeDate(Date, Y, M, D);
  if Button = btNext then begin
    if M = 12 then begin
      M := 1;
      Y := Y + 1;
    end else
      M := M + 1;
  end else begin
    if M = 1 then begin
      M := 12;
      Y := Y - 1;
    end else
      M := M - 1;
  end;
  if (D > DaysInMonth(Y, M)) then
    D := DaysInMonth(Y, M);

  Date := EncodeDate(Y, M, D);
end;
{=====}

procedure TVpMonthView.SetColor(Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpMonthView.SetDrawingStyle(Value: TVpDrawingStyle);
begin
  if FDrawingStyle <> Value then begin
    FDrawingStyle := Value;
    Repaint;
  end;
end;
{=====}

procedure TVpMonthView.SetLineColor(Value: TColor);
begin
  if FLineColor <> Value then begin
    FLineColor := Value;
    Repaint;
  end;
end;
{=====}

procedure TVpMonthView.SetOffDayColor(Value: TColor);
begin
  if Value <> FOffDayColor then begin
    FOffDayColor := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpMonthView.SetOffDayFontColor(Value: TColor);
begin
  FOffDayFontColor := Value;
  Invalidate;
end;
{=====}

procedure TVpMonthView.SetDateLabelFormat(Value: string);
begin
  if Value <> FDateLabelFormat then begin
    FDateLabelFormat := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpMonthView.SetShowEvents(Value: Boolean);
begin
  if FShowEvents <> Value then begin
    FShowEvents := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpMonthView.SetEventDayStyle(Value: TFontStyles);
begin
  if FEventDayStyle <> Value then begin
    FEventDayStyle := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpMonthView.SetDayNameStyle(Value: TVpMVDayNameStyle);
begin
  if FDayNameStyle <> Value then begin
    FDayNameStyle := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpMonthView.SetDayNumberFont(Value: TVpFont);
begin
  FDayNumberFont.Assign(Value);
  Invalidate;
end;
{=====}

procedure TVpMonthView.SetEventFont(Value: TVpFont);
begin
  FEventFont.Assign(Value);
  Invalidate;
end;
{=====}

procedure TVpMonthView.SetSelectedDayColor(Value: TColor);
begin
  if Value <> FSelectedDayColor then begin
    FSelectedDayColor := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpMonthView.SetShowEventTime(Value: Boolean);
begin
  if Value <> FShowEventTime then begin
    FShowEventTime := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpMonthView.SetTimeFormat(Value: TVpTimeFormat);
begin
  if Value <> FTimeFormat then begin
    FTimeFormat := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpMonthView.SetDate(Value: TDateTime);
begin
  if FDate <> Trunc(Value) then begin
    FDate := Trunc(Value);

    if DataStore <> nil then
      DataStore.Date := FDate;

    if mvLoaded then
      mvPopulate;
    Invalidate;

    if ControlLink <> nil then
      ControlLink.Notify(self, neDateChange, FDate);
  end;
end;
{=====}

{$IFNDEF LCL}
procedure TVpMonthView.WMSize(var Msg: TWMSize);
{$ELSE}
procedure TVpMonthView.WMSize(var Msg: TLMSize);
{$ENDIF}
begin
  inherited;
  { force a repaint on resize }
  Invalidate;
end;
{=====}

procedure TVpMonthView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_TABSTOP;
{$IFNDEF LCL}
    WindowClass.style := CS_DBLCLKS;
{$ENDIF}
  end;
end;
{=====}

procedure TVpMonthView.CreateWnd;
begin
  inherited;
  mvSpinButtons.Parent := self;
end;
{=====}

{$IFNDEF LCL}
procedure TVpMonthView.WMLButtonDown(var Msg: TWMLButtonDown);
{$ELSE}
procedure TVpMonthView.WMLButtonDown(var Msg: TLMLButtonDown);
{$ENDIF}
begin
  inherited;
  // if the mouse was pressed down in the client area, then select the cell.
  if not focused then SetFocus;

  if (Msg.YPos > mvDayHeadHeight) then
  begin
    { The mouse click landed inside the client area }
    MvSetDateByCoord(Point(Msg.XPos, Msg.YPos));
    { Did the mouse click land on an event? }
    if SelectEventAtCoord(Point(Msg.XPos, Msg.YPos))                     
    and (Assigned(FOnEventClick)) then                                   
        FOnEventClick(self, mvActiveEvent);                              
  end;
end;
{=====}

{$IFNDEF LCL}
procedure TVpMonthView.WMLButtonDblClick(var Msg: TWMLButtonDblClk);
{$ELSE}
procedure TVpMonthView.WMLButtonDblClick(var Msg: TLMLButtonDblClk);
{$ENDIF}
begin                                                                    
  inherited;                                                             
  // if the mouse was pressed down in the client area, then select the   
  // cell.                                                               
  if not focused then SetFocus;                                          
                                                                         
  if (Msg.YPos > mvDayHeadHeight) then                                   
  begin                                                                  
    { The mouse click landed inside the client area }                    
    MvSetDateByCoord(Point(Msg.XPos, Msg.YPos));                         
    { Did the mouse click land on an event? }                            
    if SelectEventAtCoord(Point(Msg.XPos, Msg.YPos))                     
    and (Assigned(FOnEventDblClick)) then                                
      FOnEventDblClick(self, mvActiveEvent);                             
  end;                                                                   
end;                                                                     
{=====}                                                                  

{$IFNDEF LCL}
procedure TVpMonthView.WMSetFocus(var Msg: TWMSetFocus);
{$ELSE}
procedure TVpMonthView.WMSetFocus(var Msg: TLMSetFocus);
{$ENDIF}
begin
  Unused(Msg);
  // if active event is nil then set active event to the first diaplsyed one.
end;
{=====}

{$IFNDEF LCL}
procedure TVpMonthView.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  Msg.Result := 1;
end;
{$ENDIF}
{=====}

{$IFNDEF LCL}
procedure TVpMonthView.WMRButtonDown(var Msg: TWMRButtonDown);
{$ELSE}
procedure TVpMonthView.WMRButtonDown(var Msg: TLMRButtonDown);
{$ENDIF}
begin
  inherited;
  if not Assigned (PopupMenu) then begin
    if not focused then
      SetFocus;
    if FRightClickChangeDate then                                        
      mvSetDateByCoord (Point (Msg.XPos, Msg.YPos));                     
  end;
end;
{=====}

procedure TVpMonthView.InitializeDefaultPopup;
var
  NewItem : TMenuItem;
begin
  if RSMonthPopupToday <> '' then begin
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := RSMonthPopupToday;
    NewItem.OnClick := PopupToday;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSMonthPopupNextMonth <> '' then begin
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := RSMonthPopupNextMonth;
    NewItem.OnClick := PopupNextMonth;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSMonthPopupPrevMonth <> '' then begin
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := RSMonthPopupPrevMonth;
    NewItem.OnClick := PopupPrevMonth;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSMonthPopupNextYear <> '' then begin
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := RSMonthPopupNextYear;
    NewItem.OnClick := PopupNextYear;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSMonthPopupPrevYear <> '' then begin
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := RSMonthPopupPrevYear;
    NewItem.OnClick := PopupPrevYear;
    FDefaultPopup.Items.Add (NewItem);
  end;
end;
{=====}

procedure TVpMonthView.PopupToday(Sender: TObject);
begin
  Date := Now;
end;
{=====}

procedure TVpMonthView.PopupNextMonth(Sender: TObject);
begin
  mvSpinButtonClick(self, btNext);
end;
{=====}

procedure TVpMonthView.PopupPrevMonth(Sender: TObject);
begin
  mvSpinButtonClick(self, btPrev);
end;
{=====}

procedure TVpMonthView.PopupNextYear(Sender: TObject);
var
  M, D, Y: Word;
begin
  DecodeDate(Date, Y, M, D);
  Date := EncodeDate(Y + 1, M, 1);
end;
{=====}

procedure TVpMonthView.PopupPrevYear(Sender: TObject);
var
  M, D, Y: Word;
begin
  DecodeDate(Date, Y, M, D);
  Date := EncodeDate(Y - 1, M, 1);
end;
{=====}

{ - renamed from EditEventAtCoord and re-written}
function TVpMonthView.SelectEventAtCoord(Point: TPoint): Boolean;        
var
  I: Integer;
begin
  result := false;
  I := 0;
  while I < Length(mvEventArray) do begin
    if mvEventArray[I].Event = nil then begin
      Inc(I);
      Break;
    end else begin
      if (Point.X > mvEventArray[I].Rec.Left)
      and (Point.X < mvEventArray[I].Rec.Right)
      and (Point.Y > mvEventArray[I].Rec.Top)
      and (Point.Y < mvEventArray[I].Rec.Bottom) then begin
        result := true;
        Break;
      end else
        Inc(I);
    end;
  end;

  if result then begin
    mvActiveEvent := TVpEvent(mvEventArray[I].Event);
    mvActiveEventRec := mvEventArray[I].Rec;
  end;
end;
{=====}

procedure TVpMonthView.mvSetDateByCoord(Point: TPoint);
var
  I: Integer;
begin
  for I := 0 to pred(Length(mvMonthdayArray)) do
    if PointInRect(Point, mvMonthdayArray[I].Rec) then begin
      Date := mvMonthdayArray[I].Date;
      break;
    end;
end;
{=====}

procedure TVpMonthView.KeyDown(var Key: Word; Shift: TShiftState);
var
  M, D, Y: Word;
  PopupPoint: TPoint;
begin
  if FKBNavigate then
    case Key of
      VK_UP :
        if ssCtrl in Shift then begin
          DecodeDate(Date, Y, M, D);
          Date := EncodeDate(Y - 1, M, 1);
        end else
          Date := Date - 7;
      VK_DOWN:
        if ssCtrl in Shift then begin
          DecodeDate(Date, Y, M, D);
          Date := EncodeDate(Y + 1, M, 1);
        end else
          Date := Date + 7;
      VK_NEXT:
        mvSpinButtonClick(self, btNext);
      VK_PRIOR:
        mvSpinButtonClick(self, btPrev);
      VK_LEFT:
        if ssCtrl in Shift then
          mvSpinButtonClick(self, btPrev)
        else
          Date := Date - 1;
      VK_RIGHT:
        if ssCtrl in Shift then
          mvSpinButtonClick(self, btNext)
        else
          Date := Date + 1;
      VK_HOME:
        begin
          DecodeDate(Date, Y, M, D);
          if D = 1 then
            mvSpinButtonClick(self, btPrev)
          else
            Date := EncodeDate(Y, M, 1);
        end;
      VK_END:
        begin
          DecodeDate(Date, Y, M, D);
          if D = DaysInMonth(Y, M) then begin
            if M = 12 then begin
              M := 1;
              Inc(Y);
            end else
              Inc(M);
          end;
          Date := EncodeDate(Y, M, DaysInMonth(Y, M));
        end;
{$IFNDEF LCL}
      VK_TAB:
        if ssShift in Shift then
          Windows.SetFocus(GetNextDlgTabItem(GetParent(Handle), Handle, False))
        else
          Windows.SetFocus(GetNextDlgTabItem(GetParent(Handle), Handle, True));
{$ENDIF}
      VK_F10:
        if (ssShift in Shift) and not Assigned(PopupMenu) then begin
          PopupPoint := GetClientOrigin;
          FDefaultPopup.Popup(PopupPoint.x + 10, PopupPoint.y + 10);
        end;
      VK_APPS:
        if not Assigned(PopupMenu) then begin
          PopupPoint := GetClientOrigin;
          FDefaultPopup.Popup(PopupPoint.x + 10, PopupPoint.y + 10);
        end;
    end;
end;
{=====}
procedure TVpMonthView.SetRightClickChangeDate(const v: Boolean);
begin                                                                    
  if v <> FRightClickChangeDate then                                     
    FRightClickChangeDate := v;                                          
end;                                                                     
{=====}
procedure TVpMonthView.SetWeekStartsOn(Value: TVpDayType);
begin
  if Value <> FWeekStartsOn then begin
    FWeekStartsOn := Value;
    Invalidate;
  end;
end;
{=====}

end.
