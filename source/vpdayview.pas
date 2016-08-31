{*********************************************************}
{*                 VPDAYVIEW.PAS 1.03                    *}
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

{
  This unit contains everything needed for the TVpDayView component (including
  the inline editor).

  The rendering of Visual PlanIt components is a bit involved.  The component's
  Paint method calls RenderToCanvas.  The RenderToCanvas method of each of
  the visual VisualPlanIt controls is repsonsible both for drawing to the
  screen (both design and run time) as well as printing.  In the case of
  printing, the component needs to render itself to an arbitrary rectangle
  and possibly rotated (for the screen the rectangle is the ClientRect
  and the rotation angle is always zero).  To achieve that goal, the
  functions in VpCanvasUtils are used to go between the rendering of the
  control and the TCanvas that it needs to render to.

  The rendering of the DayView is complex.  Look at the other components
  (MonthView and TaskList are probably the best places to start) before making
  changes to the DayView rendering.

  The in place editor is currently based off the TCustomEdit class.  This can
  probably be changed to use a TCustomMemo as its base class.  This will
  provide multi-line editing capabilities.
}

{$I vp.inc}

{.$DEFINE DEBUGDV} { Causes the DayView to operate in debug mode }

unit VpDayView;

interface

uses
  {$IFDEF LCL}
  LMessages, LCLProc, LCLType, LCLIntf,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  Classes, Graphics, Controls, ExtCtrls, StdCtrls,
  Buttons, VpBase, VpBaseDS, VpMisc, VpData, VpSR, VpConst,
  VpCanvasUtils, Menus;

type
  TVpLineRec = packed record
    Hour: TVpHours;
    Minute: Integer;
    Time: TDateTime;
    Rec: TRect;
  end;

  TVpColRec = packed record
    Rec: TRect;
    Date: TDateTime;
  end;

type
  TVpLineArray = array of TVpLineRec;

type
  TVpLineMatrix = array of TVpLineArray;
  TVpColRectArray = array of TVpColRec;

  TVpDVIconData = record                                                 
    Show: Boolean;
    Bitmap: TBitmap;
  end;

  TVpDVIconTypes = (itAlarm, itRecurring, itCategory, itCustom);

  TVpDVIcons = array [itAlarm..itCustom] of TVpDVIconData;

  TVpOnDVBeforeDrawEvent = procedure (Sender: TObject; Event: TVpEvent;
    Active: Boolean; ACanvas: TCanvas; EventRect: TRect; IconRect: TRect) of object;

  TVpOnDVAfterDrawEvent = procedure (Sender: TObject; Event: TVpEvent;
    Active: Boolean; ACanvas: TCanvas; EventRect: TRect; IconRect: TRect) of object;

  TVpOnDVDrawIcons = procedure (Sender: TObject; Event: TVpEvent;
    var Icons: TVpDVIcons) of object;

  TVpDVWrapStyle = (wsNone, wsIconFlow, wsNoFlow);

  { Forward Declarations }
  TVpDayView = class;

  TVpDvInPlaceEdit = class(TCustomEdit)
  protected{private}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    {$IFNDEF LCL}
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    {$ELSE}
    procedure WMKillFocus(var Msg: TLMKillFocus); message LM_KILLFOCUS;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TVpRHAttributes = class(TPersistent)
  protected{ private }
    FOwner: TVpDayView;
    FColor: TColor;
    FHourFont: TVpFont;
    FMinuteFont: TVpFont;
    procedure SetColor(const Value: TColor);
    procedure SetHourFont(Value: TVpFont);
    procedure SetMinuteFont(Value: TVpFont);
  public
    constructor Create(AOwner: TVpDayView);
    destructor Destroy; override;
    property Owner: TVpDayView read FOwner;
  published
    property HourFont: TVpFont read FHourFont write SetHourFont;
    property MinuteFont: TVpFont read FMinuteFont write SetMinuteFont;
    property Color: TColor read FColor write SetColor;
  end;

  TVpAllDayEventAttributes = class(TPersistent)
  protected {Private}
    FOwner: TWinControl;
    FBackgroundColor: TColor;
    FEventBackgroundColor: TColor;
    FEventBorderColor: TColor;
    FFont: TVpFont;
  public
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;
    procedure SetBackGroundColor(Value: TColor);
    procedure SetEventBackgroundColor(Value: TColor);
    procedure SetFont(Value: TVpFont);
    procedure SetEventBorderColor(Value: TColor);
  published
    property BackgroundColor: TColor
      read FBackgroundColor write SetBackGroundColor;
    property EventBorderColor: TColor
      read FEventBorderColor write SetEventBorderColor;
    property EventBackgroundColor: TColor
      read FEventBackgroundColor write SetEventBackgroundColor;
    property Font: TVpFont
      read FFont write SetFont;
  end;

  TVpCHAttributes = class(TPersistent)
  protected{ private }
    FOwner: TVpDayView;
    FColor: TColor;
    FFont: TVpFont;
    procedure SetColor(const Value: TColor);
    procedure SetFont(Value: TVpFont);
  public
    constructor Create(AOwner: TVpDayView);
    destructor Destroy; override;
    property Owner: TVpDayView read FOwner;
  published
    property Font: TVpFont read FFont write SetFont;
    property Color: TColor read FColor write SetColor;
  end;

  TVpDayViewIconAttributes = class(TPersistent)
    private
      FShowAlarmBitmap: Boolean;
      FShowCategoryBitmap: Boolean;
      FShowRecurringBitmap: Boolean;
      FAlarmBitmap: TBitmap;
      FRecurringBitmap: TBitmap;
      FShowInPrint: Boolean;
      FOwner: TVpLinkableControl;

    protected
      procedure SetAlarmBitmap(v: TBitmap);
      procedure SetRecurringBitmap(v: TBitmap);
      procedure SetShowAlarmBitmap(const v: Boolean);
      procedure SetShowCategoryBitmap(const v: Boolean);
      procedure SetShowRecurringBitmap(const v: Boolean);

    public
      constructor Create(AOwner: TVpLinkableControl);
      destructor Destroy; override;

    published
      property AlarmBitmap: TBitmap
        read FAlarmBitmap write SetAlarmBitmap;
      property RecurringBitmap: TBitmap
        read FRecurringBitmap write SetRecurringBitmap;
      property ShowAlarmBitmap: Boolean
        read FShowAlarmBitmap write SetShowAlarmBitmap default True;
      property ShowCategoryBitmap : Boolean
        read FShowCategoryBitmap write SetShowCategoryBitmap default True;
      property ShowRecurringBitmap : Boolean
        read FShowRecurringBitmap write SetShowRecurringBitmap default True;
      property ShowInPrint: Boolean
        read FShowInPrint write FShowInPrint default True;
  end;

  { TVpDayView }

  TVpDayView = class(TVpLinkableControl)
  protected{ private }
    FGranularity: TVpGranularity;
    FColumnWidth: Integer;
    FColor: TColor;
    FLineColor: TColor;
    FDefTopHour: TVpHours;
    FTopHour: TVpHours;
    FDateLabelFormat: string;
    FShowResourceName: Boolean;
    FTopLine: Integer;
    FActiveRow: Integer;
    FActiveCol: Integer;
    FActiveEvent: TVpEvent;
    FGutterWidth: Integer;
    FDefaultPopup: TPopupMenu;
    FLineCount: Integer;
    FVisibleLines: Integer;
    FTimeFormat: TVpTimeFormat;
    FDrawingStyle: TVpDrawingStyle;
    FTimeSlotColors: TVpTimeSlotColor;
    FRowHeadAttr: TVpRHAttributes;
    FHeadAttr: TVpCHAttributes;
    FAllDayEventAttr: TVpAllDayEventAttributes;
    FDisplayDate: TDateTime;
    FScrollBars: TScrollStyle;
    FIconAttributes: TVpDayViewIconAttributes;
    FWrapStyle: TVpDVWrapStyle;
    FDotDotDotColor: TColor;
    FShowEventTimes: Boolean;
    FAllowInplaceEdit: Boolean;
    FDragDropTransparent: Boolean;
    FAllowDragAndDrop: Boolean;
    { event variables }
    FOwnerDrawRowHead: TVpOwnerDrawRowEvent;
    FOwnerDrawCells: TVpOwnerDrawRowEvent;
    FOwnerDrawColHead: TVpOwnerDrawEvent;
    FBeforeEdit: TVpBeforeEditEvent;
    FAfterEdit: TVpAfterEditEvent;
    FOwnerEditEvent: TVpEditEvent;
    FOnDrawIcons: TVpOnDVDrawIcons;
    FOnBeforeDrawEvent: TVpOnDVBeforeDrawEvent;
    FOnAfterDrawEvent: TVpOnDVAfterDrawEvent;
    FOnAddEvent: TVpOnAddNewEvent;
    FNumDays: Integer;
    FIncludeWeekends: Boolean;
    { internal variables }
    dvClickTimer: TTimer;
    dvLoaded: Boolean;
    dvInLinkHandler: Boolean;
    dvRowHeight: Integer;
    dvColHeadHeight: Integer;
    dvRowHeadWidth: Integer;
    dvClientVArea: Integer;
    dvMouseDownPoint: TPoint;
    dvMouseDown: Boolean;
    dvEndingEditing: Boolean;
    dvDragging: Boolean;
    dvDragStartTime: TDateTime;

    { Nav Buttons }
    dvDayUpBtn: TSpeedButton;
    dvDayDownBtn: TSpeedButton;
    dvTodayBtn: TSpeedButton;
    dvWeekUpBtn: TSpeedButton;
    dvWeekDownBtn: TSpeedButton;

    dvLineMatrix: TVpLineMatrix;
    dvColRectArray: TVpColRectArray;
    dvEventArray: TVpEventArray;
    dvActiveEventRec: TRect;
    dvActiveIconRec: TRect;
    dvInPlaceEditor: TVpDvInPlaceEdit;
    dvCreatingEditor: Boolean;
    { the granularity based time increment for each row }
    dvTimeIncSize: double;
    dvPainting: Boolean;
    dvVScrollDelta: Integer;
    dvHotPoint: TPoint;

    { property methods }
    function GetLastVisibleDate: TDateTime;
    function GetRealNumDays(WorkDate: TDateTime) : Integer;
    procedure SetDrawingStyle(Value: TVpDrawingStyle);
    procedure SetColor(Value: TColor);
    procedure SetLineColor(Value: TColor);
    procedure SetTopHour(Value: TVpHours);
    procedure SetTopLine(Value: Integer);
    procedure SetDateLabelFormat(Value: string);
    procedure SetGutterWidth(Value: Integer);
    procedure SetDefTopHour(Value: TVpHours);
    procedure SetGranularity(Value: TVpGranularity);
    procedure SetTimeFormat(Value: TVpTimeFormat);
    procedure SetNumDays(Value: Integer);
    procedure SetIncludeWeekends(Value: Boolean);
    procedure SetDisplayDate(Value: TDateTime);
    procedure SetVScrollPos;
    procedure SetShowResourceName(Value: Boolean);
    procedure SetActiveRow(Value: Integer);
    procedure SetActiveCol(Value: Integer);
    procedure SetWrapStyle(const v: TVpDVWrapStyle);
    procedure SetDotDotDotColor(const v: TColor);
    procedure SetShowEventTimes(Value: Boolean);
    { drag-drop methods }
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    { internal methods }
    function dvCalcRowHeight(Scale: Extended; UseGran: TVpGranularity): Integer;
    function dvCalcVisibleLines(RenderHeight, ColHeadHeight, RowHeight: Integer;
      Scale: Extended; StartLine, StopLine: Integer): Integer;
    function dvCalcColHeadHeight(Scale: Extended): Integer;
    procedure dvEditInPlace(Sender: TObject);
    procedure dvHookUp;
    procedure PopupAddEvent(Sender: TObject);
    procedure PopupDeleteEvent(Sender: TObject);
    procedure PopupEditEvent(Sender: TObject);
    procedure PopupToday(Sender: TObject);
    procedure PopupTomorrow(Sender: TObject);
    procedure PopupYesterday(Sender: TObject);
    procedure PopupNextDay(Sender: TObject);
    procedure PopupPrevDay(Sender: TObject);
    procedure PopupNextWeek(Sender: TObject);
    procedure PopupPrevWeek(Sender: TObject);
    procedure PopupNextMonth(Sender: TObject);
    procedure PopupPrevMonth(Sender: TObject);
    procedure PopupNextYear(Sender: TObject);
    procedure PopupPrevYear(Sender: TObject);
    procedure InitializeDefaultPopup;
    procedure Paint; override;
    procedure Loaded; override;
    procedure dvSpawnEventEditDialog(NewEvent: Boolean);
    procedure dvSetActiveRowByCoord(Pnt: TPoint; Sloppy: Boolean);
    procedure dvSetActiveColByCoord(Pnt: TPoint);
    procedure dvPopulate;
    procedure dvNavButtonsClick(Sender: TObject);
    procedure dvScrollVertical(Lines: Integer);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure SetActiveEventByCoord(APoint: TPoint);
    function EditEventAtCoord(Point: TPoint): Boolean;
    function GetEventAtCoord(Point: TPoint): TVpEvent;
    procedure EditEvent;
    procedure EndEdit(Sender: TObject);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SetTimeIntervals(UseGran: TVpGranularity);
    { message handlers }
    procedure VpDayViewInit(var Msg: {$IFDEF DELPHI}TMessage{$ELSE}TLMessage{$ENDIF}); message Vp_DayViewInit;
    {$IFNDEF LCL}
    procedure WMLButtonDblClk(var Msg : TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMSetFocus(var Msg : TWMSetFocus); message WM_SETFOCUS;
    procedure WMEraseBackground (var Msg : TWMERASEBKGND);   // ??? wp: missing "message WM_ERASEBKGND"?
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    {$ELSE}
    function  DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function  DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function  DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure WMSize(var Msg: TLMSize); message LM_SIZE;
    procedure WMVScroll(var Msg: TLMVScroll); message LM_VSCROLL;
    procedure WMSetFocus(var Msg: TLMSetFocus); message LM_SETFOCUS;
    procedure WMEraseBackground(var Msg: TLMERASEBKGND);  message LM_ERASEBKGND;
    procedure WMLButtonDblClk(var Msg: TLMLButtonDblClk); message LM_LBUTTONDBLCLK;
    {$ENDIF}

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadLanguage;

    procedure DeleteActiveEvent(Verify: Boolean);
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
//    function HourToLine(const Value: TVpHours; const UseGran: TVpGranularity): Integer;
    procedure Invalidate; override;
    procedure LinkHandler(Sender: TComponent; NotificationType: TVpNotificationType;
      const Value: Variant); override;
    procedure EditSelectedEvent;

    function GetControlType: TVpItemType; override;
    procedure AutoScaledPaintToCanvas(PaintCanvas: TCanvas; PaintTo: TRect;
      Angle: TVpRotationAngle; RenderDate: TDateTime; StartLine, StopLine: Integer;
      UseGran: TVpGranularity);
    procedure PaintToCanvas (ACanvas: TCanvas; ARect: TRect; Angle: TVpRotationAngle;
      ADate: TDateTime; StartHour, EndHour: TVpHours; UseGran: TVpGranularity);
    procedure RenderToCanvas(RenderCanvas: TCanvas; RenderIn: TRect;
      Angle: TVpRotationAngle; Scale: Extended; RenderDate: TDateTime;
      StartLine, StopLine: Integer; UseGran: TVpGranularity; DisplayOnly: Boolean); override;

    property ActiveEvent: TVpEvent read FActiveEvent write FActiveEvent;
    property TopHour: TVpHours read FTopHour write SetTopHour;
    property TopLine: Integer read FTopLine write SetTopLine;
    property LineCount: Integer read FLineCount;
    property ActiveRow: Integer read FActiveRow write SetActiveRow;
    property ActiveCol: Integer read FActiveCol write SetActiveCol;
    property Date: TDateTime read FDisplayDate write SetDisplayDate;
    property LastVisibleDate: TDateTime read GetLastVisibleDate;
    property VisibleLines: Integer read FVisibleLines;

  published
    property Align;
    property Anchors;
    property Constraints;
    property ReadOnly;
    property TabStop;
    property TabOrder;
    property Font;
    property AllDayEventAttributes: TVpAllDayEventAttributes read FAllDayEventAttr write FAllDayEventAttr;
    property AllowDragAndDrop: Boolean read FAllowDragAndDrop write FAllowDragAndDrop default false;
    property AllowInplaceEditing: Boolean read FAllowInplaceEdit write FAllowInplaceEdit default true;
    property DotDotDotColor: TColor read FDotDotDotColor write SetDotDotDotColor default clBlack;
    property ShowEventTimes: Boolean read FShowEventTimes write SetShowEventTimes default true;
    property DragDropTransparent: Boolean read FDragDropTransparent write FDragDropTransparent default false;
    property DrawingStyle: TVpDrawingStyle read FDrawingStyle write SetDrawingStyle stored True;
    property TimeSlotColors: TVpTimeSlotColor read FTimeSlotColors write FTimeSlotColors;
    property HeadAttributes: TVpCHAttributes read FHeadAttr write FHeadAttr;
    property RowHeadAttributes: TVpRHAttributes read FRowHeadAttr write FRowHeadAttr;
    property IconAttributes: TVpDayViewIconAttributes read FIconAttributes write FIconAttributes;
    property Color: TColor read FColor write SetColor;
    property OwnerDrawRowHeader: TVpOwnerDrawRowEvent read FOwnerDrawRowHead write FOwnerDrawRowHead;
    property OwnerDrawColHeader: TVpOwnerDrawEvent read FOwnerDrawColHead write FOwnerDrawColHead;
    property OwnerDrawCells: TVpOwnerDrawRowEvent read FOwnerDrawCells write FOwnerDrawCells;
    property ShowResourceName: Boolean read FShowResourceName write SetShowResourceName;
    property LineColor: TColor read FLineColor write SetLineColor;
    property GutterWidth: Integer read FGutterWidth write SetGutterWidth;
    property DateLabelFormat: string read FDateLabelFormat write SetDateLabelFormat;
    Property Granularity: TVpGranularity read FGranularity write SetGranularity;
    property DefaultTopHour: TVpHours read FDefTopHour write SetDefTopHour;
    property TimeFormat: TVpTimeFormat read FTimeFormat write SetTimeFormat;
    property IncludeWeekends: Boolean read FIncludeWeekends write SetIncludeWeekends default True;
    property NumDays: Integer read FNumDays write SetNumDays default 1;
    property WrapStyle: TVpDVWrapStyle read FWrapStyle Write SetWrapStyle default wsIconFlow;
    {events}
    property AfterEdit: TVpAfterEditEvent read FAfterEdit write FAfterEdit;
    property BeforeEdit: TVpBeforeEditEvent read FBeforeEdit write FBeforeEdit;
    property OnAddEvent: TVpOnAddNewEvent read FOnAddEvent write FOnAddEvent;
    property OnAfterDrawEvent: TVpOnDVAfterDrawEvent read FOnAfterDrawEvent write FOnAfterDrawEvent;
    property OnBeforeDrawEvent: TVpOnDVBeforeDrawEvent read FOnBeforeDrawEvent write FOnBeforeDrawEvent;
    property OnDrawIcons: TVpOnDVDrawIcons read FOnDrawIcons Write FOnDrawIcons;
    property OnOwnerEditEvent: TVpEditEvent read FOwnerEditEvent write FOwnerEditEvent;
    property OnClick;
  end;

implementation

uses
  SysUtils, Math, Forms, Dialogs,
  VpEvntEditDlg, VpDayViewPainter;

(*****************************************************************************)
{ TVpTGInPlaceEdit }

constructor TVpDvInPlaceEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := False;
  BorderStyle := bsNone;
  DoubleBuffered := False;
end;
{=====}

procedure TVpDvInPlaceEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style{$IFNDEF LCL} or ES_MULTILINE{$ENDIF};
end;
{=====}

procedure TVpDvInPlaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// !!!! WARNING
// !!!!
// !!!! Experimental change below.  Verify this change before releasing
// !!!! VP 1.03
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
begin
  case Key of
    VK_RETURN:
      begin
        Key := 0;
        TVpDayView(Owner).EndEdit(Self);
      end;

    VK_UP:
      begin
        Key := 0;
        TVpDayView(Owner).ActiveRow := TVpDayView(Owner).ActiveRow - 1;
// !!!! TVpDayView(Owner).EndEdit(Self); !!!!  !!!!!!!!!!!!!!!!!!!!!!!!!
      end;

    VK_DOWN:
      begin
        Key := 0;
        TVpDayView(Owner).ActiveRow := TVpDayView(Owner).ActiveRow + 1;
// !!!! TVpDayView(Owner).EndEdit(Self); !!!!  !!!!!!!!!!!!!!!!!!!!!!!!!
      end;

    VK_ESCAPE:
      begin
        Key := 0;
        TVpDayView(Owner).SetFocus;
      end;

    else
      inherited;
  end;
end;
{=====}

{$IFNDEF LCL}
procedure TVpDvInPlaceEdit.WMKillFocus(var Msg: TWMKillFocus);
{$ELSE}
procedure TVpDvInPlaceEdit.WMKillFocus(var Msg: TLMKillFocus);
{$ENDIF}
begin
  Unused(Msg);
  TVpDayView(Owner).EndEdit(self);
end;
{=====}


{ TVpAllDayEventAttributes }

constructor TVpAllDayEventAttributes.Create(AOwner: TWinControl);
begin
  FOwner:= AOwner;
  FFont := TVpFont.Create(AOwner);
  FBackgroundColor := clBtnShadow;
  FEventBackgroundColor := clBtnFace;
  FEventBorderColor := cl3dDkShadow;
end;
{=====}

destructor TVpAllDayEventAttributes.Destroy;
begin
  inherited;
  FFont.Free;
end;
{=====}

procedure TVpAllDayEventAttributes.SetBackGroundColor(Value: TColor);
begin
  FBackgroundColor := Value;
  FOwner.Invalidate;
end;
{=====}

procedure TVpAllDayEventAttributes.SetEventBackgroundColor(Value: TColor);
begin
  FEventBackgroundColor := Value;
  FOwner.Invalidate;
end;
{=====}

procedure TVpAllDayEventAttributes.SetEventBorderColor(Value: TColor);
begin
  FEventBorderColor := Value;
  FOwner.Invalidate;
end;

procedure TVpAllDayEventAttributes.SetFont(Value: TVpFont);
begin
  FFont.Assign(Value);
  FFont.Owner := FOwner;
end;
{=====}

(*****************************************************************************)
{ TVpDayViewIconAttributes }

constructor TVpDayViewIconAttributes.Create(AOwner: TVpLinkableControl);
begin
  inherited Create;
  FOwner := AOwner;
  FAlarmBitmap := TBitmap.Create;
  FRecurringBitmap := TBitmap.Create;
  FShowAlarmBitmap := True;
  FShowCategoryBitmap := True;
  FShowRecurringBitmap := True;
  FShowInPrint := True;
end;

destructor TVpDayViewIconAttributes.Destroy;
begin
  FAlarmBitmap.Free;
  FRecurringBitmap.Free;
  inherited Destroy;
end;

procedure TVpDayViewIconAttributes.SetAlarmBitmap(v: TBitmap);
begin
  FAlarmBitmap.Assign(v);
  if Assigned(FOwner) then
    FOwner.Invalidate;
end;

procedure TVpDayViewIconAttributes.SetRecurringBitmap(v: TBitmap);
begin
  FRecurringBitmap.Assign(v);
  if Assigned(FOwner) then
    FOwner.Invalidate
end;

procedure TVpDayViewIconAttributes.SetShowAlarmBitmap(const v: Boolean);
begin
  if FShowAlarmBitmap <> v then begin
    FShowAlarmBitmap := v;
    if Assigned(FOwner) then
      FOwner.Invalidate
  end;
end;

procedure TVpDayViewIconAttributes.SetShowCategoryBitmap(const v: Boolean);
begin
  if FShowCategoryBitmap <> v then begin
    FShowCategoryBitmap := v;
    if Assigned(FOwner) then
      FOwner.Invalidate;
  end;
end;

procedure TVpDayViewIconAttributes.SetShowRecurringBitmap(const v: Boolean);
begin
  if FShowRecurringBitmap <> v then begin
    FShowRecurringBitmap := v;
    if Assigned(FOwner) then
      FOwner.Invalidate
  end;
end;

(*****************************************************************************)
{ TVpDayView }

constructor TVpDayView.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csCaptureMouse, csOpaque, csDoubleClicks];

  { Create internal classes and stuff }
  FTimeSlotColors := TVpTimeSlotColor.Create(self);
  FHeadAttr := TVpCHAttributes.Create(self);
  FRowHeadAttr := TVpRHAttributes.Create(self);
  FAllDayEventAttr := TVpAllDayEventAttributes.Create(self);
  dvClickTimer := TTimer.Create(self);
  FIconAttributes := TVpDayViewIconAttributes.Create(Self);

  { create Nav buttons }
  dvDayUpBtn := TSpeedButton.Create(self);
  dvDayUpBtn.Parent := self;
  dvDayDownBtn := TSpeedButton.Create(self);
  dvDayDownBtn.Parent := self;
  dvTodayBtn := TSpeedButton.Create(self);
  dvTodayBtn.Parent := self;
  dvWeekDownBtn := TSpeedButton.Create(self);
  dvWeekDownBtn.Parent := self;
  dvWeekUpBtn := TSpeedButton.Create(self);
  dvWeekUpBtn.Parent := self;
  { flat }
  dvTodayBtn.Flat := true;
  dvWeekDownBtn.Flat := true;
  dvDayDownBtn.Flat := true;
  dvDayUpBtn.Flat := true;
  dvWeekUpBtn.Flat := true;
  { transparent }
  dvTodayBtn.Transparent := true;
  dvWeekDownBtn.Transparent := true;
  dvDayDownBtn.Transparent := true;
  dvDayUpBtn.Transparent := true;
  dvWeekUpBtn.Transparent := true;
  { load their images }
  dvDayUpBtn.Glyph.LoadFromResourceName(HINSTANCE, 'VPRIGHTARROW');
  dvDayDownBtn.Glyph.LoadFromResourceName(HINSTANCE, 'VPLEFTARROW');
  dvTodayBtn.Glyph.LoadFromResourceName(HINSTANCE, 'VPTODAY');
  dvWeekUpBtn.Glyph.LoadFromResourceName(HINSTANCE, 'VPRIGHTARROWS');
  dvWeekDownBtn.Glyph.LoadFromResourceName(HINSTANCE, 'VPLEFTARROWS');
  { set their OnClick handler }
  dvDayUpBtn.OnClick := dvNavButtonsClick;
  dvDayDownBtn.OnClick := dvNavButtonsClick;
  dvTodayBtn.OnClick := dvNavButtonsClick;
  dvWeekUpBtn.OnClick := dvNavButtonsClick;
  dvWeekDownBtn.OnClick := dvNavButtonsClick;
  { Set up the hints }
  dvDayUpBtn.ShowHint := True;
  dvDayDownBtn.ShowHint := True;
  dvTodayBtn.ShowHint := True;
  dvWeekUpBtn.ShowHint := True;
  dvWeekDownBtn.ShowHint := True;

  { Set styles and initialize internal variables }
  {$IFDEF VERSION4}
  DoubleBuffered := true;
  {$ENDIF}
  NumDays := 1;
  dvInLinkHandler := false;
  dvClickTimer.Enabled := false;
  dvClickTimer.Interval := ClickDelay;
  dvClickTimer.OnTimer := dvEditInPlace;

  dvCreatingEditor := false;
  FDrawingStyle := ds3d;
  dvPainting := false;
  FShowResourceName := true;
  FColor := clWindow;
  FLineColor := clGray;
  Granularity := gr30min;
  FDefTopHour := h_07;
  FDisplayDate := Now;
  TopHour := FDefTopHour;
  FTimeFormat := tf12Hour;
  FDateLabelFormat := 'dddd, mmmm dd, yyyy';
  FColumnWidth := 200;
  FScrollBars := ssVertical;
  FActiveRow := -1;
  FGutterWidth := 7;
  dvEndingEditing := False;
  FWrapStyle := wsIconFlow;
  FDotDotDotColor := clBlack;
  FIncludeWeekends := True;
  FAllowInplaceEdit := true;

  { set up fonts and colors }
  FHeadAttr.Font.Size := 10;
  FHeadAttr.Font.Style := [];
  FHeadAttr.Color := clBtnFace;

  FRowHeadAttr.FHourFont.Size := 18;
  FRowHeadAttr.FHourFont.Style := [];
  FRowHeadAttr.FMinuteFont.Size := 9;
  FRowHeadAttr.FMinuteFont.Style := [];
  FRowHeadAttr.Color := clBtnFace;
 {$IFNDEF LCL}
  FHeadAttr.Font.Name := 'Tahoma';
  FRowHeadAttr.FHourFont.Name := 'Tahoma';
  FRowHeadAttr.FMinuteFont.Name := 'Tahoma';
 {$ENDIF}

  SetLength(dvEventArray, MaxVisibleEvents);

  DragMode := dmManual;
  dvDragging := false;

  dvMouseDownPoint := Point(0, 0);
  dvMouseDown := false;

  { size }
  Height := 225;
  Width := 265;

  FDefaultPopup := TPopupMenu.Create(Self);
  Self.PopupMenu := FDefaultPopup;
  LoadLanguage;

  dvHookUp;
end;
{=====}

destructor TVpDayView.Destroy;
begin
  FreeAndNil(dvInplaceEditor);

  FTimeSlotColors.Free;
  FHeadAttr.Free;
  FRowHeadAttr.Free;
  FAllDayEventAttr.Free;
  dvClickTimer.Free;
  FDefaultPopup.Free;
  FIconAttributes.Free;

  dvDayUpBtn.Free;
  dvDayDownBtn.Free;
  dvTodayBtn.Free;
  dvWeekUpBtn.Free;
  dvWeekDownBtn.Free;

  inherited;
end;

procedure TVpDayView.LoadLanguage;
begin
  dvDayUpBtn.Hint := RSHintNextDay; //rsHintTomorrow;
  dvDayDownBtn.Hint := RSHintPrevDay; //rsHintYesterday;
  dvTodayBtn.Hint := RSHintToday;
  dvWeekUpBtn.Hint := RSHintNextWeek;
  dvWeekDownBtn.Hint := RSHintPrevWeek;
  FDefaultPopup.Items.Clear;
  InitializeDefaultPopup;
end;

{=====}

procedure TVpDayView.DeleteActiveEvent(Verify: Boolean);
var
  DoIt: Boolean;
begin
  if ReadOnly then
    Exit;

  dvClickTimer.Enabled := false;
  EndEdit(self);

  DoIt := not Verify;

  if FActiveEvent <> nil then begin
    if Verify then
      DoIt := (MessageDlg(RSConfirmDeleteEvent + #13#10#10 + RSPermanent,
        mtConfirmation, [mbYes, mbNo], 0) = mrYes);

    if DoIt then begin
      FActiveEvent.Deleted := true;
      DataStore.PostEvents;
      Invalidate;
    end;
  end;
end;
{=====}

procedure TVpDayView.Invalidate;
begin
  inherited;
end;
{=====}

procedure TVpDayView.LinkHandler(Sender: TComponent;
  NotificationType: TVpNotificationType; const Value: Variant);
begin
  dvInLinkHandler := true;
  try
    case NotificationType of
      neDateChange      : Date := Value;
      neDataStoreChange : Invalidate;
      neInvalidate      : Invalidate;
    end;
  finally
    dvInLinkHandler := false;
  end; 
end;
{=====}

procedure TVpDayView.dvHookUp;
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
{=====}

procedure TVpDayView.InitializeDefaultPopup;
var
  NewItem: TMenuItem;
  NewSubItem: TMenuItem;
begin
  if RSDayPopupAdd <> '' then begin
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := RSDayPopupAdd;
    NewItem.OnClick := PopupAddEvent;
    NewItem.Tag := 0;
    FDefaultPopup.Items.Add(NewItem);
  end;

  if RSDayPopupEdit <> '' then begin
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := RSDayPopupEdit;
    NewItem.OnClick := PopupEditEvent;
    NewItem.Tag := 1;
    FDefaultPopup.Items.Add(NewItem);
  end;

  if RSDayPopupDelete <> '' then begin
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := RSDayPopupDelete;
    NewItem.OnClick := PopupDeleteEvent;
    NewItem.Tag := 1;
    FDefaultPopup.Items.Add(NewItem);
  end;

  if RSDayPopupNav <> '' then begin
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := RSDayPopupNav;
    NewItem.Tag := 0;
    FDefaultPopup.Items.Add(NewItem);

    if RSDayPopupNavToday <> '' then begin
      NewSubItem := TMenuItem.Create(Self);
      NewSubItem.Caption := RSDayPopupNavToday;
      NewSubItem.OnClick := PopupToday;
      NewSubItem.Tag := 0;
      NewItem.Add(NewSubItem);
    end;

    if RSDayPopupNavYesterday <> '' then begin
      NewSubItem := TMenuItem.Create(Self);
      NewSubItem.Caption := RSDayPopupNavYesterday;
      NewSubItem.OnClick := PopupYesterday;
      NewSubItem.Tag := 0;
      NewItem.Add(NewSubItem);
    end;

    if RSDayPopupNavTomorrow <> '' then begin
      NewSubItem := TMenuItem.Create(Self);
      NewSubItem.Caption := RSDayPopupNavTomorrow;
      NewSubItem.OnClick := PopupTomorrow;
      NewSubItem.Tag := 0;
      NewItem.Add(NewSubItem);
    end;

    if RSDayPopupNavNextDay <> '' then begin
      NewSubItem := TMenuItem.Create(Self);
      NewSubItem.Caption := RSDayPopupNavNextDay;
      NewSubItem.OnClick := PopupNextDay;
      NewSubItem.Tag := 0;
      NewItem.Add(NewSubItem);
    end;

    if RSDayPopupNavPrevDay <> '' then begin
      NewSubItem := TMenuItem.Create(Self);
      NewSubItem.Caption := RSDayPopupNavPrevDay;
      NewSubItem.OnClick := PopupPrevDay;
      NewSubItem.Tag := 0;
      NewItem.Add(NewSubItem);
    end;

    if RSDayPopupNavNextWeek <> '' then begin
      NewSubItem := TMenuItem.Create(Self);
      NewSubItem.Caption := RSDayPopupNavNextWeek;
      NewSubItem.OnClick := PopupNextWeek;
      NewSubItem.Tag := 0;
      NewItem.Add(NewSubItem);
    end;

    if RSDayPopupNavPrevWeek <> '' then begin
      NewSubItem := TMenuItem.Create(Self);
      NewSubItem.Caption := RSDayPopupNavPrevWeek;
      NewSubItem.OnClick := PopupPrevWeek;
      NewSubItem.Tag := 0;
      NewItem.Add(NewSubItem);
    end;

    if RSDayPopupNavNextMonth <> '' then begin
      NewSubItem := TMenuItem.Create(Self);
      NewSubItem.Caption := RSDayPopupNavNextMonth;
      NewSubItem.OnClick := PopupNextMonth;
      NewSubItem.Tag := 0;
      NewItem.Add(NewSubItem);
    end;

    if RSDayPopupNavPrevMonth <> '' then begin
      NewSubItem := TMenuItem.Create(Self);
      NewSubItem.Caption := RSDayPopupNavPrevMonth;
      NewSubItem.OnClick := PopupPrevMonth;
      NewSubItem.Tag := 0;
      NewItem.Add(NewSubItem);
    end;

    if RSDayPopupNavNextYear <> '' then begin
      NewSubItem := TMenuItem.Create(Self);
      NewSubItem.Caption := RSDayPopupNavNextYear;
      NewSubItem.OnClick := PopupNextYear;
      NewSubItem.Tag := 0;
      NewItem.Add(NewSubItem);
    end;

    if RSDayPopupNavPrevYear <> '' then begin
      NewSubItem := TMenuItem.Create(Self);
      NewSubItem.Caption := RSDayPopupNavPrevYear;
      NewSubItem.OnClick := PopupPrevYear;
      NewSubItem.Tag := 0;
      NewItem.Add(NewSubItem);
    end;
  end;
end;
{=====}

procedure TVpDayView.PopupAddEvent(Sender: TObject);
var
  StartTime: TDateTime;
  EndTime: TDateTime;
begin
  if ReadOnly then
    Exit;
  if not CheckCreateResource then
    Exit;
  if not Assigned (DataStore) then
    Exit;
  if not Assigned (DataStore.Resource) then
    Exit;

  StartTime := trunc(FDisplayDate + ActiveCol) + dvLineMatrix[ActiveCol, ActiveRow].Time;
  EndTime := StartTime + dvTimeIncSize;
  FActiveEvent := DataStore.Resource.Schedule.AddEvent(
    DataStore.GetNextID(EventsTableName),
    StartTime,
    EndTime
  );

  Repaint;
  { edit this new event }
  dvSpawnEventEditDialog(True);
end;
{=====}

procedure TVpDayView.PopupDeleteEvent(Sender: TObject);
begin
  if ReadOnly then
    Exit;
  Repaint;
  if FActiveEvent <> nil then
    DeleteActiveEvent (True);
end;
{=====}

procedure TVpDayView.PopupEditEvent(Sender: TObject);
begin
  if ReadOnly then
    Exit;
  Repaint;
  if FActiveEvent <> nil then
    { edit this Event }
    dvSpawnEventEditDialog(False);
end;
{=====}

procedure TVpDayView.PopupToday(Sender: TObject);
begin
  Date := Now;
end;
{=====}

procedure TVpDayView.PopupTomorrow(Sender: TObject);
begin
  Date := Now + 1;
end;
{=====}

procedure TVpDayView.PopupYesterday(Sender: TObject);
begin
  Date := Now - 1;
end;
{=====}

procedure TVpDayView.PopupNextDay(Sender: TObject);
begin
  Date := Date + 1;
end;
{=====}

procedure TVpDayView.PopupPrevDay(Sender: TObject);
begin
  Date := Date - 1;
end;
{=====}

procedure TVpDayView.PopupNextWeek(Sender: TObject);
begin
  Date := Date + 7;
end;
{=====}

procedure TVpDayView.PopupPrevWeek(Sender: TObject);
begin
  Date := Date - 7;
end;
{=====}

procedure TVpDayView.PopupNextMonth(Sender: TObject);
var
  M, D, Y: Word;
begin
  DecodeDate(Date, Y, M, D);
  if M = 12 then begin
    M := 1;
    Y := Y + 1;
  end else
    M := M + 1;
  if (D > DaysInMonth(Y, M)) then
    D := DaysInMonth(Y, M);

  Date := EncodeDate(Y, M, D);
end;
{=====}

procedure TVpDayView.PopupPrevMonth(Sender: TObject);
var
  M, D, Y: Word;
begin
  DecodeDate(Date, Y, M, D);
  if M = 1 then begin
    M := 12;
    Y := Y - 1;
  end else
    M := M - 1;
  if (D > DaysInMonth(Y, M)) then
    D := DaysInMonth(Y, M);

  Date := EncodeDate(Y, M, D);
end;
{=====}

procedure TVpDayView.PopupNextYear(Sender: TObject);
var
  M, D, Y : Word;
begin
  DecodeDate(Date, Y, M, D);
  Date := EncodeDate(Y + 1, M, 1);
end;
{=====}

procedure TVpDayView.PopupPrevYear(Sender: TObject);
var
  M, D, Y: Word;
begin
  DecodeDate(Date, Y, M, D);
  Date := EncodeDate(Y - 1, M, 1);
end;
{=====}

procedure TVpDayView.Loaded;
begin
  inherited;
  TopHour := DefaultTopHour;
  dvLoaded := true;
  dvPopulate;
end;
{=====}

procedure TVpDayView.Paint;
begin
  RenderToCanvas(Canvas, Rect(0, 0, Width, Height), ra0, 1, FDisplayDate,
    TopLine, -1, FGranularity, False);
  SetVScrollPos; 
end;
{=====}

procedure TVpDayView.dvPopulate;
begin
  if DataStore <> nil then
    DataStore.Date := FDisplayDate; 
end;
{=====}

procedure TVpDayView.dvNavButtonsClick(Sender: TObject);
begin
  { set the value of Date based on which button was pressed. }
  if Sender = dvDayUpBtn then
    Date := Date + 1
  else if Sender = dvDayDownBtn then
    Date := Date - 1
  else if Sender = dvTodayBtn then
    Date := trunc(Now)
  else if Sender = dvWeekUpBtn then
    Date := Date + 7
  else if Sender = dvWeekDownBtn then
    Date := Date - 7;
end;
{=====}

function TVpDayView.dvCalcVisibleLines(RenderHeight, ColHeadHeight, RowHeight: Integer;
  Scale: Extended; StartLine, StopLine: Integer): Integer;
var
  vertical: integer;
  d: Integer = 0;        // d = result of "div"
  m: Integer = 0;        // m = result of "mod"
begin
  if StartLine < 0 then
    StartLine := TopLine;

  { take into account the number lines that are allowed! }
//  vertical := Round(RenderHeight - ColHeadHeight * Scale - 2);
  vertical := Round(RenderHeight - ColHeadHeight * Scale);
  DivMod(Vertical, RowHeight, d, m);
  Result := d + ord(m <> 0);
  {
  if Vertical mod RowHeight = 0 then
    Result :=
  Result := Vertical div RowHeight + 1; // - 4; //+2;
  }
  if Result > FLineCount then
    Result := FLineCount;

  if (StopLine > 0) and (StopLine > StartLine) and (Result > Stopline-StartLine) then
    Result := StopLine - StartLine + 1;
   {
  if (StopLine > 0) and (StopLine > StartLine) then
    if Result > StopLine - StartLine then
      Result := StopLine - StartLine + 2;
  }
  FVisibleLines := Result;
end;
{=====}

procedure TVpDayView.SetColor(Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    Invalidate;
  end;
end;
{=====}

function TVpDayView.dvCalcColHeadHeight(Scale: Extended): Integer;
var
  TextHeight: Integer;
begin
  Canvas.Font.Assign(FHeadAttr.Font);
  Canvas.Font.Size := ScaleY(Canvas.Font.Size, DesignTimeDPI);

  if FShowResourceName and (DataStore <> nil) and (DataStore.Resource <> nil) then
    TextHeight := Canvas.TextHeight(TallShortChars) * 2 + TextMargin * 3
  else
    TextHeight := Canvas.TextHeight(TallShortChars) + TextMargin * 2;
  Result := Round(TextHeight * Scale);
  dvColHeadHeight := Result; 
end;
{=====}

procedure TVpDayView.DoStartDrag(var DragObject: TDragObject);
{$IFDEF LCL}
var
  P, HotSpot: TPoint;
  EventName: string;
{$ENDIF}
begin
  DvDragStartTime := 0.0;
  if ReadOnly or not FAllowDragAndDrop then
    Exit;
  if FActiveEvent <> nil then begin
    // Set the time from which this event was dragged

    DvDragStartTime := trunc(Date + ActiveCol) + dvLineMatrix[ActiveCol, ActiveRow].Time;

   {$IFDEF LCL}
    EventName := FActiveEvent.Description;
    GetCursorPos(P);
    P := TVpDayView(Self).ScreenToClient(P);
    HotSpot := Point(P.X - Self.dvActiveEventRec.Left, P.Y - Self.dvActiveEventRec.Top);

    DragObject := TVpEventDragObject.CreateWithDragImages(Self as TControl,
      HotSpot, Self.dvActiveEventRec, EventName, FDragDropTransparent);
   {$ELSE}
    DragObject := DragObject := TVpEventDragObject.Create(Self);
   {$ENDIF}

    TVpEventDragObject(DragObject).Event := FActiveEvent;
  end
  else
   {$IFDEF LCL}
    CancelDrag;
   {$ELSE}
    DragObject.Free;//EndDrag(false);
   {$ENDIF}
end;
{=====}

procedure TVpDayView.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  if ReadOnly or (not FAllowDragAndDrop) then
    Exit;
 {$IFNDEF LCL}
  TVpEventDragObject(Target).Free;
 {$ENDIF}
 // not needed for LCL: we use DragObjectEx !!
end;
{=====}

procedure TVpDayView.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  if ReadOnly or (not FAllowDragAndDrop) then begin
    Accept := False;
    Exit;
  end;
  if (X > dvRowHeadWidth + GutterWidth) and (Y > dvColHeadHeight) then begin
    { The mouse is dragging over the client area }
    dvSetActiveColByCoord(Point(X, Y));
    dvSetActiveRowByCoord(Point(X, Y), False);                           
    Accept := true;
  end else
    Accept := false;
end;
{=====}

procedure TVpDayView.DragDrop(Source: TObject; X, Y: Integer);
var
  Event: TVpEvent;
  Duration: TDateTime;
  DragToTime: TDateTime;
  i: Integer;
begin
  if ReadOnly or (not FAllowDragAndDrop) then
    Exit;

  Event := TVpEventDragObject(Source).Event;
  if Event <> nil then begin
    Duration := Event.EndTime - Event.StartTime;
    DragToTime := trunc(Date + ActiveCol)
      + dvLineMatrix[ActiveCol, ActiveRow].Time;

    if Ord(Event.RepeatCode) = 0 then
      { if this is not a recurring event then just drop it here }
      Event.StartTime := DragToTime
    else
      { if this is a recurring event, then modify the event's start time
        according to how far the event was dragged }
      Event.StartTime := Event.StartTime + (DragToTime - DvDragStartTime);
    Event.EndTime := Event.StartTime + Duration;
    DataStore.PostEvents;

    { Force a repaint. This will update the rectangles for the event }
    Repaint;

    { Reset the active event rectangle }
    for I := 0 to pred(Length(dvEventArray)) do begin
      if dvEventArray[I].Event = nil then
        Break;

      if dvEventArray[i].Event = Event then begin
        dvActiveEventRec := dvEventArray[I].Rec;
        dvActiveIconRec  := dvEventArray[I].IconRect;
        Break;
      end;
    end;

    { Invalidate; }
  end;
//  TVpEventDragObject(Source).EndDrag(False);
end;
{=====}

function TVpDayView.dvCalcRowHeight(Scale: Extended;
  UseGran: TVpGranularity): Integer;
var
  SaveFont: TFont;
  Temp: Integer;
begin
  { Calculates row height based on the largest of the RowHead's Minute }
  { font, the standard client font, and a sample character string.     }
  SaveFont := Canvas.Font;
  Canvas.Font.Assign(FRowHeadAttr.FMinuteFont);
  Canvas.Font.Size := ScaleY(Canvas.Font.Size, DesignTimeDPI);
  Canvas.Font.Height := GetRealFontHeight(Canvas.Font);
  Result := Canvas.TextHeight(TallShortChars);
  Canvas.Font.Assign(SaveFont);
  Temp := Canvas.TextHeight(TallShortChars);
  if Temp > Result then
    Result := Temp;
  Result := Result + TextMargin * 2;

  Result := Round(Result * Scale);
  dvClientVArea := Result * MinutesInDay div GranularityMinutes[UseGran];
  dvRowHeight := Result;
end;
{=====}

function TVpDayView.GetLastVisibleDate: TDateTime;
begin
  Result := Date + GetRealNumDays(Date);
end;
{=====}

function TVpDayView.GetRealNumDays(WorkDate: TDateTime) : Integer;
var
  i: Integer;
begin
  if not FIncludeWeekends then begin
    Result := 0;
    i := 0;
    while i < FNumDays do begin
      if (DayOfWeek(WorkDate) <> 1) and (DayOfWeek(WorkDate) <> 7) then
        Inc(i);
      WorkDate := WorkDate + 1;
      Inc (Result);
    end;
  end else
    Result := FNumDays;
end;
{=====}

procedure TVpDayView.SetDrawingStyle(Value: TVpDrawingStyle);
begin
  if FDrawingStyle <> Value then begin
    FDrawingStyle := Value;
    Repaint;
  end;
end;
{=====}

procedure TVpDayView.SetLineColor(Value: TColor);
begin
  if FLineColor <> Value then begin
    FLineColor := Value;
    Repaint;
  end;
end;
{=====}

procedure TVpDayView.SetTopHour(Value: TVpHours);
begin
  if FTopHour <> Value then begin
    FTopHour := Value;
    TopLine := HourToLine(FTopHour, FGranularity);
  end;
end;
{=====}

procedure TVpDayView.SetTopLine(Value: Integer);
begin
  if Value <> FTopLine then begin
    if Value + VisibleLines >= pred(LineCount) then begin
//      FTopLine := pred(LineCount) - VisibleLines + 2;    // why +2?
      FTopLine := pred(LineCount) - VisibleLines;
      if FTopLine < 0 then FTopLine := 0;
      { prevent the control from hanging at the bottom }
      if (Value < FTopLine) and (Value > 0) then
        FTopLine := Value;
    end
    else if Value < 0 then
      FTopLine := 0
    else
      FTopLine := Value;
    Invalidate;
    SetVScrollPos;
  end;
end;
{=====}

procedure TVpDayView.SetDateLabelFormat(Value: string);
begin
  if Value <> FDateLabelFormat then begin
    FDateLabelFormat := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpDayView.SetGutterWidth(Value: Integer);
begin
  if (Value <> FGutterWidth) and (Value > -1) and (Value < Width div 10) then
  begin
    FGutterWidth := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpDayView.SetDefTopHour(Value: TVpHours);
begin
  if Value <> FDefTopHour then begin
    FDefTopHour := Value;
    if csDesigning in ComponentState then
    TopHour := Value;
  end;
end;
{=====}

procedure TVpDayView.SetTimeIntervals(UseGran: TVpGranularity);
var
  I, J: Integer;
  grPerHour: Integer;
begin
  FLineCount := MinutesInDay div GranularityMinutes[UseGran];
  dvTimeIncSize := GranularityMinutes[UseGran] / MinutesInDay;
  grPerHour := 60 div GranularityMinutes[UseGran];

  SetLength(dvLineMatrix, NumDays);
  for I := 0 to pred(NumDays) do begin
    SetLength(dvLineMatrix[I], LineCount + 1);    // was +1. Why? Without it, the IDE crashes! - there is an upper loop index of LineCount in DrawCells. After correcting that, the crash is gone.
      // wp: the additional line is needed to fully display the last line of the day.
    for J := 0 to pred(LineCount) do begin
      dvLineMatrix[I,J].Hour := TVpHours(J div grPerHour);
      dvLineMatrix[I,J].Minute := (J mod grPerHour) * GranularityMinutes[UseGran];
      dvLineMatrix[I,J].Time := ord(dvLineMatrix[I,J].Hour) / 24 + dvTimeIncSize * (J mod grPerHour);
    end;
  end;

  if FLineCount <= FVisibleLines then
    FTopLine := HourToLine(h_00, FGranularity);

  SetVScrollPos;
end;

procedure TVpDayView.SetGranularity(Value: TVpGranularity);
begin
  FGranularity := Value;
  SetTimeIntervals(FGranularity);
  FTopLine := HourToLine(FTopHour, FGranularity);
  if dvRowHeight <> 0 then
    dvCalcVisibleLines(Height, dvColHeadHeight, dvRowHeight, 1, FTopLine, -1);
  if (FGranularity = gr60Min) and (FVisibleLines = LineCount) then
    FTopLine := 0;
  Invalidate;
end;
{=====}

procedure TVpDayView.SetTimeFormat(Value: TVpTimeFormat);
begin
  if Value <> FTimeFormat then begin
    FTimeFormat := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpDayView.SetDisplayDate(Value: TDateTime);
begin
  if FDisplayDate <> Value then begin
    EndEdit(self);
    FDisplayDate := Value;
    if dvLoaded then
      dvPopulate;
    Invalidate;

    if (not dvInLinkHandler) and (ControlLink <> nil) then
      ControlLink.Notify(self, neDateChange, Date);
  end;
end;
{=====}

{$IFNDEF LCL}
procedure TVpDayView.WMSize(var Msg: TWMSize);
{$ELSE}
procedure TVpDayView.WMSize(var Msg: TLMSize);
{$ENDIF}
var
  MaxLinesToDraw: Integer;
  EmptyLines: Integer;
begin
  inherited;

  { How many lines are there between TopLine and the last line of the day. }
  MaxLinesToDraw := Length(dvLineMatrix[0]) - TopLine;
  EmptyLines := FVisibleLines - MaxLinesToDraw;

  if EmptyLines > 0 then
    TopLine := TopLine - EmptyLines
  else
    Invalidate; 
end;
{=====}

procedure TVpDayView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  with Params do
  begin
    Style := Style or WS_TABSTOP;
    if FScrollBars in [ssVertical, ssBoth] then Style := Style or WS_VSCROLL;
    if FScrollBars in [ssHorizontal, ssBoth] then Style := Style or WS_HSCROLL;
{$IFNDEF LCL}
    WindowClass.style := CS_DBLCLKS;
{$ENDIF}
  end; 
end;
{=====}

procedure TVpDayView.CreateWnd;
begin
  inherited;
  PostMessage (Handle, Vp_DayViewInit, 0, 0);
end;


procedure TVpDayView.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
    begin
      dvMouseDownPoint := Point(0, 0);
      dvMouseDown := false;
      dvDragging := false;
    end
  else
    begin
    end;
end;

procedure TVpDayView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if (FActiveEvent <> nil) and (not ReadOnly) then begin
    if (not dvDragging) and dvMouseDown and
       ((dvMouseDownPoint.x <> x) or (dvMouseDownPoint.y <> y))
    then begin
      dvDragging := true;
      dvClickTimer.Enabled := false;
      BeginDrag(true);
    end;
  end;
end;

procedure TVpDayView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  i: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    dvMouseDownPoint := Point(x, y);
    dvMouseDown := true;

    { if the mouse was pressed down in the client area, then select the cell. }
    if not focused then SetFocus;

    if (x > dvRowHeadWidth - 9) and (y > dvColHeadHeight) then
    begin
      { The mouse click landed inside the client area }
      dvSetActiveColByCoord(Point(x, y));
      dvSetActiveRowByCoord(Point(x, y), True);
      if not ReadOnly then
        EditEventAtCoord(Point(x, y));
    end else
    if y > dvColHeadHeight then
      dvSetActiveRowByCoord(Point (x, y), True);

    if Assigned(OnClick) then
      OnClick(self);
  end
  else begin
    if not Focused then
      SetFocus;

    if (x > dvRowHeadWidth - 9) and (y > dvColHeadHeight) then
    begin
      { The mouse click landed inside the client area }
      dvSetActiveColByCoord(Point(x, y));
      dvSetActiveRowByCoord(Point(x, y), True);
    end;

    EditEventAtCoord(Point (x, y));
    dvClickTimer.Enabled := false;

    if not Assigned(FActiveEvent) then
      for i := 0 to FDefaultPopup.Items.Count - 1 do begin
        if (FDefaultPopup.Items[i].Tag = 1) or (ReadOnly) then
          FDefaultPopup.Items[i].Enabled := False;
      end
    else
      for i := 0 to FDefaultPopup.Items.Count - 1 do
        FDefaultPopup.Items[i].Enabled := True;
  end;
end;

{=====}

{$IFNDEF LCL}
procedure TVpDayView.WMLButtonDblClk(var Msg: TWMLButtonDblClk);
{$ELSE}
procedure TVpDayView.WMLButtonDblClk(var Msg: TLMLButtonDblClk);
{$ENDIF}
var
  StartTime, EndTime: TDateTime;
begin
  inherited;
  dvClickTimer.Enabled := false;
  dvMouseDownPoint := Point(0, 0);
  dvMouseDown := false;
  dvDragging := false;

  { if the mouse was pressed down in the client area, then select the cell. }
  if not focused then SetFocus;

  if (Msg.XPos > dvRowHeadWidth - 9) and (Msg.YPos > dvColHeadHeight) then
  begin
    { The mouse click landed inside the client area }
    dvSetActiveRowByCoord(Point(Msg.XPos, Msg.YPos), True);
    { See if we hit an active event }
    if (FActiveEvent <> nil) and (not ReadOnly) then begin
      { edit this event }
      dvSpawnEventEditDialog(False);
    end else if not ReadOnly then begin
      if not CheckCreateResource then
        Exit;
      if (DataStore = nil) or (DataStore.Resource = nil) then
        Exit;
      { otherwise, we must want to create a new event }
      StartTime := trunc(FDisplayDate + ActiveCol)
        + dvLineMatrix[ActiveCol, ActiveRow].Time;
      EndTime := StartTime + dvTimeIncSize;
      FActiveEvent := DataStore.Resource.Schedule.AddEvent(
        DataStore.GetNextID(EventsTableName), StartTime, EndTime);
      { edit this new event }
      dvSpawnEventEditDialog(True);
    end;
  end;
end;
{=====}

{$IFDEF LCL}
function TVpDayView.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TVpDayView.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
var
  delta: Integer;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if not Result then begin
    if [ssCtrl, ssShift] * Shift <> [] then begin
      delta := HourToLine(h_01, FGranularity);
      if delta = 1 then delta := 3;
    end else
      delta := 1;
    dvScrollVertical(delta);
    Result := True;
  end;
end;

function TVpDayView.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
var
  delta: Integer;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if not Result then begin
    if [ssCtrl, ssShift] * Shift <> [] then begin
      delta := HourToLine(h_01, FGranularity);
      if delta = 1 then delta := 3;
    end else
      delta := 1;
    dvScrollVertical(-delta);
    Result := True;
  end;
end;
{$ENDIF}

procedure TVpDayView.EditSelectedEvent;
begin
  if ReadOnly then
    Exit;
  if FActiveEvent <> nil then
    dvSpawnEventEditDialog(false);
end;
{=====}

procedure TVpDayView.dvSpawnEventEditDialog(NewEvent: Boolean);
var
  AllowIt: Boolean;
  EventDlg : TVpEventEditDialog;
begin
  if (DataStore = nil) or (DataStore.Resource = nil) or ReadOnly then
    Exit;

  AllowIt := false;
  if Assigned(FOwnerEditEvent) then
    FOwnerEditEvent(self, FActiveEvent, DataStore.Resource, AllowIt)
  else begin
    EventDlg := TVpEventEditDialog.Create(nil);
    try
      EventDlg.DataStore := DataStore;
      AllowIt := EventDlg.Execute(FActiveEvent);
    finally
      EventDlg.Free;
    end;
  end;

  if AllowIt then begin
    FActiveEvent.Changed := true;
    DataStore.PostEvents;
    if Assigned(FOnAddEvent) then
      FOnAddEvent(self, FActiveEvent);
    Invalidate;
  end else begin
    if NewEvent then begin
      FActiveEvent.Deleted := true;
      DataStore.PostEvents;
      FActiveEvent := nil;
      dvActiveEventRec := Rect(0, 0, 0, 0);
      dvActiveIconRec := Rect(0, 0, 0, 0);
    end else
      DataStore.PostEvents;
    Invalidate;
  end;
end;
{=====}

{$IFNDEF LCL}
procedure TVpDayView.WMSetFocus(var Msg: TWMSetFocus);
{$ELSE}
procedure TVpDayView.WMSetFocus(var Msg: TLMSetFocus);
{$ENDIF}
begin
  Unused(Msg);
  if ActiveRow = -1 then ActiveRow := TopLine;
end;
{=====}

{$IFNDEF LCL}
procedure TVpDayView.WMEraseBackground(var Msg: TWMERASEBKGND);
{$ELSE}
procedure TVpDayView.WMEraseBackground(var Msg: TLMERASEBKGND);
{$ENDIF}
begin
  Msg.Result := 1;
end;
{=====}

{$IFNDEF LCL}
procedure TVpDayView.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  Msg.Result := 1;
end;
{$ENDIF}
{=====}

procedure TVpDayView.SetActiveEventByCoord(APoint: TPoint);
var
  I : Integer;
begin
  for I := 0 to pred(Length(dvEventArray)) do begin
    if dvEventArray[I].Event = nil then
      Exit;
    if PointInRect(APoint, dvEventArray[I].Rec) then
    begin
      FActiveEvent := TVpEvent(dvEventArray[I].Event);
      dvActiveEventRec := dvEventArray[I].Rec;
      dvActiveIconRec := dvEventArray[I].IconRect;
      Exit;
    end;
  end;
end;

function TVpDayView.EditEventAtCoord(Point: TPoint): Boolean;
var
  I: Integer;
begin
  result := false;
  if ReadOnly then
    Exit;
  for I := 0 to pred(Length(dvEventArray)) do begin
    FActiveEvent := nil;                               // wp: shouldn't these be set also if ReadOnly is true?
    dvActiveEventRec := Rect(0, 0, 0, 0);
    dvActiveIconRec := Rect(0, 0, 0, 0);
    if dvEventArray[I].Event = nil then
      { we've hit the end of visible events without finding a match }
      Exit;
    if PointInRect(Point, dvEventArray[I].Rec) then
    begin
      FActiveEvent := TVpEvent(dvEventArray[I].Event);
      dvActiveEventRec := dvEventArray[I].Rec;
      dvActiveIconRec := dvEventArray[I].IconRect;
      dvClickTimer.Enabled := true;
      result := true;
      Break;
    end;
  end;
end;
{=====}

function TVpDayView.GetEventAtCoord(Point: TPoint): TVpEvent;
var
  I: Integer;
begin
  result := nil;
  for I := 0 to pred(Length(dvEventArray)) do begin
    if dvEventArray[I].Event = nil then
      Exit;
    if PointInRect(Point, dvEventArray[I].Rec) then
    begin
      result := TVpEvent(dvEventArray[I].Event);
      Exit;
    end;
  end;
end;
{=====}

procedure TVpDayView.dvEditInPlace(Sender: TObject);
begin
  { this is the timer event which spawns an in-place editor }
  { if the event is doublecliked before this timer fires, then the }
  { event is edited in a dialog based editor. }
  dvClickTimer.Enabled := false;
  EditEvent;
end;
{=====}

procedure TVpDayView.EditEvent;
var
  AllowIt: Boolean;
begin
  if ReadOnly then
    Exit;
  if not FAllowInplaceEdit then
    Exit;

  { call the user defined BeforeEdit event }
  AllowIt := true;
  if Assigned(FBeforeEdit) then
    FBeforeEdit(Self, FActiveEvent, AllowIt);
  if not AllowIt then
    exit;

  { create and spawn the in-place editor }
  if dvInPlaceEditor = nil then begin
    dvInPlaceEditor := TVpDvInPlaceEdit.Create(Self);
    dvInPlaceEditor.Parent := self;
    dvInPlaceEditor.OnExit := EndEdit;
  end;
  if FActiveEvent.AllDayEvent then
    dvInplaceEditor.SetBounds(
      dvActiveEventRec.Left + 2 * (TextMargin div 2),  // this way it is calculated in DrawAllDayEvents
      dvActiveEventRec.Top + 2 * (TextMargin div 2),
      WidthOf(dvActiveEventRec) - TextMargin div 2,
      HeightOf(dvActiveEventRec)
    )
  else
    dvInPlaceEditor.SetBounds(
      dvActiveIconRec.Right + TextMargin,
      dvActiveEventRec.Top + TextMargin,
      dvActiveEventRec.Right - dvActiveIconRec.Right - TextMargin,
      dvActiveEventRec.Bottom - dvActiveEventRec.Top - TextMargin
    );
  dvInPlaceEditor.Show;
  dvInPlaceEditor.Text := FActiveEvent.Description;
  Invalidate;
  dvInPlaceEditor.SetFocus;
end;
{=====}

procedure TVpDayView.EndEdit(Sender: TObject);
begin
  if dvEndingEditing then
    Exit;
  dvEndingEditing := True;
  try
    if (dvInPlaceEditor <> nil) and dvInplaceEditor.Visible then begin
      if dvInPlaceEditor.Text <> FActiveEvent.Description then begin
        FActiveEvent.Description := dvInPlaceEditor.Text;
        FActiveEvent.Changed := true;
        DataStore.PostEvents;
        if Assigned(FAfterEdit) then
          FAfterEdit(self, FActiveEvent);
      end;
      dvInplaceEditor.Hide;
      Invalidate;
    end;
  finally
    dvEndingEditing := False;
  end;
end;
{=====}

procedure TVpDayView.KeyDown(var Key: Word; Shift: TShiftState);
var
  PopupPoint : TPoint;
begin
  case Key of
    VK_UP:
      ActiveRow := ActiveRow - 1;
    VK_DOWN:
      ActiveRow := ActiveRow + 1;
    VK_NEXT:
      ActiveRow := ActiveRow + FVisibleLines;
    VK_PRIOR:
      ActiveRow := ActiveRow - FVisibleLines;
    VK_LEFT:
      Date := Date - 1;
    VK_RIGHT:
      Date := Date + 1;
    VK_HOME:
      ActiveRow := 0;
    VK_END:
      ActiveRow := LineCount;
    VK_DELETE:
      if not ReadOnly then
        DeleteActiveEvent(true);
{$IFNDEF LCL}
    VK_TAB:
      if ssShift in Shift then
        Windows.SetFocus (GetNextDlgTabItem(GetParent(Handle), Handle, False))
      else
        Windows.SetFocus (GetNextDlgTabItem(GetParent(Handle), Handle, True));
{$ENDIF}
    VK_F10:
      if (ssShift in Shift) and not (Assigned (PopupMenu)) then begin
        PopupPoint := GetClientOrigin;
        FDefaultPopup.Popup(PopupPoint.x + 10, PopupPoint.y + 10);
      end;
    VK_APPS :
      if not Assigned(PopupMenu) then begin
        PopupPoint := GetClientOrigin;
        FDefaultPopup.Popup(PopupPoint.x + 10, PopupPoint.y + 10);
      end;
    VK_RETURN:
      PopupEditEvent(Self);
    VK_INSERT:
      PopupAddEvent(Self);
    VK_F2:
      if Assigned(FActiveEvent) then
        dvEditInPlace(Self)
      else
      begin
        PopupPoint := dvLineMatrix[ActiveCol, ActiveRow].Rec.TopLeft;
        PopupPoint.x := PopupPoint.x + 1;
        PopupPoint.y := PopupPoint.y + 1;
        SetActiveEventByCoord (PopupPoint);
        if Assigned(FActiveEvent) then
          dvEditInPlace(Self);
      end;
  end;
end;
{=====}

{$IFNDEF LCL}
procedure TVpDayView.WMVScroll(var Msg: TWMVScroll);
{$ELSE}
procedure TVpDayView.WMVScroll(var Msg: TLMVScroll);
{$ENDIF}
begin
  { for simplicity, bail out of editing while scrolling. }
  EndEdit(Self);

  // wp: Next line should never happen after EndEdit...
  if (dvInPlaceEditor <> nil) and dvInplaceEditor.Visible then Exit;

  case Msg.ScrollCode of
    SB_LINEUP    : dvScrollVertical(-1);
    SB_LINEDOWN  : dvScrollVertical(1);
    SB_PAGEUP    : dvScrollVertical(-FVisibleLines);
    SB_PAGEDOWN  : dvScrollVertical(FVisibleLines);
    SB_THUMBPOSITION, SB_THUMBTRACK : TopLine := Msg.Pos;
  end;
end;
{=====}

procedure TVpDayView.dvScrollVertical(Lines: Integer);
begin
  TopLine := TopLine + Lines;
end;
{=====}

procedure TVpDayView.SetVScrollPos;
var
  SI : TScrollInfo;
begin
  if not HandleAllocated then
    Exit;
  with SI do begin
    cbSize := SizeOf(SI);
    fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
    nMin := 0;
    nMax := FLineCount;
    if FVisibleLines >= FLineCount then
      nPage := nMax
    else
      nPage := FVisibleLines;
    if FTopLine = pred(LineCount) - VisibleLines then
      nPos := LineCount
    else
      nPos := FTopLine;
    nTrackPos := nPos;
  end;
  SetScrollInfo(Handle, SB_VERT, SI, True);
end;
{=====}

procedure TVpDayView.SetShowResourceName(Value: Boolean);
begin
  if Value <> FShowResourceName then begin
    FShowResourceName := Value;
    Invalidate;
  end;
end;

procedure TVpDayView.SetNumDays(Value: Integer);
begin
  if (Value <> FNumDays) and (Value > 0) and (Value < 31) then begin
    FNumDays := Value;
    SetLength(dvColRectArray, FNumDays);
    SetTimeIntervals(Granularity);
    ActiveCol := 0;
    Invalidate;
  end;
end;

procedure TVpDayView.SetIncludeWeekends(Value : Boolean);
begin
  if Value <> FIncludeWeekends then begin
    FIncludeWeekends := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpDayView.SetActiveRow(Value: Integer);
var
  OldActiveRow: Integer;
begin
  if dvClickTimer.Enabled then
    dvClickTimer.Enabled := false;

  if not Focused then SetFocus;
  OldActiveRow := FActiveRow;
  { set active row }
  if (Value < 0) then
    FActiveRow := 0
  else if (Value >= pred(LineCount)) then
    FActiveRow := pred(LineCount)
  else
    FActiveRow := Value;

  { clamp in view }
  if (FActiveRow < FTopLine) then
    TopLine := FActiveRow
  else if (FActiveRow >= FTopLine + FVisibleLines) then
    TopLine := FActiveRow - FVisibleLines + 1;

  if (OldActiveRow <> FActiveRow) then begin
    Invalidate;
  end;
end;
{=====}

procedure TVpDayView.SetActiveCol(Value: Integer);
begin
  if FActiveCol <> Value then begin
    if Value < 0 then
      FActiveCol := 0
    else if Value > pred(NumDays) then
      FActiveCol := pred(NumDays)
    else
      FActiveCol := Value;
    Invalidate;
  end;
end;
{=====}
procedure TVpDayView.SetDotDotDotColor(const v: TColor);
begin
  if v <> FDotDotDotColor then begin
    FDotDotDotColor := v;
    Invalidate;
  end;
end;
{=====}

procedure TVpDayView.SetShowEventTimes(Value: Boolean);
begin
  if Value <> FShowEventTimes then begin
    FShowEventTimes := Value;
    Invalidate;
  end
end;
{=====}

procedure TVpDayView.SetWrapStyle(const v: TVpDVWrapStyle);
begin
  if v <> FWrapStyle then begin
    FWrapStyle := v;
    Invalidate;
  end;
end;
{=====}

procedure TVpDayView.dvSetActiveRowByCoord(Pnt: TPoint; Sloppy: Boolean);
var
  I : Integer;
begin
  if dvClickTimer.Enabled  then
    dvClickTimer.Enabled := false;
  for I := 0 to pred(LineCount) do begin
    if Sloppy and
       (Pnt.y <= dvLineMatrix[ActiveCol, I].Rec.Bottom) and
       (Pnt.y > dvLineMatrix[ActiveCol, I].Rec.Top)
    then begin
      ActiveRow := I;
      Exit;
    end else
    if PointInRect(Pnt, dvLineMatrix[ActiveCol, I].Rec) then
    begin
      ActiveRow := I;
      Exit;
    end;
  end;
end;
{=====}

procedure TVpDayView.dvSetActiveColByCoord(Pnt: TPoint);
var
  I : Integer;
begin
  for I := 0 to pred(length(dvColRectArray)) do begin
    if PointInRect(Pnt, dvColRectArray[I].Rec) then
    begin
      ActiveCol := I;
      Exit;
    end;
  end;
end;
{=====}

function TVpDayView.GetControlType : TVpItemType;
begin
  Result := itDayView;
end;

procedure TVpDayView.AutoScaledPaintToCanvas(PaintCanvas: TCanvas; PaintTo: TRect;
  Angle: TVpRotationAngle; RenderDate: TDateTime; StartLine, StopLine: Integer;
  UseGran: TVpGranularity);
var
  SrcResY: Integer;
  DestResY: Integer;
  Scale: Extended;
begin
  SrcResY := GetDeviceCaps(Canvas.Handle, LOGPIXELSY);
  DestResY := GetDeviceCaps(PaintCanvas.Handle, LOGPIXELSY);
  Scale := DestResY / SrcResY;
  RenderToCanvas(PaintCanvas, PaintTo, Angle, Scale, RenderDate, StartLine, StopLine, UseGran, True);
end;

procedure TVpDayView.PaintToCanvas(ACanvas: TCanvas; ARect: TRect;
  Angle: TVpRotationAngle; ADate: TDateTime; StartHour, EndHour: TVpHours;
  UseGran: TVpGranularity);
begin
  RenderToCanvas(
    ACanvas,
    ARect,
    Angle,
    1,
    ADate,
    HourToLine(StartHour, UseGran),
    HourToLine(EndHour, UseGran),
    UseGran,
    True);
end;

procedure TVpDayView.RenderToCanvas(RenderCanvas: TCanvas; RenderIn: TRect;
  Angle: TVpRotationAngle; Scale: Extended; RenderDate: TDateTime;
  StartLine, StopLine: Integer; UseGran: TVpGranularity; DisplayOnly: Boolean);
var
  painter: TVpDayViewPainter;
begin
  dvPainting := true;
  painter := TVpDayviewPainter.Create(Self, RenderCanvas);
  try
    painter.RenderToCanvas(RenderIn, Angle, Scale, RenderDate, StartLine,
      StopLine, UseGran, DisplayOnly);
  finally
    painter.Free;
    dvPainting := false;
  end;
end;

{.$IFNDEF LCL}
procedure TVpDayView.VpDayViewInit(var Msg: {$IFDEF DELPHI}TMessage{$ELSE}TLMessage{$ENDIF});
begin
  Unused(Msg);

  if csLoading in ComponentState then begin
    PostMessage(Handle, Vp_DayViewInit, 0, 0);
    Exit;
  end;

  dvCalcColHeadHeight(1);
  dvCalcRowHeight(1, FGranularity);
  dvCalcVisibleLines(Height, dvColHeadHeight, dvRowHeight, 1, TopLine, -1);
  SetVScrollPos;
end;
{.$ENDIF}

(*****************************************************************************)
{ TVpCHAttributes }

constructor TVpCHAttributes.Create(AOwner: TVpDayView);
begin
  inherited Create;
  FOwner := AOwner;
  FFont := TVpFont.Create(AOwner);
end;
{=====}

destructor TVpCHAttributes.Destroy;
begin
  FFont.Free;
  inherited;
end;
{=====}

procedure TVpCHAttributes.SetColor(const Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    FOwner.Invalidate;
  end;
end;
{=====}

procedure TVpCHAttributes.SetFont(Value: TVpFont);
begin
  FFont.Assign(Value);
end;
{=====}

(*****************************************************************************)
{ TVpRHAttributes }

constructor TVpRHAttributes.Create(AOwner: TVpDayView);
begin
  inherited Create;
  FOwner := AOwner;
  FHourFont := TVpFont.Create(AOwner);
  FMinuteFont := TVpFont.Create(AOwner);
 {$IFNDEF LCL}
  FHourFont.Name := 'Tahoma';
  FMinuteFont.Name := 'Tahoma';
 {$ENDIF}
end;
{=====}

destructor TVpRHAttributes.Destroy;
begin
  FHourFont.Free;
  FMinuteFont.Free;
  inherited;
end;
{=====}

procedure TVpRHAttributes.SetColor(const Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    FOwner.Invalidate;
  end;
end;
{=====}

procedure TVpRHAttributes.SetHourFont(Value: TVpFont);
begin
  if Value <> FHourFont then begin
    FHourFont.Assign(Value);
    FOwner.Invalidate;
  end;
end;
{=====}

procedure TVpRHAttributes.SetMinuteFont(Value: TVpFont);
begin
  if Value <> FMinuteFont then begin
    FMinuteFont.Assign(Value);
    FOwner.Invalidate;
  end;
end;
{=====}

end.
