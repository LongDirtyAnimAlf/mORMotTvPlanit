{*********************************************************}
{*                 VPTASKLIST.PAS 1.03                   *}
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

unit VpTaskList;

interface

uses
  {$IFDEF LCL}
  LMessages, LCLProc, LCLType, LCLIntf,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  Classes, Graphics, Controls, ExtCtrls, StdCtrls,
  VpBase, VpBaseDS, VpMisc, VpData, VpSR, VpConst, VpCanvasUtils, Menus;

type
  TVpTaskRec = packed record
    Task: Pointer;
    LineRect: TRect;
    CheckRect: TRect;
  end;

type
  TVpTaskArray = array of TVpTaskRec;

  { forward declarations }
  TVpTaskList = class;

  TVpTaskDisplayOptions = class(TPersistent)
  protected{private}
    FTaskList: TVpTaskList;
    FShowAll: Boolean;
    FShowCompleted: Boolean;
    FShowDueDate: Boolean;
    FDueDateFormat: string;
    FCheckColor: TColor;
    FCheckBGColor: TColor;
    FCheckStyle: TVpCheckStyle;
    FOverdueColor: TColor;
    FNormalColor: TColor;
    FCompletedColor: TColor;
    procedure SetCheckColor(Value: TColor);
    procedure SetCheckBGColor(Value: TColor);
    procedure SetCheckStyle(Value: TVpCheckStyle);
    procedure SetDueDateFormat(Value: string);
    procedure SetShowCompleted(Value: Boolean);
    procedure SetShowDueDate(Value: Boolean);
    procedure SetShowAll(Value: Boolean);
    procedure SetOverdueColor(Value: TColor);
    procedure SetNormalColor(Value: TColor);
    procedure SetCompletedColor(Value: TColor);
  public
    constructor Create(Owner: TVpTaskList);
    destructor Destroy; override;
  published
    property CheckBGColor: TColor read FCheckBGColor write SetCheckBGColor;
    property CheckColor: TColor read FCheckColor write SetCheckColor;
    property CheckStyle: TVpCheckStyle read FCheckStyle write SetCheckStyle;
    property DueDateFormat: string read FDueDateFormat write SetDueDateFormat;
    property ShowCompletedTasks: Boolean read FShowCompleted write SetShowCompleted;
    property ShowAll: Boolean read FShowAll write SetShowAll;
    property ShowDueDate: Boolean read FShowDueDate write SetShowDueDate;
    property OverdueColor: TColor read FOverdueColor write SetOverdueColor;
    property NormalColor: TColor read FNormalColor write SetNormalColor;
    property CompletedColor: TColor read FCompletedColor write SetCompletedColor;
  end;

  { InPlace Editor }
  TVpTLInPlaceEdit = class(TCustomEdit)
  protected{private}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
//    procedure Move(const Loc: TRect; Redraw: Boolean);
  end;

  TVpTaskHeadAttr = class(TVpPersistent)
  protected{private}
    FTaskList: TVpTaskList;
    FFont: TVpFont;
    FColor: TColor;
    procedure SetColor(Value: TColor);
    procedure SetFont(Value: TVpFont);
  public
    constructor Create(AOwner: TVpTaskList);
    destructor Destroy; override;
    procedure Invalidate; override;
      { The Invalidate method is used as a bridge between FFont & FTaskList. }
    property TaskList: TVpTaskList read FTaskList;
  published
    property Color: TColor read FColor write SetColor;
    property Font: TVpFont read FFont write SetFont;
  end;

  { Task List }
  TVpTaskList = class(TVpLinkableControl)
  protected{ private }
    FColor: TColor;
    FCaption: string;
    FDisplayOptions: TVpTaskDisplayOptions;
    FLineColor: TColor;
    FActiveTask: TVpTask;
    FShowResourceName: Boolean;
    FTaskIndex: Integer;
    FScrollBars: TScrollStyle;
    FTaskHeadAttr: TVpTaskHeadAttr;
    FMaxVisibleTasks: Word;
    FDrawingStyle: TVpDrawingStyle;
    FTaskID: Integer;
    FDefaultPopup: TPopupMenu;
    FShowIcon: Boolean;
    FAllowInplaceEdit: Boolean;
    { task variables }
    FOwnerDrawTask: TVpOwnerDrawTask;
    FBeforeEdit: TVpBeforeEditTask;
    FAfterEdit: TVpAfterEditTask;
    FOwnerEditTask: TVpEditTask;
    { internal variables }
    tlVisibleTaskArray: TVpTaskArray;
    tlAllTaskList: TList;
    tlItemsBefore: Integer;
    tlItemsAfter: Integer;
    tlVisibleItems: Integer;
    tlHitPoint: TPoint;
    tlClickTimer: TTimer;
    tlLoaded: Boolean;
    tlRowHeight: Integer;
    tlInPlaceEditor: TVpTLInPlaceEdit;
    tlCreatingEditor: Boolean;
    tlPainting: Boolean;
    tlVScrollDelta: Integer;
    tlHotPoint: TPoint;

    { property methods }
    function GetTaskIndex: Integer;
    procedure SetLineColor(Value: TColor);
    procedure SetMaxVisibleTasks(Value: Word);
    procedure SetTaskIndex(Value: Integer);
    procedure SetDrawingStyle(const Value: TVpDrawingStyle);
    procedure SetColor(const Value: TColor); reintroduce;
    procedure SetShowIcon(const v: Boolean);
    procedure SetShowResourceName(Value: Boolean);
    { internal methods }
    procedure InitializeDefaultPopup;
    procedure PopupAddTask(Sender: TObject);
    procedure PopupDeleteTask(Sender: TObject);
    procedure PopupEditTask(Sender: TObject);
    procedure tlSetVScrollPos;
    procedure tlCalcRowHeight;
    procedure tlEditInPlace(Sender: TObject);
    procedure tlHookUp;
    procedure Paint; override;
    procedure Loaded; override;
    procedure tlSpawnTaskEditDialog(NewTask: Boolean);
    procedure tlSetActiveTaskByCoord(Pnt: TPoint);
    function tlVisibleTaskToTaskIndex(const VisTaskIndex: Integer) : Integer;
    function tlTaskIndexToVisibleTask(const ATaskIndex: Integer) : Integer;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    {$IFNDEF LCL}
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Msg: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMRButtonDown (var Msg: TWMRButtonDown); message WM_RBUTTONDOWN;
    {$ELSE}
    procedure WMLButtonDown(var Msg: TLMLButtonDown); message LM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Msg: TLMLButtonDblClk); message LM_LBUTTONDBLCLK;
    procedure WMRButtonDown (var Msg: TLMRButtonDown); message LM_RBUTTONDOWN;
    {$ENDIF}
    procedure EditTask;
    procedure EndEdit(Sender: TObject);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    { message handlers }
    {$IFNDEF LCL}
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey);
      message CM_WANTSPECIALKEY;
    {$ELSE}
    procedure WMSize(var Msg: TLMSize); message LM_SIZE;
    procedure WMVScroll(var Msg: TLMVScroll); message LM_VSCROLL;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteActiveTask(Verify: Boolean);
    procedure LoadLanguage;
    procedure LinkHandler(Sender: TComponent; NotificationType: TVpNotificationType;
      const Value: Variant); override;
    function GetControlType: TVpItemType; override;
    procedure PaintToCanvas(ACanvas: TCanvas; ARect: TRect; Angle: TVpRotationAngle);
    procedure RenderToCanvas(RenderCanvas: TCanvas; RenderIn: TRect;
      Angle: TVpRotationAngle; Scale: Extended; RenderDate: TDateTime;
      StartLine, StopLine: Integer; UseGran: TVpGranularity; DisplayOnly: Boolean); override;

    property ActiveTask: TVpTask read FActiveTask;
    property TaskIndex: Integer read GetTaskIndex write SetTaskIndex;

  published
    {inherited properties}
    property Align;
    property Anchors;
    property Font;
    property TabStop;
    property TabOrder;
    property ReadOnly;

    property AllowInplaceEditing: Boolean
      read FAllowInplaceEdit write FAllowInplaceEdit default true;
    property DisplayOptions: TVpTaskDisplayOptions read FDisplayOptions write FDisplayOptions;
    property LineColor: TColor read FLineColor write SetLineColor;
    property MaxVisibleTasks: Word read FMaxVisibleTasks write SetMaxVisibleTasks;
    property TaskHeadAttributes: TVpTaskHeadAttr read FTaskHeadAttr write FTaskHeadAttr;
    property DrawingStyle: TVpDrawingStyle read FDrawingStyle write SetDrawingStyle;
    property Color: TColor read FColor write SetColor;
    property ShowIcon: Boolean read FShowIcon write SetShowIcon default True;
    property ShowResourceName: Boolean read FShowResourceName write SetShowResourceName;
    { events }
    property BeforeEdit: TVpBeforeEditTask read FBeforeEdit write FBeforeEdit;
    property AfterEdit: TVpAfterEditTask read FAfterEdit write FAfterEdit;
    property OnOwnerEditTask: TVpEditTask read FOwnerEditTask write FOwnerEditTask;
  end;

implementation

uses
  SysUtils, Forms, Dialogs, VpTaskEditDlg, VpDlg, VpTasklistPainter;


(*****************************************************************************)


{ TVpTaskDisplayOptions }
constructor TVpTaskDisplayOptions.Create(Owner: TVpTaskList);
begin
  inherited Create;
  FTaskList := Owner;
  FDueDateFormat := DefaultFormatSettings.ShortDateFormat;
  FShowDueDate := true;
  FCheckColor := cl3DDkShadow;
  FCheckBGColor := clWindow;
  FCheckStyle := csCheck;
  FOverdueColor := clRed;
  FCompletedColor := clGray;
  FNormalColor := clBlack;
end;
{=====}

destructor TVpTaskDisplayOptions.Destroy;
begin
  inherited;
end;
{=====}

procedure TVpTaskDisplayOptions.SetOverdueColor(Value : TColor);
begin
  if FOverdueColor <> Value then begin
    FOverdueColor := Value;
    FTaskList.Invalidate;
  end;
end;
{=====}

procedure TVpTaskDisplayOptions.SetNormalColor(Value: TColor);
begin
  if FNormalColor <> Value then begin
    FNormalColor := Value;
    FTaskList.Invalidate;
  end;
end;
{=====}

procedure TVpTaskDisplayOptions.SetCompletedColor(Value: TColor);
begin
  if FCompletedColor <> Value then begin
    FCompletedColor := Value;
    FTaskList.Invalidate;
  end;
end;
{=====}

procedure TVpTaskDisplayOptions.SetCheckColor(Value: TColor);
begin
  if FCheckColor <> Value then begin
    FCheckColor := Value;
    FTaskList.Invalidate;
  end;
end;
{=====}

procedure TVpTaskDisplayOptions.SetCheckBGColor(Value: TColor);
begin
  if FCheckBGColor <> Value then begin
    FCheckBGColor := Value;
    FTaskList.Invalidate;
  end;
end;
{=====}

procedure TVpTaskDisplayOptions.SetCheckStyle(Value: TVpCheckStyle);
begin
  if Value <> FCheckStyle then begin
    FCheckStyle := Value;
    FTaskList.Invalidate;
  end;
end;
{=====}

procedure TVpTaskDisplayOptions.SetDueDateFormat(Value: string);
begin
  if FDueDateFormat <> Value then begin
    FDueDateFormat := Value;
    FTaskList.Invalidate;
  end;
end;
{=====}

procedure TVpTaskDisplayOptions.SetShowCompleted(Value : Boolean);
begin
  if FShowCompleted <> Value then begin
    FShowCompleted := Value;
    FTaskList.Invalidate;
  end;
end;
{=====}

procedure TVpTaskDisplayOptions.SetShowDueDate(Value: Boolean);
begin
  if FShowDueDate <> Value then begin
    FShowDueDate := Value;
    FTaskList.Invalidate;
  end;
end;
{=====}

procedure TVpTaskDisplayOptions.SetShowAll(Value: Boolean);
begin
  if FShowAll <> Value then begin
    FShowAll := Value;
    FTaskList.Invalidate;
  end;
end;
{=====}

{ TVpTaskHeadAttr }
constructor TVpTaskHeadAttr.Create(AOwner: TVpTaskList);
begin
  inherited Create;
  FTaskList := AOwner;
  FFont := TVpFont.Create(self);
  FFont.Assign(FTaskList.Font);
  FColor := clSilver;
end;
{=====}

destructor TVpTaskHeadAttr.Destroy;
begin
  FFont.Free;
end;
{=====}

procedure TVpTaskHeadAttr.Invalidate;
begin
  if Assigned(FTaskList) then
    FTaskList.Invalidate;
end;
{=====}

procedure TVpTaskHeadAttr.SetColor(Value: TColor);
begin
  if Value <> FColor then begin
    FColor := Value;
    TaskList.Invalidate;
  end;
end;
{=====}

procedure TVpTaskHeadAttr.SetFont(Value: TVpFont);
begin
  if Value <> FFont then begin
    FFont.Assign(Value);
    TaskList.Invalidate;
  end;
end;
{=====}


{ TVpCGInPlaceEdit }

constructor TVpTLInPlaceEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := False;
  BorderStyle := bsNone;
  {$IFDEF VERSION4}
  DoubleBuffered := False;
  {$ENDIF}
end;
{=====}
                           {
procedure TVpTLInPlaceEdit.Move(const Loc: TRect; Redraw: Boolean);
begin
  CreateHandle;
  Redraw := Redraw or not IsWindowVisible(Handle);
  Invalidate;
  SetBounds(Loc.Left, Loc.Top, Loc.Right-Loc.Left, Loc.Bottom-Loc.Top);
  if Redraw then Invalidate;
  SetFocus;
end;                        }
{=====}

procedure TVpTLInPlaceEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style{ or ES_MULTILINE};
end;
{=====}

procedure TVpTLInPlaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  TaskList: TVpTaskList;
begin
  TaskList := TVpTaskList(Owner);

  case Key of
    VK_RETURN:
      begin
        Key := 0;
        TaskList.EndEdit(Self);
      end;

    VK_UP:
      begin
        Key := 0;
        TaskList.TaskIndex := TaskList.TaskIndex - 1;
      end;

    VK_DOWN:
      begin
        Key := 0;
        TaskList.TaskIndex := TaskList.TaskIndex + 1;
      end;

    VK_ESCAPE:
      begin
        Key := 0;
        TaskList.EndEdit(Self);
      end;

    else
      inherited;
  end;
end;
{=====}

(*****************************************************************************)
{ TVpTaskList }

constructor TVpTaskList.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csCaptureMouse, csOpaque, csDoubleClicks];
  { Create internal classes and stuff }
  tlClickTimer := TTimer.Create(self);
  FTaskHeadAttr := TVpTaskHeadAttr.Create(Self);
  FDisplayOptions := TVpTaskDisplayOptions.Create(self);
  tlAllTaskList := TList.Create;

  { Set styles and initialize internal variables }
  {$IFDEF VERSION4}
  DoubleBuffered := true;
  {$ENDIF}
  tlItemsBefore := 0;
  tlItemsAfter := 0;
  tlVisibleItems := 0;
  tlClickTimer.Enabled := false;
  FMaxVisibleTasks := 250;
  tlClickTimer.Interval := ClickDelay;
  tlClickTimer.OnTimer := tlEditInPlace;
  tlCreatingEditor := false;
  FDrawingStyle := ds3d;
  tlPainting := false;
  FShowResourceName := true;
  FColor := clWindow;
  FLineColor := clGray;
  FScrollBars := ssVertical;
  FTaskIndex := -1;
  FShowIcon := True;
  FAllowInplaceEdit := true;

  SetLength(tlVisibleTaskArray, MaxVisibleTasks);

  { size }
  Height := 225;
  Width := 169;

  FDefaultPopup := TPopupMenu.Create(Self);
  InitializeDefaultPopup;

  tlHookUp;
end;
{=====}

destructor TVpTaskList.Destroy;
begin
  FreeAndNil(tlInplaceEditor);
  tlClickTimer.Free;
  FDisplayOptions.Free;
  tlAllTaskList.Free;
  FTaskHeadAttr.Free;
  FDefaultPopup.Free;

  inherited;
end;
{=====}

procedure TVpTaskList.DeleteActiveTask(Verify: Boolean);
var
  DoIt: Boolean;
begin
  DoIt := not Verify;
  if FActiveTask <> nil then begin
    if Verify then
      DoIt := (MessageDlg(RSConfirmDeleteTask + #13#10#10 + RSPermanent,
        mtConfirmation, [mbYes, mbNo], 0) = mrYes);

    if DoIt then begin
      FActiveTask.Deleted := true;
      if Assigned(DataStore) then
        if Assigned(DataStore.Resource) then
          DataStore.Resource.TasksDirty := True;
      DataStore.PostTasks;
      DataStore.RefreshTasks;
      Invalidate;
    end;
  end;
end;
{=====}

procedure TVpTaskList.LoadLanguage;
begin
  FDefaultPopup.Items.Clear;
  InitializeDefaultPopup;
end;

procedure TVpTaskList.LinkHandler(Sender: TComponent;
  NotificationType: TVpNotificationType; const Value: Variant);
begin
  Unused(Value);
  case NotificationType of
    neDataStoreChange : Invalidate;
    neInvalidate      : Invalidate;
  end;
end;
{=====}

procedure TVpTaskList.tlHookUp;
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

procedure TVpTaskList.Loaded;
begin
  inherited;
  tlLoaded := true;
end;
{=====}

function TVpTaskList.GetControlType: TVpItemType;
begin
  Result := itTasks;
end;
{=====}

procedure TVpTaskList.Paint;
begin
  { paint simply calls RenderToCanvas and passes in the screen canvas. }
  RenderToCanvas(
    Canvas,   { Screen Canvas}
    Rect(0, 0, Width, Height), { Clipping Rectangle             }
    ra0,      { Rotation Angle                                  }
    1,        { Scale                                           }
    Now,      { Render Date                                     }
    tlItemsBefore,  { Starting Line                             }
    -1,       { Stop Line                                       }
    gr30Min,  { Granularity - Not used int the task list        }
    False     { Display Only - True for a printed version,      }
  );          {                False for an interactive version }
end;
{=====}

procedure TVpTaskList.PaintToCanvas(ACanvas: TCanvas; ARect: TRect;
  Angle: TVpRotationAngle);
begin
  RenderToCanvas(ACanvas, ARect, Angle, 1, Now, -1, -1, gr30Min, True);
end;
{=====}

procedure TVpTaskList.RenderToCanvas(RenderCanvas: TCanvas; RenderIn: TRect;
  Angle: TVpRotationAngle; Scale: Extended; RenderDate : TDateTime;
  StartLine, StopLine: Integer; UseGran: TVpGranularity; DisplayOnly: Boolean);
var
  painter: TVpTaskListPainter;
begin
  tlPainting := true;
  painter := TVpTaskListPainter.Create(Self, RenderCanvas);
  try
    painter.RenderToCanvas(RenderIn, Angle, Scale, RenderDate, StartLine,
      StopLine, UseGran, DisplayOnly);
  finally
    painter.Free;
    tlPainting := false;
  end;
end;

procedure TVpTaskList.SetColor(const Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpTaskList.tlCalcRowHeight;
var
  SaveFont: TFont;
  Temp: Integer;
begin
  { Calculates row height based on the largest of the RowHead's Minute }
  { font, the standard client font, and a sample character string.     }
  SaveFont := Canvas.Font;
  Canvas.Font.Assign(FTaskHeadAttr.Font);
  tlRowHeight := Canvas.TextHeight(TallShortChars);
  Canvas.Font.Assign(SaveFont);
  Temp := Canvas.TextHeight(TallShortChars);
  if Temp > tlRowHeight then
    tlRowHeight := Temp;
  tlRowHeight := tlRowHeight + TextMargin * 2;
  Canvas.Font.Assign(SaveFont);
end;
{=====}

procedure TVpTaskList.SetDrawingStyle(const Value: TVpDrawingStyle);
begin
  if FDrawingStyle <> Value then begin
    FDrawingStyle := Value;
    Repaint;
  end;
end;
{=====}

procedure TVpTaskList.SetTaskIndex(Value: Integer);
begin
  if (tlInPlaceEditor <> nil) and tlInplaceEditor.Visible then
    EndEdit(self);

  if (Value < DataStore.Resource.Tasks.Count) and (FTaskIndex <> Value) then
  begin
    FTaskIndex := Value;
    if FTaskIndex > -1 then
      FActiveTask := DataStore.Resource.Tasks.GetTask(Value)
    else
      FActiveTask := nil;
    Invalidate;
  end;
end;
{=====}

function TVpTaskList.GetTaskIndex: Integer;
begin
  if FActiveTask = nil then
    result := -1
  else
    result := FActiveTask.ItemIndex;
end;
{=====}

procedure TVpTaskList.SetLineColor(Value: TColor);
begin
  if Value <> FLineColor then begin
    FLineColor := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpTaskList.SetMaxVisibleTasks(Value: Word);
begin
  if Value <> FMaxVisibleTasks then begin
    FMaxVisibleTasks := Value;
    SetLength(tlVisibleTaskArray, FMaxVisibleTasks);
    Invalidate;
  end;
end;
{=====}

{$IFNDEF LCL}
procedure TVpTaskList.WMSize(var Msg: TWMSize);
{$ELSE}
procedure TVpTaskList.WMSize(var Msg: TLMSize);
{$ENDIF}
begin
  inherited;
  { force a repaint on resize }
  Invalidate;
end;
{=====}

procedure TVpTaskList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_TABSTOP;
    Style := Style or WS_VSCROLL;
{$IFNDEF LCL}
    WindowClass.style := CS_DBLCLKS;
{$ENDIF}
  end;
end;
{=====}

procedure TVpTaskList.CreateWnd;
begin
  inherited;
  tlCalcRowHeight;
  tlSetVScrollPos;
end;
{=====}

{$IFNDEF LCL}
procedure TVpTaskList.WMLButtonDown(var Msg: TWMLButtonDown);
{$ELSE}
procedure TVpTaskList.WMLButtonDown(var Msg: TLMLButtonDown);
{$ENDIF}
begin
  inherited;

  if not Focused then SetFocus;

  if not (csDesigning in ComponentState) then begin
    {See if the user clicked on a checkbox}
    tlSetActiveTaskByCoord (Point(Msg.XPos, Msg.YPos));
  end;
end;
{=====}

{$IFNDEF LCL}
procedure TVpTaskList.WMRButtonDown(var Msg: TWMRButtonDown);
{$ELSE}
procedure TVpTaskList.WMRButtonDown(var Msg: TLMRButtonDown);
{$ENDIF}
var
  ClientOrigin: TPoint;
  i: Integer;
begin
  inherited;

  if not Assigned (PopupMenu) then begin
    if not Focused then 
      SetFocus;
    tlSetActiveTaskByCoord(Point(Msg.XPos, Msg.YPos));
    tlClickTimer.Enabled := False;
    ClientOrigin := GetClientOrigin;

    if not Assigned(FActiveTask) then
      for i := 0 to FDefaultPopup.Items.Count - 1 do begin
        if (FDefaultPopup.Items[i].Tag = 1) or (ReadOnly) then           
          FDefaultPopup.Items[i].Enabled := False;
      end
    else
      for i := 0 to FDefaultPopup.Items.Count - 1 do
        FDefaultPopup.Items[i].Enabled := True;

    FDefaultPopup.Popup(Msg.XPos + ClientOrigin.x, Msg.YPos + ClientOrigin.y);
  end;
end;
{=====}

{$IFNDEF LCL}
procedure TVpTaskList.WMLButtonDblClk(var Msg: TWMLButtonDblClk);
{$ELSE}
procedure TVpTaskList.WMLButtonDblClk(var Msg: TLMLButtonDblClk);
{$ENDIF}
begin
  inherited;
  tlClickTimer.Enabled := false;
  { if the mouse was pressed down in the client area, then select the cell. }
  if not Focused then 
    SetFocus;
  { The mouse click landed inside the client area }
  tlSetActiveTaskByCoord(Point(Msg.XPos, Msg.YPos));
  { Spawn the TaskList editor }
  if not ReadOnly then                                                   
    tlSpawnTaskEditDialog(FActiveTask = nil);
end;
{=====}

procedure TVpTaskList.InitializeDefaultPopup;
var
  NewItem: TMenuItem;
begin
  if RSTaskPopupAdd <> '' then begin
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := RSTaskPopupAdd;
    NewItem.OnClick := PopupAddTask;
    NewItem.Tag := 0;
    FDefaultPopup.Items.Add(NewItem);
  end;

  if RSTaskPopupEdit <> '' then begin
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := RSTaskPopupEdit;
    NewItem.OnClick := PopupEditTask;
    NewItem.Tag := 1;
    FDefaultPopup.Items.Add(NewItem);
  end;

  if RSTaskPopupDelete <> '' then begin
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := RSTaskPopupDelete;
    NewItem.OnClick := PopupDeleteTask;
    NewItem.Tag := 1;
    FDefaultPopup.Items.Add(NewItem);
  end;
end;
{=====}

procedure TVpTaskList.PopupAddTask(Sender: TObject);
begin
  if ReadOnly then                                                     
    Exit;                                                              
  if not CheckCreateResource then                                      
    Exit;                                                              
  { Allow the user to fill in all the new information }
  Repaint;                                                             
  tlSpawnTaskEditDialog(True);
end;
{=====}

procedure TVpTaskList.PopupDeleteTask(Sender: TObject);
begin
  if ReadOnly then                                                     
    Exit;                                                              
  if FActiveTask <> nil then begin                                       
    Repaint;                                                             
    DeleteActiveTask(True);
  end;                                                                   
end;
{=====}

procedure TVpTaskList.PopupEditTask(Sender: TObject);
begin
  if ReadOnly then                                                     
    Exit;                                                              
  if FActiveTask <> nil then begin                                       
    Repaint;                                                             
    { edit this Task }
    tlSpawnTaskEditDialog(False);
  end;                                                                   
end;
{=====}

procedure TVpTaskList.tlSpawnTaskEditDialog(NewTask: Boolean);
var
  AllowIt: Boolean;
  Task: TVpTask;
  TaskDlg: TVpTaskEditDialog;
begin
  tlClickTimer.Enabled := false;
  if not CheckCreateResource then                                      
    Exit;                                                              
  if (DataStore = nil) or (DataStore.Resource = nil) then
    Exit;

  AllowIt := false;
  if NewTask then begin
    Task := DataStore.Resource.Tasks.AddTask(DataStore.GetNextID('Tasks'));
    Task.CreatedOn := now;
    Task.DueDate := Now + 7;
  end else
    Task := FActiveTask;

  if Assigned(FOwnerEditTask) then
    FOwnerEditTask(self, Task, DataStore.Resource, AllowIt)
  else begin
    TaskDlg := TVpTaskEditDialog.Create(nil);
    try
      TaskDlg.Options := TaskDlg.Options + [doSizeable];
      TaskDlg.DataStore := DataStore;
      AllowIt := TaskDlg.Execute(Task);
    finally
      TaskDlg.Free;
    end;
  end;

  if AllowIt then begin
    DataStore.PostTasks();
    DataStore.NotifyDependents;
    Invalidate;
  end else begin
    if NewTask then begin
      DataStore.Resource.Tasks.DeleteTask(Task);
    end;
    DataStore.PostTasks;
    Invalidate;
  end;
end;
{=====}

{$IFNDEF LCL}
procedure TVpTaskList.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  Msg.Result := 1;
end;
{$ENDIF}
{=====}

procedure TVpTaskList.tlEditInPlace(Sender: TObject);
begin
  { this is the timer Task which spawns an in-place editor }
  { if the task is doublecliked before this timer fires, then the }
  { task is edited in a dialog based editor. }
  tlClickTimer.Enabled := false;
  EditTask;
end;
{=====}

procedure TVpTaskList.EditTask;
var
  AllowIt: Boolean;
  R: TRect;
  VisTask: Integer;
begin
  {don't allow a user to edit a completed task in place.}
  if FActiveTask.Complete then
    Exit;

  if not FAllowInplaceEdit then
    exit;

  if Assigned(tlInplaceEditor) and tlInplaceEditor.Visible then
    exit;

  AllowIt := true;

  VisTask := tlTaskIndexToVisibleTask(TaskIndex);
  if VisTask < 0 then
    Exit;

  { call the user defined BeforeEdit task }
  if Assigned(FBeforeEdit) then
    FBeforeEdit(Self, FActiveTask, AllowIt);

  if AllowIt then begin
    { build the editor's rectangle }
    R := tlVisibleTaskArray[VisTask].LineRect;
    R.Top := R.Top + TextMargin;
    R.Left := R.Left + TextMargin - 1;
    { create and spawn the in-place editor }
    if tlInplaceEditor = nil then begin
      tlInPlaceEditor := TVpTLInPlaceEdit.Create(Self);
      tlInPlaceEditor.Parent := self;
      tlInPlaceEditor.OnExit := EndEdit;
    end;
    tlInPlaceEditor.Font.Assign(Font);
    tlInPlaceEditor.SetBounds(R.Left, R.Top, WidthOf(R), HeightOf(R));
    tlInPlaceEditor.Text := FActiveTask.Description;
    tlInplaceEditor.Show;
    tlInplaceEditor.SetFocus;
    Invalidate;
  end;
end;
{=====}

procedure TVpTaskList.EndEdit(Sender: TObject);
begin
  if (tlInPlaceEditor <> nil) and tlInplaceEditor.Visible then begin
    if tlInPlaceEditor.Text <> FActiveTask.Description then begin
      FActiveTask.Description := tlInPlaceEditor.Text;
      FActiveTask.Changed := true;
      DataStore.Resource.TasksDirty := true;
      DataStore.PostTasks;
      if Assigned(FAfterEdit) then
        FAfterEdit(self, FActiveTask);
    end;
    tlInPlaceEditor.Hide;
    SetFocus;
    Invalidate;
  end;
end;
{=====}

procedure TVpTaskList.KeyDown(var Key: Word; Shift: TShiftState);
var
  PopupPoint: TPoint;
begin
  case Key of
    VK_UP:
      if TaskIndex > 0 then
        TaskIndex := TaskIndex - 1
      else
        TaskIndex := Pred(DataStore.Resource.Tasks.Count);
    VK_DOWN:
      if TaskIndex < Pred(DataStore.Resource.Tasks.Count) then
        TaskIndex := TaskIndex + 1
      else
        TaskIndex := 0;
    VK_NEXT:
      if TaskIndex < Pred(DataStore.Resource.Tasks.Count) - tlVisibleItems then
        TaskIndex := TaskIndex + tlVisibleItems                          
      else                                                               
        TaskIndex := Pred(DataStore.Resource.Tasks.Count);               
    VK_PRIOR :
      if TaskIndex > tlVisibleItems then                                 
        TaskIndex := TaskIndex - tlVisibleItems                          
      else                                                               
        TaskIndex := 0;                                                  
    VK_HOME:
      TaskIndex := 0;
    VK_END:
      TaskIndex := Pred(DataStore.Resource.Tasks.Count);
    VK_DELETE:
      DeleteActiveTask(true);
    VK_RETURN:
      tlSpawnTaskEditDialog (False);
    VK_INSERT:
      tlSpawnTaskEditDialog (True);
    VK_F2:
      if Assigned(DataStore) then begin
        if Assigned(DataStore.Resource) then
          tlEditInPlace(Self);
      end;
    VK_SPACE:
      if Assigned(FActiveTask) then begin
        FActiveTask.Complete := not FActiveTask.Complete;
        Invalidate;
      end;
{$IFNDEF LCL}
    VK_TAB:
      if ssShift in Shift then
        Windows.SetFocus(GetNextDlgTabItem(GetParent(Handle), Handle, False))
      else
        Windows.SetFocus(GetNextDlgTabItem(GetParent(Handle), Handle, True));
{$ENDIF}
    VK_F10:
      if (ssShift in Shift) and not (Assigned(PopupMenu)) then begin
        PopupPoint := GetClientOrigin;
        FDefaultPopup.Popup(PopupPoint.x + 10, PopupPoint.y + 10);
      end;
    VK_APPS:
      if not Assigned(PopupMenu) then begin
        PopupPoint := GetClientOrigin;
        FDefaultPopup.Popup(PopupPoint.x + 10, PopupPoint.y + 10);
      end;
  end;

  if TaskIndex < tlItemsBefore then                                      
    tlItemsBefore := TaskIndex;                                          
  if TaskIndex >= tlItemsBefore + tlVisibleItems then                    
    tlItemsBefore := TaskIndex - tlVisibleItems + 1;                     
end;
{=====}

{$IFNDEF LCL}
procedure TVpTaskList.WMVScroll(var Msg: TWMVScroll);
{$ELSE}
procedure TVpTaskList.WMVScroll(var Msg: TLMVScroll);
{$ENDIF}
begin
  { for simplicity, bail out of editing while scrolling. }
  EndEdit(Self);
  if (tlInPlaceEditor <> nil) and tlInplaceEditor.Visible then Exit;

  case Msg.ScrollCode of
    SB_LINEUP:
      if tlItemsBefore > 0 then
        tlItemsBefore := tlItemsBefore - 1;
    SB_LINEDOWN:
      if tlItemsAfter > 0 then
        tlItemsBefore := tlItemsBefore + 1;
    SB_PAGEUP:
      if tlItemsBefore >= tlVisibleItems then
        tlItemsBefore := tlItemsBefore - tlVisibleItems
      else
        tlItemsBefore := 0;
    SB_PAGEDOWN:
      if tlItemsAfter >= tlVisibleItems then
        tlItemsBefore := tlItemsBefore + tlVisibleItems
      else
        tlItemsBefore := tlAllTaskList.Count - tlVisibleItems;
    SB_THUMBPOSITION, SB_THUMBTRACK:
      tlItemsBefore := Msg.Pos;
  end;
  Invalidate;
end;
{=====}

procedure TVpTaskList.tlSetVScrollPos;
var
  SI: TScrollInfo;
begin
  if (not HandleAllocated) or (DataStore = nil) or (DataStore.Resource = nil)
    or (csDesigning in ComponentState) then Exit;

  with SI do begin
    cbSize := SizeOf(SI);
    fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
    nMin := 1;
    nMax := tlAllTaskList.Count;
    nPage := tlVisibleItems;
    if tlItemsAfter = 0 then
      nPos := tlAllTaskList.Count
    else
      nPos := tlItemsBefore;
    nTrackPos := nPos;
  end;
  SetScrollInfo(Handle, SB_VERT, SI, True);
end;
{=====}
procedure TVpTaskList.SetShowIcon(const v: Boolean);
begin                                                                    
  if v <> FShowIcon then begin                                           
    FShowIcon := v;                                                      
    Invalidate;                                                          
  end;                                                                   
end;                                                                     
{=====}

procedure TVpTaskList.SetShowResourceName(Value: Boolean);
begin
  if Value <> FShowResourceName then begin
    FShowResourceName := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpTaskList.tlSetActiveTaskByCoord(Pnt: TPoint);
var
  I: integer;
begin
  if (DataStore = nil) or (DataStore.Resource = nil) then
    Exit;

  if not ReadOnly and tlClickTimer.Enabled then
    tlClickTimer.Enabled := false;

  TaskIndex := -1;

  for I := 0 to pred(Length(tlVisibleTaskArray)) do begin
    { we've hit the end of active tasks, so bail }
    if tlVisibleTaskArray[I].Task = nil then
      Exit;

    { if the point is in an active task's check box... }

    if PointInRect(Pnt, tlVisibleTaskArray[I].CheckRect) then begin
      { set the active task index }
      TaskIndex := tlVisibleTaskToTaskIndex (I);
      if not ReadOnly then begin                                         
        { toggle the complete flag. }
        FActiveTask.Complete := not FActiveTask.Complete;
        FActiveTask.Changed := true;
        DataStore.Resource.TasksDirty := true;
        DataStore.PostTasks;
        Invalidate;
      end;                                                               
      Exit;
    end;

    { if the point is in an active task..}
    if PointInRect(Pnt, tlVisibleTaskArray[I].LineRect) then begin
      { Set ActiveTask to the selected one }
      TaskIndex := tlVisibleTaskToTaskIndex (I);
      if not ReadOnly then
        tlClickTimer.Enabled := true;
      Exit;
    end;
  end;
end;
{=====}

function TVpTaskList.tlVisibleTaskToTaskIndex(const VisTaskIndex: Integer): Integer;
var
  RealTask: TVpTask;
begin
  Result := -1;
  if (VisTaskIndex < 0) or (VisTaskIndex >= Length(tlVisibleTaskArray)) then
    Exit;
  RealTask := TVpTask(tlVisibleTaskArray[VisTaskIndex].Task);
  Result := RealTask.ItemIndex;
end;

function TVpTaskList.tlTaskIndexToVisibleTask(const ATaskIndex: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(tlVisibleTaskArray) - 1 do
    if ATaskIndex = TVpTask(tlVisibleTaskArray[i].Task).ItemIndex then begin
      Result := i;
      Break;
    end;
end;

{=====}

end.

