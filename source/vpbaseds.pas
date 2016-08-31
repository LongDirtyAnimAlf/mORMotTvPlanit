{*********************************************************}
{*                  VPBASEDS.PAS 1.03                    *}
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

unit VpBaseDS;
  { Base DataStore classes }

interface

uses
  {$IFDEF LCL}
  LMessages, LCLProc, LCLIntf, LazFileUtils,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  Classes, Dialogs, SysUtils, Graphics, Controls, StdCtrls, ExtCtrls,
  VpBase, VpData, Forms, VpPrtFmt, VpLocalize;

type
  TVpResourceUpdate = (ruOnChange, ruOnExit, ruOnDropDownClose);         

  { Forward Declarations }
  TVpCustomDataStore = class;
  TVpLinkableControl = class;
  TVpControlLink = class;

  { enumerated types }
  TVpNotificationType = (neDateChange, neResourceChange, neDataStoreChange,
                         neInvalidate);

  { Printing events }
  TVpOnGetVariableEvent = procedure (Sender: TObject; VarName: string;
    Found: Boolean; var Value: string; var Change: TVpChangeVar)
    of object;

  TVpOnPageStartEvent = procedure (Sender: TObject; PageNum: Integer;
    ADate: TDateTime) of object;

  TVpOnPageEndEvent = procedure (Sender: TObject; PageNum: Integer;
    ADate: TDateTime; LastPage: Boolean) of object;

  { generic events }
  TVpControlNotifyEvent = procedure(Sender: TComponent;
    NotificationEvent: TVpNotificationType; const Value: Variant) of object;

  TVpNoResources = procedure(Sender: TObject;
    Resource: TVpResource) of object;

  TVpNoLocalizationFile = procedure (Sender: TObject;
    FileName: string) of object;

  TVpDateChangedEvent = procedure (Sender: TObject;                      
    Date: TDateTime) of object;                                          

  { contact events }
  TVpContactEvent = procedure(Sender: TObject; Contact: TVpContact) of object;

  TVpEditContactEvent = procedure(Sender: TObject; Contact: TVpContact;
    Resource: TVpResource; var AllowIt: Boolean) of object;              

  TVpOwnerDrawContactEvent = procedure(Sender: TObject; const Canvas: TCanvas;
    R: TRect; Contact: TVpContact; var Drawn: Boolean) of object;

  TVpCGColWidthChangeEvent = procedure(Sender: TObject;                  
    NewColWidth: Integer) of object;                                     

  { task events }
  TVpBeforeEditTask = procedure(Sender: TObject; Task: TVpTask;
    var AllowIt: Boolean) of object;

  TVpAfterEditTask = procedure(Sender: TObject; Task: TVpTask) of object;

  TVpEditTask = procedure(Sender: TObject; Task: TVpTask;
    Resource: TVpResource; var AllowIt: Boolean) of object;              

  TVpOwnerDrawTask = procedure(Sender: TObject; const Canvas: TCanvas;
    R: TRect; Task: TVpTask; var Drawn: Boolean) of object;

  { event events }
  TVpBeforeEditEvent = procedure(Sender: TObject; Event: TVpEvent;
    var AllowIt: Boolean) of object;

  TVpEventEvent = procedure(Sender: TObject; Event: TVpEvent) of object;

  TVpAfterEditEvent = procedure(Sender: TObject; Event: TVpEvent) of object;

  TVpEditEvent = procedure(Sender: TObject; Event: TVpEvent;
    Resource:TVpResource; var AllowIt: Boolean) of object;

  TVpOnAddNewEvent = procedure (Sender: TObject;                         
    Event: TVpEvent) of object;                                          

  { resource events }

  TVpResourceEvent = procedure(Sender: TObject;
    Resource: TVpResource) of object;

  { Is created by the control where dragging starts.  The Event property  }
  { holds a reference to the event being dragged, and the Sender contains }
  { a reference to the control where dragging started.                    }
  TVpEventDragObject = class({$IFDEF LCL}TDragObjectEx{$ELSE}TDragObject{$ENDIF})
  protected {private}
    FEvent: TVpEvent;
    FSender: TObject;
   {$IFDEF LCL}
    FDragTitle: string;
    FDragImages: TDragImageList;
    function GetDragImages: TDragImageList; override;
   {$ENDIF}
  public
   {$IFDEF LCL}
    constructor CreateWithDragImages(AControl: TControl; AHotspot: TPoint;
      ACellRect: TRect; const ADragTitle: string; const ATransparent: boolean);
    destructor Destroy; override;
    property DragTitle: string read FDragTitle;
   {$ENDIF}
    property Event: TVpEvent read FEvent write FEvent;
    property Sender: TObject read FSender write FSender;
  end;

  TVpResourceCombo = class(TCustomComboBox)
    protected {private}
      FDataStore: TVpCustomDataStore;
      {internal variables}
      rcLoading: Boolean;
      OldItemIndex: Integer;
      FResourceUpdateStyle: TVpResourceUpdate;

      procedure VpDataStoreChanged(var Msg: {$IFDEF DELPHI}TMessage{$ELSE}TLMessage{$ENDIF}); message Vp_DataStoreChanged;
      procedure SetDataStore(const Value: TVpCustomDataStore);
      function GetAbout: string;
      procedure SetAbout(const Value: string);
      procedure SetResourceUpdateStyle(const v: TVpResourceUpdate);
      procedure ResourceChanged(Sender: TObject);
      procedure LoadItems;
      {$IFNDEF LCL}
      procedure CNCommand (var Msg: TWMCommand); message CN_COMMAND;     
      {$ENDIF}

    public
      constructor Create (AOwner : TComponent); override;
      destructor Destroy; override;

     {$IFDEF LCL}
      property ChildSizing;
     {$ENDIF}

    published
      property DataStore: TVpCustomDataStore
        read FDataStore write SetDataStore;
      property ResourceUpdateStyle: TVpResourceUpdate
        read FResourceUpdateStyle write SetResourceUpdateStyle default ruOnChange;
      property Version: string
        read GetAbout write SetAbout stored False;

      property Align;
      property Anchors;
      property Constraints;
      property Style;
     {$IFDEF LCL}
      property Borderspacing;
     {$ENDIF}
  end;


  TVpDependentInfo = class { Used by the ControlLink component }
  protected{private}
    FComponent: Pointer;
    FEventHandler: TVpControlNotifyEvent;
  public
    property Component: Pointer read FComponent write FComponent;
    property EventHandler: TVpControlNotifyEvent
      read FEventHandler write FEventHandler;
  end;


  TVpCustomDataStore = class(TVpComponent)
  private
    FMediaFolder       : String;
    function IsStoredMediaFolder: Boolean;

  protected{private}
    FAutoCreate        : Boolean;
    FAutoConnect       : Boolean;
    FLoading           : Boolean;
    FCategoryColorMap  : TVpCategoryColorMap;
    FResources         : TVpResources;
    FTimeRange         : TVpTimeRange;
    FActiveDate        : TDateTime;
    FConnected         : Boolean;
    FEventTimerEnabled : Boolean;
    FPlayEventSounds   : Boolean;
    FDefaultEventSound : string;
    FDayBuffer         : Integer;
    FResourceID        : Integer;
    FResource          : TVpResource;
    dsAlertTimer       : TTimer;           { fires the alerts }
    FNotifiers         : TList;

    {events}
    FOnConnect         : TNotifyEvent;
    FOnDisconnect      : TNotifyEvent;
    FOnAlert           : TVpEventEvent;
    FOnResourceChange  : TVpResourceEvent;
    FOnDateChanged     : TVpDateChangedEvent;                            
    FOnPlaySound       : TVpPlaySoundEvent;

    procedure dsOnTimer(Sender: TObject);
    procedure dsDoOnAlert(Event: TVpEvent);
    procedure SetActiveDate(Value: TDateTime);
    procedure SetAutoConnect(Value: Boolean);
    procedure SetConnected(const Value: boolean); virtual;
    procedure SetResourceID(Value: Integer);
    procedure SetResource(Value: TVpResource);
    procedure SetEventTimerEnabled(Value: Boolean);
    procedure SetDayBuffer(Value: Integer);
    procedure SetRange(StartTime, EndTime: TDateTime);
    procedure NotifyLinked;

    property AutoConnect: Boolean
      read FAutoConnect write SetAutoConnect;
    property AutoCreate: Boolean
      read FAutoCreate write FAutoCreate;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DeregisterAllWatchers;
    procedure DeregisterWatcher (Watcher : THandle);
    function GetNextID(TableName: string): Integer; virtual; abstract;
    property Resources: TVpResources read FResources;
    procedure Load; virtual;
    procedure LoadEvents; virtual; abstract;
    procedure LoadContacts; virtual; abstract;
    procedure LoadTasks; virtual; abstract;
    procedure NotifyDependents;
    procedure RefreshEvents; virtual;
    procedure RefreshContacts; virtual;
    procedure RefreshTasks; virtual;
    procedure RefreshResource; virtual;
{ - Increased visibility to Public}
    procedure PurgeResource(Res: TVpResource); virtual; {abstract;}   
    procedure PurgeEvents(Res: TVpResource); virtual; {abstract;}     
    procedure PurgeContacts(Res: TVpResource); virtual; {abstract;}   
    procedure PurgeTasks(Res: TVpResource); virtual; {abstract;}      
{ - End}
    procedure SetResourceByName(Value: string); virtual; abstract;
    property Connected : boolean read FConnected write SetConnected;
    procedure PostEvents; virtual; abstract;
    procedure PostContacts; virtual; abstract;
    procedure PostTasks; virtual; abstract;
    procedure PostResources; virtual; abstract;
    procedure RegisterWatcher (Watcher : THandle);
    procedure PlaySound(const AWavFile: String; APlaySoundMode: TVpPlaySoundMode);
    property Loading : Boolean
      read FLoading write FLoading;
    property Resource: TVpResource
      read FResource write SetResource;
    property ResourceID: Integer
      read FResourceID write SetResourceID;
    property DayBuffer: Integer
      read FDayBuffer write SetDayBuffer;
    property Date: TDateTime
      read FActiveDate write SetActiveDate;
    property TimeRange: TVpTimeRange
      read FTimeRange;
  published
    property CategoryColorMap: TVpCategoryColorMap
      read FCategoryColorMap write FCategoryColorMap;
    property DefaultEventSound: string
      read FDefaultEventSound write FDefaultEventSound;
    property EnableEventTimer: Boolean
      read FEventTimerEnabled write SetEventTimerEnabled;
    property PlayEventSounds: Boolean
      read FPlayEventSounds write FPlayEventSounds;
    property MediaFolder: String
      read FMediaFolder write FMediaFolder stored IsStoredMediaFolder;
    {events}
    property OnAlert: TVpEventEvent
      read FOnAlert write FOnAlert;
    property OnConnect: TNotifyEvent
      read FOnConnect write FOnConnect;
    property OnDateChanged: TVpDateChangedEvent                          
      read FOnDateChanged write FOnDateChanged;                          
    property OnDisconnect: TNotifyEvent
      read FOnDisconnect write FOnDisconnect;
    property OnResourceChange: TVpResourceEvent
      read FOnResourceChange write FOnResourceChange;
    property OnPlaySound: TVpPlaySoundEvent
      read FOnPlaySound write FOnPlaySound;
  end;


  {TVpLinkableControl}
  TVpLinkableControl = class(TVpCustomControl)
  protected{private}
    FDataStore     : TVpCustomDataStore;
    FReadOnly      : Boolean;
    FControlLink   : TVpControlLink;
    FLastPrintLine : Integer;
    function CheckCreateResource : Boolean;                                   
    procedure SetDataStore (const Value : TVpCustomDataStore); virtual;
    procedure SetControlLink (const Value : TVpControlLink);
    procedure CMEnter(var Msg: {$IFDEF DELPHI}TMessage{$ELSE}TLMessage{$ENDIF}); message CM_ENTER;
    procedure CMExit(var Msg: {$IFDEF DELPHI}TMessage{$ELSE}TLMessage{$ENDIF}); message CM_EXIT;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetLastPrintLine: Integer;
    function GetControlType: TVpItemType; virtual; abstract;
    procedure RenderToCanvas(RenderCanvas: TCanvas; RenderIn: TRect;
      Angle: TVpRotationAngle; Scale: Extended; RenderDate: TDateTime;
      StartLine, StopLine: Integer; UseGran: TVpGranularity;
      DisplayOnly: Boolean); virtual; abstract;
    procedure LinkHandler(Sender: TComponent; NotificationType: TVpNotificationType;
      const Value: Variant); virtual; abstract;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
  published
    property PopupMenu;
    property DataStore: TVpCustomDataStore read FDataStore write SetDataStore;
    property ControlLink: TVpControlLink read FControlLink write SetControlLink;

    property Color;                                                      
    property Font;
    property ParentColor;                                                
    property ParentFont;                                                 
    property ParentShowHint;                                             

    property AfterEnter;                                                 
    property AfterExit;                                                  
    property OnMouseWheel;                                               

  end;


  {TVpControlLink}
  TVpControlLink = class(TVpComponent)
  private
    FPrinter          : TVpPrinter;
    FDataStore        : TVpCustomDataStore;
    FOnGetVariable    : TVpOnGetVariableEvent;
    FOnNoLocalizationFile : TVpNoLocalizationFile;                       
    FOnPageStart      : TVpOnPageStartEvent;
    FOnPageEnd        : TVpOnPageEndEvent;
    FLocalization     : TVpLocalization;
    FLocalizationFile : string;
    FDefaultCountry   : string;
    FCityStateZipFormat: String;
  protected{private}
    DependentList: TList;
    procedure Attach (Sender : TComponent);
    procedure Detach (Sender : TComponent);
    procedure ReleaseDependents;
    procedure SetCityStateZipFormat(const Value: String);
    procedure SetDataStore (const Value : TVpCustomDataStore);
    procedure SetDefaultCountry (const v : string);
    procedure SetLocalizationFile (const v : string);
    procedure SetPrinter (const v : TVpPrinter);
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDependentList : TList;
    procedure LoadLocalizationInfo (const FileName : string);
    procedure Notify (Sender: TComponent; NotificationType: TVpNotificationType; const Value: Variant);
    procedure TriggerOnGetVariable (Sender  : TObject; VarName: string; Found: Boolean;  var Value: string; var Change: TVpChangeVar);
    procedure TriggerOnPageEnd (Sender: TObject; PageNum: Integer; ADate: TDateTime; LastPage: Boolean);
    procedure TriggerOnPageStart (Sender: TObject; PageNum: Integer; ADate: TDateTime);
    property Localization : TVpLocalization read FLocalization write FLocalization;
  published
    property CityStateZipFormat: String read FCityStateZipFormat write SetCityStateZipFormat;
      // Use symbols @CITY, @STATE, @ZIP to define the order of these strings
    property DataStore: TVpCustomDataStore read FDataStore write SetDataStore;
    property DefaultCountry : string read FDefaultCountry write SetDefaultCountry;
    property LocalizationFile : string read FLocalizationFile write SetLocalizationFile;
    property Printer : TVpPrinter read FPrinter write SetPrinter;
    property OnGetVariable : TVpOnGetVariableEvent read FOnGetVariable write FOnGetVariable;
    property OnNoLocalizationFile : TVpNoLocalizationFile read FOnNoLocalizationFile write FOnNoLocalizationFile;
    property OnPageStart : TVpOnPageStartEvent read FOnPageStart write FOnPageStart;
    property OnPageEnd : TVpOnPageEndEvent read FOnPageEnd write FOnPageEnd;
  end;


implementation

uses
  VpSR, VpConst, VpMisc, VpResEditDlg, VpAlarmDlg,
{$IFDEF WINDOWS}
  mmSystem,
{$ENDIF}
  VpDlg, VpSelResDlg;

(*****************************************************************************)
{ TVpCustomDataStore }

constructor TVpCustomDataStore.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;

  FNotifiers := TList.Create;
  
  FAutoCreate := true;
  FResources := TVpResources.Create(Self);
  FTimeRange := TVpTimeRange.Create(Self);
  FCategoryColorMap := TVpCategoryColorMap.Create;
  FActiveDate := Now;                                                 
  FDayBuffer := 31;  {One full month before and after the current date. }
  FTimeRange.StartTime := Now - FDayBuffer;                           
  FTimeRange.EndTime := Now + FDayBuffer;                             

  FPlayEventSounds := true;

  FEventTimerEnabled := true;

  { Set Alert Timer }
  if not (csDesigning in ComponentState) then begin
    dsAlertTimer := TTimer.Create(self);
    dsAlertTimer.Enabled := false;
    { Create the event timer and allow it to fire within the next half second }
    dsAlertTimer.OnTimer := dsOnTimer;
    dsAlertTimer.Interval := 500;
  end;


  { If the DataStore is being dropped onto a form for the first time... }
  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    I := 0;
    { Auto connect to the first available ControlLink component found }
    while (I < Owner.ComponentCount) do begin
      if (Owner.Components[I] is TVpControlLink)
      and (TVpControlLink(Owner.Components[I]).DataStore = nil) then begin
        TVpControlLink(Owner.Components[I]).DataStore := self;
        Break;
      end;
      Inc(I);
    end;

    I := 0;
    { Then Auto connect to all available LinkableControl components found   }
    while (I < Owner.ComponentCount) do begin
      if (Owner.Components[I] is TVpLinkableControl) then begin
        if TVpLinkableControl(Owner.Components[I]).DataStore = nil then
          TVpLinkableControl(Owner.Components[I]).DataStore := self;
      end
      else if (Owner.Components[I] is TVpResourceCombo) then begin
        if TVpResourceCombo(Owner.Components[I]).DataStore = nil then
          TVpResourceCombo(Owner.Components[I]).DataStore := self;
      end
      else if (Owner.Components[I] is TVpBaseDialog) then begin
        if TVpBaseDialog(Owner.Components[I]).DataStore = nil then
          TVpBaseDialog(Owner.Components[I]).DataStore := self;
      end
      else if (Owner.Components[I] is TVpControlLink) then begin
        if TVpControlLink(Owner.Components[I]).DataStore = nil then
          TVpControlLink(Owner.Components[I]).DataStore := self;
      end;
      Inc(I);
    end;
  end;

  { enable the event timer }
  if not (csDesigning in ComponentState) then
    dsAlertTimer.Enabled := true;
end;
{=====}

destructor TVpCustomDataStore.Destroy;
var
  I: Integer;
begin
  DeregisterAllWatchers;
  FNotifiers.Free;
  FNotifiers := nil;
  
  { Remove self from all dependent controls }
  if Owner <> nil then begin
    I := 0;
    { Remove self from dependent Control Links first }
    while (I < Owner.ComponentCount) do begin
      if (Owner.Components[I] is TVpControlLink) then begin
        if TVpControlLink(Owner.Components[I]).DataStore = self then
          TVpControlLink(Owner.Components[I]).DataStore := nil;
      end;
      Inc(I);
    end;

    I := 0;
    { Then remove self from dependent controls }
    while (I < Owner.ComponentCount) do begin
      if (Owner.Components[I] is TVpLinkableControl) then begin
        if TVpLinkableControl(Owner.Components[I]).DataStore = self then
          TVpLinkableControl(Owner.Components[I]).DataStore := nil;
      end
      else if (Owner.Components[I] is TVpResourceCombo) then begin
        if TVpResourceCombo(Owner.Components[I]).DataStore = self then
          TVpResourceCombo(Owner.Components[I]).DataStore := nil;
      end
      else if (Owner.Components[I] is TVpBaseDialog) then begin
        if TVpBaseDialog(Owner.Components[I]).DataStore = self then
          TVpBaseDialog(Owner.Components[I]).DataStore := nil;
      end;
      Inc(I);
    end;
  end;

  FResources.Free;
  FTimeRange.Free;
  FCategoryColorMap.Free;

  if dsAlertTimer <> nil then
    dsAlertTimer.Free;

  inherited;
end;
{=====}

procedure TVpCustomDataStore.DeregisterAllWatchers;
var
  i : Integer;

begin
  for i := FNotifiers.Count - 1 downto 0 do
    if Assigned (FNotifiers[i]) then begin
      FreeMem (FNotifiers[i]);
      FNotifiers.Delete (i);
    end;
end;
{=====}

procedure TVpCustomDataStore.DeregisterWatcher (Watcher : THandle);
var
  i : Integer;

begin
  for i := FNotifiers.Count - 1 downto 0 do
    if Assigned (FNotifiers[i]) then
      if PVpWatcher (FNotifiers[i]).Handle = Watcher then begin
        FreeMem (FNotifiers[i]);
        FNotifiers.Delete (i);
        Exit;
      end;
end;
{=====}

procedure TVpCustomDataStore.dsOnTimer(Sender: TObject);
var
//  Hour, Min, Sec, MSec: Word;
  NHour, NMin, NSec, NMSec: Word;
  Event: TVpEvent;
  I: integer;
  AdvanceTime: TDateTime;
  AlarmTime: TDateTime;
begin
  { don't fire the timer at designtime }
  if csDesigning in ComponentState then begin
    dsAlertTimer.Enabled := false;
    Exit;
  end;

  if Resource <> nil then begin
    for I := 0 to pred(Resource.Schedule.EventCount) do begin
      Event := Resource.Schedule.GetEvent(I);

      if (Event <> nil) and Event.AlarmSet then begin
        AdvanceTime := GetAlarmAdvanceTime(Event.AlarmAdvance, Event.AlarmAdvanceType);
        AlarmTime := Event.StartTime - AdvanceTime;

        { if the AlarmTime has already passed, then show the alarm notification }
        if (AlarmTime < Now) then begin
          if Event.SnoozeTime < now then
            dsDoOnAlert(Event);
        end;

(* Simplified
        else begin
          { Check to see if the event comes due today before going further    }
          if (Trunc(AlarmTime) = Trunc(Now)) then begin
            DecodeTime(AlarmTime, Hour, Min, Sec, MSec);
            DecodeTime(Now, NHour, NMin, NSec, NMsec);
            if (Hour = NHour) and (Min = NMin) then begin
              { this event has come due so spawn the alert dialog             }
              dsDoOnAlert(Event);
            end;
          end; {if (Trunc(AlarmTime) = Trunc(Now))}
        end; {if (AlarmTime < Now)}
*)

      end; {if Event.AlarmSet}
    end; {for}
  end;

  { Set next interval }
  DecodeTime(Now, NHour, NMin, NSec, NMSec);
  dsAlertTimer.Interval := (60 - NSec) * 1000;
end;
{=====}

procedure TVpCustomDataStore.dsDoOnAlert(Event: TVpEvent);
begin
  if Event.AlertDisplayed then
    Exit;

  if Assigned(FOnAlert) then
    FOnAlert(Self, Event)
  else begin
    {Ding!}
    if FPlayEventSounds then begin
      if FileExists(Event.DingPath) then
        { if the event has a sound of its own, then play that one. }
        PlaySound(Event.DingPath, psmASync)
      else if FileExists(FDefaultEventSound) then
        { otherwise, if there is a default sound assigned, then play that one }
        PlaySound(FDefaultEventSound, psmASync)
      else
        { otherwise just ding }
        Beep;
    end;

    with TVpNotificationDialog.Create(nil) do
      try
        DataStore := Self;
        Execute(Event);
      finally
        Free;
      end;  { with }
  end; { if }
end;
{=====}

function TVpCustomDatastore.IsStoredMediaFolder: Boolean;
begin
  Result := FMediaFolder <> '';
end;

procedure TVpCustomDataStore.NotifyLinked;
var
  i : Integer;
  
begin
  for i := 0 to FNotifiers.Count - 1 do
    if Assigned (FNotifiers[i]) then
      PostMessage (PVpWatcher (FNotifiers[i]).Handle, Vp_DataStoreChanged, 0, 0);
end;
{=====}

procedure TVpCustomDataStore.SetActiveDate(Value: TDateTime);
var
  OY, OM, Day, NY, NM: Word;
  OldDate: TDateTime;
begin
  OldDate := FActiveDate;
  FActiveDate := Value;

  DecodeDate(OldDate, oy, om, Day);
  DecodeDate(FActiveDate, ny, nm, Day);

  { If the date has reached the end of the data buffer }
  if (FActiveDate >= FTimeRange.EndTime)
  or (FActiveDate <= FTimeRange.StartTime)
  { or the month or year has changed... }
  or (nm <> om) or (ny <> oy) then begin
  { then load the data that falls into the current time range }
    SetRange(FActiveDate - FDayBuffer, FActiveDate + FDayBuffer);
    RefreshEvents;
  end;

  if Assigned(FOnDateChanged) then                                       
    FOnDateChanged(Self, FActiveDate);                                   
end;
{=====}

procedure TVpCustomDataStore.SetAutoConnect(Value: Boolean);
begin
  if Value <> FAutoConnect then
    FAutoConnect := value;
end;
{=====}

procedure TVpCustomDataStore.SetConnected(const Value: boolean);
var
  WasConnected: Boolean;
begin
  WasConnected := FConnected;
  if Value <> FConnected then begin
    FConnected := Value;
    if not FConnected then begin
      FResources.ClearResources;
      FResource := nil;
      if WasConnected and Assigned(OnDisconnect) then
        FOnDisconnect(self);
    end
    else begin
      if not WasConnected and Assigned(OnConnect) then
        FOnConnect(self);
    end;

    if not (csDestroying in ComponentState) then 
      NotifyDependents;
  end;
end;
{=====}

procedure TVpCustomDataStore.SetResourceID(Value: Integer);
begin
  if (Value <> FResourceID) or (Value = 0) then begin
    FResource := FResources.GetResource(Value);
    if FResource = nil then
      Exit;
    FResourceID := Value;
    RefreshEvents;                                                     
    RefreshContacts;                                                   
    RefreshTasks;                                                      
    if Assigned(FOnResourceChange) then
      FOnResourceChange(Self, FResource);
    if not Loading then
      NotifyDependents;
  end;
end;
{=====}

procedure TVpCustomDataStore.SetResource(Value: TVpResource);
begin
  if Value <> FResource then begin
    FResource := Value;
    if FResource <> nil then begin                                     
      FResourceID := FResource.ResourceID;                             
      RefreshEvents;                                                   
      RefreshContacts;                                                 
      RefreshTasks;                                                    
    end else                                                           
      FResourceID := -1;
    if not Loading then
      NotifyDependents;
  end;
end;
{=====}

procedure TVpCustomDataStore.SetEventTimerEnabled(Value: Boolean);
begin
  if Value <> FEventTimerEnabled then begin
    FEventTimerEnabled := Value;
    if not (csDesigning in ComponentState) then begin
      if FEventTimerEnabled and (dsAlertTimer = nil) then
        dsAlertTimer := TTimer.Create(self);
      dsAlertTimer.Enabled := FEventTimerEnabled;
      dsAlertTimer.Interval := 500; { Make it fire within a half second }
    end;
  end;
end;
{=====}

procedure TVpCustomDataStore.SetDayBuffer(Value: Integer);
begin
  FDayBuffer := Value;
  SetRange(FActiveDate - FDayBuffer, FActiveDate + FDayBuffer);
end;
{=====}

procedure TVpCustomDataStore.Load;
begin
  FResources.Sort;
  NotifyDependents;
end;
{=====}

procedure TVpCustomDataStore.RefreshEvents;
begin
  if not Loading then
    NotifyDependents;
end;
{=====}

procedure TVpCustomDataStore.RefreshContacts;
begin
  if not Loading then
    NotifyDependents;
end;
{=====}

procedure TVpCustomDataStore.RefreshTasks;
begin
  if not Loading then
    NotifyDependents;
end;
{=====}

procedure TVpCustomDataStore.RefreshResource;
begin
  if not Loading then
    NotifyDependents;
end;
{=====}

{ - Added}
procedure TVpCustomDataStore.PurgeResource(Res: TVpResource);         
begin
  Unused(Res);
  if not Loading then                                                 
    NotifyDependents;                                                 
end;                                                                  
{=====}                                                               

procedure TVpCustomDataStore.PurgeEvents(Res: TVpResource);           
begin                                                                 
  Res.Schedule.ClearEvents;                                           
  if not Loading then                                                 
    NotifyDependents;                                                 
end;                                                                  
{=====}                                                               

procedure TVpCustomDataStore.PurgeContacts(Res: TVpResource);         
begin                                                                 
  Res.Contacts.ClearContacts;                                         
  if not Loading then                                                 
    NotifyDependents;                                                 
end;                                                                  
{=====}                                                               

procedure TVpCustomDataStore.PurgeTasks(Res: TVpResource);            
begin                                                                 
  Res.Tasks.ClearTasks;                                               
  if not Loading then                                                 
    NotifyDependents;                                                 
end;                                                                  
{=====}                                                               
{ - End}

procedure TVpCustomDataStore.RegisterWatcher (Watcher : THandle);
var
  i          : Integer;
  NewHandle  : PVpWatcher;

begin
  for i := 0 to FNotifiers.Count - 1 do
    if Assigned (FNotifiers[i]) then
      if PVpWatcher (FNotifiers[i]).Handle = Watcher then
        Exit;
  GetMem (NewHandle, SizeOf (TVpWatcher));
  NewHandle.Handle := Watcher;
  FNotifiers.Add (NewHandle);
end;
{=====}

procedure TVpCustomDataStore.NotifyDependents;
var
  I: Integer;
begin
  if (Owner = nil) or Loading then                                    
    Exit;

  for I := 0 to pred(Owner.ComponentCount) do begin
    if (Owner.Components[I] is TVpLinkableControl) then begin
      if (TVpLinkableControl(Owner.Components[I]).DataStore = self) then
        TVpLinkableControl(Owner.Components[I]).Invalidate;
    end
  end;
  NotifyLinked;
end;
{=====}

procedure TVpCustomDataStore.SetRange(StartTime, EndTime: TDateTime);
begin
  if EndTime > StartTime then begin
    { Force the startdate's time to 12:00 am }
    FTimeRange.StartTime := trunc(StartTime);
    { Force the enddate's time to midnight   }
    FTimeRange.EndTime := trunc(EndTime) + 1;
  end;
end;
{=====}

procedure TVpCustomDatastore.PlaySound(const AWavFile: String;
  APlaySoundMode: TVpPlaySoundMode);
begin
  if Assigned(FOnPlaySound) then
    FOnPlaySound(Self, AWavFile, APlaySoundMode)
  else begin
   {$IFDEF WINDOWS}
    case APlaySoundMode of
      psmSync  : SndPlaySound(PChar(AWavFile), SND_SYNC);
      psmAsync : SndPlaySound(PChar(AWavFile), SND_ASYNC);
      psmStop  : SndPlaySound(nil, 0);
    end;
   {$ENDIF}
  end;
end;
{=====}


{ TVpEventDragObject }

function TVpEventDragObject.GetDragImages: TDragImageList;
begin
  Result := FDragImages;
end;

constructor TVpEventDragObject.CreateWithDragImages(AControl: TControl;
  AHotspot: TPoint; ACellRect: TRect; const ADragTitle: string;
  const ATransparent: boolean);
const
  OffsX = 0;
  OffsY = 0;
var
  bmp: TBitmap;
  bmpIdx: Integer;
  R: TRect;
begin
  Create(AControl);
  FDragTitle := ADragTitle;
  bmp := TBitmap.Create;
  try
//    bmp.Canvas.Font.Name := 'Arial';
    bmp.Canvas.Font.Style := Bmp.Canvas.Font.Style + [fsItalic];
    bmp.Height := ACellRect.Bottom - ACellRect.Top;
    bmp.Width := ACellRect.Right - ACellRect.Left;
    R := bmp.Canvas.ClipRect;
    if ATransparent
      then bmp.Canvas.Brush.Color := clOlive
      else bmp.Canvas.Brush.Color := clSilver;
    bmp.Canvas.FillRect(R);
    bmp.Canvas.TextOut(OffsX, OffsY, FDragTitle);

    // if a real picture is needed ...
    //if AControl is TWinControl then
    //    (AControl as TWinControl).PaintTo(Bmp.Canvas, 0, 0);

    FDragImages := TDragImageList.Create(AControl);
    AlwaysShowDragImages := True;
    FDragImages.Width := bmp.Width;
    FDragImages.Height := bmp.Height;
    if ATransparent
      then bmpIdx := FDragImages.AddMasked(bmp, clOlive)
      else bmpIdx := FDragImages.Add(bmp, nil);
    FDragImages.SetDragImage(bmpIdx, AHotspot.X, AHotspot.Y);
  finally
    Bmp.Free;
  end;
end;

destructor TVpEventDragObject.Destroy;
begin
  if (Assigned(FDragImages)) then FDragImages.Free;
  inherited Destroy;
end;


{ TVpResourceCombo }

constructor TVpResourceCombo.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;

  OnChange := ResourceChanged;

  FResourceUpdateStyle := ruOnChange;                                    

  Style := csDropDownList;

  DoubleBuffered := true;

  { If the ResourceCombo is being dropped onto a form for the first }
  { time then connect to the first DataStore component found.       }
  I := 0;
  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    while (I < Owner.ComponentCount) and (DataStore = nil) do
      if (Owner.Components[I] is TVpCustomDataStore) then
        DataStore := TVpCustomDataStore(Owner.Components[I])
      else
        Inc(I);
end;

destructor TVpResourceCombo.Destroy;
begin
  inherited;
end;
{=====}

{$IFNDEF LCL}
procedure TVpResourceCombo.CNCommand (var Msg: TWMCommand);              
begin                                                                    
  if Msg.NotifyCode = CBN_CLOSEUP then begin                             
    if (FResourceUpdateStyle = ruOnDropDownClose) then                   
      ResourceChanged (Self)                                             
    else                                                                 
      inherited;                                                         
  end else                                                               
    inherited;                                                           
end;
{$ENDIF}
{=====}

procedure TVpResourceCombo.VpDataStoreChanged(var Msg: {$IFDEF DELPHI}TMessage{$ELSE}TLMessage{$ENDIF});
begin
  Unused(Msg);
  LoadItems;
end;
{=====}

function TVpResourceCombo.GetAbout: string;
begin
  Result := VpVersionStr;
end;
{=====}

procedure TVpResourceCombo.LoadItems;
var
  I: Integer;
  Res: TVpResource;
begin
  if DataStore = nil then
    Exit;

  rcLoading := true;
  try
    Items.Clear;

    for I := 0 to pred(DataStore.Resources.Count) do begin
      Res := DataStore.Resources.Items[I];
      if Res = nil then
        Continue;
      if Res.Description <> '' then
        Items.Add(Res.Description);
    end;

    if DataStore.Resource = nil then
      ItemIndex := -1
    else
      ItemIndex := Items.IndexOf(DataStore.Resource.Description);

  finally
    rcLoading := false;
  end;
end;
{=====}

procedure TVpResourceCombo.ResourceChanged(Sender: TObject);
begin
  if (OldItemIndex <> ItemIndex) or (ItemIndex = 0) then begin
    if (DataStore <> nil) and not rcLoading then
      DataStore.SetResourceByName(Text);
    OldItemIndex := ItemIndex;
  end;
end;
{=====}

procedure TVpResourceCombo.SetAbout(const Value: string);
begin
  Unused(Value);
  //Empty on purpose
end;
{=====}
procedure TVpResourceCombo.SetResourceUpdateStyle (                      
              const v : TVpResourceUpdate);                              
begin                                                                    
  if v <> FResourceUpdateStyle then begin                                
    FResourceUpdateStyle := v;                                           
    case FResourceUpdateStyle of                                         
      ruOnChange  : begin                                                
        OnChange := ResourceChanged;                                     
        OnExit   := nil;                                                 
      end;                                                               
      ruOnExit : begin                                                   
        OnChange := nil;                                                 
        OnExit   := ResourceChanged;                                     
      end;                                                               
      ruOnDropDownClose : begin                                          
        OnChange := nil;                                                 
        OnExit   := nil;                                                 
      end;                                                               
    end;                                                                 
  end;                                                                   
end;                                                                     
{=====}

procedure TVpResourceCombo.SetDataStore(const Value: TVpCustomDataStore);
begin
  if FDataStore <> Value then begin
    if (Assigned (FDataStore)) and
       (not (csDesigning in ComponentState)) then
      FDataStore.DeregisterWatcher (Handle);
    FDataStore := Value;
    if (Assigned (FDataStore)) and
       (not (csDesigning in ComponentState)) then
      FDataStore.RegisterWatcher (Handle);
    if not (csDesigning in ComponentState) then
      LoadItems;
    Invalidate;
  end;
end;
{=====}



{ TVpLinkableControl }

constructor TVpLinkableControl.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  { If the control is being dropped onto a form for the first time then }
  { Auto connect to the first ControlLink component found               }
  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    I := 0;
    while (I < Owner.ComponentCount) and (ControlLink = nil) do begin
      if (Owner.Components[I] is TVpControlLink) then
        ControlLink := TVpControlLink(Owner.Components[I]);
      Inc(I);
    end;
  end;
  FLastPrintLine := -1;
end;
{=====}

destructor TVpLinkableControl.Destroy;
begin
  if ControlLink <> nil then
    ControlLink.Detach(Self);
  inherited;
end;
{=====}

function TVpLinkableControl.CheckCreateResource : Boolean;             
var                                                                    
  ResEdit           : TVpResourceEditDialog;                           
  frmSelectResource : TfrmSelectResource;

begin                                                                  
  Result := False;                                                     
  if not Assigned (DataStore) then                                     
    Exit;                                                              
  if not Assigned (DataStore.Resource) then begin                      
    if DataStore.Resources.Count > 0 then begin                        
      { No resource is selected, select one }                          
      if MessageDlg (RSSelectResource, mtConfirmation,                 
                     [mbYes, mbNo], 0) = mrYes then begin              
        frmSelectResource := TfrmSelectResource.Create (Self);         
        try                                                            
          frmSelectResource.VpResourceCombo1.DataStore := DataStore;   
          frmSelectResource.VpResourceEditDialog1.DataStore := DataStore; 
          if frmSelectResource.ShowModal = mrOk then begin             
            Result := True;                                            
          end else                                                     
            Exit;                                                                
        finally                                                        
          frmSelectResource.Free;                                      
        end;                                                           
      end else                                                         
        Exit;                                                          
    end else begin                                                     
      { There are no resources at all, add one }                       
      if MessageDlg (RSAddNewResource, mtConfirmation,                 
                     [mbYes, mbNo], 0) = mrYes then begin              
        ResEdit := TVpResourceEditDialog.Create (Self);                
        try                                                            
          ResEdit.DataStore := DataStore;                              
          Result := ResEdit.AddNewResource;                            
          Exit;                                                        
        finally                                                        
          ResEdit.Free;                                                
        end;                                                           
      end else                                                         
        Exit;                                                          
    end;                                                               
  end else                                                             
    Result := True;                                                    
end;

{=====}
function TVpLinkableControl.GetLastPrintLine : Integer;
begin
  Result := FLastPrintLine;
end;
{=====}
procedure TVpLinkableControl.SetDataStore(const Value: TVpCustomDataStore);
begin
  if Value = nil then begin
    FDataStore := nil;
    Invalidate;
  end else if FDataStore <> Value then begin
    FDataStore := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpLinkableControl.SetControlLink(const Value: TVpControlLink);
var
  CL: TVpControlLink;
begin
  if (FControlLink <> Value) then begin

    if (FControlLink <> nil) then begin
      { FControlLink is currently set to a ControlLink component }
      { save old control link value }
      CL := FControlLink;

      if (Value = nil) then begin
        { We are detaching ourself from the control link and not replacing }
        { it with another one.                                             }
        FControlLink := nil;
        CL.Detach(Self);
      end;

      if (Value <> nil) then begin
        { We are replacing the current ControlLink with another one }
        FControlLink := nil;
        CL.Detach(Self);
        FControlLink := Value;
        FControlLink.Attach(Self);
      end;
    end else begin
      { FControlLink was nil and is now being set }
      FControlLink := Value;
      FControlLink.Attach(Self);
    end;
  end;
end;
{=====}

procedure TVpLinkableControl.CMEnter(var Msg: {$IFDEF DELPHI}TMessage{$ELSE}TLMessage{$ENDIF});
begin
  Unused(Msg);
  invalidate;
end;
{=====}

procedure TVpLinkableControl.CMExit(var Msg: {$IFDEF DELPHI}TMessage{$ELSE}TLMessage{$ENDIF});
begin
  Unused(Msg);
  invalidate;
end;
{=====}


{ TVpControlLink }

constructor TVpControlLink.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;

  DependentList := TList.Create;

  { If the ControlLink is being dropped onto a form for the first time ... }
  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    { Auto connect to the first DataStore found }
    I := 0;
    while (I < Owner.ComponentCount) and (DataStore = nil) do begin
      if (Owner.Components[I] is TVpCustomDataStore) then
          DataStore := TVpCustomDataStore(Owner.Components[I]);
      Inc(I);
    end;

    { Then auto connect to all available LinkableControl components found.}
    I := 0;
    while (I < Owner.ComponentCount) do begin
      if (Owner.Components[I] is TVpLinkableControl) then begin
        if (TVpLinkableControl(Owner.Components[I]).ControlLink = nil)
        and (TVpLinkableControl(Owner.Components[I]).DataStore = DataStore)
        then
          TVpLinkableControl(Owner.Components[I]).ControlLink := self;
      end;
      Inc(I);
    end;
  end;

  FPrinter := TVpPrinter.Create (Self);
  FLocalization := TVpLocalization.Create;
end;
{=====}

destructor TVpControlLink.Destroy;
var
  I: Integer;
begin
  FPrinter.Free;
  FPrinter := nil;

  FLocalization.Free;
  FLocalization := nil;

  { Detach self from any dependent controls }
  if Owner <> nil then begin
    I := 0;
    while (I < Owner.ComponentCount) do begin
      if (Owner.Components[I] is TVpLinkableControl)
      and (TVpLinkableControl(Owner.Components[I]).ControlLink = self) then
        Detach(TVpLinkableControl(Owner.Components[I]));
      Inc(I);
    end;
  end;

  ReleaseDependents;
  DependentList.Free;
  inherited;
end;
{=====}

procedure TVpControlLink.ReleaseDependents;
var
  I : Integer;
begin
  for I := pred(DependentList.Count) downto 0 do
    Detach(TVpDependentInfo(DependentList[i]).Component);
//  for I := 0 to pred(DependentList.Count) do
//    Detach(TVpDependentInfo(DependentList.List^[I]).Component);
end;
{=====}

procedure TVpControlLink.Detach(Sender: TComponent);
var
  I: Integer;
begin
  try                                                                    
    for I := 0 to pred(DependentList.Count) do
//      if TVpDependentInfo(DependentList.List^[I]).Component = Sender then
      if TVpDependentInfo(DependentList[I]).Component = Sender then
      begin
//        TVpDependentInfo(DependentList.List^[I]).Free;
        TVpDependentInfo(DependentList[I]).Free;
        DependentList.Delete(I);
        if Sender is TVpLinkableControl then
          TVpLinkableControl(Sender).ControlLink := nil;
        Exit;
      end;
  except                                                                 
    // swallow exceptions                                                
  end;                                                                   
end;
{=====}

procedure TVpControlLink.Attach(Sender: TComponent);
var
  I : Integer;
  Exists: Boolean;
  Link: TVpDependentInfo;
begin
  Exists := false;
  for I := 0 to pred(DependentList.Count) do
//    if TVpDependentInfo(DependentList.List^[I]).Component = Sender then begin
    if TVpDependentInfo(DependentList[I]).Component = Sender then begin
      Exists := true;
      Break;
    end;

  if not Exists then begin
    Link := TVpDependentInfo.Create;
    Link.Component := Sender;
    if Sender is TVpLinkableCOntrol then
      Link.EventHandler := TVpLinkableControl(Sender).LinkHandler;
    DependentList.Add(Link);
  end;
end;
{=====}

function TVpControlLink.GetDependentList : TList;
begin
  Result := DependentList;
end;
{=====}

procedure TVpControlLink.LoadLocalizationInfo (const FileName : string);
begin
  LocalizationFile := FileName;
end;
{=====}

procedure TVpControlLink.Notify(Sender: TComponent;
  NotificationType: TVpNotificationType; const Value: Variant);
var
  I : Integer;
begin
  for I := 0 to pred(DependentList.Count) do begin
//    with TVpDependentInfo(DependentList.List^[I]) do begin
    with TVpDependentInfo(DependentList[I]) do begin
      if Component <> Sender then
        EventHandler(Sender, NotificationType, Value);
      end;
    end;
end;
{=====}

procedure TVpControlLink.SetCityStateZipFormat(const Value: String);
begin
  if FCityStateZipFormat <> Value then begin
    FCityStateZipFormat := Value;
    Notify(self, neInvalidate, 0);
  end;
end;

procedure TVpControlLink.SetDataStore(const Value: TVpCustomDataStore);
begin
  if FDataStore <> Value then
    FDataStore := Value;
end;
{=====}
procedure TVpControlLink.SetDefaultCountry (const v : string);
begin
  if v <> FDefaultCountry then begin
    FDefaultCountry := v;
  end;
end;
{=====}

procedure TVpControlLink.SetLocalizationFile (const v : string);
var
  fn: String;
begin
  if v <> FLocalizationFile then begin
    FLocalizationFile := v;
    if (FLocalizationFile <> '') and not (csDesigning in ComponentState) then
    begin
      fn := ExpandFilename(v);
      if not FileExists(fn) then begin
        if Assigned(FOnNoLocalizationFile) then
          FOnNoLocalizationFile(Self, fn);
      end else                                                                                                                             
        FLocalization.LoadFromFile(fn, False);
    end;                                                                 
  end;
end;
{=====}

procedure TVpControlLink.SetPrinter (const v : TVpPrinter);
begin
  FPrinter.Assign (v);
end;
{=====}

procedure TVpControlLink.TriggerOnGetVariable(Sender  : TObject;
  VarName: string;  Found: Boolean; var Value: string;
  var Change: TVpChangeVar);
begin
  if Assigned (FOnGetVariable) then
    FOnGetVariable (Sender, VarName, Found, Value, Change);
end;
{=====}

procedure TVpControlLink.TriggerOnPageEnd(Sender: TObject; PageNum: Integer;
  ADate: TDateTime;  LastPage: Boolean);
begin
  if Assigned (FOnPageEnd) then
    FOnPageEnd (Sender, PageNum, ADate, LastPage);
end;
{=====}

procedure TVpControlLink.TriggerOnPageStart(Sender: TObject; PageNum : Integer;
  ADate   : TDateTime);
begin
  if Assigned (FOnPageStart) then
    FOnPageStart (Sender, PageNum, ADate);
end;
{=====}

end.
