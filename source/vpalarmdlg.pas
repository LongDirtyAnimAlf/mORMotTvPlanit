{*********************************************************}
{*                  VPALARMDLG.PAS 1.03                  *}
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

unit VpAlarmDlg;
  { Alarm Notification Dialog }

interface

uses
  {$IFDEF LCL}
  LCLProc, LCLType, LCLIntf, LResources,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VpDlg, VpData, ExtCtrls, StdCtrls, VpEvntEditDlg, VpBaseDS, VpConst;

type
  { forward declarations }
  TVpNotificationDialog = class;

  { TAlarmNotifyForm }

  TAlarmNotifyForm = class(TForm)
    DismissBtn: TButton;
    EventDialog: TVpEventEditDialog;
    lTime: TLabel;
    lSubject: TLabel;
    lNotes: TLabel;
    OpenItemBtn: TButton;
    Panel1: TPanel;
    SnoozeBtn: TButton;
    SnoozeCaption: TLabel;
    SnoozeCombo: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure SnoozeComboChange(Sender: TObject);
    procedure SnoozeBtnClick(Sender: TObject);
    procedure DismissBtnClick(Sender: TObject);
    procedure OpenItemBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    SnoozeDelay: TDateTime;
    ShowTime   : TDateTime;

    procedure CalcSnooze;
  public
    Event: TVpEvent;
    DataStore: TVpCustomDataStore;
    procedure PopulateSelf;
  end;

  TVpNotificationDialog = class(TVpBaseDialog)
  protected {private}
    FBGColor   : TColor;
    ceEditDlg  : TAlarmNotifyForm;
    ceTask     : TVpTask;
  public
    constructor Create(AOwner : TComponent); override;
    procedure Execute(Event: TVpEvent); reintroduce;
  published
    {properties}
    property BackgroundColor: TColor
      read FBGColor write FBGColor default clInfoBk;
    property DataStore;
    property Placement;
  end;

implementation

{$IFDEF LCL}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

uses
  StrUtils, VpMisc, VpSR;

{ TVpNotificationDialog }

constructor TVpNotificationDialog.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FBGColor := clWindow;
  FPlacement.Position := mpCustom;
  FPlacement.Width := 412; 
end;
{=====}


procedure TVpNotificationDialog.Execute(Event: TVpEvent);
var
  AlarmNotifyForm: TAlarmNotifyForm;
begin
  if (Event <> nil) and (not Event.AlertDisplayed) then begin
   AlarmNotifyForm := TAlarmNotifyForm.Create(Self);
    try
      try
        Event.AlertDisplayed := true;
        DoFormPlacement(AlarmNotifyForm);
        AlarmNotifyForm.Color := BackgroundColor;
        AlarmNotifyForm.DataStore := DataStore;
        AlarmNotifyForm.Event := Event;
        AlarmNotifyForm.PopulateSelf;
        AlarmNotifyForm.ShowModal;
      finally
        Event.AlertDisplayed := false;
      end;
      if Event.Changed then
        DataStore.PostEvents;
    finally
      AlarmNotifyForm.Release;
    end;
  end;
end;
{=====}

{ TAlarmNotifyForm }

procedure TAlarmNotifyForm.PopulateSelf;
var
  fmt: String;
begin
  if Event <> nil then begin
//    SubjectCaption.Caption := RSSubjectCaption;
//    NotesCaption.Caption := RSNotesCaption;
    SnoozeCaption.Caption := RSSnoozeCaption;
    DismissBtn.Caption := RSDismissBtn;
    SnoozeBtn.Caption := RSSnoozeBtn;
    OpenItemBtn.Caption := RSOpenItemBtn;
    lNotes.Caption := Event.Notes;
    lSubject.Caption := Event.Description;

    fmt := IfThen(trunc(Event.StartTime) = Date(), 't', 'ddddd t');
    lTime.Caption := Format('%s - %s', [
      FormatDateTime(fmt, Event.StartTime),
      FormatDateTime(fmt, Event.EndTime)] );
    if Event.Location <> '' then
      lTime.Caption := lTime.Caption + ' (' + Event.Location + ')';

    Caption := Format('%s : %s', [
      IfThen(Now > Event.StartTime, RSOverdue, RSReminder),
      FormatDateTime(fmt, Event.StartTime) ]);

    SnoozeCombo.Items.Clear;
    SnoozeCombo.Items.Add(RS1Minute);
    SnoozeCombo.Items.Add(Format(RSXMinutes, [5]));
    SnoozeCombo.Items.Add(Format(RSXMinutes, [10]));
    SnoozeCombo.Items.Add(Format(RSXMinutes, [15]));
    SnoozeCombo.Items.Add(Format(RSXMinutes, [30]));
    SnoozeCombo.Items.Add(Format(RSXMinutes, [45]));
    SnoozeCombo.Items.Add(RS1Hour);
    SnoozeCombo.Items.Add(Format(RSXHours, [2]));
    SnoozeCombo.Items.Add(Format(RSXHours, [3]));
    SnoozeCombo.Items.Add(Format(RSXHours, [4]));
    SnoozeCombo.Items.Add(Format(RSXHours, [5]));
    SnoozeCombo.Items.Add(Format(RSXHours, [6]));
    SnoozeCombo.Items.Add(Format(RSXHours, [7]));
    SnoozeCombo.Items.Add(Format(RSXHours, [8]));
    SnoozeCombo.Items.Add(RS1Day);
    SnoozeCombo.Items.Add(Format(RSXDays, [2]));
    SnoozeCombo.Items.Add(Format(RSXDays, [3]));
    SnoozeCombo.Items.Add(Format(RSXDays, [4]));
    SnoozeCombo.Items.Add(Format(RSXDays, [5]));
    SnoozeCombo.Items.Add(Format(RSXDays, [6]));
    SnoozeCombo.Items.Add(RS1Week);
    SnoozeCombo.ItemIndex := 0;
    SnoozeDelay := 5 / MinutesInDay;
    ShowTime := Now;
  end;
end;
{=====}


procedure TAlarmNotifyForm.SnoozeComboChange(Sender: TObject);
begin
  case SnoozeCombo.ItemIndex of
    0 : SnoozeDelay :=  1  / MinutesInDay; { 1 minute  }
    1 : SnoozeDelay :=  5  / MinutesInDay; { 5 minutes }
    2 : SnoozeDelay := 10  / MinutesInDay; {10 Minutes }
    3 : SnoozeDelay := 15  / MinutesInDay; {15 Minutes }
    4 : SnoozeDelay := 30  / MinutesInDay; {30 Minutes }
    5 : SnoozeDelay := 45  / MinutesInDay; {45 Minutes }
    6 : SnoozeDelay := 60  / MinutesInDay; {1 Hour     }
    7 : SnoozeDelay := 120 / MinutesInDay; {2 Hours    }
    8 : SnoozeDelay := 180 / MinutesInDay; {3 Hours    }
    9 : SnoozeDelay := 240 / MinutesInDay; {4 Hours    }
    10: SnoozeDelay := 300 / MinutesInDay; {5 Hours    }
    11: SnoozeDelay := 360 / MinutesInDay; {6 Hours    }
    12: SnoozeDelay := 420 / MinutesInDay; {7 Hours    }
    13: SnoozeDelay := 480 / MinutesInDay; {8 Hours    }
    14: SnoozeDelay := 1.0;                {1 day      }
    15: SnoozeDelay := 2.0;                {2 day      }
    16: SnoozeDelay := 3.0;                {3 day      }
    17: SnoozeDelay := 4.0;                {4 day      }
    18: SnoozeDelay := 5.0;                {5 day      }
    19: SnoozeDelay := 6.0;                {6 day      }
    20: SnoozeDelay := 7.0;                {1 week     }
  end;
end;
{=====}

procedure TAlarmNotifyForm.SnoozeBtnClick(Sender: TObject);
begin
  CalcSnooze;
  Close;
end;
{=====}

procedure TAlarmNotifyForm.DismissBtnClick(Sender: TObject);
var
  t0: TTime;
begin
  if Event.RepeatCode = rtNone then
    begin
      Event.AlarmSet := false
    end
  else
    begin
      SnoozeDelay := 0;
      t0 := Trunc(Now) + frac(Event.StartTime) - EncodeTime(0, Event.AlarmAdvance, 0, 0);
      case Event.RepeatCode of
      rtDaily:
        Event.SnoozeTime := t0 + 1; //Trunc(Now)+1+(Frac(Event.StartTime)-EncodeTime(0,Event.AlarmAdvance,0,0));
      rtWeekly:
        Event.SnoozeTime := t0 + 7; //Trunc(Now)+7+(Frac(Event.StartTime)-EncodeTime(0,Event.AlarmAdvance,0,0));
//TODO:      rtMonthlyByDay:
//TODO:      rtMonthlyByDate:
//TODO:      rtYearlyByDay:
//TODO:      rtYearlyByDate:
//TODO:      rtCustom:
      else
        Event.AlarmSet := false
      end;
    end;
  Close;
end;

procedure TAlarmNotifyForm.FormCreate(Sender: TObject);
begin
  SnoozeCombo.Top := SnoozeBtn.Top + (SnoozeBtn.Height - SnoozeCombo.Height) div 2;
end;

{=====}

procedure TAlarmNotifyForm.OpenItemBtnClick(Sender: TObject);
begin
  Self.Hide;
  EventDialog.DataStore := DataStore;
  EventDialog.Execute(Event);
  Close;
end;
{=====}

procedure TAlarmNotifyForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Unused(Shift);
  if Key = VK_ESCAPE then begin
    CalcSnooze;
    Close;
  end;
end;
{=====}

procedure TAlarmNotifyForm.CalcSnooze;
begin
  Event.SnoozeTime := Now + SnoozeDelay;
end;
{=====}

procedure TAlarmNotifyForm.FormShow(Sender: TObject);
begin
  Self.Width := 410;
  Self.Height := 210;
  OpenItemBtn.SetFocus;
end;

end.
 
