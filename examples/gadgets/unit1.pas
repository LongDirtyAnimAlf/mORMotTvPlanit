unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, VpLEDLabel, VpClock;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnStartStop: TButton;
    CbNewClockFace: TCheckBox;
    CbMilitaryTime: TCheckBox;
    EdCountDownTime: TEdit;
    LblCountDownTime: TLabel;
    LblElapsedTime: TLabel;
    RgDisplayMode: TRadioGroup;
    RgClockMode: TRadioGroup;
    VpClock1: TVpClock;
    VpLEDLabel1: TVpLEDLabel;
    procedure AnalogClockCountdownDone(Sender: TObject);
    procedure CbMilitaryTimeChange(Sender: TObject);
    procedure VpClockTimeChange(Sender: TObject);
    procedure BtnStartStopClick(Sender: TObject);
    procedure CbNewClockFaceChange(Sender: TObject);
    procedure EdCountDownTimeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RgClockModeClick(Sender: TObject);
    procedure RgDisplayModeClick(Sender: TObject);
    procedure VpClock1CountdownDone(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.AnalogClockCountdownDone(Sender: TObject);
begin
  ShowMessage('Countdown finished.');
end;

procedure TForm1.VpClockTimeChange(Sender: TObject);
begin
  LblElapsedTime.Caption := Format('Elapsed: %d hrs, %d min, %d sec', [
    VpClock1.ElapsedHours, VpClock1.ElapsedMinutes, VpClock1.ElapsedSeconds
  ]);
end;

procedure TForm1.BtnStartStopClick(Sender: TObject);
var
  isStarted: Boolean;
  willStart: Boolean;
begin
  isStarted := VpClock1.Active;
  willStart := not isStarted;

  if willStart and (RgClockMode.ItemIndex = ord(cmCountdownTimer)) then
    RgClockModeClick(nil);

  VpClock1.Active := willStart;
  if VpClock1.Active then
    BtnStartStop.Caption := 'Stop' else
    BtnStartStop.Caption := 'Start';
end;

procedure TForm1.CbMilitaryTimeChange(Sender: TObject);
var
  t: TDateTime;
begin
  t := VpClock1.Time;
  VpClock1.DigitalOptions.MilitaryTime := CbMilitaryTime.Checked;
  VpClock1.Time := t;
end;

procedure TForm1.CbNewClockFaceChange(Sender: TObject);
begin
  if CbNewClockFace.Checked then begin
    VpClock1.AnalogOptions.ClockFace.LoadFromFile('clockface.bmp');
    VpClock1.AnalogOptions.HourHandWidth := 2;
    VpClock1.AnalogOptions.MinuteHandWidth := 2;
    VpClock1.AnalogOptions.SecondHandWidth := 1;
    VpClock1.Width := 100;
    VpClock1.Height := 100;
  end else begin
    VpClock1.AnalogOptions.ClockFace := nil;
    VpClock1.AnalogOptions.HourHandWidth := 4;
    VpClock1.AnalogOptions.MinuteHandWidth := 3;
    VpClock1.AnalogOptions.SecondHandWidth := 1;
    VpClock1.Width := 200;
    VpClock1.Height := 200;
  end;
  VpClock1.AnalogOptions.DrawMarks := not CbNewClockFace.Checked;
  if RgDisplayMode.ItemIndex = ord(dmAnalog) then
    VpClock1.Invalidate;
end;

procedure TForm1.EdCountDownTimeChange(Sender: TObject);
var
  t: TTime;
begin
  if VpClock1.ClockMode = cmCountDownTimer then
    if TryStrToTime(EdCountdownTime.Text, t) then
      VpClock1.Time := t;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CbMilitaryTime.Top := CbNewClockFace.Top;
end;

procedure TForm1.RgClockModeClick(Sender: TObject);
var
  h,m,s,ms: Word;
begin
  VpClock1.Active := false;
  BtnStartStop.Caption := 'Start';
  VpClock1.ClockMode := TVpClockMode(RgClockMode.ItemIndex);
  case VpClock1.ClockMode of
    cmClock:
      begin
        VpClock1.Time := now;
        VpClock1.Active := true;
        BtnStartStop.Caption := 'Stop';
      end;
    cmTimer:
      VpClock1.Time := 0;
    cmCountdownTimer:
      begin
        DecodeTime(StrToTime(EdCountDownTime.Text), h,m,s,ms);
        VpClock1.HourOffset := h;
        VpClock1.MinuteOffset := m;
        VpClock1.SecondOffset := s;
      end;
  end;
  EdCountDownTime.Visible := VpClock1.ClockMode = cmCountDownTimer;
  LblCountDownTime.Visible := EdCountDownTime.Visible;
end;

procedure TForm1.RgDisplayModeClick(Sender: TObject);
var
  t: TDateTime;
begin
  t := VpClock1.Time;
  VpClock1.DisplayMode := TVpClockDisplayMode(RgDisplayMode.ItemIndex);
  case VpClock1.DisplayMode of
    dmAnalog:
      CbNewClockFaceChange(nil);
    dmDigital:
      begin
        VpClock1.Width := 136;
        VpClock1.Height := 30;
      end;
  end;
  CbMilitaryTime.Visible := VpClock1.DisplayMode = dmDigital;
  CbNewClockface.Visible := VpClock1.DisplayMode = dmAnalog;

  VpClock1.Time := t;
end;

procedure TForm1.VpClock1CountdownDone(Sender: TObject);
begin
  ShowMessage('Countdown completed.');
end;


end.

