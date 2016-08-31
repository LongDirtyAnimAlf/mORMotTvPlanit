unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  VpBaseDS, VpBufDS, VpDayView, VpWeekView, VpMonthView;

type

  { TForm1 }

  TForm1 = class(TForm)
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    Datastore: TVpBufDSDatastore;
    ControlLink: TVpControlLink;
    WeekView: TVpWeekView;
    DayView: TVpDayView;
    MonthView: TVpMonthView;
    combo: TVpResourceCombo;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  ControlLink := TVpControlLink.Create(self);

  Datastore := TVpBufDSDatastore.Create(self);
  Datastore.Directory := '.';
  Datastore.AutoCreate := true;
  Datastore.Connected := true;

  DayView := TVpDayview.Create(self);
  DayView.Parent := self;
  DayView.Align := alLeft;
  DayView.ControlLink := ControlLink;
  DayView.Datastore := Datastore;

  WeekView := TVpWeekView.Create(self);
  WeekView.Parent := self;
  Weekview.Align := alClient;
  WeekView.ControlLink := ControlLink;
  WeekView.Datastore := Datastore;

  MonthView := TVpMonthView.Create(self);
  MonthView.Parent := self;
  MonthView.Align := alRight;
  MonthView.ControlLink := ControlLink;
  MonthView.Datastore := Datastore;

  Combo := TVpResourceCombo.Create(Self);
  Combo.Parent := Panel1;
  Combo.Left := 8;
  Combo.Top := 8;
  Combo.Width := 200;
  Combo.Datastore := Datastore;

  if Datastore.Resources.Count > 0 then
    Datastore.ResourceID := Datastore.Resources.Items[0].ResourceID;
end;

end.

