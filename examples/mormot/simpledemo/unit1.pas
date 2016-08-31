unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  VpBaseDS,
  //VpBufDS,
  VpmORMotDS,
  VpDayView, VpWeekView, VpMonthView;

type

  { TForm1 }

  TForm1 = class(TForm)
    Panel1: TPanel;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    TerminateTimer:boolean;
    Datastore: TVpmORMotDatastore;
    //Datastore: TVpBufDSDatastore;
    ControlLink: TVpControlLink;
    WeekView: TVpWeekView;
    DayView: TVpDayView;
    MonthView: TVpMonthView;
    combo: TVpResourceCombo;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  TerminateTimer:=false;

  ControlLink := TVpControlLink.Create(self);

  Datastore := TVpmORMotDatastore.Create(self);
  // if the HostIP is set, it will look for a running server on this IP address when connecting.
  // leave blank (comment out) for a local (and private) database
  //DataStore.HostIP:='localhost';

  Datastore.Directory:='data';
  Datastore.Connected := true;

  if (Length(Datastore.HostIP)>0) AND (NOT Datastore.Connected) then
  begin
    MessageDlg('Cannot connect with server', mtError, [mbOk], 0);
    Close;
  end;


  //Datastore := TVpBufDSDatastore.Create(self);
  //Datastore.Directory := '.';
  //Datastore.AutoCreate := true;
  //Datastore.Connected := true;

  DayView := TVpDayview.Create(self);
  DayView.Parent := self;
  DayView.Align := alLeft;
  DayView.ControlLink := ControlLink;
  DayView.Datastore := Datastore;
  DayView.AllowDragAndDrop:=True;

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

  Timer1.Enabled:=True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:=False;
  Datastore.CheckUpdate;
  Timer1.Enabled:=True;
end;

end.

