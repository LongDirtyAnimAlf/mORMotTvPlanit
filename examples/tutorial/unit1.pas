unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  VpDayView, VpMonthView, VpTaskList, VpContactGrid, VpContactButtons,
  VpWeekView, VpIniDs, VpBaseDS, VpXmlDs;

type

  { TForm1 }

  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    VpContactButtonBar1: TVpContactButtonBar;
    VpContactGrid1: TVpContactGrid;
    VpControlLink1: TVpControlLink;
    VpDayView1: TVpDayView;
    VpMonthView1: TVpMonthView;
    VpTaskList1: TVpTaskList;
    VpWeekView1: TVpWeekView;
    VpXmlDatastore1: TVpXmlDatastore;
    procedure FormCreate(Sender: TObject);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  VpXmlDatastore1.Filename := 'data.xml';
  VpXmlDatastore1.Connected := true;
  if VpXmlDatastore1.Resources.Count > 0 then
    VpXmlDatastore1.Resource := VpXmlDatastore1.Resources.Items[0];
end;

end.

