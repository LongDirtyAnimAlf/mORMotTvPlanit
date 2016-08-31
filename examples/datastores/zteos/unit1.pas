unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, VpBaseDS, VpZeosDs, VpDayView, VpWeekView, VpTaskList,
  VpContactGrid, VpMonthView, VpResEditDlg, VpContactButtons, ZConnection;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnNewRes: TButton;
    BtnEditRes: TButton;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    VpContactButtonBar1: TVpContactButtonBar;
    VpContactGrid1: TVpContactGrid;
    VpControlLink1: TVpControlLink;
    VpDayView1: TVpDayView;
    VpMonthView1: TVpMonthView;
    VpResourceCombo1: TVpResourceCombo;
    VpResourceEditDialog1: TVpResourceEditDialog;
    VpTaskList1: TVpTaskList;
    VpWeekView1: TVpWeekView;
    VpZeosDatastore1: TVpZeosDatastore;
    ZConnection1: TZConnection;
    procedure BtnNewResClick(Sender: TObject);
    procedure BtnEditResClick(Sender: TObject);
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

uses
  LazFileUtils;

const
  DBFILENAME = 'data.db';

{ TForm1 }

// Adds a new resource
procedure TForm1.BtnNewResClick(Sender: TObject);
begin
  VpResourceEditDialog1.AddNewResource;
end;

// Edits the currently selected resource
procedure TForm1.BtnEditResClick(Sender: TObject);
begin
  // Open the resource editor dialog, everything is done here.
  VpResourceEditDialog1.Execute;
end;

// Setting up the database connection and the datastore. Preselect a resource
// in the resource combo.
procedure TForm1.FormCreate(Sender: TObject);
begin
  try
    ZConnection1.Database := AppendPathDelim(Application.Location) + DBFILENAME;
    ZConnection1.Protocol := 'sqlite-3';

    VpZeosDatastore1.Connection := ZConnection1;
    VpZeosDatastore1.AutoCreate := true;
    VpZeosDatastore1.Connected := true;

    if VpZeosDatastore1.Resources.Count > 0 then
      VpZeosDatastore1.ResourceID := 1;

  except
    on E:Exception do
    begin
      MessageDlg(E.Message + LineEnding + 'Or copy sqlite3.dll to the exe folder.',
        mtError, [mbOK], 0);
      Close;
    end;
  end;
end;

end.

