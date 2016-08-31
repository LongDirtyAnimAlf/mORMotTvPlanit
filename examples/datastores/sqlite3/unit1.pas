unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, VpBaseDS, VpDayView, VpWeekView, VpTaskList,
  VpContactGrid, VpMonthView, VpResEditDlg, VpContactButtons, VpSQLite3DS,
  sqlite3conn, sqldb, VpData;

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
    SQLite3Connection1: TSQLite3Connection;
    SQLTransaction1: TSQLTransaction;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    VpContactButtonBar1: TVpContactButtonBar;
    VpContactGrid1: TVpContactGrid;
    VpControlLink1: TVpControlLink;
    VpDayView1: TVpDayView;
    VpMonthView1: TVpMonthView;
    VpResourceCombo1: TVpResourceCombo;
    VpResourceEditDialog1: TVpResourceEditDialog;
    VpSqlite3Datastore1: TVpSqlite3Datastore;
    VpTaskList1: TVpTaskList;
    VpWeekView1: TVpWeekView;
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
    SQLite3Connection1.DatabaseName := AppendPathDelim(Application.Location) + DBFILENAME;
    SQLTransaction1.Action := caCommit;

    VpSqlite3Datastore1.Connection := SQLite3Connection1;
    VpSqlite3Datastore1.AutoCreate := true;
    VpSqlite3Datastore1.Connected := true;

    if VpSqlite3Datastore1.Resources.Count > 0 then
      VpSqlite3Datastore1.ResourceID := VpSqlite3Datastore1.Resources.Items[0].ResourceID;

  except
    on E:Exception do
    begin
      MessageDlg('sqlite3.dll not found. Copy it to the exe folder and restart the program.',
        mtError, [mbOK], 0);
      Close;
    end;
  end;
end;

end.

