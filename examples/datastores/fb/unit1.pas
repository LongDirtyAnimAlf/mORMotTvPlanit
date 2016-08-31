unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, sqldb, IBConnection,
  VpBaseDS, VpDayView, VpWeekView, VpTaskList, VpContactGrid,
  VpMonthView, VpResEditDlg, VpContactButtons, VpData, VpFBDS;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnNewRes: TButton;
    BtnEditRes: TButton;
    IBConnection1: TIBConnection;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    VpContactButtonBar1: TVpContactButtonBar;
    VpContactGrid1: TVpContactGrid;
    VpControlLink1: TVpControlLink;
    VpDayView1: TVpDayView;
    VpFirebirdDatastore1: TVpFirebirdDatastore;
    VpMonthView1: TVpMonthView;
    VpResourceCombo1: TVpResourceCombo;
    VpResourceEditDialog1: TVpResourceEditDialog;
    VpTaskList1: TVpTaskList;
    VpWeekView1: TVpWeekView;
    procedure BtnNewResClick(Sender: TObject);
    procedure BtnEditResClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
  DBFILENAME = 'data.fdb';

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
    IBConnection1.DatabaseName := AppendPathDelim(Application.Location) + DBFILENAME;
    IBConnection1.Username := 'SYSDBA';
    IBConnection1.Password := 'masterkey';

//    SQLTransaction1.Action := caCommitRetaining;

    VpFirebirdDatastore1.Connection := IBConnection1;
    VpFirebirdDatastore1.AutoCreate := true;
    VpFirebirdDatastore1.CreateTables;


    VpFirebirdDatastore1.Connected := true;

    if VpFirebirdDatastore1.Resources.Count > 0 then
      VpFirebirdDatastore1.ResourceID := VpFirebirdDatastore1.Resources.Items[0].ResourceID;

  except
    on E:Exception do
    begin
      MessageDlg('ERROR with Firebird installation:' + LineEnding + E.Message,
        mtError, [mbOK], 0);
      Close;
    end;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SQLTransaction1.Commit;
end;

end.

