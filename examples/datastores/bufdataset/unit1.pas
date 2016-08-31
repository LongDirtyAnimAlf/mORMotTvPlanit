unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, VpBaseDS, VpDayView, VpWeekView, VpTaskList,
  VpContactGrid, VpMonthView, VpResEditDlg, VpContactButtons, VpBufDS;

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
    VpBufDSDataStore1: TVpBufDSDataStore;
    VpContactButtonBar1: TVpContactButtonBar;
    VpContactGrid1: TVpContactGrid;
    VpControlLink1: TVpControlLink;
    VpDayView1: TVpDayView;
    VpMonthView1: TVpMonthView;
    VpResourceCombo1: TVpResourceCombo;
    VpResourceEditDialog1: TVpResourceEditDialog;
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
  LazFileUtils,
  VpData;


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

// Load the last resource.
procedure TForm1.FormCreate(Sender: TObject);
var
  lastRes: TVpResource;
begin
  { if you want to set the data directory at runtime, don't set AutoCreate and
    AutoConnect to true, and use this code

  VpBufDSDatastore1.Directory := 'some directory';
  if not DirectoryExistsUTF8(VpBufDSDatastore1.Directory) then
    CreateDir(VpBufDSDatastore1.Directory);
  VpBufDSDatastore1.CreateTables;
  VPBufDSDatastore1.Connected := true;
  }

  if VpBufDSDatastore1.Resources.Count > 0 then
  begin
    lastRes := VpBufDSDatastore1.Resources.Items[VpBufDSDatastore1.Resources.Count-1];
    VpBufDSDatastore1.ResourceID := lastRes.ResourceID;
  end;
end;


end.

