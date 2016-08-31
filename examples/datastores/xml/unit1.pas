unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Menus,
  VpBaseDS, VpDayView, VpWeekView, VpTaskList, VpContactGrid, VpMonthView,
  VpResEditDlg, VpContactButtons, VpXmlDs;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnNewRes: TButton;
    BtnEditRes: TButton;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
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
    VpXmlDatastore1: TVpXmlDatastore;
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
  datastore: TVpCustomDatastore;
begin
  datastore := VpControlLink1.Datastore;
  if datastore.Resources.Count > 0 then
  begin
    lastRes := datastore.Resources.Items[datastore.Resources.Count-1];
    datastore.ResourceID := lastRes.ResourceID;
  end;
end;

end.

