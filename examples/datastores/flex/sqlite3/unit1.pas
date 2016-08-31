unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls,
  db, DBGrids, DbCtrls, sqldb, sqlite3conn,
  VpBaseDS, VpDayView, VpWeekView, VpTaskList, VpContactGrid, VpMonthView,
  VpResEditDlg, VpContactButtons, VpData, VpFlxDS;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnNewRes: TButton;
    BtnEditRes: TButton;
    DsTasks: TDataSource;
    DsEvents: TDataSource;
    DsContacts: TDataSource;
    DsResources: TDataSource;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    QryResources: TSQLQuery;
    QryContacts: TSQLQuery;
    QryEvents: TSQLQuery;
    QryTasks: TSQLQuery;
    SQLite3Connection1: TSQLite3Connection;
    SQLTransaction1: TSQLTransaction;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    VpContactButtonBar1: TVpContactButtonBar;
    VpContactGrid1: TVpContactGrid;
    VpControlLink1: TVpControlLink;
    VpDayView1: TVpDayView;
    VpFlexDataStore1: TVpFlexDataStore;
    VpMonthView1: TVpMonthView;
    VpResourceCombo1: TVpResourceCombo;
    VpResourceEditDialog1: TVpResourceEditDialog;
    VpTaskList1: TVpTaskList;
    VpWeekView1: TVpWeekView;
    procedure BtnNewResClick(Sender: TObject);
    procedure BtnEditResClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure CreateContacts;
    procedure CreateDB(AFileName: String);
    procedure CreateEvents;
    procedure CreateResources;
    procedure CreateTasks;
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
  DB_NAME = 'data.db';

  // More information on the use of these values is below.
  // They need not be set as constants in your application. They can be any valid value
  APPLICATION_ID = 1189021115; // must be a 32-bit Unsigned Integer (Longword 0 .. 4294967295)
  USER_VERSION = 23400001;  // must be a 32-bit Signed Integer (LongInt -2147483648 .. 2147483647)


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

procedure TForm1.Button1Click(Sender: TObject);
begin
  VpFlexDatastore1.Load;
end;

procedure TForm1.CreateDB(AFileName: String);
begin
  SQLite3Connection1.Close; // Ensure the connection is closed when we start

  // Create the database and the tables
  try
    SQLite3Connection1.DatabaseName := AFileName;
    SQLite3Connection1.Open;
    SQLTransaction1.Active := true;

    // Per the SQLite Documentation (edited for clarity):
    // The pragma user_version is used to set or get the value of the user-version.
    // The user-version is a big-endian 32-bit signed integer stored in the database header at offset 60.
    // The user-version is not used internally by SQLite. It may be used by applications for any purpose.
    // http://www.sqlite.org/pragma.html#pragma_schema_version
    SQLite3Connection1.ExecuteDirect('PRAGMA user_version = ' + IntToStr(user_version) + ';');

    // Per the SQLite Documentation:
    // The application_id PRAGMA is used to query or set the 32-bit unsigned big-endian
    // "Application ID" integer located at offset 68 into the database header.
    // Applications that use SQLite as their application file-format should set the
    // Application ID integer to a unique integer so that utilities such as file(1) can
    // determine the specific file type rather than just reporting "SQLite3 Database".
    // A list of assigned application IDs can be seen by consulting the magic.txt file
    // in the SQLite source repository.
    // http://www.sqlite.org/pragma.html#pragma_application_id
    SQLite3Connection1.ExecuteDirect('PRAGMA application_id = ' + IntToStr(application_id) + ';');

    CreateContacts;
    CreateEvents;
    CreateTasks;
    CreateResources;

    SQLTransaction1.Commit;
    SQLite3Connection1.Close;

  except
    ShowMessage(Format('Unable to create database "%s".', [AFileName]));
  end;
end;

procedure TForm1.CreateContacts;
begin
  SQLite3Connection1.ExecuteDirect(
    'CREATE TABLE Contacts (' +
      '"RecordID"       INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, '+
      '"ResourceID"     INTEGER NOT NULL, ' +
      '"FirstName"      VARCHAR(50), '+
      '"LastName"       VARCHAR(50), '+
      '"Birthdate"      DATETIME, '+
      '"Anniversary"    DATETIME, '+
      '"Title"          VARCHAR(50), '+
      '"Company"        VARCHAR(50), '+
      '"Job_Position"   VARCHAR(30), '+
      '"Address"        VARCHAR(100), '+
      '"City"           VARCHAR(50), '+
      '"State"          VARCHAR(25), '+
      '"Zip"            VARCHAR(10), '+
      '"Country"        VARCHAR(25), '+
      '"Notes"          VARCHAR(1024), '+
      '"Phone1"         VARCHAR(25), '+
      '"Phone2"         VARCHAR(25), '+
      '"Phone3"         VARCHAR(25), '+
      '"Phone4"         VARCHAR(25), '+
      '"Phone5"         VARCHAR(25), '+
      '"PhoneType1"     INTEGER, '+
      '"PhoneType2"     INTEGER, '+
      '"PhoneType3"     INTEGER, '+
      '"PhoneType4"     INTEGER, '+
      '"PhoneType5"     INTEGER, '+
      '"Category"       INTEGER, '+
      '"EMail"          VARCHAR (100), '+
      '"Custom1"        VARCHAR (100), '+
      '"Custom2"        VARCHAR (100), '+
      '"Custom3"        VARCHAR (100), '+
      '"Custom4"        VARCHAR (100)); '
  );
  SQLite3Connection1.ExecuteDirect(
    'CREATE UNIQUE INDEX "Contacts_RecordID_idx" ON "Contacts" ("RecordID");'
  );
  SQLite3Connection1.ExecuteDirect(
    'CREATE INDEX "Contacts_ResourceID_idx" ON "Contacts" ("ResourceID");'
  );
end;

procedure TForm1.CreateEvents;
begin
  SQLite3Connection1.ExecuteDirect(
    'CREATE TABLE Events (' +
      '"RecordID"         INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, ' +
      '"ResourceID"       INTEGER NOT NULL, ' +
      '"StartTime"        DATETIME NOT NULL, ' +
      '"EndTime"          DATETIME NOT NULL, ' +
      '"Description"      VARCHAR (255), ' +
      '"Location"         VARCHAR (255), ' +
      '"Notes"            VARCHAR (1024), ' +
      '"Category"         INTEGER, ' +
      '"AllDayEvent"      BOOLEAN, ' +
      '"DingPath"         VARCHAR (255), ' +
      '"AlarmSet"         BOOLEAN, ' +
      '"AlarmAdvance"     INTEGER, ' +
      '"AlarmAdvanceType" INTEGER, ' +
      '"SnoozeTime"       DATETIME, ' +
      '"RepeatCode"       INTEGER, ' +
      '"RepeatRangeEnd"   DATETIME, ' +
      '"CustomInterval"   INTEGER );'
  );
  SQLite3Connection1.ExecuteDirect(
    'CREATE UNIQUE INDEX "Events_RecordID_idx" ON "Events" ("RecordID");'
  );
  SQLite3Connection1.ExecuteDirect(
    'CREATE INDEX "Events_ResourceID_idx" ON "Events" ("ResourceID");'
  );
  SQLite3Connection1.ExecuteDirect(
    'CREATE INDEX "Events_StartTime_idx" ON "Events" ("StartTime");'
  );
  SQLIte3Connection1.ExecuteDirect(
    'CREATE INDEX "Events_EndTime_idx" ON "Events" ("EndTime");');
end;

procedure TForm1.CreateResources;
begin
  SQLite3Connection1.ExecuteDirect(
    'CREATE TABLE Resources (' +
      '"ResourceID"     INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, '+
      '"Description"    VARCHAR (255), ' +
      '"Notes"          VARCHAR (1024), ' +
      '"ImageIndex"     INTEGER, ' +
      '"ResourceActive" BOOLEAN );'
  );
  SQLite3Connection1.ExecuteDirect(
    'CREATE UNIQUE INDEX "Resources_ResourceID_idx" ON "Resources" ("ResourceID");'
  );
end;

procedure TForm1.CreateTasks;
begin
  SQLite3Connection1.ExecuteDirect(
    'CREATE TABLE Tasks (' +
      '"RecordID"       INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, ' +
      '"ResourceID"     INTEGER NOT NULL, ' +
      '"Complete"       BOOLEAN, ' +
      '"Description"    VARCHAR(255), ' +
      '"Details"        VARCHAR(1024), ' +
      '"CreatedOn"      DATETIME, ' +
      '"Priority"       INTEGER, ' +
      '"Category"       INTEGER, ' +
      '"CompletedOn"    DATETIME, ' +
      '"DueDate"        DATETIME);'
  );
  SQLite3Connection1.ExecuteDirect(
    'CREATE UNIQUE INDEX "Tasks_RecordID_idx" ON "Tasks" ("RecordID");'
  );
  SQLite3Connection1.ExecuteDirect(
    'CREATE INDEX "Tasks_ResourceID_idx" ON "Tasks" ("ResourceID");'
  );
end;

// Setting up the database connection and the datastore. Preselect a resource
// in the resource combo.
procedure TForm1.FormCreate(Sender: TObject);
begin
  try
    // Connection
    Sqlite3Connection1.DatabaseName := DB_NAME;

    // Transaction
    SQLTransaction1.DataBase := Sqlite3Connection1;

    if not FileExists(AppendPathDelim(Application.Location) + DB_NAME) then
      CreateDB(AppendPathDelim(Application.Location) + DB_NAME);

    // Connect the datastore. This opens the datasets and loads them into the store.
    VpFlexDatastore1.Connected := true;

    // Pre-select the first resource item
    if VpFlexDatastore1.Resources.Count > 0 then
      VpFlexDatastore1.Resource := VpFlexDatastore1.Resources.Items[0];

    PageControl1.ActivePageIndex := 0;

  except
    on E:Exception do
    begin
      MessageDlg('ERROR', mtError, [mbOK], 0);
      Close;
    end;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Sqlite3Connection1.Connected := false;
end;

end.

