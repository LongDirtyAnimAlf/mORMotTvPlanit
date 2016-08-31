unit caMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, odbcconn, sqldb, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, EditBtn, ComCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    BtnCreateDB: TButton;
    BtnClose: TButton;
    CbCreateVPFields: TCheckBox;
    FileNameEdit: TFileNameEdit;
    Label1: TLabel;
    ODBCConnection1: TODBCConnection;
    Panel1: TPanel;
    SQLTransaction1: TSQLTransaction;
    StatusBar1: TStatusBar;
    procedure BtnCreateDBClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
  private
    function CreateAccessDatabase(DatabaseFile: string): boolean;
    procedure CreateContactsTable;
    procedure CreateEventsTable;
    procedure CreateResourceTable;
    procedure CreateTasksTable;
    procedure StatusMsg(const AText: String);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  LCLType, LazFileUtils;

Const
   ODBC_ADD_DSN = 1;
   ODBC_CONFIG_DSN = 2;
   ODBC_REMOVE_DSN = 3;
   ODBC_ADD_SYS_DSN = 4;
   ODBC_CONFIG_SYS_DSN = 5;
   ODBC_REMOVE_SYS_DSN = 6;
   ODBC_REMOVE_DEFAULT_DSN = 7;

function SQLConfigDataSource(hwndParent: Integer; fRequest: Integer;
  lpszDriverString: PChar; lpszAttributes: PChar): Integer; stdcall; external 'odbccp32.dll';

function SQLInstallerError(iError: integer; pfErrorCode: PInteger;
  lpszErrorMsg: string; cbErrorMsgMax: integer; pcbErrorMsg: PInteger): integer; stdcall; external 'odbccp32.dll';


{ TForm1 }

procedure TForm1.BtnCreateDBClick(Sender: TObject);
var
  fn: String;
begin
  if FileNameEdit.FileName = '' then
    exit;
  fn := ExpandFileNameUTF8(FilenameEdit.FileName);
  if FileExistsUTF8(fn) then
    DeleteFileUTF8(fn);

  // Create empty database file
  CreateAccessDatabase(fn);
  StatusMsg('Database file created');

  if CbCreateVPFields.Checked then begin
    //connection
    ODBCConnection1.Driver := 'Microsoft Access Driver (*.mdb, *.accdb)';
    ODBCConnection1.Params.Add('DBQ=' + fn);
    ODBCConnection1.Params.Add('Locale Identifier=1031');
    ODBCConnection1.Params.Add('ExtendedAnsiSQL=1');
    ODBCConnection1.Params.Add('CHARSET=ansi');
    ODBCConnection1.Connected := True;
    ODBCConnection1.KeepConnection := True;

    //transaction
    SQLTransaction1.DataBase := ODBCConnection1;
    SQLTransaction1.Action := caCommit;
    SQLTransaction1.Active := True;

    // Create tables
    CreateResourceTable;
    CreateContactsTable;
    CreateEventsTable;
    CreateTasksTable;

    SQLTransaction1.Active := false;
    ODBCConnection1.Connected := false;

    Statusbar1.SimpleText := 'All tables created.';
  end;
end;

procedure TForm1.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

function TForm1.CreateAccessDatabase(DatabaseFile: string): boolean;
var
  dbType: string;
  driver: string;
  ErrorCode, ResizeErrorMessage: integer;
  ErrorMessage: PChar;
  retCode: integer;
begin
  driver := 'Microsoft Access Driver (*.mdb, *.accdb)';

  { With this driver,
  CREATE_DB/CREATE_DBV12 will create an .accdb format database;
  CREATE_DBV4 will create an mdb
  http://stackoverflow.com/questions/9205633/how-do-i-specify-the-odbc-access-driver-format-when-creating-the-database
  }

  case Lowercase(ExtractFileExt(DatabaseFile)) of
    '', '.', '.mdb':
      dbType := 'CREATE_DBV4="' + DatabaseFile + '"';
    '.accdb':
      dbtype := 'CREATE_DBV12="' + DatabaseFile + '"';
    else
      raise Exception.CreateFmt('File format "%s" not supported.', [ExtractFileExt(DatabaseFile)]);
  end;

//  DBPChar := 'CREATE_DBV4="' + DatabaseFile + '"';
  retCode := SQLConfigDataSource(Hwnd(nil), ODBC_ADD_DSN, PChar(driver), PChar(dbType));
  if retCode <> 0 then
  begin
    //try alternate driver
    driver := 'Microsoft Access Driver (*.mdb)';
    dbType := 'CREATE_DB="' + DatabaseFile + '"';
    retCode := SQLConfigDataSource(Hwnd(nil), ODBC_ADD_DSN, PChar(driver), PChar(dbType));
  end;
  if retCode = 0 then
    result := true
  else
  begin
    result := false;
    ErrorCode := 0;
    ResizeErrorMessage := 0;
    // todo: verify how the DLL is called - use pointers?; has not been tested.
    GetMem(ErrorMessage, 512);
    try
      SQLInstallerError(1, @ErrorCode, ErrorMessage, SizeOf(ErrorMessage), @ResizeErrorMessage);
    finally
      FreeMem(ErrorMessage);
    end;
    raise Exception.CreateFmt('Error creating Access database: %s', [ErrorMessage]);
  end;
end;

procedure TForm1.CreateContactsTable;
begin
  ODBCConnection1.ExecuteDirect(
    'CREATE TABLE Contacts ('+
      'RecordID COUNTER, ' +
      'ResourceID INTEGER,' +
      'FirstName VARCHAR(50) ,'+
      'LastName VARCHAR(50) , '+
      'Birthdate DATE, '+
      'Anniversary DATE, '+
      'Title VARCHAR(50) ,'+
      'Company VARCHAR(50) ,'+
      'Job_Position VARCHAR(30), '+
      'Address VARCHAR(100), '+
      'City VARCHAR(50), '+
      'State VARCHAR(25), '+
      'Zip VARCHAR(10), '+
      'Country VARCHAR(25), '+
      'Notes VARCHAR, '+
      'Phone1 VARCHAR(25), '+
      'Phone2 VARCHAR(25), '+
      'Phone3 VARCHAR(25), '+
      'Phone4 VARCHAR(25), '+
      'Phone5 VARCHAR(25), '+
      'PhoneType1 INTEGER, '+
      'PhoneType2 INTEGER, '+
      'PhoneType3 INTEGER, '+
      'PhoneType4 INTEGER, '+
      'PhoneType5 INTEGER, '+
      'Category INTEGER, '+
      'EMail VARCHAR(100), '+
      'Custom1 VARCHAR(100), '+
      'Custom2 VARCHAR(100),'+
      'Custom3 VARCHAR(100), '+
      'Custom4 VARCHAR(100) )'
    );
  ODBCConnection1.ExecuteDirect(
    'CREATE UNIQUE INDEX piCRecordID ON Contacts(RecordID) WITH PRIMARY');
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX siCResourceID ON Contacts(ResourceID)');
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX siCName ON Contacts(LastName, FirstName)' );
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX siCCompany ON Contacts(Company)');
  StatusMsg('Table "Contacts" created.');
end;

procedure TForm1.CreateEventsTable;
begin
  ODBCConnection1.ExecuteDirect(
    'CREATE TABLE Events ('+
      'RecordID COUNTER, ' +
      'ResourceID INTEGER, '+
      'StartTime DATETIME, '+
      'EndTime DATETIME, '+
      'Description VARCHAR(255), '+
      'Location VARCHAR(255), '+
      'Notes VARCHAR, ' +
      'Category INTEGER, '+
      'AllDayEvent LOGICAL, '+
      'DingPath VARCHAR(255), '+
      'AlarmSet LOGICAL, '+
      'AlarmAdvance INTEGER, '+
      'AlarmAdvanceType INTEGER, '+
      'SnoozeTime DATETIME, '+
      'RepeatCode INTEGER, '+
      'RepeatRangeEnd DATETIME, '+
      'CustomInterval INTEGER)'
  );
  ODBCConnection1.ExecuteDirect(
    'CREATE UNIQUE INDEX piERecordID ON Events(RecordID) WITH PRIMARY');
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX EResourceID ON Events(ResourceID)');
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX EStartTime ON Events(StartTime)');
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX EEndTime ON Events(EndTime)');

  StatusMsg('Table "Events" created.');
end;

procedure TForm1.CreateResourceTable;
begin
  ODBCConnection1.ExecuteDirect(
    'CREATE TABLE Resources ( '+
       'ResourceID COUNTER, ' +
       'Description VARCHAR(255), '+
       'Notes VARCHAR, '+                        // 1024 --> -
       'ImageIndex INTEGER, '+
       'ResourceActive LOGICAL, '+                  // BOOL --> LOGICAL
       'UserField0 VARCHAR(100), '+
       'UserField1 VARCHAR(100), '+
       'UserField2 VARCHAR(100), '+
       'UserField3 VARCHAR(100), '+
       'UserField4 VARCHAR(100), '+
       'UserField5 VARCHAR(100), '+
       'UserField6 VARCHAR(100), '+
       'UserField7 VARCHAR(100), '+
       'UserField8 VARCHAR(100), '+
       'UserField9 VARCHAR(100) )'
  );
  ODBCConnection1.ExecuteDirect(
    'CREATE UNIQUE INDEX piRResourceID ON Resources(ResourceID) WITH PRIMARY'
  );
  StatusMsg('Table "Resources" created.');
end;

procedure TForm1.CreateTasksTable;
begin
  ODBCConnection1.ExecuteDirect(
    'CREATE TABLE Tasks ('+
      'RecordID COUNTER, ' +
      'ResourceID INTEGER, '+
      'Complete LOGICAL, '+
      'Description VARCHAR(255), '+
      'Details VARCHAR, '+
      'CreatedOn DATETIME, '+
      'Priority INTEGER, '+
      'Category INTEGER, '+
      'CompletedOn DATETIME, '+
      'DueDate DATETIME)'
  );
  ODBCConnection1.ExecuteDirect(
    'CREATE UNIQUE INDEX piTRecordID ON Tasks(RecordID) WITH PRIMARY'
  );
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX siTDueDate ON Tasks(DueDate)'
  );
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX siTCompletedOn ON Tasks(CompletedOn)'
  );
  StatusMsg('Table "Tasks" created.');
end;

procedure TForm1.StatusMsg(const AText: String);
begin
  Statusbar1.SimpleText := AText;
  Application.ProcessMessages;
end;

end.

