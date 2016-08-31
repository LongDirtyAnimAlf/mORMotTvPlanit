{$I vp.inc}

unit VpSQLite3DS;

interface

uses
  SysUtils, Classes, DB,
  VpBaseDS, VpDBDS,
  sqlite3conn, sqldb;

type
  TVpSqlite3Datastore = class(TVpCustomDBDatastore)
  private
    FConnection: TSqlite3Connection;
    FContactsTable: TSQLQuery;
    FEventsTable: TSQLQuery;
    FResourceTable: TSQLQuery;
    FTasksTable: TSQLQuery;
    procedure SetConnection(const AValue: TSqlite3Connection);

  protected
    procedure CreateTable(const ATableName: String);
    function GetContactsTable: TDataset; override;
    function GetEventsTable: TDataset; override;
    function GetResourceTable: TDataset; override;
    function GetTasksTable: TDataset; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OpenTables;
    procedure SetConnected(const AValue: Boolean); override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateTables;
    function GetNextID(TableName: string): integer; override;
    procedure PostEvents; override;
    procedure PostContacts; override;
    procedure PostTasks; override;
    procedure PostResources; override;

    property ResourceTable;
    property EventsTable;
    property ContactsTable;
    property TasksTable;

  published
    property Connection: TSqlite3Connection read FConnection write SetConnection;

    // inherited
    property AutoConnect;
    property AutoCreate;
    property DayBuffer;
  end;

var
  // More information on the use of these values is below.
  // They need not be set as constants in your application. They can be any valid value
  APPLICATION_ID : LongWord = 1189021115; // must be a 32-bit Unsigned Integer (Longword 0..4294967295)
  USER_VERSION   : LongInt  = 23400001;   // must be a 32-bit Signed Integer (LongInt -2147483648..2147483647)

implementation

uses
  LazFileUtils,
  VpConst, VpMisc;


{ TVpSqlite3Datastore }

constructor TVpSqlite3Datastore.Create(AOwner: TComponent);
begin
  inherited;

  FContactsTable := TSQLQuery.Create(self);
  FContactsTable.SQL.Add('SELECT * FROM Contacts');

  FEventsTable := TSQLQuery.Create(Self);
  FEventsTable.SQL.Add('SELECT * FROM Events');

  FResourceTable := TSQLQuery.Create(self);
  FResourceTable.SQL.Add('SELECT * FROM Resources');

  FTasksTable := TSQLQuery.Create(self);
  FTasksTable.SQL.Add('SELECT * FROM Tasks');
end;

// Connection and tables are active afterwards!
procedure TVpSqlite3Datastore.CreateTables;
var
  wasConnected: Boolean;
begin
  if FileExists(FConnection.DatabaseName) then
    exit;

  wasConnected := FConnection.Connected;

  FConnection.Close;

  if FContactsTable.Transaction = nil then
    FContactsTable.Transaction := FConnection.Transaction;
  if FEventsTable.Transaction = nil then
    FEventsTable.Transaction := FConnection.Transaction;
  if FResourceTable.Transaction = nil then
    FResourceTable.Transaction := FConnection.Transaction;
  if FTasksTable.Transaction = nil then
    FTasksTable.Transaction := FConnection.Transaction;

  CreateTable(ContactsTableName);
  CreateTable(EventsTableName);
  CreateTable(ResourceTableName);
  CreateTable(TasksTableName);

  SetConnected(wasConnected or AutoConnect);
end;

procedure TVpSqlite3Datastore.CreateTable(const ATableName: String);
begin
  if ATableName = ContactsTableName then begin
    FConnection.ExecuteDirect(
      'CREATE TABLE Contacts ('+
        'RecordID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, '+
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
        'Notes VARCHAR(1024), '+
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
        'Custom4 VARCHAR(100), '+
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
    FConnection.ExecuteDirect(
      'CREATE INDEX ContactsResourceID_idx ON Contacts(ResourceID)'
    );
    FConnection.ExecuteDirect(
      'CREATE INDEX ContactsName_idx ON Contacts(LastName, FirstName)'
    );
    FConnection.ExecuteDirect(
      'CREATE INDEX ContactsCompany_idx ON Contacts(Company)'
    );
  end else
  if ATableName = EventsTableName then begin
    FConnection.ExecuteDirect(
      'CREATE TABLE Events ('+
        'RecordID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, '+
        'StartTime TIMESTAMP, '+
        'EndTime TIMESTAMP, '+
        'ResourceID INTEGER, '+
        'Description VARCHAR(255), '+
        'Location VARCHAR(255), '+
        'Notes VARCHAR(1024), ' +
        'Category INTEGER, '+
        'AllDayEvent BOOL, '+
        'DingPath VARCHAR(255), '+
        'AlarmSet BOOL, '+
        'AlarmAdvance INTEGER, '+
        'AlarmAdvanceType INTEGER, '+
        'SnoozeTime TIMESTAMP, '+
        'RepeatCode INTEGER, '+
        'RepeatRangeEnd TIMESTAMP, '+
        'CustomInterval INTEGER, '+
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
    FConnection.ExecuteDirect(
      'CREATE INDEX EventsResourceID_idx ON Events(ResourceID)'
    );
    FConnection.ExecuteDirect(
      'CREATE INDEX EventsStartTime_idx ON Events(StartTime)'
    );
    FConnection.ExecuteDirect(
      'CREATE INDEX EventsEndTime_idx ON Events(EndTime)'
    );
  end else
  if ATableName = ResourceTableName then begin
    FConnection.ExecuteDirect(
      'CREATE TABLE Resources ( '+
         'ResourceID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, '+
         'Description VARCHAR(255), '+
         'Notes VARCHAR(1024), '+
         'ImageIndex INTEGER, '+
         'ResourceActive BOOL, '+
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
  end else
  if ATableName = TasksTableName then begin
    FConnection.ExecuteDirect(
      'CREATE TABLE Tasks ('+
        'RecordID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, '+
        'ResourceID INTEGER, '+
        'Complete BOOL, '+
        'Description VARCHAR(255), '+
        'Details VARCHAR(1024), '+
        'CreatedOn TIMESTAMP, '+
        'Priority INTEGER, '+
        'Category INTEGER, '+
        'CompletedOn TIMESTAMP, '+
        'DueDate TIMESTAMP, '+
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
    FConnection.ExecuteDirect(
      'CREATE INDEX TasksResourceID_idx ON Tasks(ResourceID)'
    );
    FConnection.ExecuteDirect(
      'CREATE INDEX TasksDueDate_idx ON Tasks(DueDate)'
    );
    FConnection.ExecuteDirect(
      'CREATE INDEX TasksCompletedOn_idx ON Tasks(CompletedOn)'
    );
  end;
end;

function TVpSqlite3Datastore.GetContactsTable: TDataset;
begin
  Result := FContactsTable;
end;

function TVpSqlite3Datastore.GetEventsTable: TDataset;
begin
  Result := FEventsTable;
end;

function TVpSqlite3DataStore.GetNextID(TableName: string): integer;
begin
  Unused(TableName);
  { This is not needed in the SQLITE3 datastore as these tables use
    autoincrement fields. }
  Result := -1;
end;

function TVpSqlite3Datastore.GetResourceTable : TDataset;
begin
  Result := FResourceTable;
end;

function TVpSqlite3Datastore.GetTasksTable : TDataset;
begin
  Result := FTasksTable;
end;

procedure TVpSqlite3Datastore.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Connected := AutoConnect and (AutoCreate or FileExists(FConnection.DatabaseName));
end;

procedure TVpSqlite3Datastore.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FConnection) then
    FConnection := nil;
end;

procedure TVpSqlite3Datastore.OpenTables;
begin
  if FContactsTable.Transaction = nil then
    FContactsTable.Transaction := FConnection.Transaction;
  FContactsTable.Open;

  if FEventsTable.Transaction = nil then
    FEventsTable.Transaction := FConnection.Transaction;
  FEventsTable.Open;

  if FResourceTable.Transaction = nil then
    FResourceTable.Transaction := FConnection.Transaction;
  FResourceTable.Open;

  if FTasksTable.Transaction = nil then
    FTasksTable.Transaction := FConnection.Transaction;
  FTasksTable.Open;
end;

procedure TVpSqlite3Datastore.PostContacts;
begin
  inherited;
  FContactsTable.ApplyUpdates;
end;

procedure TVpSqlite3Datastore.PostEvents;
begin
  inherited;
  FEventsTable.ApplyUpdates;
end;

procedure TVpSqlite3Datastore.PostResources;
begin
  inherited;
  FResourceTable.ApplyUpdates;
end;

procedure TVpSqlite3Datastore.PostTasks;
begin
  inherited;
  FTasksTable.ApplyUpdates;
end;

procedure TVpSqlite3Datastore.SetConnected(const AValue: Boolean);
begin
  if (FConnection = nil) or (FConnection.Transaction = nil) then
    exit;

  if AValue = FConnection.Connected then
    exit;

  if AValue and AutoCreate then
    CreateTables;

  FConnection.Connected := AValue;
  if AValue then
  begin
    FConnection.Transaction.Active := true;
    OpenTables;
  end;

  inherited SetConnected(AValue);

  if FConnection.Connected then
    Load;
end;

procedure TVpSqlite3Datastore.SetConnection(const AValue: TSqlite3Connection);
var
  wasConnected: Boolean;
begin
  if AValue = FConnection then
    exit;

  // To do: clear planit lists...
  if FConnection <> nil then begin
    wasConnected := FConnection.Connected;
    Connected := false;
  end;
  FConnection := AValue;

  FContactsTable.Database := FConnection;
  FContactsTable.Transaction := FConnection.Transaction;

  FEventsTable.Database := FConnection;
  FEventsTable.Transaction := FConnection.Transaction;

  FResourceTable.Database := FConnection;
  FResourceTable.Transaction := FConnection.Transaction;

  FTasksTable.Database := FConnection;
  FTasksTable.Transaction := FConnection.Transaction;
  if wasConnected then Connected := true;
end;

end.
