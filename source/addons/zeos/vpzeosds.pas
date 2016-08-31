{$I vp.inc}

unit VpZeosDs;

interface

uses
  SysUtils, Classes, DB,
  VpBaseDS, VpDBDS,
  ZCompatibility, ZConnection, ZDataset;

type
  TVpZeosDatastore = class(TVpCustomDBDatastore)
  private
    FConnection: TZConnection;
    FContactsTable: TZTable;
    FEventsTable: TZTable;
    FResourceTable: TZTable;
    FTasksTable: TZTable;
    procedure SetConnection(const AValue: TZConnection);

  protected
    procedure CreateTable(const ATableName: String);
    procedure CreateAllTables;
    function GetContactsTable: TDataset; override;
    function GetEventsTable: TDataset; override;
    function GetResourceTable: TDataset; override;
    function GetTasksTable: TDataset; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetConnected(const AValue: Boolean); override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateTables;
    function GetNextID(TableName: string): integer; override;

    property ResourceTable;
    property EventsTable;
    property ContactsTable;
    property TasksTable;

  published
    property Connection: TZConnection read FConnection write SetConnection;

    // inherited
    property AutoConnect default false;
    property AutoCreate default false;
    property Daybuffer;
  end;


implementation

uses
  LazFileUtils,
  VpConst;

{ TVpZeosDatastore }

constructor TVpZeosDatastore.Create(AOwner: TComponent);
begin
  inherited;

  FContactsTable := TZTable.Create(self);
  FContactsTable.TableName := 'Contacts';

  FEventsTable := TZTable.Create(Self);
  FEventsTable.TableName := 'Events';

  FResourceTable := TZTable.Create(self);
  FResourceTable.TableName := 'Resources';

  FTasksTable := TZTable.Create(self);
  FTasksTable.TableName := 'Tasks';
end;

procedure TVpZeosDatastore.CreateAllTables;
begin
  if not FContactsTable.Exists then CreateTable(ContactsTableName);
  if not FEventsTable.Exists then CreateTable(EventsTableName);
  if not FResourceTable.Exists then CreateTable(ResourceTableName);
  if not FTasksTable.Exists then CreateTable(TasksTableName);
end;

procedure TVpZeosDatastore.CreateTable(const ATableName: String);
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

procedure TVpZeosDatastore.CreateTables;
var
  wasConnected: Boolean;
begin
  wasConnected := FConnection.Connected;
  FConnection.Connected := true;
  CreateAllTables;
  SetConnected(wasConnected or AutoConnect);
end;

function TVpZeosDatastore.GetContactsTable: TDataset;
begin
  Result := FContactsTable;
end;

function TVpZeosDatastore.GetEventsTable: TDataset;
begin
  Result := FEventsTable;
end;

function TVpZeosDataStore.GetNextID(TableName: string): integer;
begin
  { This is not needed in the ZEOS datastore as these tables use
    autoincrement fields. }
  result := -1;
end;

function TVpZeosDatastore.GetResourceTable : TDataset;
begin
  Result := FResourceTable;
end;

function TVpZeosDatastore.GetTasksTable : TDataset;
begin
  Result := FTasksTable;
end;

procedure TVpZeosDatastore.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Connected := AutoConnect and (
      AutoCreate or
      (FContactsTable.Exists and FEventsTable.Exists and FResourceTable.Exists and FTasksTable.Exists)
    );
end;

procedure TVpZeosDatastore.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FConnection) then
    FConnection := nil;
end;

procedure TVpZeosDatastore.SetConnected(const AValue: Boolean);
begin
  if (AValue = Connected) or (FConnection = nil) then
    exit;

  if AValue and AutoCreate then
    CreateTables;

  FConnection.Connected := AValue;
  if FConnection.Connected then begin
    FContactsTable.Open;
    FEventsTable.Open;
    FResourceTable.Open;
    FTasksTable.Open;
  end;

  inherited SetConnected(AValue);

  if FConnection.Connected then
    Load;
end;

procedure TVpZeosDatastore.SetConnection(const AValue: TZConnection);
var
  wasConnected: Boolean;
begin
  if AValue = FConnection then
    exit;

  // To do: clear planit lists...
  if FConnection <> nil then begin
    wasConnected := FConnection.Connected;
    Connected := false;
  end else
    wasConnected := false;
  FConnection := AValue;
  FContactsTable.Connection := FConnection;
  FEventsTable.Connection := FConnection;
  FResourceTable.Connection := FConnection;
  FTasksTable.Connection := FConnection;
  if wasConnected then Connected := true;
end;

end.
