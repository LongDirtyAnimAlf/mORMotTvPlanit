{$I vp.inc}

{ A datastore for a Firebird database accessed via SQLDB }

unit VpFBDS;

interface

uses
  SysUtils, Classes, DB,
  VpBaseDS, VpDBDS,
  IBConnection, sqldb;

type
  TVpFirebirdDatastore = class(TVpCustomDBDatastore)
  private
    FConnection: TIBConnection;
    FContactsTable: TSQLQuery;
    FEventsTable: TSQLQuery;
    FResourceTable: TSQLQuery;
    FTasksTable: TSQLQuery;
    FConnectLock: Integer;
    procedure SetConnection(const AValue: TIBConnection);

  protected
    procedure CreateAllTables(dbIsNew: Boolean);
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
    property Connection: TIBConnection read FConnection write SetConnection;

    // inherited
    property AutoConnect;
    property AutoCreate;
    property DayBuffer;
  end;


implementation

uses
  LazFileUtils,
  VpConst, VpMisc;

{ TVpIBDatastore }

constructor TVpFirebirdDatastore.Create(AOwner: TComponent);
begin
  inherited;

  FContactsTable := TSQLQuery.Create(self);
  FContactsTable.SQL.Add('SELECT * FROM Contacts');

  FEventsTable := TSQLQuery.Create(Self);
  FEventsTable.SQL.Add('SELECT * FROM Events');

  FResourceTable := TSQLQuery.Create(self);
  FResourceTable.SQL.Add(
    'SELECT * '+
    'FROM Resources'
  );
  {
  FResourceTable.InsertSQL.Add(
    'INSERT INTO Resources (' +
      'ResourceID, Description, Notes, ResourceActive, ' +
      'UserField0, UserField1, UserField2, UserField3, UserField4, ' +
      'UserField5, UserField6, UserField7, UserField8, UserField9) ' +
    'VALUES(' +
      ':ResourceID, :Description, :Notes, :ResourceActive, ' +
      ':UserField0, :UserField1, :UserField2, :UserField3, :UserField4, ' +
      ':UserField5, :UserField6, :UserField7, :UserField8, :UserField9);'
  );
   }
  FTasksTable := TSQLQuery.Create(self);
  FTasksTable.SQL.Add('SELECT * FROM Tasks');
end;

procedure TVpFirebirdDatastore.CreateAllTables(dbIsNew: Boolean);
var
  tableNames: TStringList;
  needCommit: Boolean;
begin
  needCommit := false;
  if dbIsNew then begin
    CreateTable(ContactsTableName);
    CreateTable(EventsTableName);
    CreateTable(ResourceTableName);
    CreateTable(TasksTableName);
    needCommit := true;
  end else
  begin
    tablenames := TStringList.Create;
    try
      tablenames.CaseSensitive := false;
      FConnection.GetTableNames(tablenames);

      if tablenames.IndexOf(ContactsTableName) = -1 then begin
        CreateTable(ContactsTableName);
        needCommit := true;
      end;

      if tablenames.IndexOf(EventsTableName) = -1 then begin
        CreateTable(EventsTableName);
        needCommit := true;
      end;

      if tablenames.IndexOf(ResourceTableName) = -1 then begin
        CreateTable(ResourceTableName);
        needCommit := true;
      end;

      if tablenames.IndexOf(TasksTableName) = -1 then begin
        CreateTable(TasksTableName);
        needCommit := true;
      end;
    finally
      tablenames.Free;
    end;
  end;

  if needCommit then
    FConnection.Transaction.Commit;
end;

// Connection and tables are active afterwards!
procedure TVpFirebirdDatastore.CreateTables;
var
  wasConnected: Boolean;
  isNew: Boolean;
begin
  isNew := false;
  wasConnected := FConnection.Connected;
  if not FileExistsUTF8(FConnection.DatabaseName) then begin
    FConnection.Connected := false;
    FConnection.CreateDB;
    isNew := true;
  end;
  FConnection.Connected := true;
  CreateAllTables(isNew);
  SetConnected(wasConnected or AutoConnect);
end;

{ Note: Firebird with version < 3 does not support AutoInc fields.
  Use a generator and trigger to create AutoInc values:
  http://www.firebirdfaq.org/faq29/ }
procedure TVpFirebirdDatastore.CreateTable(const ATableName: String);
begin
  if ATableName = ContactsTableName then begin
    FConnection.ExecuteDirect(
      'CREATE TABLE Contacts (' +
        'RecordID       INTEGER NOT NULL PRIMARY KEY, '+
        'ResourceID     INTEGER NOT NULL, ' +
        'FirstName      VARCHAR(50), '+
        'LastName       VARCHAR(50), '+
        'Birthdate      DATE, '+
        'Anniversary    DATE, '+
        'Title          VARCHAR(50), '+
        'Company        VARCHAR(50), '+
        'Job_Position   VARCHAR(30), '+
        'Address        VARCHAR(100), '+
        'City           VARCHAR(50), '+
        'State          VARCHAR(25), '+
        'Zip            VARCHAR(10), '+
        'Country        VARCHAR(25), '+
        'Notes          VARCHAR(1024), '+
        'Phone1         VARCHAR(25), '+
        'Phone2         VARCHAR(25), '+
        'Phone3         VARCHAR(25), '+
        'Phone4         VARCHAR(25), '+
        'Phone5         VARCHAR(25), '+
        'PhoneType1     INTEGER, '+
        'PhoneType2     INTEGER, '+
        'PhoneType3     INTEGER, '+
        'PhoneType4     INTEGER, '+
        'PhoneType5     INTEGER, '+
        'Category       INTEGER, '+
        'EMail          VARCHAR (100), '+
        'Custom1        VARCHAR (100), '+
        'Custom2        VARCHAR (100), '+
        'Custom3        VARCHAR (100), '+
        'Custom4        VARCHAR (100), '+
        'UserField0     VARCHAR(100), '+
        'UserField1     VARCHAR(100), '+
        'UserField2     VARCHAR(100), '+
        'UserField3     VARCHAR(100), '+
        'UserField4     VARCHAR(100), '+
        'UserField5     VARCHAR(100), '+
        'UserField6     VARCHAR(100), '+
        'UserField7     VARCHAR(100), '+
        'UserField8     VARCHAR(100), '+
        'UserField9     VARCHAR(100) )'
    );
    FConnection.ExecuteDirect(
      'CREATE UNIQUE INDEX Contacts_RecordID_idx ON Contacts (RecordID);');
    FConnection.ExecuteDirect(
      'CREATE INDEX Contacts_ResourceID_idx ON Contacts (ResourceID);');

    FConnection.ExecuteDirect(
      'CREATE GENERATOR Contacts_AUTOINC; ');
    FConnection.ExecuteDirect(
      'SET GENERATOR Contacts_AUTOINC TO 0; ');
    FConnection.ExecuteDirect(
      'CREATE TRIGGER C_AUTOINC_TRG FOR Contacts ' +
      'ACTIVE BEFORE INSERT POSITION 0 ' +
      'AS ' +
      'BEGIN ' +
        'NEW.RecordID = GEN_ID(Contacts_AUTOINC, 1); ' +
      'END '
    );
  end else
  if ATableName = EventsTableName then begin
    FConnection.ExecuteDirect(
      'CREATE TABLE Events (' +
        'RecordID         INTEGER NOT NULL PRIMARY KEY, ' +
        'ResourceID       INTEGER NOT NULL, ' +
        'StartTime        TIMESTAMP NOT NULL, ' +
        'EndTime          TIMESTAMP NOT NULL, ' +
        'Description      VARCHAR (255), ' +
        'Location         VARCHAR (255), ' +
        'Notes            VARCHAR (1024), ' +
        'Category         INTEGER, ' +
        'AllDayEvent      CHAR(1), ' +
        'DingPath         VARCHAR (255), ' +
        'AlarmSet         CHAR(1), ' +
        'AlarmAdvance     INTEGER, ' +
        'AlarmAdvanceType INTEGER, ' +
        'SnoozeTime       TIMESTAMP, ' +
        'RepeatCode       INTEGER, ' +
        'RepeatRangeEnd   TIMESTAMP, ' +
        'CustomInterval   INTEGER, ' +
        'UserField0       VARCHAR(100), '+
        'UserField1       VARCHAR(100), '+
        'UserField2       VARCHAR(100), '+
        'UserField3       VARCHAR(100), '+
        'UserField4       VARCHAR(100), '+
        'UserField5       VARCHAR(100), '+
        'UserField6       VARCHAR(100), '+
        'UserField7       VARCHAR(100), '+
        'UserField8       VARCHAR(100), '+
        'UserField9       VARCHAR(100) )'
    );
    FConnection.ExecuteDirect(
      'CREATE UNIQUE INDEX Events_RecordID_idx ON Events (RecordID);');
    FConnection.ExecuteDirect(
      'CREATE INDEX Events_ResourceID_idx ON Events (ResourceID);');
    FConnection.ExecuteDirect(
      'CREATE INDEX Events_StartTime_idx ON Events (StartTime);');
    FConnection.ExecuteDirect(
      'CREATE INDEX Events_EndTime_idx ON Events (EndTime);');

    FConnection.ExecuteDirect(
      'CREATE GENERATOR Events_AUTOINC; ');
    FConnection.ExecuteDirect(
      'SET GENERATOR Events_AUTOINC TO 0; ');
    FConnection.ExecuteDirect(
      'CREATE TRIGGER E_AUTOINC_TRG FOR Events ' +
      'ACTIVE BEFORE INSERT POSITION 0 ' +
      'AS ' +
      'BEGIN ' +
        'NEW.RecordID = GEN_ID(Events_AUTOINC, 1); ' +
      'END '
    );
  end else
  if ATableName = ResourceTableName then begin
    FConnection.ExecuteDirect(
      'CREATE TABLE Resources (' +
        'ResourceID     INTEGER NOT NULL PRIMARY KEY, '+
        'Description    VARCHAR (255), ' +
        'Notes          VARCHAR (1024), ' +
        'ImageIndex     INTEGER, ' +
        'ResourceActive CHAR(1), ' +
        'UserField0     VARCHAR(100), '+
        'UserField1     VARCHAR(100), '+
        'UserField2     VARCHAR(100), '+
        'UserField3     VARCHAR(100), '+
        'UserField4     VARCHAR(100), '+
        'UserField5     VARCHAR(100), '+
        'UserField6     VARCHAR(100), '+
        'UserField7     VARCHAR(100), '+
        'UserField8     VARCHAR(100), '+
        'UserField9     VARCHAR(100) )'
    );
    FConnection.ExecuteDirect(
      'CREATE UNIQUE INDEX Resources_ResourceID_idx ON Resources (ResourceID);');

    FConnection.ExecuteDirect(
      'CREATE GENERATOR Resources_AUTOINC; ');
    FConnection.ExecuteDirect(
      'SET GENERATOR Resources_AUTOINC TO 0; ');
    FConnection.ExecuteDirect(
      'CREATE TRIGGER R_AUTOINC_TRG FOR Resources ' +
      'ACTIVE BEFORE INSERT POSITION 0 ' +
      'AS ' +
      'BEGIN ' +
        'NEW.ResourceID = GEN_ID(Resources_AUTOINC, 1); ' +
      'END '
    );
  end else
  if ATableName = TasksTableName then begin
    FConnection.ExecuteDirect(
      'CREATE TABLE Tasks (' +
        'RecordID       INTEGER NOT NULL PRIMARY KEY, ' +
        'ResourceID     INTEGER NOT NULL, ' +
        'Complete       CHAR(1), ' +
        'Description    VARCHAR(255), ' +
        'Details        VARCHAR(1024), ' +
        'CreatedOn      TIMESTAMP, ' +
        'Priority       INTEGER, ' +
        'Category       INTEGER, ' +
        'CompletedOn    TIMESTAMP, ' +
        'DueDate        TIMESTAMP, ' +
        'UserField0     VARCHAR(100), '+
        'UserField1     VARCHAR(100), '+
        'UserField2     VARCHAR(100), '+
        'UserField3     VARCHAR(100), '+
        'UserField4     VARCHAR(100), '+
        'UserField5     VARCHAR(100), '+
        'UserField6     VARCHAR(100), '+
        'UserField7     VARCHAR(100), '+
        'UserField8     VARCHAR(100), '+
        'UserField9     VARCHAR(100) )'
    );
    FConnection.ExecuteDirect(
      'CREATE UNIQUE INDEX Tasks_RecordID_idx ON Tasks (RecordID);');
    FConnection.ExecuteDirect(
      'CREATE INDEX Tasks_ResourceID_idx ON Tasks (ResourceID);');

    FConnection.ExecuteDirect(
      'CREATE GENERATOR Tasks_AUTOINC; ');
    FConnection.ExecuteDirect(
      'SET GENERATOR Tasks_AUTOINC TO 0; ');
    FConnection.ExecuteDirect(
      'CREATE TRIGGER T_AUTOINC_TRG FOR Tasks ' +
      'ACTIVE BEFORE INSERT POSITION 0 ' +
      'AS ' +
      'BEGIN ' +
        'NEW.RecordID = GEN_ID(Tasks_AUTOINC, 1); ' +
      'END '
    );
  end;
end;

function TVpFirebirdDatastore.GetContactsTable: TDataset;
begin
  Result := FContactsTable;
end;

function TVpFirebirdDatastore.GetEventsTable: TDataset;
begin
  Result := FEventsTable;
end;

function TVpFirebirdDataStore.GetNextID(TableName: string): integer;
begin
  Unused(TableName);
  { This is not needed in the Firebird datastore as these tables use a
    generator and trigger for autoincrement fields.
    http://www.firebirdfaq.org/faq29/ }
  Result := -1;
end;

function TVpFirebirdDatastore.GetResourceTable : TDataset;
begin
  Result := FResourceTable;
end;

function TVpFirebirdDatastore.GetTasksTable : TDataset;
begin
  Result := FTasksTable;
end;

procedure TVpFirebirdDatastore.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Connected := AutoConnect and (AutoCreate or FileExists(FConnection.DatabaseName));
end;

procedure TVpFirebirdDatastore.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FConnection) then
    FConnection := nil;
end;

{ Note: Set the property Required of the PrimaryKey field to false. Otherwise
  Firebird will complain about this field not being specified when posting. }
procedure TVpFirebirdDatastore.OpenTables;
begin
  if FContactsTable.Transaction = nil then
    FContactsTable.Transaction := FConnection.Transaction;
  FContactsTable.Open;
  FContactsTable.Fields[0].Required := false;

  if FEventsTable.Transaction = nil then
    FEventsTable.Transaction := FConnection.Transaction;
  FEventsTable.Open;
  FEventsTable.Fields[0].Required := false;

  if FResourceTable.Transaction = nil then
    FResourceTable.Transaction := FConnection.Transaction;
  FResourceTable.Open;
  FResourceTable.Fields[0].Required := false;

  if FTasksTable.Transaction = nil then
    FTasksTable.Transaction := FConnection.Transaction;
  FTasksTable.Open;
  FTasksTable.Fields[0].Required := false;
end;

procedure TVpFirebirdDatastore.PostContacts;
begin
  inherited;
  FContactsTable.ApplyUpdates;
end;

procedure TVpFirebirdDatastore.PostEvents;
begin
  inherited;
  FEventsTable.ApplyUpdates;
end;

procedure TVpFirebirdDatastore.PostResources;
begin
  inherited;
  FResourceTable.ApplyUpdates;
end;

procedure TVpFirebirdDatastore.PostTasks;
begin
  inherited;
  FTasksTable.ApplyUpdates;
end;

procedure TVpFirebirdDatastore.SetConnected(const AValue: Boolean);
begin
  if (AValue = Connected) or (FConnection = nil) or (FConnectLock > 0) then
    exit;

  inc(FConnectLock);
  if AValue and AutoCreate then
    CreateTables;
  FConnection.Connected := AValue;
  if FConnection.Connected then
    OpenTables;

  inherited SetConnected(AValue);

  if FConnection.Connected then
    Load;
  dec(FConnectLock);
end;
(*
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
end;      *)


procedure TVpFirebirdDatastore.SetConnection(const AValue: TIBConnection);
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
