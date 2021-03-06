{*********************************************************}
{*                  VPBDEDS.PAS 1.03                     *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{$I Vp.INC}

unit VpBDEDS;
  { BDE DataStore Component }

interface

uses
  Windows, Classes, Dialogs, SysUtils, Db, DbTables,
  VpBase, VpData, VpSR, VpBaseDS, VpDBDS, VpException;

type
  TVpBDEDataStore = class(TVpCustomDBDataStore)
  protected{private}
    FDatabase        : TDatabase;
    FAutoCreateAlias : Boolean;
    FResourceTable   : TQuery;
    FEventsTable     : TQuery;
    FContactsTable   : TQuery;
    FTasksTable      : TQuery;
    FRecordIDTable   : TQuery;
    FAliasName       : string;
    FDriverName      : string;
    FLoginPrompt     : boolean;
    FParams          : TStrings;
    FSessionName     : string;
    { property getters }
    function GetDatabaseName: string;
    function GetConnected: Boolean;

    { anscestor property getters }
    function GetResourceTable : TDataset; override;
    function GetEventsTable : TDataset; override;
    function GetContactsTable : TDataset; override;
    function GetTasksTable : TDataset; override;

    { property setters }
    procedure SetAutoCreateAlias(Value: Boolean);
    procedure InitializeRecordIDTable;
    procedure SetAliasName(const Value: string);
    procedure SetConnected(const Value: boolean); override;
    procedure SetDriverName(const Value: string);
    procedure SetLoginPrompt(const Value: boolean);
    procedure SetParams(const Value: TStrings);
    procedure SetFilterCriteria(aTable : TDataset; aUseDateTime : Boolean;
      aResourceID : Integer; aStartDateTime : TDateTime;
      aEndDateTime : TDateTime); override;

    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetNextID(TableName: string): integer; override;
    procedure Load; override;
    procedure CreateTable(TableName: string);
    procedure CreateIndexDefs(const TableName : string;
                                    IndexDefs : TIndexDefs); override;

    procedure PostResources; override;
    procedure PostEvents; override;
    procedure PostContacts; override;
    procedure PostTasks; override;

    procedure PurgeResource(Res: TVpResource); override;
    procedure PurgeEvents(Res: TVpResource); override;
    procedure PurgeContacts(Res: TVpResource); override;
    procedure PurgeTasks(Res: TVpResource); override;

    property Database: TDatabase read FDatabase;

  published
    property AutoConnect;
    property AutoCreate;

    { properties }
    property AutoCreateAlias: Boolean
      read FAutoCreateAlias write SetAutoCreateAlias;
    property DayBuffer;
    property ResourceID;
    property AliasName: string read FAliasName write SetAliasName;
    property DriverName : string read FDriverName write SetDriverName;
    property LoginPrompt : boolean read FLoginPrompt write SetLoginPrompt;
    property Params : TStrings read FParams write SetParams;
    property ReadOnly;
    { events }
  end;

implementation

uses
{$IFDEF VERSION6}
  Variants,
{$ELSE}
  FileCtrl,
{$ENDIF}
 VpConst;

(*****************************************************************************)
{ TVpBDEDataStore }

constructor TVpBDEDataStore.Create(AOwner: TComponent);
begin
  inherited;

  FParams := TStringList.Create;

  FAliasName := '';
  FConnected := false;
  FDriverName := 'STANDARD';
  FLoginPrompt := false;
  FParams.Clear;
  FSessionName := 'Default';
  FResourceID := 0;

  FDatabase := TDatabase.Create(self);
  FDatabase.TransIsolation := tiDirtyRead;

  FResourceTable := TQuery.Create(self);
  FResourceTable.DatabaseName := FDatabase.Name;
  FResourceTable.CachedUpdates := false;
  FResourceTable.SQL.Text := 'SELECT * FROM Resources';

  FEventsTable := TQuery.Create(self);
  FEventsTable.DatabaseName := FDatabase.Name;
  FEventsTable.CachedUpdates := false;
  FEventsTable.SQL.Text := 'SELECT * FROM Events '
  + 'WHERE (ResourceID = :ResID) '
  + 'AND ((StartTime >= :STime AND EndTime <= :ETime) '                  
  + 'OR (RepeatCode > 0 AND :STime <= RepeatRangeEnd))';                 

  FContactsTable := TQuery.Create(self);
  FContactsTable.DatabaseName := FDatabase.Name;
  FContactsTable.CachedUpdates := false;
  FContactsTable.SQL.Text := 'SELECT * FROM Contacts '
  + 'WHERE ResourceID = :ResID';

  FTasksTable := TQuery.Create(self);
  FTasksTable.DatabaseName := FDatabase.Name;
  FTasksTable.CachedUpdates := false;
  FTasksTable.SQL.Text := 'SELECT * FROM Tasks '
  + 'WHERE ResourceID = :ResID';

  FRecordIDTable := TQuery.Create(self);
  FRecordIDTable.DatabaseName := FDatabase.Name;                         
  FRecordIDTable.CachedUpdates := false;                                 

  FDatabase.DatabaseName := '';
  FDatabase.AliasName := FAliasName;
  FDatabase.Connected := false;
  FDatabase.DriverName := FDriverName;
  FDatabase.LoginPrompt := FLoginPrompt;
  FDatabase.Params := FParams;
  FDatabase.ReadOnly := FReadOnly;
  FDatabase.SessionName := FSessionName;
end;
{=====}

destructor TVpBDEDataStore.Destroy;
begin
  FParams.Free;

  { free tables }
  FDatabase.Close;
  FDatabase.Free;
  FResourceTable.Free;
  FEventsTable.Free;
  FContactsTable.Free;
  FTasksTable.Free;
  FRecordIDTable.Free;

  inherited;
end;
{=====}

function TVpBDEDataStore.GetDatabaseName: string;
begin
  result := FDataBase.DatabaseName;
end;
{=====}

function TVpBDEDataStore.GetConnected: Boolean;
begin
  result := FDatabase.Connected;
end;
{=====}

function TVpBDEDataStore.GetResourceTable : TDataset;
begin
  Result := FResourceTable;
end;
{=====}

function TVpBDEDataStore.GetEventsTable : TDataset;
begin
  Result := FEventsTable;
end;
{=====}

function TVpBDEDataStore.GetContactsTable : TDataset;
begin
  Result := FContactsTable
end;
{=====}

function TVpBDEDataStore.GetTasksTable : TDataset;
begin
  Result := FTasksTable;
end;
{=====}

procedure TVpBDEDataStore.Load;
begin
  if not FDatabase.Connected then exit;

  FResourceTable.Close;
  FEventsTable.Close;
  FContactsTable.Close;
  FTasksTable.Close;

  inherited;
end;
{=====}

function TVpBDEDataStore.GetNextID(TableName: string): Integer;
var
  Query: TQuery;
  GotIt: Boolean;
  ID   : Integer;
  FieldName: string;
begin
  { The BDEDataStore uses a support table called RecordIDS, or whatever is   }
  { defined in the RecordIDTableName constant.  It has one record, and is    }
  { used to keep track of the last ID used for each table.                   }

  { In a multi-user environment, This prevents collisions between two users  }
  { who happen to enter the same type of new record at the same time.        }

  { New record ID's are created here and then the Record ID table is         }
  { immediately updated to reflect the new value.  If the table is           }
  { unsuccessfully updated, then it is assumed that another user has claimed }
  { that ID, so the ID is incremented and another attempt is made, until we  }
  { are successful.                                                          }

  Query := TQuery.Create(self);
  ID := 0;
  try
    Query.DatabaseName := FDatabase.DatabaseName;

    Query.Sql.Text := 'Select * from ' + RecordIDTableName;
    Query.Open;

    if TableName = ResourceTableName then begin
      FieldName := 'ResourceID';
      ID := Query.FieldByName('ResourceID').AsInteger;

    end else if TableName = TasksTableName then begin
      FieldName := 'TaskID';
      ID := Query.FieldByName('TaskID').AsInteger;

    end else if TableName = EventsTableName then begin
      FieldName := 'EventID';
      ID := Query.FieldByName('EventID').AsInteger;

    end else if TableName = ContactsTableName then begin
      FieldName := 'ContactID';
      ID := Query.FieldByName('ContactID').AsInteger;

    end else begin
      raise EInvalidTable.Create;
      Exit;
    end;

    Query.Close;
    Query.SQL.Text := 'Update ' + RecordIDTableName + ' Set ' + FieldName
      + ' = :NewID Where (' + FieldName + ' = :OldID)';

    GotIt := false;
    while not GotIt do begin
      Inc(ID);
      Query.ParamByName('NewID').AsInteger := ID;
      Query.ParamByName('OldID').AsInteger := ID - 1;
      Query.ExecSQL;

      GotIt := (Query.RowsAffected = 1);
    end;
  finally
    Query.Close;
    Query.Free;
  end;

  result := ID;
end;
{=====}

procedure TVpBDEDataStore.CreateTable(TableName: string);
var
  Table: TTable;
begin
  Table := TTable.Create(self);
  try
    Table.DatabaseName := FDatabase.DatabaseName;

    if TableName = ResourceTableName then begin
      { Create Resources Table }
      Table.Active := false;
      Table.TableName := ResourceTableName;
    end

    else if TableName = EventsTableName then begin
      { Create Events Table }
      Table.Active := false;
      Table.TableName := EventsTableName;
    end

    else if TableName = ContactsTableName then begin
      { Create Contacts Table }
      Table.Active := false;
      Table.TableName := ContactsTableName;
    end

    else if TableName = TasksTableName then begin
      { Create Tasks Table }
      Table.Active := false;
      Table.TableName := TasksTableName;
    end

    else if TableName = RecordIDTableName then begin
      { Create Tasks Table }
      Table.Active := false;
      Table.TableName := RecordIDTableName;
    end;

    Table.DatabaseName := FDatabase.DatabaseName;
    Table.TableType := ttParadox;
    CreateFieldDefs(TableName, Table.FieldDefs);
    CreateIndexDefs(TableName, Table.IndexDefs);

    if Table <> nil then
      Table.CreateTable;

    if TableName = RecordIDTableName then
      InitializeRecordIDTable;

  finally
    Table.Free;
  end;
end;
{=====}

procedure TVpBDEDataStore.InitializeRecordIDTable;
var
  Qry: TQuery;
  ID: Integer;
begin
  Qry := TQuery.Create(self);
  try
    Qry.DatabaseName := FDatabase.DatabaseName;

    Qry.SQL.Text := 'Select * from ' + RecordIDTableName;
    Qry.Open;
    if Qry.RowsAffected = 0 then begin
      { create one record in the table }
      Qry.SQL.Clear;
      Qry.SQL.Text := 'INSERT INTO ' + RecordIDTableName
      + '(ResourceID, EventID, TaskID, ContactID) '
      + 'VALUES(0, 0, 0, 0)';
      Qry.ExecSQL;
    end;
    Qry.Close;

    { Initialize Resource ID }
    Qry.SQL.Text := 'Select Max(ResourceID) as MaxRes from '
      + ResourceTableName;
    Qry.Open;
    ID := Qry.Fields[0].AsInteger;
    Qry.Close;

    Qry.SQL.Text := 'Update ' + RecordIDTableName + ' Set ResourceID = :ResID';
    Qry.ParamByName('ResID').AsInteger := ID;
    Qry.ExecSQL;

    { Initialize Event RecordID }
    Qry.SQL.Text := 'Select Max(RecordID) as MaxEvent from '
      + EventsTableName;
    Qry.Open;
    ID := Qry.Fields[0].AsInteger;
    Qry.Close;

    Qry.SQL.Text := 'Update ' + RecordIDTableName + ' Set EventID = :EvID';
    Qry.ParamByName('EvID').AsInteger := ID;
    Qry.ExecSQL;

    { Initialize Contact RecordID }
    Qry.SQL.Text := 'Select Max(RecordID) as MaxContact from '
      + ContactsTableName;
    Qry.Open;
    ID := Qry.Fields[0].AsInteger;
    Qry.Close;

    Qry.SQL.Text := 'Update ' + RecordIDTableName + ' Set ContactID = :CoID';
    Qry.ParamByName('CoID').AsInteger := ID;
    Qry.ExecSQL;

    { Initialize Task RecordID }
    Qry.SQL.Text := 'Select Max(RecordID) as MaxTask from ' + TasksTableName;
    Qry.Open;
    ID := Qry.Fields[0].AsInteger;
    Qry.Close;

    Qry.SQL.Text := 'Update ' + RecordIDTableName + ' Set TaskID = :TsID';
    Qry.ParamByName('TsID').AsInteger := ID;
    Qry.ExecSQL;

  finally
    Qry.Free;
  end;
end;
{=====}

procedure TVpBDEDataStore.SetAliasName(const Value: string);
var
  WasOpen: Boolean;
begin
  WasOpen := Connected;
  SetConnected(False);
  if FAliasName <> Value then begin
    FAliasName := Value;
    FDatabase.AliasName := FAliasName;
  end;
  SetConnected(WasOpen);
end;
{=====}

procedure TVpBDEDataStore.SetAutoCreateAlias(Value: Boolean);
begin
  if Value <> FAutoCreateAlias then
    FAutoCreateAlias := Value;
end;
{=====}

procedure TVpBDEDataStore.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Connected := AutoConnect;
end;
{=====}

procedure TVpBDEDataStore.PostResources;
var
  I: Integer;
  Resource: TVpResource;
  Qry: TQuery;
begin
  if (Resources.Count > 0) then begin
    Qry := TQuery.Create(self);
    Qry.DatabaseName := AliasName;
    Qry.RequestLive := true;
    try
      for I := pred(Resources.Count) downto 0 do begin
        Resource := Resources.Items[I];
        if Resource = nil then begin
          Continue;
        end;

        if Resource.Deleted then begin
          PurgeEvents(Resource);
          PurgeContacts(Resource);
          PurgeTasks(Resource);
          Qry.SQL.Text := 'DELETE FROM Resources '
          + 'WHERE ResourceID = :ResID';
          Qry.ParamByName('ResID').AsInteger := Resource.ResourceID;
          Qry.ExecSQL;
          Resource.Free;
          Continue;
        end

        else if Resource.Changed then begin
          Qry.SQL.Text := 'SELECT * FROM Resources '
            + 'WHERE ResourceID = :ResID';
          Qry.ParamByName('ResID').AsInteger := Resource.ResourceID;
          Qry.Open;

          if Qry.Locate('ResourceID', Resource.ResourceID, [])
          then begin
            { existing record found }
            Qry.Edit;
            try
              Qry.FieldByName('ResourceID').AsInteger := Resource.ResourceID;
              Qry.FieldByName('Description').AsString := Resource.Description;
              Qry.FieldByName('Notes').AsString := Resource.Notes;
              Qry.FieldByName('ResourceActive').AsBoolean := Resource.ResourceActive;
              Qry.FieldByName('UserField0').AsString := Resource.UserField0;
              Qry.FieldByName('UserField1').AsString := Resource.UserField1;
              Qry.FieldByName('UserField2').AsString := Resource.UserField2;
              Qry.FieldByName('UserField3').AsString := Resource.UserField3;
              Qry.FieldByName('UserField4').AsString := Resource.UserField4;
              Qry.FieldByName('UserField5').AsString := Resource.UserField5;
              Qry.FieldByName('UserField6').AsString := Resource.UserField6;
              Qry.FieldByName('UserField7').AsString := Resource.UserField7;
              Qry.FieldByName('UserField8').AsString := Resource.UserField8;
              Qry.FieldByName('UserField9').AsString := Resource.UserField9;
              Qry.Post;
            except
              Qry.Cancel;
              raise EDBPostError.Create;
            end;
          end else begin
            Qry.SQL.Clear;
            Qry.SQL.Text := 'INSERT INTO Resources '
            + '(ResourceID, Description, Notes, ResourceActive, UserField0, '
            + 'UserField1, UserField2, UserField3, UserField4, UserField5, '
            + 'UserField6, UserField7, UserField8, UserField9) '
            + 'VALUES(:ResID, :Descr, :Notes, :ResActive, :UserField0, '
            + ':UserField1, :UserField2, :UserField3, :UserField4, '
            + ':UserField5, :UserField6, :UserField7, :UserField8, '
            + ':UserField9)';

            Qry.ParamByName('ResID').AsInteger :=  Resource.ResourceID;
            Qry.ParamByName('Descr').Asstring := Resource.Description;
            Qry.ParamByName('Notes').AsString := Resource.Notes;
            Qry.ParamByName('ResActive').AsBoolean := Resource.Active;
            Qry.ParamByName('UserField0').AsString := Resource.UserField0;
            Qry.ParamByName('UserField1').AsString := Resource.UserField1;
            Qry.ParamByName('UserField2').AsString := Resource.UserField2;
            Qry.ParamByName('UserField3').AsString := Resource.UserField3;
            Qry.ParamByName('UserField4').AsString := Resource.UserField4;
            Qry.ParamByName('UserField5').AsString := Resource.UserField5;
            Qry.ParamByName('UserField6').AsString := Resource.UserField6;
            Qry.ParamByName('UserField7').AsString := Resource.UserField7;
            Qry.ParamByName('UserField8').AsString := Resource.UserField8;
            Qry.ParamByName('UserField9').AsString := Resource.UserField9;

            Qry.ExecSQL;
          end;
          Resource.Changed := false;
        end;
        { if this is the active resource, then update all of its stuff }
        if Resource.ResourceID = ResourceID then begin
          PostEvents;
          PostContacts;
          PostTasks;
        end;
      end;
      Resources.Sort;
      NotifyDependents;
    finally
      Qry.Close;
      Qry.Free;
    end;
  end;
end;
{=====}

procedure TVpBDEDataStore.PostEvents;
var
  I: Integer;
  Event: TVpEvent;
  Qry: TQuery;
  F: TField;
  FixedLoc, FixedLocP: String;
begin
  if (Resource <> nil) and Resource.EventsDirty then begin
    Qry := TQuery.Create(self);
    try
      Qry.DatabaseName := AliasName;
      Qry.RequestLive := true;

      for I := pred(Resource.Schedule.EventCount) downto 0 do begin
        Event := Resource.Schedule.GetEvent(I);
        if Event.Deleted then begin
          Qry.SQL.Text := 'DELETE FROM Events '
          + 'WHERE RecordID = :ID';
          Qry.ParamByName('ID').AsInteger := Event.RecordID;
          Qry.ExecSQL;
          Event.Free;
          Continue;
        end

        else if Event.Changed then begin
          Qry.SQL.Text := 'SELECT * FROM Events '
            + 'WHERE RecordID = :ID';
          Qry.ParamByName('ID').AsInteger := Event.RecordID;
          Qry.Open;

          if Qry.FieldByName('Location') = nil then begin
            FixedLoc := '';
            FixedLocP := '';
          end else begin
            FixedLoc := 'Location, ';
            FixedLocP := ':Loc, ';
          end;

          if Qry.Locate('RecordID', Event.RecordID, [])
          then begin
            { existing record found }
            Qry.Edit;
            try
              Qry.FieldByName('RecordID').AsInteger := Event.RecordID;
              Qry.FieldByName('StartTime').AsDateTime := Event.StartTime;
              Qry.FieldByName('EndTime').AsDateTime := Event.EndTime;
              Qry.FieldByName('ResourceID').AsInteger := Resource.ResourceID;
              Qry.FieldByName('Description').AsString := Event.Description;
              F := Qry.FieldByName('Location');               // newe
              if F <> nil then F.AsString := Event.Location;
              Qry.FieldByName('Notes').AsString := Event.Notes;
              Qry.FieldByName('Category').AsInteger := Event.Category;
              Qry.FieldByName('DingPath').AsString := Event.DingPath;
              Qry.FieldByName('AllDayEvent').AsBoolean := Event.AllDayEvent;
              Qry.FieldByName('AlarmSet').AsBoolean := Event.AlarmSet;
              Qry.FieldByName('AlarmAdvance').AsInteger := Event.AlarmAdvance;
              Qry.FieldByName('AlarmAdvanceType').AsInteger := Ord(Event.AlarmAdvanceType);
              Qry.FieldByName('SnoozeTime').AsDateTime := Event.SnoozeTime;
              Qry.FieldByName('RepeatCode').AsInteger := Ord(Event.RepeatCode);
              Qry.FieldByName('RepeatRangeEnd').AsDateTime := Event.RepeatRangeEnd;
              Qry.FieldByName('CustomInterval').AsInteger := Event.CustomInterval;
              Qry.FieldByName('UserField0').AsString := Event.UserField0;
              Qry.FieldByName('UserField1').AsString := Event.UserField1;
              Qry.FieldByName('UserField2').AsString := Event.UserField2;
              Qry.FieldByName('UserField3').AsString := Event.UserField3;
              Qry.FieldByName('UserField4').AsString := Event.UserField4;
              Qry.FieldByName('UserField5').AsString := Event.UserField5;
              Qry.FieldByName('UserField6').AsString := Event.UserField6;
              Qry.FieldByName('UserField7').AsString := Event.UserField7;
              Qry.FieldByName('UserField8').AsString := Event.UserField8;
              Qry.FieldByName('UserField9').AsString := Event.UserField9;
              Qry.Post;
            except
              Qry.Cancel;
              raise EDBPostError.Create;
            end;
          end else begin
            Qry.Close;
            Qry.SQL.Text := 'INSERT INTO Events '
            + '(RecordID, StartTime, EndTime, ResourceID, Description, ' + FixedLocation
            + 'Notes, SnoozeTime, Category, DingPath, AllDayEvent, AlarmSet, '
            + 'AlarmAdvance, AlarmAdvanceType, RepeatCode, '
            + 'RepeatRangeEnd, CustomInterval, '
            + 'UserField0, UserField1, UserField2, UserField3, UserField4, '
            + 'UserField5, UserField6, UserField7, UserField8, UserField9) '
            + 'VALUES(:RecID, :STime, :ETime, :ResID, :Desc, ' + FixedLoc
            + ':Notes, :SnTime, :Cat, :DPath, :ADEvent, :ASet, :AAdvance, '
            + ':AAdvanceType, :RCode, :RRangeEnd, :CInterval, :UserField0, '
            + ':UserField1, :UserField2, :UserField3, :UserField4, '
            + ':UserField5, :UserField6, :UserField7, :UserField8, '
            + ':UserField9)';

            Qry.ParamByName('RecID').AsInteger := Event.RecordID;
            Qry.ParamByName('STime').AsDateTime := Event.StartTime;
            Qry.ParamByName('ETime').AsDateTime := Event.EndTime;
            Qry.ParamByName('ResID').AsInteger := Resource.ResourceID;
            Qry.ParamByName('Desc').AsString := Event.Description;
            if FixedLocP <> '' then Qry.ParamByName('Loc').AsString := Event.Location;
            Qry.ParamByName('Notes').AsString := Event.Notes;
            Qry.ParamByName('SnTime').AsDateTime := Event.SnoozeTime;
            Qry.ParamByName('Cat').AsInteger := Event.Category;
            Qry.ParamByName('DPath').AsString := Event.DingPath;
            Qry.ParamByName('ADEvent').AsBoolean := Event.AllDayEvent;
            Qry.ParamByName('ASet').AsBoolean := Event.AlarmSet;
            Qry.ParamByName('AAdvance').AsInteger := Event.AlarmAdv;
            Qry.ParamByName('AAdvanceType').AsInteger := Ord(Event.AlarmAdvType);
            Qry.ParamByName('RCode').AsInteger := Ord(Event.RepeatCode);
            Qry.ParamByName('RRangeEnd').AsDateTime := Event.RepeatRangeEnd;
            Qry.ParamByName('CInterval').AsInteger := Event.CustInterval;
            Qry.ParamByName('UserField0').AsString := Event.UserField0;
            Qry.ParamByName('UserField1').AsString := Event.UserField1;
            Qry.ParamByName('UserField2').AsString := Event.UserField2;
            Qry.ParamByName('UserField3').AsString := Event.UserField3;
            Qry.ParamByName('UserField4').AsString := Event.UserField4;
            Qry.ParamByName('UserField5').AsString := Event.UserField5;
            Qry.ParamByName('UserField6').AsString := Event.UserField6;
            Qry.ParamByName('UserField7').AsString := Event.UserField7;
            Qry.ParamByName('UserField8').AsString := Event.UserField8;
            Qry.ParamByName('UserField9').AsString := Event.UserField9;

            Qry.ExecSQL;
          end;
          Event.Changed := false;
        end;
      end;
      Resource.Schedule.Sort;
      NotifyDependents;
    finally
      Qry.Close;
      Qry.Free;
    end;
    Resource.EventsDirty := false;
  end;
end;
{=====}

procedure TVpBDEDataStore.PostContacts;
var
  I: Integer;
  Contact: TVpContact;
  Qry: TQuery;
  F: TField;
  FixedNote, FixedNoteP: String;
begin
  if (Resource <> nil) and Resource.ContactsDirty then begin
    { Dump this resource's dirty contacts to the DB }
    Qry := TQuery.Create(self);
    try
      Qry.DatabaseName := AliasName;
      Qry.RequestLive := true;

      for I := pred(Resource.Contacts.Count) downto 0 do begin
        Contact := Resource.Contacts.GetContact(I);

        if Contact.Deleted then begin
          Qry.SQL.Text := 'DELETE FROM Contacts '
          + 'WHERE RecordID = :ID';
          Qry.ParamByName('ID').AsInteger := Contact.RecordID;
          Qry.ExecSQL;
          Contact.Free;
          Continue;
        end

        else if Contact.Changed then begin
          Qry.SQL.Text := 'SELECT * FROM Contacts '
            + 'WHERE RecordID = :ID';
          Qry.ParamByName('ID').AsInteger := Contact.RecordID;
          Qry.Open;

          // Fix name change of "Note" field
          if Qry.FieldByName('Notes') <> nil then FixedNote := 'Notes, ' else
            if Qry.FieldByName('Note') <> nil then FixedNote := 'Note, '
              else FixedNote := '';
          if FixedNote <> '' then FixedNoteP := ':Notes, ' else FixeNoteP := '';

          if Qry.Locate('RecordID', Contact.RecordID, [])
          then begin
            { existing record found }
            Qry.Edit;
            try
              Qry.FieldByName('ResourceID').AsInteger := Resource.ResourceID;
              Qry.FieldByName('RecordID').AsInteger := Contact.RecordID;
              Qry.FieldByName('FirstName').AsString := Contact.FirstName;
              Qry.FieldByName('LastName').AsString := Contact.LastName;
              { - begin}
              Qry.FieldByName('Birthdate').AsDateTime := Contact.BirthDate;
              Qry.FieldByName('Anniversary').AsDateTime := Contact.Anniversary;
              { - end}
              Qry.FieldByName('Title').AsString := Contact.Title;
              Qry.FieldByName('Company').AsString := Contact.Company;
              Qry.FieldByName('Job_Position').AsString := Contact.Position;
              Qry.FieldByName('EMail').AsString := Contact.EMail;
              Qry.FieldByName('Address').AsString := Contact.Address;
              Qry.FieldByName('City').AsString := Contact.City;
              Qry.FieldByName('State').AsString := Contact.State;
              Qry.FieldByName('Zip').AsString := Contact.Zip;
              Qry.FieldByName('Country').AsString := Contact.Country;
              F := Qry.FieldByName('Notes');
              if F = nil then F := Qry.FieldByName('Note');  // deprecated
              if F <> nil then F.AsString := Contact.Notes;
              Qry.FieldByName('Phone1').AsString := Contact.Phone1;
              Qry.FieldByName('Phone2').AsString := Contact.Phone2;
              Qry.FieldByName('Phone3').AsString := Contact.Phone3;
              Qry.FieldByName('Phone4').AsString := Contact.Phone4;
              Qry.FieldByName('Phone5').AsString := Contact.Phone5;
              Qry.FieldByName('PhoneType1').AsInteger := Contact.PhoneType1;
              Qry.FieldByName('PhoneType2').AsInteger := Contact.PhoneType2;
              Qry.FieldByName('PhoneType3').AsInteger := Contact.PhoneType3;
              Qry.FieldByName('PhoneType4').AsInteger := Contact.PhoneType4;
              Qry.FieldByName('PhoneType5').AsInteger := Contact.PhoneType5;
              Qry.FieldByName('Category').AsInteger := Contact.Category;
              Qry.FieldByName('Custom1').AsString := Contact.Custom1;
              Qry.FieldByName('Custom2').AsString := Contact.Custom2;
              Qry.FieldByName('Custom3').AsString := Contact.Custom3;
              Qry.FieldByName('Custom4').AsString := Contact.Custom4;
              Qry.FieldByName('UserField0').AsString := Contact.UserField0;
              Qry.FieldByName('UserField1').AsString := Contact.UserField1;
              Qry.FieldByName('UserField2').AsString := Contact.UserField2;
              Qry.FieldByName('UserField3').AsString := Contact.UserField3;
              Qry.FieldByName('UserField4').AsString := Contact.UserField4;
              Qry.FieldByName('UserField5').AsString := Contact.UserField5;
              Qry.FieldByName('UserField6').AsString := Contact.UserField6;
              Qry.FieldByName('UserField7').AsString := Contact.UserField7;
              Qry.FieldByName('UserField8').AsString := Contact.UserField8;
              Qry.FieldByName('UserField9').AsString := Contact.UserField9;

              Qry.Post;
            except
              Qry.Cancel;
              raise EDBPostError.Create;
            end;
          end else begin
            Qry.Close;

            { - Modified}
            Qry.SQL.Text := 'INSERT INTO Contacts '
            + '(ResourceID, RecordID, FirstName, LastName, Birthdate, '
            + 'Anniversary, Title, Company, Job_Position, EMail, Address, '
            + 'City, State, Zip, Country, ' + FixedNote + 'Phone1, Phone2, Phone3, '
            + 'Phone4, Phone5, PhoneType1, PhoneType2, PhoneType3, PhoneType4, '
            + 'PhoneType5, Category, Custom1, Custom2, Custom3, Custom4, '
            + 'UserField0, UserField1, UserField2, UserField3, UserField4, '
            + 'UserField5, UserField6, UserField7, UserField8, UserField9 ) '

            + 'VALUES(:ResourceID, :RecordID, :FirstName, :LastName, '
            + ':Birthdate, :Anniversary, :Title, :Company, :Job_Position, '
            + ':EMail, :Address, :City, :State, :Zip, :Country, ' + FixedNoteP
            + ':Phone1, :Phone2, :Phone3, :Phone4, :Phone5, :PhoneType1, '
            + ':PhoneType2, :PhoneType3, :PhoneType4, :PhoneType5, :Category, '
            + ':Custom1, :Custom2, :Custom3, :Custom4, :UserField0, '
            + ':UserField1, :UserField2, :UserField3, :UserField4, :UserField5, '
            + ':UserField6, :UserField7, :UserField8, :UserField9)';

            Qry.ParamByName('ResourceID').AsInteger := Resource.ResourceID;
            Qry.ParamByName('RecordID').AsInteger := Contact.RecordID;
            Qry.ParamByName('FirstName').AsString := Contact.FirstName;
            Qry.ParamByName('LastName').AsString := Contact.LastName;
            { - begin}
            Qry.ParamByName('Birthdate').AsDateTime := Contact.Birthdate;
            Qry.ParamByName('Anniversary').AsDateTime := Contact.Anniversary;
            { - end}
            Qry.ParamByName('Title').AsString := Contact.Title;
            Qry.ParamByName('Company').AsString := Contact.Company;
            Qry.ParamByName('Job_Position').AsString := Contact.Position;
            Qry.ParamByName('EMail').AsString := Contact.EMail;
            Qry.ParamByName('Address').AsString := Contact.Address;
            Qry.ParamByName('City').AsString := Contact.City;
            Qry.ParamByName('State').AsString := Contact.State;
            Qry.ParamByName('Zip').AsString := Contact.Zip;
            Qry.ParamByName('Country').AsString := Contact.Country;
            if FixedNote <> '' then Qry.ParamByName('Notes').AsString := Contact.Notes;
            Qry.ParamByName('Phone1').AsString := Contact.Phone1;
            Qry.ParamByName('Phone2').AsString := Contact.Phone2;
            Qry.ParamByName('Phone3').AsString := Contact.Phone3;
            Qry.ParamByName('Phone4').AsString := Contact.Phone4;
            Qry.ParamByName('Phone5').AsString := Contact.Phone5;
            Qry.ParamByName('PhoneType1').AsInteger := Contact.PhoneType1;
            Qry.ParamByName('PhoneType2').AsInteger := Contact.PhoneType2;
            Qry.ParamByName('PhoneType3').AsInteger := Contact.PhoneType3;
            Qry.ParamByName('PhoneType4').AsInteger := Contact.PhoneType4;
            Qry.ParamByName('PhoneType5').AsInteger := Contact.PhoneType5;
            Qry.ParamByName('Category').AsInteger := Contact.Category;
            Qry.ParamByName('Custom1').AsString := Contact.Custom1;
            Qry.ParamByName('Custom2').AsString := Contact.Custom2;
            Qry.ParamByName('Custom3').AsString := Contact.Custom3;
            Qry.ParamByName('Custom4').AsString := Contact.Custom4;
            Qry.ParamByName('UserField0').AsString := Contact.UserField0;
            Qry.ParamByName('UserField1').AsString := Contact.UserField1;
            Qry.ParamByName('UserField2').AsString := Contact.UserField2;
            Qry.ParamByName('UserField3').AsString := Contact.UserField3;
            Qry.ParamByName('UserField4').AsString := Contact.UserField4;
            Qry.ParamByName('UserField5').AsString := Contact.UserField5;
            Qry.ParamByName('UserField6').AsString := Contact.UserField6;
            Qry.ParamByName('UserField7').AsString := Contact.UserField7;
            Qry.ParamByName('UserField8').AsString := Contact.UserField8;
            Qry.ParamByName('UserField9').AsString := Contact.UserField9;

            Qry.ExecSQL;
          end;
          Contact.Changed := false;
        end;
      end;

    finally
      Qry.Free;
    end;
    Resource.ContactsDirty := false;
  end;
end;
{=====}

procedure TVpBDEDataStore.PostTasks;
var
  I: Integer;
  Task: TVpTask;
  Qry : TQuery;
begin
  if (Resource <> nil) and Resource.TasksDirty then begin
    { Dump this resource's dirty contacts to the DB }
    Qry := TQuery.Create(self);
    try
      Qry.DatabaseName := AliasName;
      Qry.RequestLive := true;

      for I := pred(Resource.Tasks.Count) downto 0 do begin
        Task := Resource.Tasks.GetTask(I);
        if Task.Deleted then begin
          Qry.SQL.Text := 'DELETE FROM Tasks '
          + 'WHERE RecordID = :ID';
          Qry.ParamByName('ID').AsInteger := Task.RecordID;
          Qry.ExecSQL;
          Task.Free;
          Continue;
        end

        else if Task.Changed then begin
          Qry.SQL.Text := 'SELECT * FROM Tasks '
            + 'WHERE RecordID = :ID';
          Qry.ParamByName('ID').AsInteger := Task.RecordID;
          Qry.Open;

          if Qry.Locate('RecordID', Task.RecordID, [])
          then begin
            { existing record found }
            Qry.Edit;
            try
              Qry.FieldByName('ResourceID').AsInteger := Resource.ResourceID;
              Qry.FieldByName('Description').AsString := Task.Description;
              Qry.FieldByName('Details').AsString := Task.Details;
              Qry.FieldByName('Complete').AsBoolean := Task.Complete;
              Qry.FieldByName('DueDate').AsDateTime := Task.DueDate;
              Qry.FieldByName('CreatedOn').AsDateTime := Task.CreatedOn;
              Qry.FieldByName('CompletedOn').AsDateTime := Task.CompletedOn;
              Qry.FieldByName('Priority').AsInteger := Task.Priority;
              Qry.FieldByName('Category').AsInteger := Task.Category;
              Qry.FieldByName('UserField0').AsString := Task.UserField0;
              Qry.FieldByName('UserField1').AsString := Task.UserField1;
              Qry.FieldByName('UserField2').AsString := Task.UserField2;
              Qry.FieldByName('UserField3').AsString := Task.UserField3;
              Qry.FieldByName('UserField4').AsString := Task.UserField4;
              Qry.FieldByName('UserField5').AsString := Task.UserField5;
              Qry.FieldByName('UserField6').AsString := Task.UserField6;
              Qry.FieldByName('UserField7').AsString := Task.UserField7;
              Qry.FieldByName('UserField8').AsString := Task.UserField8;
              Qry.FieldByName('UserField9').AsString := Task.UserField9;
              Qry.Post;
            except
              Qry.Cancel;
              raise EDBPostError.Create;
            end;
          end else begin
            Qry.Close;
            Qry.SQL.Text := 'INSERT INTO Tasks '
            + '(RecordID, ResourceID, Description, Details, '
            + 'Complete, DueDate, CreatedOn, CompletedOn, Priority, Category, '
            + 'UserField0, UserField1, UserField2, UserField3, UserField4, '
            + 'UserField5, UserField6, UserField7, UserField8, UserField9) '

            + 'VALUES(:RecordID, :ResourceID, :Description, :Details, '
            + ':Complete, :DueDate, :CreatedOn, :CompletedOn, :Priority, '
            + ':Category, :UserField0, :UserField1, :UserField2, :UserField3, '
            + ':UserField4, :UserField5, :UserField6, :UserField7, '
            + ':UserField8, :UserField9)';

            Qry.ParamByName('RecordID').AsInteger     := Task.RecordID;
            Qry.ParamByName('ResourceID').AsInteger   := Resource.ResourceID;
            Qry.ParamByName('Description').AsString   := Task.Description;
            Qry.ParamByName('Details').AsString       := Task.Details;
            Qry.ParamByName('Complete').AsBoolean     := Task.Complete;
            Qry.ParamByName('DueDate').AsDateTime     := Task.DueDate;
            Qry.ParamByName('CreatedOn').AsDateTime   := Task.CreatedOn;
            Qry.ParamByName('CompletedOn').AsDateTime := Task.CompletedOn;
            Qry.ParamByName('Priority').AsInteger := Task.Priority;
            Qry.ParamByName('Category').AsInteger := Task.Category;
            Qry.ParamByName('UserField0').AsString := Task.UserField0;
            Qry.ParamByName('UserField1').AsString := Task.UserField1;
            Qry.ParamByName('UserField2').AsString := Task.UserField2;
            Qry.ParamByName('UserField3').AsString := Task.UserField3;
            Qry.ParamByName('UserField4').AsString := Task.UserField4;
            Qry.ParamByName('UserField5').AsString := Task.UserField5;
            Qry.ParamByName('UserField6').AsString := Task.UserField6;
            Qry.ParamByName('UserField7').AsString := Task.UserField7;
            Qry.ParamByName('UserField8').AsString := Task.UserField8;
            Qry.ParamByName('UserField9').AsString := Task.UserField9;
            Qry.ExecSQL;
          end;
          Task.Changed := false;
        end
      end;

    finally
      Qry.Free;
    end;

    Resource.TasksDirty := false;
  end;
end;
{=====}

procedure TVpBDEDataStore.PurgeResource(Res: TVpResource);
begin
  Resource.Deleted := true;
  PostResources;
  Load;
end;
{=====}

procedure TVpBDEDataStore.PurgeEvents(Res: TVpResource);
var
  Qry: TQuery;
begin
  Qry := TQuery.Create(self);
  try
    Qry.DatabaseName := FDataBase.DatabaseName;

    Qry.SQL.Text := 'delete from Events where ResourceID = :ResID';
    Qry.ParamByName('ResID').AsInteger := Resource.ResourceID;
    Qry.ExecSQL;
  finally
    Qry.Free;
  end;
  Resource.Schedule.ClearEvents;
end;
{=====}

procedure TVpBDEDataStore.PurgeContacts(Res: TVpResource);
var
  Qry: TQuery;
begin
  Qry := TQuery.Create(self);
  try
    Qry.DatabaseName := FDataBase.DatabaseName;

    Qry.SQL.Text := 'delete from Contacts where ResourceID = :ResID';
    Qry.ParamByName('ResID').AsInteger := Resource.ResourceID;
    Qry.ExecSQL;
  finally
    Qry.Free;
  end;
  Resource.Contacts.ClearContacts;
end;
{=====}

procedure TVpBDEDataStore.PurgeTasks(Res: TVpResource);
var
  Qry: TQuery;
begin
  Qry := TQuery.Create(self);
  try
    Qry.DatabaseName := FDataBase.DatabaseName;

    Qry.SQL.Text := 'delete from Tasks where ResourceID = :ResID';
    Qry.ParamByName('ResID').AsInteger := Resource.ResourceID;
    Qry.ExecSQL;
  finally
    Qry.Free;
  end;
  Resource.Tasks.ClearTasks;
end;
{=====}

procedure TVpBDEDataStore.SetConnected(const Value: boolean);
var
  Tmp, AliasPath: string;
  Qry: TQuery;
  StringList: TStringList;
begin
  { disconnect if destroying }
  if csDestroying in ComponentState then begin
    FDataBase.Connected := false;
    Exit;
  end;

  { Don't connect at designtime }
  if csDesigning in ComponentState then Exit;

  { Don't try to connect until we're all loaded up }
  if csLoading in ComponentState then Exit;

  if FAutoCreateAlias then begin
    { if there is no defined alias name then create one based on the }
    { application executable file's name and assign it to FDatabase  }
    if FAliasName = '' then begin
      Tmp := ExtractFileName(ParamStr(0));
      FAliasName := Tmp;
      FAliasName := Copy(FAliasName, 1, Pos('.', FAliasName) - 1);
      FDatabase.AliasName := FAliasName;
    end;
    { if the alias doesn't exist, then create it }
    if not Session.IsAlias(FDatabase.AliasName) then begin
      AliasPath := ExtractFilePath(ParamStr(0)) + 'Data';
      if not DirectoryExists(AliasPath) then
        ForceDirectories(AliasPath);
      Session.AddStandardAlias(FDatabase.AliasName, AliasPath, 'PARADOX');
      { Make sure the alias is saved to the BDE config file. }
      Session.SaveConfigFile;
    end;
  end else
    if not Session.IsAlias(FDatabase.AliasName) then Exit;

  if FDatabase.DatabaseName = '' then
    FDatabase.DatabaseName := 'VpDatabase' + Name[Length(Name)];

  FDataBase.Connected := Value;
  if FDataBase.Connected then begin
    Qry := TQuery.Create(self);
    Qry.DatabaseName := FAliasName;
    try

      StringList := TStringList.Create;
      try
        Session.GetAliasParams(FAliasName, StringList);
        AliasPath := Copy(StringList[0], Pos('=', StringList[0]) + 1, Length(StringList[0]));
      finally
        StringList.Free;
      end;

      { Create / Open Resources Table}
      FResourceTable.DatabaseName := FDatabase.DatabaseName;
      if (AliasPath <> '')
      and (not FileExists(AliasPath + '\' + ResourceTableName + '.*'))
      then CreateTable(ResourceTableName);
      try
        FResourceTable.Open;
      except
        if AutoCreate then begin
          CreateTable(ResourceTableName);
          FResourceTable.Open;
        end;
      end;

      { Create / Open Events Table }
      FEventsTable.DatabaseName := FDatabase.DatabaseName;
      if (AliasPath <> '')
      and (not FileExists(AliasPath + '\' + EventsTableName + '.*'))
      then CreateTable(EventsTableName);
      SetFilterCriteria(FEventsTable,
                        True,
                        ResourceTable.FieldByName('ResourceID').AsInteger,
                        TimeRange.StartTime,
                        TimeRange.EndTime);
      try
        FEventsTable.Open;
      except
        if AutoCreate then begin
          CreateTable(EventsTableName);
          FEventsTable.Open;
        end;
      end;

      { Create / Open Contacts Table }
      FContactsTable.DatabaseName := FDatabase.DatabaseName;
      if (AliasPath <> '')
      and (not FileExists(AliasPath + '\' + ContactsTableName + '.*'))
      then CreateTable(ContactsTableName);
      SetFilterCriteria(FContactsTable, False,
                        ResourceTable.FieldByName('ResourceID').AsInteger,
                        0, 0);
      try
        FContactsTable.Open;
      except
        if AutoCreate then begin
          CreateTable(ContactsTableName);
          FContactsTable.Open;
        end;
      end;


      { Create / Open Tasks Table }
      FTasksTable.DatabaseName := FDatabase.DatabaseName;
      if (AliasPath <> '')
      and (not FileExists(AliasPath + '\' + TasksTableName + '.*'))
      then CreateTable(TasksTableName);
      SetFilterCriteria(FTasksTable, False,
                        ResourceTable.FieldByName('ResourceID').AsInteger,
                        0, 0);
      try
        FTasksTable.Open;
      except
        if AutoCreate then begin
          CreateTable(TasksTableName);
          FTasksTable.Open;
        end;
      end;

      { Create / Open RecordID Table }
      FRecordIDTable.DatabaseName := FDatabase.DatabaseName;
      if (AliasPath <> '')
      and (not FileExists(AliasPath + '\' + RecordIDTableName + '.*'))
      then CreateTable(RecordIDTableName);

    finally
      Qry.Free;
    end;

    Load;
  end
  else begin
    FTasksTable.Close;
    FContactsTable.Close;
    FResourceTable.Close;
    FEventsTable.Close;
  end;

  inherited SetConnected(Database.Connected);
end;
{=====}

procedure TVpBDEDataStore.SetDriverName(const Value: string);
begin
  FDriverName := Value;
end;
{=====}

procedure TVpBDEDataStore.SetLoginPrompt(const Value: boolean);
begin
  FLoginPrompt := Value;
end;
{=====}

procedure TVpBDEDataStore.SetParams(const Value: TStrings);
begin
  FParams.Assign(Value);
end;
{=====}

{ Called by the ancestor to properly filter the data for each table, }
{ based on the ResourceID, Date and DayBuffer values.                }
{ Each TVpCustomDBDataStore descendant should define their own       }
{ SetFilterCriteria procedure.                                       }
procedure TVpBDEDataStore.SetFilterCriteria(aTable : TDataset;
  aUseDateTime : Boolean; aResourceID : Integer; aStartDateTime : TDateTime;
  aEndDateTime : TDateTime);
var
  Qry: TQuery;
begin
  Qry := (aTable as TQuery);

  Qry.Close;

  Qry.ParamByName('ResID').AsInteger := aResourceID;                  

  if Qry = EventsTable then begin
    Qry.ParamByName('STime').AsDateTime := aStartDateTime;
    Qry.ParamByName('ETime').AsDateTime := aEndDateTime;
  end;

  Qry.Open;
end;
{=====}

procedure TVpBDEDataStore.CreateIndexDefs(const TableName: string;
  IndexDefs: TIndexDefs);
begin
  if TableName = ResourceTableName then begin
    with IndexDefs do begin
      Clear;
      { Paradox primary keys have no name }
      with AddIndexDef do begin
        Name := '';
        Fields := 'ResourceID';
        Options := [ixPrimary];
      end;
    end;
  end else if TableName = EventsTableName then begin
    with IndexDefs do begin
      Clear;
      { Paradox primary keys have no name }
      with AddIndexDef do begin
        Name := '';
        Fields := 'RecordID';
        Options := [ixUnique, ixPrimary];
      end;
    end;
  end else if TableName = ContactsTableName then begin
    with IndexDefs do begin
      Clear;
      { Paradox primary keys have no name }
      with AddIndexDef do begin
        Name := '';
        Fields := 'RecordID';
        Options := [ixPrimary];
      end;
    end;
  end else if TableName = TasksTableName then begin
    with IndexDefs do begin
      Clear;
      { Paradox primary keys have no name }
      with AddIndexDef do begin
        Name := '';
        Fields := 'RecordID';
        Options := [ixPrimary];
      end;
    end;
  end;

  inherited CreateIndexDefs(TableName, IndexDefs);
end;
{=====}

end.
