{*********************************************************}
{*                  VPMORMOTDS.PAS 1.00                  *}
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

unit VpmORMotDS;

interface

uses
  Classes, Dialogs, SysUtils, Db,
  VpData, VpBaseDS, VpDBDS,
  mORMotVCL,
  SynCommons,
  mORMot,mORMotSQLite3,
  SynSQLite3Static,mORMotHttpClient
  ;

type
  TSynSQLTableDataSetWithLocate = class(TSynSQLTableDataSet)
  public
    procedure Delete; override;
  end;


type
  TVpmORMotDataStore = class(TVpCustomDBDataStore)
  protected

    FResourceTable    : TSynSQLTableDataSetWithLocate;
    FEventsTable      : TSynSQLTableDataSetWithLocate;
    FContactsTable    : TSynSQLTableDataSetWithLocate;
    FTasksTable       : TSynSQLTableDataSetWithLocate;

    FDatabase         : TSQLRest;
    FModel            : TSQLModel;
    FHostIP           : string;
    FDirectory        : string;

    aSQLResourceTable : TSQLTable;
    aSQLEventTable    : TSQLTable;
    aSQLContactTable  : TSQLTable;
    aSQLTaskTable     : TSQLTable;

    procedure RefreshTable(aTable:TDataset);

    { property getters }
    function GetContactsTable: TDataset; override;
    function GetEventsTable: TDataset; override;
    function GetResourceTable: TDataset; override;
    function GetTasksTable: TDataset; override;
    function CheckServer: boolean;

    { property setters }
    procedure SetConnected(const Value: boolean); override;
    procedure SetHostIP(const Value: string);
    procedure SetDirectory(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetNextID(TableName: string): Integer; override;

    procedure PostResources; override;
    procedure PostEvents; override;
    procedure PostContacts; override;
    procedure PostTasks; override;

    procedure RefreshEvents; override;
    procedure RefreshContacts; override;
    procedure RefreshTasks; override;
    procedure RefreshResource; override;

    procedure PurgeResource(Res: TVpResource); override;
    procedure PurgeEvents(Res: TVpResource); override;
    procedure PurgeContacts(Res: TVpResource); override;
    procedure PurgeTasks(Res: TVpResource); override;

    property HostIP: string read FHostIP write SetHostIP;
    property Directory: string read FDirectory write SetDirectory;

    property CheckUpdate:boolean read CheckServer;
  end;

implementation

uses
  VpMisc,
  VpException,
  {$ifdef WITHRTTI}
  TypInfo,
  {$endif}
  Variants,
  SynSQLite3,
  RESTdata;

{$ifdef WITHRTTI}
procedure GetFieldValuesByRTTI(aVpTable:TObject;aRecord:TSQLRecord);
const
    TypeKinds: TTypeKinds =
                          [
                          tkInteger,tkEnumeration,tkFloat,
                          tkSet,tkSString,tkLString,tkAString,
                          tkWString,tkBool,tkInt64,tkQWord,tkUString
                          ];
var
    K,L: Integer;
    PropList: PPropList;
    PropInfo: PPropInfo;
    aFloat:Double;
    PropName:string;
begin
    K := GetPropList(aVpTable.ClassInfo, TypeKinds, nil);
    GetMem(PropList, K * SizeOf(PPropInfo));
    try
      GetPropList(aVpTable.ClassInfo, TypeKinds, PropList);
      for L := 0 to K - 1 do
      begin
        PropInfo := PropList^[L];
        PropName:=PropInfo^.Name;
        if PropName='ResourceID' then continue;
        if Assigned(PropInfo^.GetProc) then
        begin
           case PropInfo^.PropType^.Kind of
              tkString, tkLString, tkUString, tkWString{$ifdef FPC},tkAString{$endif}:
                 aRecord.SetFieldVariant(PropInfo^.Name, TypInfo.GetStrProp(aVpTable, PropName));
              tkInt64{$ifdef FPC}, tkQWord{$endif}:
                 aRecord.SetFieldVariant(PropInfo^.Name, TypInfo.GetInt64Prop(aVpTable, PropName));
              tkEnumeration, tkSet, tkInteger{$ifdef FPC},tkBool{$endif} :
                 aRecord.SetFieldVariant(PropInfo^.Name, TypInfo.GetOrdProp(aVpTable, PropName));
              tkFloat:begin
                if PropInfo^.PropType = TypeInfo(TDateTime) then
                begin
                  aFloat:=TypInfo.GetFloatProp(aVpTable, PropName);
                  aRecord.SetFieldVariant(PropInfo^.Name, TDateTime(aFloat));
                end else aRecord.SetFieldVariant(PropInfo^.Name, TypInfo.GetFloatProp(aVpTable, PropName));
              end;

           end;
        end;
      end;

    finally
      FreeMem(PropList);
    end;
end;
{$endif}

procedure TSynSQLTableDataSetWithLocate.Delete;
begin
  CheckActive;
  if IsEmpty then exit;
  if State in [dsInsert] then
   begin
     Cancel;
   end else begin
     DataEvent(deCheckBrowseMode,0);
     DoBeforeDelete;
     DoBeforeScroll;
     Table.DeleteRow(RecNo);
     SetState(dsBrowse);
     Resync([]);
     DoAfterDelete;
     DoAfterScroll;
   end;
end;

(*****************************************************************************)
{ TVpmORMotDataStore }

constructor TVpmORMotDataStore.Create(AOwner: TComponent);
begin
  inherited;
  FHostIP          := '';
  FModel           := DataModel;
end;
{=====}

function TVpmORMotDataStore.GetNextID(TableName: string): Integer;
begin
  Unused(TableName);
  Result := -1
end;
{=====}

destructor TVpmORMotDataStore.Destroy;
begin
  { free tables }
  FreeAndNil(FResourceTable);
  FreeAndNil(FEventsTable);
  FreeAndNil(FContactsTable);
  FreeAndNil(FTasksTable);
  { free database }
  FreeAndNil(FDatabase);
  FreeAndNil(FModel);
  inherited;
end;
{=====}

procedure TVpmORMotDataStore.PostResources;
var
  I: Integer;
  Res: TVpResource;
  aResourceTable: TSQLRecordClass;
  aRecord: TSQLRecord;
  aNewID:TID;
begin

  Loading := true;

  try

    if (Resources.Count > 0) then
    begin

      aResourceTable := aSQLResourceTable.QueryRecordType;

      if not ResourceTable.Active then
         ResourceTable.Open;
      ResourceTable.First;

      for I := pred(Resources.Count) downto 0 do
      begin
        Res := Resources.Items[I];

        if Res = nil then Continue;


        if Res.Deleted then
        begin
          PurgeEvents(Res);
          PurgeContacts(Res);
          PurgeTasks(Res);
          if (aResourceTable<>nil) then
          begin
            // delete record from database
            if FDatabase.Delete(aResourceTable,Res.ResourceID) then
            begin
              // delete record from dataset
              if ResourceTable.Locate('ResourceID', Res.ResourceID, [])
                 then ResourceTable.Delete;
            end;
          end;
          if Resource = Res then
            ResourceID := -1;
          Res.Free;
          Continue;
        end

        else if Res.Changed then
        begin

          aRecord := aResourceTable.Create(FDatabase,Res.ResourceID,true);
          try

            {$ifdef WITHRTTI}

            GetFieldValuesByRTTI(Res,aRecord);

            {$else}

            aRecord.SetFieldVariant('Description', Res.Description);
            aRecord.SetFieldVariant('Notes', Res.Notes);
            aRecord.SetFieldVariant('ResourceActive', Res.ResourceActive);
            aRecord.SetFieldVariant('UserField0', Res.UserField0);
            aRecord.SetFieldVariant('UserField1', Res.UserField1);
            aRecord.SetFieldVariant('UserField2', Res.UserField2);
            aRecord.SetFieldVariant('UserField3', Res.UserField3);
            aRecord.SetFieldVariant('UserField4', Res.UserField4);
            aRecord.SetFieldVariant('UserField5', Res.UserField5);
            aRecord.SetFieldVariant('UserField6', Res.UserField6);
            aRecord.SetFieldVariant('UserField7', Res.UserField7);
            aRecord.SetFieldVariant('UserField8', Res.UserField8);
            aRecord.SetFieldVariant('UserField9', Res.UserField9);

            {$endif}

            aNewID:=FDatabase.AddOrUpdate(aRecord);

            // do we have a new resource ?
            if Res.ResourceID<>aNewID then
            begin
              Res.ResourceID:=aNewID;
              aRecord.SetFieldVariant('ResourceID', aNewID);
              FDatabase.Update(aRecord,'ResourceID',true);
            end;

            FDatabase.UnLock(aRecord);

            if Res.ResourceID=0 then
            begin
              // we have a mORMot error !!
              raise EDBPostError.Create;
            end;

          finally
            aRecord.Free;
          end;

          RefreshTable(FResourceTable);
          if (Res.ResourceID = ResourceID) then
          begin
            PostEvents;
            PostContacts;
            PostTasks;
          end;

          Res.Changed := false;

        end;
      end;

      if not Loading then
         NotifyDependents;

      if Assigned(AfterPostEvents) then
         FAfterPostEvents(self);

    end;

  finally
    Loading := false;
  end;
end;
{=====}


procedure TVpmORMotDataStore.PostEvents;
var
  J: Integer;
  Event: TVpEvent;
  aEventTable: TSQLRecordClass;
  aRecord: TSQLRecord;
  aNewID:TID;
begin
  if (Resource <> nil) and Resource.EventsDirty then
  begin
    if ResourceTable.Locate('ResourceID', Resource.ResourceID, []) then
    begin

      aEventTable := aSQLEventTable.QueryRecordType;

      for J := pred(Resource.Schedule.EventCount) downto 0 do
      begin

        Event := Resource.Schedule.GetEvent(J);

        { if the delete flag is set then delete it from the database }
        { and free the event instance }
        if Event.Deleted then
        begin
          if (aEventTable<>nil) then
          begin
            // delete record from database
            if FDatabase.Delete(aEventTable,Event.RecordID) then
            begin
              // delete record from dataset
              if EventsTable.Locate('RecordID', Event.RecordID, [])
                 then EventsTable.Delete;
            end;
          end;
          Event.Free;
          Continue;
        end;

        if Event.Changed then
        begin

          aRecord:=aEventTable.Create(FDatabase, Event.RecordID, true);
          try

            {$ifdef WITHRTTI}

            GetFieldValuesByRTTI(Event,aRecord);

            {$else}

            aRecord.SetFieldVariant('StartTime', Event.StartTime);
            aRecord.SetFieldVariant('EndTime', Event.EndTime);
            aRecord.SetFieldVariant('Description', Event.Description);
            aRecord.SetFieldVariant('Location', Event.Location);
            aRecord.SetFieldVariant('Notes', Event.Notes);
            aRecord.SetFieldVariant('Category', Event.Category);
            aRecord.SetFieldVariant('DingPath', Event.DingPath);
            aRecord.SetFieldVariant('AllDayEvent', Event.AllDayEvent);
            aRecord.SetFieldVariant('AlarmSet', Event.AlarmSet);
            aRecord.SetFieldVariant('AlarmAdvance', Event.AlarmAdvance);
            aRecord.SetFieldVariant('AlarmAdvanceType', Ord(Event.AlarmAdvanceType));
            aRecord.SetFieldVariant('SnoozeTime', Event.SnoozeTime);
            aRecord.SetFieldVariant('RepeatCode', Ord(Event.RepeatCode));
            aRecord.SetFieldVariant('RepeatRangeEnd', Event.RepeatRangeEnd);
            aRecord.SetFieldVariant('CustomInterval', Event.CustomInterval);
            aRecord.SetFieldVariant('UserField0', Event.UserField0);
            aRecord.SetFieldVariant('UserField1', Event.UserField1);
            aRecord.SetFieldVariant('UserField2', Event.UserField2);
            aRecord.SetFieldVariant('UserField3', Event.UserField3);
            aRecord.SetFieldVariant('UserField4', Event.UserField4);
            aRecord.SetFieldVariant('UserField5', Event.UserField5);
            aRecord.SetFieldVariant('UserField6', Event.UserField6);
            aRecord.SetFieldVariant('UserField7', Event.UserField7);
            aRecord.SetFieldVariant('UserField8', Event.UserField8);
            aRecord.SetFieldVariant('UserField9', Event.UserField9);

            {$endif}

            aRecord.SetFieldVariant('ResourceID', Resource.ResourceID);

            aNewID:=FDatabase.AddOrUpdate(aRecord);

            // do we have a new event ?
            if Event.RecordID<>aNewID then
            begin
              Event.RecordID:=aNewID;
              aRecord.SetFieldVariant('RecordID', aNewID);
              FDatabase.Update(aRecord,'RecordID',true);
            end;

            FDatabase.UnLock(aRecord);

            if Event.RecordID=0 then
            begin
              // we have a mORMot error !!
              raise EDBPostError.Create;
            end;

          finally
            aRecord.Free;
          end;

          RefreshTable(FEventsTable);

          Event.Changed := false;
        end;
      end;

    end;

    Resource.EventsDirty := false;
    Resource.Schedule.Sort;

    if not Loading then
      NotifyDependents;

    if Assigned(AfterPostEvents) then
      FAfterPostEvents(self);
  end;
end;
{=====}

procedure TVpmORMotDataStore.PostContacts;
var
  J: Integer;
  Contact: TVpContact;
  aContactTable: TSQLRecordClass;
  aRecord: TSQLRecord;
  aNewID:TID;
begin
  if (Resource <> nil) and Resource.ContactsDirty then
  begin
    if ResourceTable.Locate('ResourceID', Resource.ResourceID, []) then
    begin

      aContactTable := aSQLContactTable.QueryRecordType;

      for J := pred(Resource.Contacts.Count) downto 0 do
      begin
        Contact := Resource.Contacts.GetContact(J);

        { if the delete flag is set then delete it from the database }
        { and free the Contact instance }
        if Contact.Deleted then
        begin
          if (aContactTable<>nil) then
          begin
            // delete record from database
            if FDatabase.Delete(aContactTable,Contact.RecordID) then
            begin
              // delete record from dataset
              if ContactsTable.Locate('RecordID', Contact.RecordID, [])
                 then ContactsTable.Delete;
            end;
          end;
          Contact.Free;
          Continue;
        end;

        if Contact.Changed then
        begin

          aRecord:=aContactTable.Create(FDatabase, Contact.RecordID, true);
          try

            {$ifdef WITHRTTI}

            GetFieldValuesByRTTI(Contact,aRecord);

            {$else}

            aRecord.SetFieldVariant('Job_Position',Contact.Job_Position);
            aRecord.SetFieldVariant('FirstName',Contact.FirstName);
            aRecord.SetFieldVariant('LastName',Contact.LastName);
            aRecord.SetFieldVariant('BirthDate',Contact.BirthDate);
            aRecord.SetFieldVariant('Anniversary',Contact.Anniversary);
            aRecord.SetFieldVariant('Title',Contact.Title);
            aRecord.SetFieldVariant('Company',Contact.Company);
            aRecord.SetFieldVariant('EMail',Contact.EMail);
            aRecord.SetFieldVariant('Phone1',Contact.Phone1);
            aRecord.SetFieldVariant('Phone2',Contact.Phone2);
            aRecord.SetFieldVariant('Phone3',Contact.Phone3);
            aRecord.SetFieldVariant('Phone4',Contact.Phone4);
            aRecord.SetFieldVariant('Phone5',Contact.Phone5);
            aRecord.SetFieldVariant('PhoneType1',Contact.PhoneType1);
            aRecord.SetFieldVariant('PhoneType2',Contact.PhoneType2);
            aRecord.SetFieldVariant('PhoneType3',Contact.PhoneType3);
            aRecord.SetFieldVariant('PhoneType4',Contact.PhoneType4);
            aRecord.SetFieldVariant('PhoneType5',Contact.PhoneType5);
            aRecord.SetFieldVariant('Address',Contact.Address);
            aRecord.SetFieldVariant('City',Contact.City);
            aRecord.SetFieldVariant('State',Contact.State);
            aRecord.SetFieldVariant('Zip',Contact.Zip);
            aRecord.SetFieldVariant('Country',Contact.Country);
            aRecord.SetFieldVariant('Notes',Contact.Notes);
            aRecord.SetFieldVariant('Category',Contact.Category);
            aRecord.SetFieldVariant('Custom1',Contact.Custom1);
            aRecord.SetFieldVariant('Custom2',Contact.Custom2);
            aRecord.SetFieldVariant('Custom3',Contact.Custom3);
            aRecord.SetFieldVariant('Custom4',Contact.Custom4);
            aRecord.SetFieldVariant('UserField0', Contact.UserField0);
            aRecord.SetFieldVariant('UserField1', Contact.UserField1);
            aRecord.SetFieldVariant('UserField2', Contact.UserField2);
            aRecord.SetFieldVariant('UserField3', Contact.UserField3);
            aRecord.SetFieldVariant('UserField4', Contact.UserField4);
            aRecord.SetFieldVariant('UserField5', Contact.UserField5);
            aRecord.SetFieldVariant('UserField6', Contact.UserField6);
            aRecord.SetFieldVariant('UserField7', Contact.UserField7);
            aRecord.SetFieldVariant('UserField8', Contact.UserField8);
            aRecord.SetFieldVariant('UserField9', Contact.UserField9);

            {$endif}

            aRecord.SetFieldVariant('ResourceID', Resource.ResourceID);

            aNewID:=FDatabase.AddOrUpdate(aRecord);

            // do we have a new contact ?
            if Contact.RecordID<>aNewID then
            begin
              Contact.RecordID:=aNewID;
              aRecord.SetFieldVariant('RecordID', aNewID);
              FDatabase.Update(aRecord,'RecordID',true);
            end;

            FDatabase.UnLock(aRecord);

            if Contact.RecordID=0 then
            begin
              // we have a mORMot error !!
              raise EDBPostError.Create;
            end;

          finally
            aRecord.Free;
          end;

          RefreshTable(FContactsTable);

          Contact.Changed := false;
        end;
      end;


    end;

    Resource.ContactsDirty := false;

    if not Loading then
      NotifyDependents;

    if Assigned(AfterPostContacts) then
      FAfterPostContacts(self);
  end;
end;
{=====}

procedure TVpmORMotDataStore.PostTasks;
var
  J: Integer;
  Task: TVpTask;
  aTaskTable: TSQLRecordClass;
  aRecord: TSQLRecord;
  aNewID:TID;
begin
  if (Resource <> nil) and Resource.TasksDirty then
  begin
    if ResourceTable.Locate('ResourceID', Resource.ResourceID, []) then
    begin

      aTaskTable := aSQLTaskTable.QueryRecordType;

      for J := pred(Resource.Tasks.Count) downto 0 do
      begin
        Task := Resource.Tasks.GetTask(J);

        { if the delete flag is set then delete it from the database }
        { and free the Task instance }
        if Task.Deleted then
        begin
          if (aTaskTable<>nil) then
          begin
            // delete record from database
            if FDatabase.Delete(aTaskTable,Task.RecordID) then
            begin
              // delete record from dataset
              if TasksTable.Locate('RecordID', Task.RecordID, [])
                 then TasksTable.Delete;
            end;
          end;
          Task.Free;
          Continue;
        end;

        if Task.Changed then
        begin

          aRecord:=aTaskTable.Create(FDatabase, Task.RecordID, true);
          try

            {$ifdef WITHRTTI}

            GetFieldValuesByRTTI(Task,aRecord);

            {$else}

            aRecord.SetFieldVariant('Description', Task.Description);
            aRecord.SetFieldVariant('Details', Task.Details);
            aRecord.SetFieldVariant('Complete', Task.Complete);
            aRecord.SetFieldVariant('DueDate', Task.DueDate);
            aRecord.SetFieldVariant('CreatedOn', Task.CreatedOn);
            aRecord.SetFieldVariant('CompletedOn', Task.CompletedOn);
            aRecord.SetFieldVariant('Priority', Task.Priority);
            aRecord.SetFieldVariant('Category', Task.Category);

            aRecord.SetFieldVariant('UserField0', Task.UserField0);
            aRecord.SetFieldVariant('UserField1', Task.UserField1);
            aRecord.SetFieldVariant('UserField2', Task.UserField2);
            aRecord.SetFieldVariant('UserField3', Task.UserField3);
            aRecord.SetFieldVariant('UserField4', Task.UserField4);
            aRecord.SetFieldVariant('UserField5', Task.UserField5);
            aRecord.SetFieldVariant('UserField6', Task.UserField6);
            aRecord.SetFieldVariant('UserField7', Task.UserField7);
            aRecord.SetFieldVariant('UserField8', Task.UserField8);
            aRecord.SetFieldVariant('UserField9', Task.UserField9);

            {$endif}

            aRecord.SetFieldVariant('ResourceID', Resource.ResourceID);

            aNewID:=FDatabase.AddOrUpdate(aRecord);

            // do we have a new task ?
            if Task.RecordID<>aNewID then
            begin
              Task.RecordID:=aNewID;
              aRecord.SetFieldVariant('RecordID', aNewID);
              FDatabase.Update(aRecord,'RecordID',true);
            end;

            FDatabase.UnLock(aRecord);

            if Task.RecordID=0 then
            begin
              // we have a mORMot error !!
              raise EDBPostError.Create;
            end;

          finally
            aRecord.Free;
          end;

          RefreshTable(FTasksTable);

          Task.Changed := false;
        end;
      end;


    end;

    Resource.TasksDirty := false;

    if not Loading then
      NotifyDependents;

    if Assigned(AfterPostTasks) then
      FAfterPostTasks(self);
  end;
end;
{=====}

procedure TVpmORMotDataStore.PurgeResource(Res: TVpResource);
begin
  RefreshTable(FResourceTable);
  inherited;
end;
{=====}

procedure TVpmORMotDataStore.PurgeEvents(Res: TVpResource);
begin
  RefreshTable(FEventsTable);
  inherited;
end;
{=====}

procedure TVpmORMotDataStore.PurgeContacts(Res: TVpResource);
begin
  RefreshTable(FContactsTable);
  inherited;
end;
{=====}

procedure TVpmORMotDataStore.PurgeTasks(Res: TVpResource);
begin
  RefreshTable(FTasksTable);
  inherited;
end;
{=====}

procedure TVpmORMotDataStore.SetConnected(const Value: boolean);
var
  aTable     : TSQLRecordClass;
  aVpTable   : TSynSQLTableDataSetWithLocate;
  aSQLTable  : TSQLTable;
  aFieldType : TSQLFieldType;
  i,j        : integer;
  aDBFile    : string;
begin

  { Don't do anything with live data until run time. }
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
     Exit;

  inherited SetConnected(Value);

  if (Value) then
  begin

    aDBFile:=ChangeFileExt(paramstr(0),'.db3');

    if (length(HostIP)=0) AND (Length(FDirectory)>0) then
    begin
       aDBFile := EnsureDirectoryExists(ExpandFileName(FDirectory),true)+ExtractFileName(aDBFile);
    end;

    if Assigned(FDatabase) then FDatabase.Free;
    if length(HostIP)>0
       then FDatabase:=TSQLHttpClient.Create(FHostIP,HTTP_PORT,FModel)
       else FDatabase:=TSQLRestServerDB.Create(FModel,aDBFile,True);

    if FDatabase.InheritsFrom(TSQLRestClient) then
    begin
      if NOT TSQLHttpClient(FDatabase).SetUser('User','synopse') then
      begin
        inherited SetConnected(False);
        FConnected:=False;
        exit;
      end;
    end;

    if FDatabase.InheritsFrom(TSQLRestServer) then
    begin
      TSQLRestServerDB(FDataBase).CreateMissingTables;
    end;

    for j:=0 to 3 do
    begin

      aTable:=nil;
      case j of
       0:aTable:=FModel.Table['VpResource'];
       1:aTable:=FModel.Table['VpEvent'];
       2:aTable:=FModel.Table['VpContact'];
       3:aTable:=FModel.Table['VpTask'];
      end;

      if aTable=nil then continue;

      // fill readonly table
      if j=0
         then aSQLTable:=FDatabase.MultiFieldValues(aTable,'*','order by ID')
         else aSQLTable:=FDatabase.MultiFieldValues(aTable,'*','%=?',['ResourceID'],[ResourceID]);

      // tricky ... force set field size
      for i := 0 to aSQLTable.FieldCount-1 do
      begin
        aFieldType:=aSQLTable.FieldType(i,nil);
        if aFieldType in [sftAnsiText,sftUTF8Text,sftUTF8Custom] then aSQLTable.SetFieldType(i,aFieldType,nil,100);
        if aSQLTable.FieldNames[i]='Notes' then aSQLTable.SetFieldType(i,aFieldType,nil,500);
      end;

      case j of
       0:aSQLResourceTable:=aSQLTable;
       1:aSQLEventTable:=aSQLTable;
       2:aSQLContactTable:=aSQLTable;
       3:aSQLTaskTable:=aSQLTable;
      end;

      // create simple readonly dataset
      aVpTable:=TSynSQLTableDataSetWithLocate.CreateOwnedTable(nil,aSQLTable);

      case j of
       0:FResourceTable:=aVpTable;
       1:FEventsTable:=aVpTable;
       2:FContactsTable:=aVpTable;
       3:FTasksTable:=aVpTable;
      end;

    end;

    Load;

  end else if Assigned(FDatabase) then FDatabase.Free;

end;

procedure TVpmORMotDataStore.SetHostIP(const Value: string);
begin
  if FHostIP<>Value then
  begin
    FHostIP:=Value;
  end;
end;

procedure TVpmORMotDataStore.SetDirectory(const Value: string);
begin
  if Value = FDirectory then
    exit;
  if Connected then
    raise Exception.Create('Set directory before connecting.');
  FDirectory := Value;
end;

function TVpmORMotDataStore.CheckServer: boolean;
var
  ref: boolean;
begin

  result:=false;

  if FDatabase.InheritsFrom(TSQLRestClient) then
  begin

    if Assigned(aSQLResourceTable) then
    begin
      if (TSQLRestClientURI(FDatabase).UpdateFromServer([aSQLResourceTable],ref) AND ref) then
      begin
        RefreshResource;
        result:=true;
      end;
    end;

    if Assigned(aSQLEventTable) then
    begin
      if (TSQLRestClientURI(FDatabase).UpdateFromServer([aSQLEventTable],ref) AND ref) then
      begin
        RefreshEvents;
        result:=true;
      end;
    end;

    if Assigned(aSQLContactTable) then
    begin
      if (TSQLRestClientURI(FDatabase).UpdateFromServer([aSQLContactTable],ref) AND ref) then
      begin
        RefreshContacts;
        result:=true;
      end;
    end;

    if Assigned(aSQLTaskTable) then
    begin
      if (TSQLRestClientURI(FDatabase).UpdateFromServer([aSQLTaskTable],ref) AND ref) then
      begin
        RefreshTasks;
        result:=true;
      end;
    end;

  end;
end;

procedure TVpmORMotDataStore.RefreshTable(aTable:TDataset);
var
  aSQLTable:TSQLTable;
  aSQLRecordClass:TSQLRecordClass;
begin

  aSQLTable:=nil;

  if aTable=ResourceTable then aSQLTable:=aSQLResourceTable;
  if aTable=EventsTable then aSQLTable:=aSQLEventTable;
  if aTable=ContactsTable then aSQLTable:=aSQLContactTable;
  if aTable=TasksTable then aSQLTable:=aSQLTaskTable;

  aSQLRecordClass:=aSQLTable.QueryRecordType;

  aSQLTable.Free;
  if aTable=ResourceTable
     then aSQLTable:=FDatabase.MultiFieldValues(aSQLRecordClass,'*','order by ID')
     else aSQLTable:=FDatabase.MultiFieldValues(aSQLRecordClass,'*','%=?',['ResourceID'],[ResourceID]);

  TSynSQLTableDatasetWithLocate(aTable).Table:=aSQLTable;

  aTable.Refresh;

end;
{=====}


procedure TVpmORMotDataStore.RefreshResource;
begin
  RefreshTable(FResourceTable);
  inherited;
end;
{=====}

procedure TVpmORMotDataStore.RefreshEvents;
begin
  RefreshTable(FEventsTable);
  inherited;
end;
{=====}

procedure TVpmORMotDataStore.RefreshContacts;
begin
  RefreshTable(FContactsTable);
  inherited;
end;
{=====}

procedure TVpmORMotDataStore.RefreshTasks;
begin
  RefreshTable(FTasksTable);
  inherited;
end;
{=====}


function TVpmORMotDataStore.GetResourceTable : TDataset;
begin
  Result := FResourceTable AS TDataset;
end;

function TVpmORMotDataStore.GetEventsTable : TDataset;
begin
  Result := FEventsTable AS TDataset;
end;

function TVpmORMotDataStore.GetContactsTable : TDataset;
begin
  Result := FContactsTable AS TDataset;
end;

function TVpmORMotDataStore.GetTasksTable : TDataset;
begin
  Result := FTasksTable AS TDataset;
end;


end.
