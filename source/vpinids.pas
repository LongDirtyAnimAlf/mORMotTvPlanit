{ Visual PlanIt datastore using an ini file }

{$I vp.inc}

unit VpIniDs;

interface

uses
  SysUtils, Classes,
  VpData, VpBaseDS;

type
  TVpIniDatastore = class(TVpCustomDatastore)
  private
    FFilename: String;
    FFormatSettings: TFormatSettings;
    procedure SetFilename(const AValue: String);

  protected
    function ContactToStr(AContact: TVpContact): String;
    function EventToStr(AEvent: TVpEvent): String;
    function ResourceToStr(AResource: TVpResource): String;
    function TaskToStr(ATask: TVpTask): String;

    procedure StrToContact(AString: String; AContact: TVpContact);
    procedure StrToEvent(AString: String; AEvent: TVpEvent);
    procedure StrToEventTimes(AString: String; out AStartTime, AEndTime: TDateTime);
    procedure StrToResource(AString: String; AResource: TVpResource);
    procedure StrToTask(AString: String; ATask: TVpTask);

    procedure SetConnected(const AValue: Boolean); override;
    procedure Split(const AString: String; AList: TStrings);
    function UniqueID(AValue: Integer): Boolean;

    procedure Loaded; override;
    procedure ReadFromIni;
    procedure WriteToIni;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetNextID(TableName: string): Integer; override;
    procedure LoadEvents; override;
    procedure LoadContacts; override;
    procedure LoadTasks; override;
    procedure PostContacts; override;
    procedure PostEvents; override;
    procedure PostResources; override;
    procedure PostTasks; override;
    procedure SetResourceByName(Value: String); override;

  published
    property AutoConnect default false;
    property FileName: String read FFileName write SetfileName;

  end;

implementation

uses
  typinfo, StrUtils, Strings, IniFiles,
  VpConst, VpMisc, VpSR;

procedure IniError(const AMsg: String);
begin
  raise Exception.Create(AMsg);
end;

{ TVpIniDatastore }
constructor TVpIniDatastore.Create(AOwner: TComponent);
begin
  inherited;
  FFormatSettings := DefaultFormatSettings;
  FFormatSettings.DecimalSeparator := '.';
  FFormatSettings.ThousandSeparator := #0;
  FFormatSettings.ShortDateFormat := 'yyyy/mm/dd';
  FFormatSettings.LongTimeFormat := 'hh:nn:ss';
  FFormatSettings.DateSeparator := '/';
  FFormatSettings.TimeSeparator := ':';
  FDayBuffer := 1000*365;  // 1000 years, i.e. deactivate daybuffer mechanism
end;

destructor TVpIniDatastore.Destroy;
begin
  SetConnected(false);
  inherited;
end;

function TVpIniDatastore.ContactToStr(AContact: TVpContact): String;
begin
  Result := '{' +    // RecordID is stored in ini value name.
    AContact.FirstName + '}|{' +
    AContact.LastName + '}|{' +
    IfThen(AContact.BirthDate = 0.0, '',
      FormatDateTime('ddddd', AContact.BirthDate, FFormatSettings)) + '}|{' +   // Short date format
    IfThen(AContact.Anniversary = 0.0, '',
      FormatDateTime('ddddd', AContact.Anniversary, FFormatSettings)) + '}|{' +
    AContact.Title + '}|{' +
    AContact.Company + '}|{' +
    AContact.Job_Position + '}|{' +
    AContact.EMail + '}|{' +
    AContact.Address + '}|{' +
    AContact.City + '}|{' +
    AContact.State + '}|{' +
    AContact.Zip + '}|{' +
    AContact.Country + '}|{' +
    EncodeLineEndings(AContact.Notes) + '}|{' +
    AContact.Phone1 + '}|{' +
    AContact.Phone2 + '}|{' +
    AContact.Phone3 + '}|{' +
    AContact.Phone4 + '}|{' +
    AContact.Phone5 + '}|{' +
    IntToStr(AContact.PhoneType1) + '}|{' +
    IntToStr(AContact.PhoneType2) + '}|{' +
    IntToStr(AContact.PhoneType3) + '}|{' +
    IntToStr(AContact.PhoneType4) + '}|{' +
    IntToStr(AContact.PhoneType5) + '}|{' +
    IntToStr(AContact.Category) + '}|{' +
    AContact.Custom1 + '}|{' +
    AContact.Custom2 + '}|{' +
    AContact.Custom3 + '}|{' +
    AContact.Custom4 + '}|{' +
    AContact.UserField0 + '}|{' +
    AContact.UserField1 + '}|{' +
    AContact.UserField2 + '}|{' +
    AContact.UserField3 + '}|{' +
    AContact.UserField4 + '}|{' +
    AContact.UserField5 + '}|{' +
    AContact.UserField6 + '}|{' +
    AContact.UserField7 + '}|{' +
    AContact.UserField8 + '}|{' +
    AContact.UserField9 + '}';
end;

function TVpIniDatastore.EventToStr(AEvent: TVpEvent): String;
begin
  Result := '{' +     // RecordID is stored as ini value name
    IfThen(AEvent.StartTime = 0.0, '',
      FormatDateTime('c', AEvent.StartTime, FFormatSettings)) + '}|{' +          // Short date + long time
    IfThen(AEvent.EndTime = 0.0, '',
      FormatDateTime('c', AEvent.EndTime, FFormatSettings)) +'}|{' +
    AEvent.Description + '}|{' +
    AEvent.Location + '}|{' +
    EncodeLineEndings(AEvent.Notes) + '}|{' +
    IntToStr(AEvent.Category) + '}|{' +
    AEvent.DingPath + '}|{' +
    BoolToStr(AEvent.AllDayEvent, strTRUE, strFALSE) + '}|{' +
    BoolToStr(AEvent.AlarmSet, strTRUE, strFALSE) + '}|{' +
    IntToStr(AEvent.AlarmAdvance) + '}|{' +
    GetEnumName(TypeInfo(TVpAlarmAdvType), ord(AEvent.AlarmAdvanceType)) + '}|{' +
    FormatDateTime('tt', AEvent.SnoozeTime, FFormatSettings) + '}|{' +         // long time format
    GetEnumName(TypeInfo(TVpRepeatType), ord(AEvent.RepeatCode)) + '}|{' +
    IfThen(AEvent.RepeatRangeEnd = 0.0, '',
      FormatDateTime('ddddd', AEvent.RepeatRangeEnd, FFormatSettings)) + '}|{' +  // Short date format
    IntToStr(AEvent.CustomInterval) + '}|{' +
    AEvent.UserField0 + '}|{' +
    AEvent.UserField1 + '}|{' +
    AEvent.UserField2 + '}|{' +
    AEvent.UserField3 + '}|{' +
    AEvent.UserField4 + '}|{' +
    AEvent.UserField5 + '}|{' +
    AEvent.UserField6 + '}|{' +
    AEvent.UserField7 + '}|{' +
    AEvent.UserField8 + '}|{' +
    AEvent.UserField9 + '}';
end;

function TVpIniDatastore.GetNextID(TableName: string): Integer;
begin
  Unused(TableName);
  repeat
    Result := Random(High(Integer));
  until UniqueID(Result) and (Result <> -1);
end;

function TVpIniDatastore.UniqueID(AValue: Integer): Boolean;
var
  i, j: Integer;
  res: TVpResource;
begin
  Result := false;
  for i:=0 to Resources.Count-1 do begin
    res := Resources.Items[i];
    if res.ResourceID = AValue then
      exit;
    for j:=0 to res.Contacts.Count-1 do
      if res.Contacts.GetContact(j).RecordID = AValue then
        exit;
    for j:=0 to res.Tasks.Count-1 do
      if res.Tasks.GetTask(j).RecordID = AValue then
        exit;
    for j:=0 to res.Schedule.EventCount-1 do
      if res.Schedule.GetEvent(j).RecordID = AValue then
        exit;
  end;
  Result := true;
end;

function TVpIniDatastore.ResourceToStr(AResource: TVpResource): String;
begin
  result := '{' +
    AResource.Description + '}|{' +
    EncodeLineEndings(AResource.Notes) + '}|{' +
    BoolToStr(AResource.ResourceActive, strTRUE, strFALSE) + '}|{' +
    AResource.UserField0 + '}|{' +
    AResource.UserField1 + '}|{' +
    AResource.UserField2 + '}|{' +
    AResource.UserField3 + '}|{' +
    AResource.UserField4 + '}|{' +
    AResource.UserField5 + '}|{' +
    AResource.UserField6 + '}|{' +
    AResource.UserField7 + '}|{' +
    AResource.UserField8 + '}|{' +
    AResource.UserField9 + '}';
end;

procedure TVpIniDatastore.SetConnected(const AValue: Boolean);
begin
  if AValue = Connected then
    exit;

  if AValue then
    ReadFromIni
  else
    WriteToIni;

  inherited SetConnected(AValue);
end;

procedure TVpIniDatastore.SetResourceByName(Value: string);
var
  I: integer;
  res : TVpResource;
begin
  for I := 0 to pred(Resources.Count) do begin
    res := Resources.Items[I];
    if Res = nil then
      Continue;

    if res.Description = Value then begin
      if ResourceID <> Res.ResourceID then begin
        ResourceID := Res.ResourceID;
        RefreshResource;
      end;
      Exit;
    end;
  end;
end;

procedure TVpIniDatastore.Split(const AString: String; AList: TStrings);
var
  p: PChar;
  pStart, pEnd: PChar;

  procedure AddString;
  var
    s: String;
  begin
    SetLength(s, {%H-}PtrInt(pEnd) - {%H-}PtrInt(pStart));
    StrLCopy(PChar(s), pStart, {%H-}PtrInt(pEnd) - {%H-}PtrInt(pStart));
    AList.Add(s);
  end;

begin
  AList.Clear;
  if AString = '' then
    exit;
  p := @AString[1];
  if p^ <> '{' then
    IniError(RSIniFileStructure);
  inc(p);
  pStart := p;
  while true do begin
    case p^ of
      #0:  break;
      '}': begin
             pEnd := p;
             inc(p);
             if p^ = #0 then begin
               AddString;
               exit;
             end;
             if p^ <> '|' then
               IniError(RSIniFileStructure);
             inc(p);
             if p^ <> '{' then
               IniError(RSIniFileStructure);
             AddString;
             inc(p);
             pstart := p;
           end;
      else inc(p);
    end;
  end;
end;

function TVpIniDatastore.TaskToStr(ATask: TVpTask): String;
begin
  Result := '{' +    // RecordID is stored as ini value name.
    BoolToStr(ATask.Complete, strTRUE, strFALSE) + '}|{' +
    ATask.Description + '}|{' +
    EncodeLineendings(ATask.Details) + '}|{' +
    IfThen(ATask.CreatedOn = 0.0, '',
      FormatDateTime('ddddd', ATask.CreatedOn, FFormatsettings)) + '}|{' +
    IfThen(ATask.CompletedOn = 0.0, '',
      FormatDateTime('ddddd', ATask.CompletedOn, FFormatSettings)) + '}|{' +
    IntToStr(ATask.Priority) + '}|{' +
    IntToStr(ATask.Category) + '}|{' +
    IfThen(ATask.DueDate = 0.0, '',
      FormatDateTime('ddddd', ATask.DueDate, FFormatSettings)) + '}|{' +
    ATask.UserField0 + '}|{' +
    ATask.UserField1 + '}|{' +
    ATask.UserField2 + '}|{' +
    ATask.UserField3 + '}|{' +
    ATask.UserField4 + '}|{' +
    ATask.UserField5 + '}|{' +
    ATask.UserField6 + '}|{' +
    ATask.UserField7 + '}|{' +
    ATask.UserField8 + '}|{' +
    ATask.UserField9 + '}'
end;

procedure TVpIniDatastore.SetFileName(const AValue: String);
begin
  FFileName := AValue;
  if AutoConnect then ReadFromIni;
end;

procedure TVpIniDatastore.LoadContacts;
begin
  // Nothing to do here...
end;

procedure TVpIniDatastore.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Connected := AutoConnect;
end;

procedure TVpIniDatastore.LoadEvents;
begin
  // Nothing to do here...
end;

procedure TVpIniDatastore.LoadTasks;
begin
  // Nothing to do here...
end;

procedure TVpIniDatastore.PostContacts;
var
  i: Integer;
  contact: TVpContact;
begin
  if Resource = nil then
    exit;
  for i := Resource.Contacts.Count-1 downto 0 do begin
    contact := Resource.Contacts.GetContact(i);
    if contact.Deleted then
      contact.Free;
  end;
  RefreshContacts;
end;

procedure TVpIniDatastore.PostEvents;
var
  i: Integer;
  event: TVpEvent;
begin
  if Resource = nil then
    exit;
  for i := Resource.Schedule.EventCount-1 downto 0 do begin
    event := Resource.Schedule.GetEvent(i);
    if event.Deleted then
      event.Free;
  end;
  RefreshEvents;
end;

procedure TVpIniDatastore.PostResources;
begin
  // Nothing to do...
end;

procedure TVpIniDatastore.PostTasks;
var
  i: Integer;
  task: TVpTask;
begin
  if Resource = nil then
    exit;
  for i := Resource.Tasks.Count-1 downto 0 do begin
    task := Resource.Tasks.GetTask(i);
    if task.Deleted then
      task.Free;
  end;
  RefreshTasks;
end;

procedure TVpIniDatastore.StrToContact(AString: String; AContact: TVpContact);
var
  L: TStrings;
begin
  L := TStringList.Create;
  try
    Split(AString, L);
    if L.Count <> 39 then
      IniError(RSIniFileStructure);
    AContact.FirstName := L[0];
    AContact.LastName := L[1];
    if L[2] = '' then
      AContact.BirthDate := 0.0 else
      AContact.BirthDate := StrToDate(L[2], FFormatSettings);
    if L[3] = '' then
      AContact.Anniversary := 0.0 else
      AContact.Anniversary := StrToDate(L[3], FFormatSettings);
    AContact.Title := L[4];
    AContact.Company := L[5];
    AContact.Job_Position := L[6];
    AContact.EMail := L[7];
    AContact.Address := L[8];
    AContact.City := L[9];
    AContact.State := L[10];
    AContact.Zip := L[11];
    AContact.Country := L[12];
    AContact.Notes := DecodeLineEndings(L[13]);
    AContact.Phone1 := L[14];
    AContact.Phone2 := L[15];
    AContact.Phone3 := L[16];
    AContact.Phone4 := L[17];
    AContact.Phone5 := L[18];
    AContact.PhoneType1 := StrToInt(L[19]);
    AContact.PhoneType2 := StrToInt(L[20]);
    AContact.PhoneType3 := StrToInt(L[21]);
    AContact.PhoneType4 := StrToInt(L[22]);
    AContact.PhoneType5 := StrToInt(L[23]);
    AContact.Category := StrToInt(L[24]);
    AContact.Custom1 := L[25];
    AContact.Custom2 := L[26];
    AContact.Custom3 := L[27];
    AContact.Custom4 := L[28];
    AContact.UserField0 := L[29];
    AContact.UserField1 := L[30];
    AContact.UserField2 := L[31];
    AContact.UserField3 := L[32];
    AContact.UserField4 := L[33];
    AContact.UserField5 := L[34];
    AContact.UserField6 := L[35];
    AContact.UserField7 := L[36];
    AContact.UserField8 := L[37];
    AContact.UserField9 := L[38];
  finally
    L.Free;
  end;
end;

procedure TVpIniDatastore.StrToEventTimes(AString: String;
  out AStartTime, AEndTime: TDateTime);
var
  L: TStrings;
begin
  L := TStringList.Create;
  try
    Split(AString, L);
    if L.Count < 2 then
      IniError(RSIniFileStructure);
    if L[0] = '' then
      AStartTime := 0 else
      AStartTime := StrToDateTime(L[0], FFormatSettings);
    if L[1] = '' then
      AEndTime := 0 else
      AEndtime := StrToDateTime(L[1], FFormatSettings);
  finally
    L.Free;
  end;
end;

procedure TVpIniDatastore.StrToEvent(AString: String; AEvent: TVpEvent);
var
  L: TStrings;
begin
  L := TStringList.Create;
  try
    Split(AString, L);
    if L.Count <> 25 then
      IniError(RSIniFileStructure);
    if L[0] = '' then
      AEvent.StartTime := 0 else
      AEvent.StartTime := StrToDateTime(L[0], FFormatSettings);
    if L[1] = '' then
      AEvent.EndTime := 0 else
      AEvent.EndTime := StrToDateTime(L[1], FFormatSettings);
    AEvent.Description := L[2];
    AEvent.Location := L[3];
    AEvent.Notes := DecodeLineEndings(L[4]);
    AEvent.Category := StrToInt(L[5]);
    AEvent.DingPath := L[6];
    AEvent.AllDayEvent := StrToBool(L[7]);
    AEvent.AlarmSet := StrToBool(L[8]);
    AEvent.AlarmAdvance := StrToInt(L[9]);
    AEvent.AlarmAdvanceType := TVpAlarmAdvType(GetEnumValue(TypeInfo(TVpAlarmAdvType), L[10]));
    AEvent.SnoozeTime := StrToTime(L[11]);
    AEvent.RepeatCode := TVpRepeatType(GetEnumValue(TypeInfo(TVpRepeatType), L[12]));
    if L[13] = '' then
      AEvent.RepeatRangeEnd := 0 else
      AEvent.RepeatRangeEnd := StrToDate(L[13], FFormatSettings);
    AEvent.CustomInterval := StrToInt(L[14]);
    AEvent.UserField0 := L[15];
    AEvent.UserField1 := L[16];
    AEvent.UserField2 := L[17];
    AEvent.UserField3 := L[18];
    AEvent.UserField4 := L[19];
    AEvent.UserField5 := L[20];
    AEvent.UserField6 := L[21];
    AEvent.UserField7 := L[22];
    AEvent.UserField8 := L[23];
    AEvent.UserField9 := L[24];
  finally
    L.Free;
  end;
end;

procedure TVpIniDatastore.StrToResource(AString: String; AResource: TVpResource);
var
  L: TStrings;
begin
  L := TStringList.Create;
  try
    Split(AString, L);
    if L.Count <> 13 then
      IniError(RSIniFileStructure);
    AResource.Description := L[0];
    AResource.Notes := DecodeLineEndings(L[1]);
    AResource.ResourceActive := StrToBool(L[2]);
    AResource.UserField0 := L[3];
    AResource.UserField1 := L[4];
    AResource.UserField2 := L[5];
    AResource.UserField3 := L[6];
    AResource.UserField4 := L[7];
    AResource.UserField5 := L[8];
    AResource.UserField6 := L[9];
    AResource.UserField7 := L[10];
    AResource.UserField8 := L[11];
    AResource.UserField9 := L[12];
  finally
    L.Free;
  end;
end;

procedure TVpIniDatastore.StrToTask(AString: String; ATask: TVpTask);
var
  L: TStrings;
begin
  L := TStringList.Create;
  try
    Split(AString, L);
    if L.Count <> 18 then
      IniError(RSIniFileStructure);
    ATask.Complete := StrToBool(L[0]);
    ATask.Description := L[1];
    ATask.Details := DecodeLineEndings(L[2]);
    if L[3] = '' then
      ATask.CreatedOn := 0.0 else
      ATask.CreatedOn := StrToDate(L[3], FFormatSettings);
    if L[4] = '' then
      ATask.CompletedOn := 0.0 else
      ATask.CompletedOn := StrToDate(L[4], FFormatSettings);
    ATask.Priority := StrToInt(L[5]);
    ATask.Category := StrToInt(L[6]);
    if L[7] = '' then
      ATask.DueDate := 0.0 else
      ATask.DueDate := StrtoDate(L[7], FFormatSettings);
    ATask.UserField0 := L[8];
    ATask.UserField1 := L[9];
    ATask.UserField2 := L[10];
    ATask.UserField3 := L[11];
    ATask.UserField4 := L[12];
    ATask.UserField5 := L[13];
    ATask.UserField6 := L[14];
    ATask.UserField7 := L[15];
    ATask.UserField8 := L[16];
    ATask.UserField9 := L[17];
  finally
    L.Free;
  end;
end;

procedure TVpIniDatastore.ReadFromIni;
var
  ini: TCustomIniFile;
  ResList, L: TStrings;
  res: TVpResource;
  contact: TVpContact;
  event: TVpEvent;
  task: TVpTask;
  i,j: Integer;
  s: String;
  key: String;
  resID, id: Integer;
  tStart, tEnd: TDateTime;
begin
  if FFileName = '' then
    exit;

  ini := TMemIniFile.Create(FFileName);
  ResList := TStringList.Create;
  L := TStringList.Create;
  try
    Resources.ClearResources;

    ini.ReadSection('Resources', ResList);
    for i:=0 to ResList.Count-1 do begin
      s := ini.ReadString('Resources', ResList[i], '');
      if s = '' then
        IniError(RSIniFileStructure);
      resID := StrToInt(ResList[i]);
      res := Resources.AddResource(resID);
      StrToResource(s, res);

      key := Format('Contacts of resource %d', [resID]);
      L.Clear;
      ini.ReadSection(key, L);
      for j:=0 to L.Count-1 do begin
        id := StrToInt(L[j]);
        contact := res.Contacts.AddContact(id);
        s := ini.ReadString(key, L[j], '');
        StrToContact(s, contact);
      end;

      key := Format('Events of resource %d', [resID]);
      L.Clear;
      ini.ReadSection(key, L);
      for j:=0 to L.Count-1 do begin
        id := StrToInt(L[j]);
        s := ini.ReadString(key, L[j], '');
        StrToEventTimes(s, tStart, tEnd);
        event := res.Schedule.AddEvent(id, tStart, tEnd);
        StrToEvent(s, event);
      end;

      key := Format('Tasks of resource %d', [resID]);
      L.Clear;
      ini.ReadSection(key, L);
      for j:=0 to L.Count-1 do begin
        id := StrToInt(L[j]);
        task := res.Tasks.AddTask(id);
        s := ini.ReadString(key, L[j], '');
        StrToTask(s, task);
      end;
    end;

  finally
    ini.Free;
    L.Free;
    ResList.Free;
  end;
end;

procedure TVpIniDatastore.WriteToIni;
var
  ini: TMemIniFile;
  i, j: Integer;
  res: TVpResource;
  contact: TVpContact;
  event: TVpEvent;
  task: TVpTask;
  key: String;
begin
  if FFileName = '' then
    exit;

  ini := TMemIniFile.Create(FFileName);
  try
    ini.Clear;

    for i:=0 to Resources.Count-1 do begin
      res := Resources.Items[i];
      if not res.Deleted then
        ini.WriteString('Resources', IntToStr(res.ResourceID), ResourceToStr(res));
    end;

    for i:=0 to Resources.Count-1 do begin
      res := Resources.Items[i];
      key := Format('Contacts of resource %d', [res.ResourceID]);
      for j:=0 to res.Contacts.Count-1 do begin
        contact := res.Contacts.GetContact(j);
        if not contact.Deleted then
          ini.WriteString(key, IntToStr(contact.RecordID), ContactToStr(contact));
      end;
    end;

    for i:=0 to Resources.Count-1 do begin
      res := Resources.Items[i];
      key := Format('Tasks of resource %d', [res.ResourceID]);
      for j:=0 to res.Tasks.Count-1 do begin
        task := res.Tasks.GetTask(j);
        if not task.Deleted then
          ini.WriteString(key, IntToStr(task.RecordID), TaskToStr(task));
      end;
    end;

    for i:=0 to Resources.Count-1 do begin
      res := Resources.Items[i];
      key := Format('Events of resource %d', [res.ResourceID]);
      for j:=0 to res.Schedule.EventCount-1 do begin
        event := res.Schedule.GetEvent(j);
        if not event.Deleted then
          ini.WriteString(key, IntToStr(event.RecordID), EventToStr(event));
      end;
    end;

  finally
    ini.Free;
  end;
end;

end.
