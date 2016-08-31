{ Visual PlanIt datastore using an xml file }

{$I vp.inc}

unit VpXmlDs;

interface

uses
  SysUtils, Classes, laz2_xmlread, laz2_xmlwrite, laz2_DOM,
  VpData, VpBaseDS;

type
  TVpXmlDatastore = class(TVpCustomDatastore)
  private
    FFilename: String;
    FParentNode: String;
    FXmlSettings: TFormatSettings;
    procedure SetFilename(const AValue: String);
    procedure SetParentNode(const AValue: String);

  protected
    procedure Loaded; override;
    procedure SetConnected(const AValue: Boolean); override;
    function UniqueID(AValue: Integer): Boolean;

    procedure CleanNode(AParentNode: TDOMNode);
    function CreateStoreNode(ADoc: TDOMDocument): TDOMNode;
    function FindStoreNode(ADoc: TDOMDocument): TDOMNode;

    procedure ReadContact(ANode: TDOMNode; AContacts: TVpContacts);
    procedure ReadContacts(ANode: TDOMNode; AContacts: TVpContacts);
    procedure ReadEvent(ANode: TDOMNode; ASchedule: TVpSchedule);
    procedure ReadEvents(ANode: TDOMNode; ASchedule: TVpSchedule);
    procedure ReadResource(ANode: TDOMNode);
    procedure ReadResources(ANode: TDOMNode);
    procedure ReadTask(ANode: TDOMNode; ATasks: TVpTasks);
    procedure ReadTasks(ANode: TDOMNode; ATasks: TVpTasks);

    procedure WriteContact(ADoc: TDOMDocument; AContactNode: TDOMNode; AContact: TVpContact);
    procedure WriteContacts(ADoc: TDOMDocument; AParentNode: TDOMNode; AResource: TVpResource);
    procedure WriteEvent(ADoc: TDOMDocument; AEventNode: TDOMNode; AEvent: TVpEvent);
    procedure WriteEvents(ADoc: TDOMDocument; AParentNode: TDOMNode; AResource: TVpResource);
    procedure WriteResources(ADoc: TDOMDocument; AParentNode: TDOMNode);
    procedure WriteTask(ADoc: TDOMDocument; ATaskNode: TDOMNode; ATask: TVpTask);
    procedure WriteTasks(ADoc: TDOMDocument; AParentNode: TDOMNode; AResource: TVpResource);

    procedure ReadFromXML;
    procedure WriteToXML;

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
    property FileName: String read FFileName write SetFileName;
    property ParentNode: String read FParentNode write SetParentNode;

  end;

implementation

uses
  typinfo,
  VpConst, VpMisc, VpSR;

const
  STORE_NODE_NAME = 'DataStore';

procedure XmlError(const AMsg: String);
begin
  raise Exception.Create(AMsg);
end;

function GetNodeValue(ANode: TDOMNode): String;
var
  child: TDOMNode;
begin
  child := ANode.FirstChild;
  if child <> nil then
    Result := child.NodeValue else
    Result := '';
end;

function GetAttrValue(ANode: TDOMNode; AAttrName: string) : string;
var
  i: LongWord;
  Found: Boolean;
begin
  Result := '';
  if (ANode = nil) or (ANode.Attributes = nil) then
    exit;

  Found := false;
  i := 0;
  while not Found and (i < ANode.Attributes.Length) do begin
    if ANode.Attributes.Item[i].NodeName = AAttrName then begin
      Found := true;
      Result := ANode.Attributes.Item[i].NodeValue;
    end;
    inc(i);
  end;
end;

function GetBoolAttrValue(ANode: TDOMNode; AAttrName: String): boolean;
var
  s: String;
begin
  s := GetAttrValue(ANode, AAttrName);
  if s <> '' then begin
    if (Lowercase(s) = 'true') or ((Length(s) = 1) and (s[1] in ['t', 'T', '1'])) then
      Result := true
    else
    if (Lowercase(s) = 'false') or ((Length(s) = 1) and (s[1] in ['f', 'F', '0'])) then
      Result := false
    else
      XMLError(Format('Illegal boolean value "%s" for "%s"', [s, AAttrName]));
  end else
    Result := false;
end;

function GetIntAttrValue(ANode: TDOMNode; AAttrName: String): Integer;
var
  s: String;
begin
  s := GetAttrValue(ANode, AAttrName);
  if s <> '' then begin
    if not TryStrToInt(s, Result) then
      XMLError(Format('Illegal integer value "%s" for "%s"', [s, AAttrName]));
  end else
    Result := 0;
end;

function GetDateTimeAttrValue(ANode: TDOMNode; AAttrName: String;
  const AFormatSettings: TFormatSettings): TDateTime;
var
  s: String;
begin
  s := GetAttrValue(ANode, AAttrName);
  if s <> '' then begin
    if not TryStrToDateTime(s, Result, AFormatSettings) then
      XMLError(Format('Illegal date/time value "%s" for "%s"', [s, AAttrName]));
  end else
    Result := 0;
end;

function GetDateAttrValue(ANode: TDOMNode; AAttrName: String;
  const AFormatSettings: TFormatSettings): TDateTime;
var
  s: String;
begin
  s := GetAttrValue(ANode, AAttrName);
  if s <> '' then begin
    if not TryStrToDate(s, Result, AFormatSettings) then
      XMLError(Format('Illegal date value "%s" for "%s"', [s, AAttrName]));
  end else
    Result := 0;
end;

function GetTimeAttrValue(ANode: TDOMNode; AAttrName: String;
  const AFormatSettings: TFormatSettings): TDateTime;
var
  s: String;
begin
  s := GetAttrValue(ANode, AAttrName);
  if s <> '' then begin
    if not TryStrToTime(s, Result, AFormatSettings) then
      XMLError(Format('Illegal time value "%s" for "%s"', [s, AAttrName]));
  end else
    Result := 0;
end;

function GetPhoneTypeAttrValue(ANode: TDOMNode; AAttrName: String;
  ANr: Integer): TVpPhoneType;
var
  s: String;
  n: Integer;
begin
  s := GetAttrValue(ANode, AAttrName);
  n := GetEnumValue(TypeInfo(TVpPhoneType), s);
  if (n >= ord(Low(TVpPhoneType))) and (n <= ord(High(TVpPhoneType))) then
    Result := TVpPhoneType(n)
  else
    XMLError(Format('Illegal PhoneType%d value: "%s"', [ANr, s]));
end;


{ TVpXmlDatastore }

constructor TVpXmlDatastore.Create(AOwner: TComponent);
begin
  inherited;
  FXmlSettings := DefaultFormatSettings;
  FXmlSettings.DecimalSeparator := '.';
  FXmlSettings.ThousandSeparator := #0;
  FXmlSettings.ShortDateFormat := 'yyyy/mm/dd';
  FXmlSettings.LongTimeFormat := 'hh:nn:ss';
  FXmlSettings.DateSeparator := '/';
  FXmlSettings.TimeSeparator := ':';
  FDayBuffer := 1000*365;  // 1000 years, i.e. deactivate daybuffer mechanism
end;

destructor TVpXmlDatastore.Destroy;
begin
  SetConnected(false);
  inherited;
end;

procedure TVpXmlDatastore.CleanNode(AParentNode: TDOMNode);
var
  node: TDOMNode;
begin
  node := AParentNode.FirstChild;
  while node <> nil do begin
    AParentNode.RemoveChild(node);
    node := AParentNode.FirstChild;
  end;
end;

function TVpXmlDatastore.CreateStoreNode(ADoc: TDOMDocument): TDOMNode;
var
  L: TStrings;
  i: Integer;
  node: TDOMNode;
  appending: Boolean;
  {%H-}nodename: String;
begin
  L := TStringList.Create;
  try
    if FParentNode <> '' then begin
      L.Delimiter := '/';
      L.StrictDelimiter := true;
      L.DelimitedText := StringReplace(FParentNode, '\', '/', [rfReplaceAll]);
    end;

    if (L.Count = 0) then begin
      // no parent node specified --> is the root node the node of the data store?
      Result := ADoc.FindNode(STORE_NODE_NAME);
      // no: attach as child of root
      if Result = nil then begin
        Result := ADoc.CreateElement(STORE_NODE_NAME);
        ADoc.AppendChild(Result);
      end;
      exit;
    end;

    // Remove empty path elements due to consecutive slashes
    for i := L.Count-1 downto 0 do
      if L[i] = '' then L.Delete(i);
    // Add the name of the datastore node to the path list of node names
    L.Add(STORE_NODE_NAME);

    // Now iterate through all elements of the path. Begin a new subtree at the
    // element where the ParehtNode path differs from the path in the document.
    node := ADoc;
    appending := false;
    for i:=0 to L.Count-1 do begin
      if not appending then begin
        Result := node.FindNode(L[i]);
        // Result is nil if the path element L[i] is not found. In this case
        // set the flag "appending" to true to indicate that a new sub-tree
        // begins here.
        if (Result = nil) then
          appending := true;
      end;
      if appending then begin
        Result := ADoc.CreateElement(L[i]);
        node.AppendChild(Result);
      end;
      node := Result;
    end;
  finally
    L.Free;
  end;
end;

{ Finds the node with the caption STORE_NODE_NAME, or returns nil if not found.
  Follows the path given by ParentNode }
function TVpXmlDatastore.FindStoreNode(ADoc: TDOMDocument): TDOMNode;
var
  L: TStringList;
  nodename: String;
  i: Integer;
begin
  L := TStringList.Create;
  try
    if FParentNode <> '' then begin
      L.Delimiter := '/';
      L.StrictDelimiter := true;
      L.DelimitedText := StringReplace(FParentNode, '\', '/', [rfReplaceAll]);
    end;

    // ParentNode is empty --> DataStore node is root node
    if (L.Count = 0) then begin
      Result := ADoc.FirstChild;
      if Result <> nil then begin
        nodeName := Result.NodeName;
        if nodeName <> STORE_NODE_NAME then
          Result := nil;
      end;
    end else begin
      // Remove empty path elements due to consecutive slashes
      for i := L.Count-1 downto 0 do
        if L[i] = '' then L.Delete(i);
      // Add the name of the datastore node to the path list of node names
      L.Add(STORE_NODE_NAME);
      // Beginning with root dig deeper along the path specified until the
      // node of the datastore is found (or not).
      Result := ADoc;
      for i:=0 to L.Count-1 do begin
        Result := Result.FindNode(L[i]);
        if Result = nil then
          exit;
      end;
    end;
  finally
    L.Free;
  end;
end;

function TVpXmlDatastore.GetNextID(TableName: string): Integer;
begin
  Unused(TableName);
  repeat
    Result := Random(High(Integer));
  until UniqueID(Result) and (Result <> -1);
end;

procedure TVpXmlDatastore.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Connected := AutoConnect;
end;

function TVpXmlDatastore.UniqueID(AValue: Integer): Boolean;
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

procedure TVpXmlDatastore.SetConnected(const AValue: Boolean);
begin
  if AValue = Connected then
    exit;

  if AValue then
    ReadFromXml
  else
    WriteToXml;

  inherited SetConnected(AValue);
end;

procedure TVpXmlDatastore.SetResourceByName(Value: string);
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

procedure TVpXmlDatastore.SetFileName(const AValue: String);
begin
  FFileName := AValue;
  if AutoConnect then ReadFromXml;
end;

procedure TVpXmlDatastore.SetParentNode(const AValue: String);
begin
  FParentNode := AValue;
  if (FFileName <> '') and AutoConnect then
    ReadFromXml;
end;

procedure TVpXmlDatastore.LoadContacts;
begin
  // Nothing to do here...
end;

procedure TVpXmlDatastore.LoadEvents;
begin
  // Nothing to do here...
end;

procedure TVpXmlDatastore.LoadTasks;
begin
  // Nothing to do here...
end;

procedure TVpXmlDatastore.PostContacts;
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

procedure TVpXmlDatastore.PostEvents;
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

procedure TVpXmlDatastore.PostResources;
begin
  // Nothing to do...
end;

procedure TVpXmlDatastore.PostTasks;
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

procedure TVpXmlDatastore.ReadFromXml;
var
  doc: TXMLDocument;
  node, storeNode: TDOMNode;
  nodename: String;
begin
  if FFileName = '' then
    exit;

  if not FileExists(FFileName) then
    exit;

  doc := nil;
  try
    ReadXMLFile(doc, FFileName);
    storeNode := FindStoreNode(doc);
    if storeNode = nil then
      exit;

    nodeName := storeNode.NodeName;
    Resources.ClearResources;
    node := storeNode.FirstChild;
      while node <> nil do begin
        nodeName := node.NodeName;
        if nodeName = 'Resources' then
          ReadResources(node);
        node := node.NextSibling;
      end;
  finally
    doc.Free;
  end;
end;

procedure TVpXmlDatastore.ReadContact(ANode: TDOMNode; AContacts: TVpContacts);
var
  node: TDOMNode;
  nodeName: String;
  cont: TVpContact;
  id: Integer;
  s: String;
begin
  s := GetAttrValue(ANode, 'RecordID');
  if s = '' then
    XMLError('RecordID missing');
  if not TryStrToInt(s, id) then
    XMLError('RecordID must be a number.');

  cont := AContacts.AddContact(id);
  cont.BirthDate := GetDateAttrValue(ANode, 'BirthDate', FXmlSettings);
  cont.Anniversary := GetDateAttrValue(ANode, 'Anniversary', FXmlSettings);
  cont.Category := GetIntAttrValue(ANode, 'Category');

  node := ANode.FirstChild;
  while node <> nil do begin
    nodeName := node.NodeName;
    if nodeName = 'FirstName' then
      cont.FirstName := GetNodeValue(node)
    else if nodeName = 'LastName' then
      cont.LastName := GetNodeValue(node)
    else if nodeName = 'Job_Position' then
      cont.Job_Position := GetNodeValue(node)
    else if nodeName = 'Title' then
      cont.Title := GetNodeValue(node)
    else if nodeName = 'Company' then
      cont.Company := GetNodeValue(node)
    else if nodeName = 'Address' then
      cont.Address := GetNodeValue(node)
    else if nodeName = 'City' then
      cont.City := GetNodeValue(node)
    else if nodeName = 'Zip' then
      cont.Zip := GetNodeValue(node)
    else if nodeName = 'State' then
      cont.State := GetNodeValue(node)
    else if nodeName = 'Country' then
      cont.Country := GetNodeValue(node)
    else if nodeName = 'EMail'  then
      cont.EMail := GetNodeValue(node)
    else if nodeName = 'Phone1' then begin
      cont.Phone1 := GetNodeValue(node);
      cont.PhoneType1 := ord(GetPhoneTypeAttrValue(node, 'Type', 1));
    end else if nodeName = 'Phone2' then begin
      cont.Phone2 := GetNodeValue(node);
      cont.PhoneType2 := ord(GetPhoneTypeAttrValue(node, 'Type', 2));
    end else if nodeName = 'Phone3' then begin
      cont.Phone3 := GetNodeValue(node);
      cont.PhoneType3 := ord(GetPhoneTypeAttrValue(node, 'Type', 3));
    end else if nodeName = 'Phone4' then begin
      cont.Phone4 := GetNodeValue(node);
      cont.PhoneType4 := ord(GetPhoneTypeAttrValue(node, 'Type', 4));
    end else if nodeName = 'Phone5' then begin
      cont.Phone5 := GetNodeValue(node);
      cont.PhoneType5 := ord(GetPhoneTypeAttrValue(node, 'Type', 5));
    end else if nodeName = 'Notes' then
      cont.Notes := GetNodeValue(node)
    else if nodeName = 'Custom1' then
      cont.Custom1 := GetNodeValue(node)
    else if nodeName = 'Custom2' then
      cont.Custom2 := GetNodeValue(node)
    else if nodeName = 'Custom3' then
      cont.Custom3 := GetNodeValue(node)
    else if nodeName = 'Custom4' then
      cont.Custom3 := GetNodeValue(node)
    else if nodeName = 'UserField0' then
      cont.UserField0 := GetNodeValue(node)
    else if nodeName = 'UserField1' then
      cont.UserField1 := GetNodeValue(node)
    else if nodeName = 'UserField2' then
      cont.UserField2 := GetNodeValue(node)
    else if nodeName = 'UserField3' then
      cont.UserField3 := GetNodeValue(node)
    else if nodeName = 'UserField4' then
      cont.UserField4 := GetNodeValue(node)
    else if nodeName = 'UserField5' then
      cont.UserField5 := GetNodeValue(node)
    else if nodeName = 'UserField6' then
      cont.UserField6 := GetNodeValue(node)
    else if nodeName = 'UserField7' then
      cont.UserField7 := GetNodeValue(node)
    else if nodeName = 'UserField8' then
      cont.UserField8 := GetNodeValue(node)
    else if nodeName = 'UserField9' then
      cont.UserField9 := GetNodeValue(node);
    node := node.NextSibling;
  end;
end;

procedure TVpXmlDatastore.ReadContacts(ANode: TDOMNode; AContacts: TVpContacts);
var
  node: TDOMNode;
  nodeName: String;
begin
  node := ANode.FirstChild;
  while node <> nil do begin
    nodeName := node.NodeName;
    if nodeName = 'Contact' then
      ReadContact(node, AContacts);
    node := node.NextSibling;
  end;
end;

procedure TVpXmlDatastore.ReadEvent(ANode: TDOMNode; ASchedule: TVpSchedule);
var
  node: TDOMNode;
  nodeName: String;
  ev: TVpEvent;
  id: Integer;
  s: String;
  n: Integer;
  t1, t2: TDateTime;
begin
  s := GetAttrValue(ANode, 'RecordID');
  if s = '' then
    XMLError('RecordID missing');
  if not TryStrToInt(s, id) then
    XMLError('RecordID must be a number.');
  t1 := GetDateTimeAttrValue(ANode, 'StartTime', FXmlSettings);
  t2 := GetDateTimeAttrValue(ANode, 'EndTime', FXmlSettings);

  ev := ASchedule.AddEvent(id, t1, t2);
  ev.AlarmAdvance := GetIntAttrValue(ANode, 'AlarmAdvance');
  ev.AlarmSet := GetBoolAttrValue(ANode, 'AlarmSet');
  ev.AlertDisplayed := GetBoolAttrValue(ANode, 'AlertDisplayed');
  ev.AllDayEvent := GetBoolAttrValue(ANode, 'AllDayEvent');
  ev.Category := GetIntAttrValue(ANode, 'Category');
  ev.StartTime := t1;
  ev.EndTime := t2;
  ev.SnoozeTime := GetDateTimeAttrValue(ANode, 'SnoozeTime', FXmlSettings);
  ev.RepeatRangeEnd := GetDateTimeAttrValue(ANode, 'RepeatRangeEng', FXmlSettings);
  ev.CustomInterval := GetIntAttrValue(ANode, 'CustomInterval');

  s := GetAttrValue(ANode, 'AlarmAdvanceType');
  if s <> '' then begin
    n := GetEnumValue(TypeInfo(TVpAlarmAdvType), s);
    if (n >= ord(Low(TVpAlarmAdvType))) and (n <= ord(High(TVpAlarmAdvType))) then
      ev.AlarmAdvanceType := TVpAlarmAdvType(n)
    else
      XMLError(Format('Incorrect AdvanceType value: "%s"', [s]));
  end;

  s := GetAttrValue(ANode, 'RepeatCode');
  if s <> '' then begin
    n := GetEnumValue(TypeInfo(TVpRepeatType), s);
    if (n >= ord(Low(TVpRepeatType))) and (n <= ord(High(TVpRepeatType))) then
      ev.RepeatCode := TVpRepeatType(n)
    else
      XMLError(Format('Incorrect RepeatCode value: "%s"', [s]));
  end;

  node := ANode.FirstChild;
  while node <> nil do begin
    nodeName := node.NodeName;
    if nodeName = 'Description' then
      ev.Description := GetNodeValue(node)
    else if nodeName = 'Notes' then
      ev.Notes := GetNodeValue(node)
    else if nodeName = 'Location' then
      ev.Location := GetNodeValue(node)
    else if nodeName = 'DingPath' then
      ev.DingPath := GetNodeValue(node)
    else if nodeName = 'UserField0' then
      ev.UserField0 := GetNodeValue(node)
    else if nodeName = 'UserField1' then
      ev.UserField1 := GetNodeValue(node)
    else if nodeName = 'UserField2' then
      ev.UserField2 := GetNodeValue(node)
    else if nodeName = 'UserField3' then
      ev.UserField3 := GetNodeValue(node)
    else if nodeName = 'UserField4' then
      ev.UserField4 := GetNodeValue(node)
    else if nodeName = 'UserField5' then
      ev.UserField5 := GetNodeValue(node)
    else if nodeName = 'UserField6' then
      ev.UserField6 := GetNodeValue(node)
    else if nodeName = 'UserField7' then
      ev.UserField7 := GetNodeValue(node)
    else if nodeName = 'UserField8' then
      ev.UserField8 := GetNodeValue(node)
    else if nodeName = 'UserField9' then
      ev.UserField9 := GetNodeValue(node);
    node := node.NextSibling;
  end;
end;

procedure TVpXmlDatastore.ReadEvents(ANode: TDOMNode; ASchedule: TVpSchedule);
var
  node: TDOMNode;
  nodeName: String;
begin
  node := ANode.FirstChild;
  while node <> nil do begin
    nodeName := node.NodeName;
    if nodeName = 'Event' then
      ReadEvent(node, ASchedule);
    node := node.NextSibling;
  end;
end;

// <Resource ResourceID="1178568021" ResourceActive="true">
//    <Description>some test</Description>
// </Resource>
procedure TVpXmlDatastore.ReadResource(ANode: TDOMNode);
var
  node: TDOMNode;
  nodeName: String;
  res: TVpResource;
  id: Integer;
  s: String;
begin
  s := GetAttrValue(ANode, 'ResourceID');
  if s = '' then
    XMLError('ResourceID missing');
  if not TryStrToInt(s, id) then
    XMLError('ResourceID must be a number.');

  res := Resources.AddResource(id);
  res.ResourceActive := GetBoolAttrValue(ANode, 'ResourceActive');

  node := ANode.FirstChild;
  while node <> nil do begin
    nodeName := node.NodeName;
    if nodeName = 'Description' then
      res.Description := GetNodeValue(node)
    else if nodeName = 'Notes' then
      res.Notes := GetNodeValue(node)
    else if nodeName = 'Contacts' then
      ReadContacts(node, res.Contacts)
    else if nodeName = 'Events' then
      ReadEvents(node, res.Schedule)
    else if nodeName = 'Tasks' then
      ReadTasks(node, res.Tasks)
    else if nodeName = 'UserField0' then
      res.UserField0 := GetNodeValue(node)
    else if nodeName = 'UserField1' then
      res.UserField1 := GetNodeValue(node)
    else if nodeName = 'UserField2' then
      res.UserField2 := GetNodeValue(node)
    else if nodeName = 'UserField3' then
      res.UserField3 := GetNodeValue(node)
    else if nodeName = 'UserField4' then
      res.UserField4 := GetNodeValue(node)
    else if nodeName = 'UserField5' then
      res.UserField5 := GetNodeValue(node)
    else if nodeName = 'UserField6' then
      res.UserField6 := GetNodeValue(node)
    else if nodeName = 'UserField7' then
      res.UserField7 := GetNodeValue(node)
    else if nodeName = 'UserField8' then
      res.UserField8 := GetNodeValue(node)
    else if nodeName = 'UserField9' then
      res.UserField9 := GetNodeValue(node);
    node := node.NextSibling;
  end;
end;

procedure TVpXmlDatastore.ReadResources(ANode: TDOMNode);
var
  node: TDOMNode;
  nodeName: String;
begin
  node := ANode.FirstChild;
  while node <> nil do begin
    nodeName := node.NodeName;
    if nodeName = 'Resource' then
      ReadResource(node);
    node := node.NextSibling;
  end;
end;

procedure TVpXmlDatastore.ReadTask(ANode: TDOMNode; ATasks: TVpTasks);
var
  node: TDOMNode;
  nodeName: String;
  t: TVpTask;
  id: Integer;
  s: String;
begin
  s := GetAttrValue(ANode, 'RecordID');
  if s = '' then
    XMLError('RecordID missing');
  if not TryStrToInt(s, id) then
    XMLError('RecordID must be a number.');

  t := ATasks.AddTask(id);
  t.DueDate := GetDateAttrValue(ANode, 'DueDate', FXmlSettings);
  t.CompletedOn := GetDateAttrValue(ANode, 'CompletedOn', FXmlSettings);
  t.CreatedOn := GetDateAttrValue(ANode, 'CreatedOn', FXmlSettings);
  t.Complete := GetBoolAttrValue(ANode, 'Complete');
  t.Priority := GetIntAttrValue(ANode, 'Priority');
  t.Category := GetIntAttrValue(ANode, 'Categoriy');

  node := ANode.FirstChild;
  while node <> nil do begin
    nodeName := node.NodeName;
    if nodeName = 'Description' then
      t.Description := GetNodeValue(node)
    else if nodeName = 'Details' then
      t.Details := GetNodeValue(node)
    else if nodeName = 'UserField0' then
      t.UserField0 := GetNodeValue(node)
    else if nodeName = 'UserField1' then
      t.UserField1 := GetNodeValue(node)
    else if nodeName = 'UserField2' then
      t.UserField2 := GetNodeValue(node)
    else if nodeName = 'UserField3' then
      t.UserField3 := GetNodeValue(node)
    else if nodeName = 'UserField4' then
      t.UserField4 := GetNodeValue(node)
    else if nodeName = 'UserField5' then
      t.UserField5 := GetNodeValue(node)
    else if nodeName = 'UserField6' then
      t.UserField6 := GetNodeValue(node)
    else if nodeName = 'UserField7' then
      t.UserField7 := GetNodeValue(node)
    else if nodeName = 'UserField8' then
      t.UserField8 := GetNodeValue(node)
    else if nodeName = 'UserField9' then
      t.UserField9 := GetNodeValue(node);
    node := node.NextSibling;
  end;
end;

procedure TVpXmlDatastore.ReadTasks(ANode: TDOMNode; ATasks: TVpTasks);
var
  node: TDOMNode;
  nodeName: String;
begin
  node := ANode.FirstChild;
  while node <> nil do begin
    nodeName := node.NodeName;
    if nodeName = 'Task' then
      ReadTask(node, ATasks);
    node := node.NextSibling;
  end;
end;

procedure TVpXmlDatastore.WriteContact(ADoc: TDOMDocument; AContactNode: TDOMNode;
  AContact: TVpContact);
var
  child, txt: TDOMNode;
begin
  with TDOMElement(AContactNode) do begin
    SetAttribute('RecordID', IntToStr(AContact.RecordID));
    SetAttribute('Category', IntToStr(AContact.Category));
    if AContact.BirthDate <> 0 then
      SetAttribute('BirthDate', DateToStr(AContact.BirthDate, FXmlSettings));
    if AContact.Anniversary <> 0 then
      SetAttribute('Anniversary', DateToStr(AContact.Anniversary, FXmlSettings));
  end;

  if AContact.FirstName <> '' then begin
    child := ADoc.CreateElement('FirstName');
    txt := ADoc.CreateTextNode(AContact.FirstName);
    child.AppendChild(txt);
    AContactNode.Appendchild(child);
  end;

  if AContact.LastName <> '' then begin
    child := ADoc.CreateElement('LastName');
    txt := ADoc.CreateTextNode(AContact.LastName);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.Title <> '' then begin
    child := ADoc.CreateElement('Title');
    txt := ADoc.CreateTextNode(AContact.Title);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.Company <> '' then begin
    child := ADoc.CreateElement('Company');
    txt := ADoc.CreateTextNode(AContact.Company);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.Job_Position <> '' then begin
    child := ADoc.CreateElement('Job_Position');
    txt := ADoc.CreateTextNode(AContact.Job_Position);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.EMail <> '' then begin
    child := ADoc.CreateElement('EMail');
    txt := ADoc.CreateTextNode(AContact.EMail);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.Phone1 <> '' then begin
    child := ADoc.CreateElement('Phone1');
    TDOMElement(child).SetAttribute('Type',
      GetEnumName(TypeInfo(TVpPhoneType), ord(AContact.PhoneType1)));
    txt := ADoc.CreateTextNode(AContact.Phone1);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.Phone2 <> '' then begin
    child := ADoc.CreateElement('Phone2');
    TDOMElement(child).SetAttribute('Type',
      GetEnumName(TypeInfo(TVpPhoneType), ord(AContact.PhoneType2)));
    txt := ADoc.CreateTextNode(IntToStr(AContact.PhoneType2));
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.Phone3 <> '' then begin
    child := ADoc.CreateElement('Phone3');
    TDOMElement(child).SetAttribute('Type',
      GetEnumName(TypeInfo(TVpPhoneType), ord(AContact.PhoneType3)));
    txt := ADoc.CreateTextNode(AContact.Phone3);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.Phone4 <> '' then begin
    child := ADoc.CreateElement('Phone4');
    TDOMElement(child).SetAttribute('Type',
      GetEnumName(TypeInfo(TVpPhoneType), ord(AContact.PhoneType4)));
    txt := ADoc.CreateTextNode(AContact.Phone4);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.Phone5 <> '' then begin
    child := ADoc.CreateElement('Phone5');
    TDOMElement(child).SetAttribute('Type',
      GetEnumName(TypeInfo(TVpPhoneType), ord(AContact.PhoneType5)));
    txt := ADoc.CreateTextNode(AContact.Phone5);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.Address <> '' then begin
    child := ADoc.CreateElement('Address');
    txt := ADoc.CreateTextNode(AContact.Address);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.City <> '' then begin
    child := ADoc.CreateElement('City');
    txt := ADoc.CreateTextNode(AContact.City);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.State <> '' then begin
    child := ADoc.CreateElement('State');
    txt := ADoc.CreateTextNode(AContact.State);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.Zip <> '' then begin
    child := ADoc.CreateElement('Zip');
    txt := ADoc.CreateTextNode(AContact.Zip);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.Country <> '' then begin
    child := ADoc.CreateElement('Country');
    txt := ADoc.CreateTextNode(AContact.Country);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.Notes <> '' then begin
    child := ADoc.CreateElement('Notes');
    txt := ADoc.CreateTextNode(AContact.Notes);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.Custom1 <> '' then begin
    child := ADoc.CreateElement('Custom1');
    txt := ADoc.CreateTextNode(AContact.Custom1);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.Custom2 <> '' then begin
    child := ADoc.CreateElement('Custom2');
    txt := ADoc.CreateTextNode(AContact.Custom2);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.Custom3 <> '' then begin
    child := ADoc.CreateElement('Custom3');
    txt := ADoc.CreateTextNode(AContact.Custom3);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.Custom4 <> '' then begin
    child := ADoc.CreateElement('Custom4');
    txt := ADoc.CreateTextNode(AContact.Custom4);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.UserField0 <> '' then begin
    child := ADoc.CreateElement('UserField0');
    txt := ADoc.CreateTextNode(AContact.UserField0);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.UserField1 <> '' then begin
    child := ADoc.CreateElement('UserField1');
    txt := ADoc.CreateTextNode(AContact.UserField1);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.UserField2 <> '' then begin
    child := ADoc.CreateElement('UserField2');
    txt := ADoc.CreateTextNode(AContact.UserField2);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.UserField3 <> '' then begin
    child := ADoc.CreateElement('UserField3');
    txt := ADoc.CreateTextNode(AContact.UserField3);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.UserField4 <> '' then begin
    child := ADoc.CreateElement('UserField4');
    txt := ADoc.CreateTextNode(AContact.UserField4);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.UserField5 <> '' then begin
    child := ADoc.CreateElement('UserField5');
    txt := ADoc.CreateTextNode(AContact.UserField5);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.UserField6 <> '' then begin
    child := ADoc.CreateElement('UserField6');
    txt := ADoc.CreateTextNode(AContact.UserField6);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.UserField7 <> '' then begin
    child := ADoc.CreateElement('UserField7');
    txt := ADoc.CreateTextNode(AContact.UserField7);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.UserField8 <> '' then begin
    child := ADoc.CreateElement('UserField8');
    txt := ADoc.CreateTextNode(AContact.UserField8);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;

  if AContact.UserField9 <> '' then begin
    child := ADoc.CreateElement('UserField9');
    txt := ADoc.CreateTextNode(AContact.UserField9);
    child.AppendChild(txt);
    AContactNode.AppendChild(child);
  end;
end;

procedure TVpXmlDatastore.WriteContacts(ADoc: TDOMDocument; AParentNode: TDOMNode;
  AResource: TVpResource);
var
  i: Integer;
  node, contNode: TDOMNode;
  cont: TVpContact;
begin
  node := ADoc.CreateElement('Contacts');
  TDOMElement(node).SetAttribute('Count', IntToStr(AResource.Contacts.Count));
  AParentNode.AppendChild(node);

  for i := 0 to AResource.Contacts.Count-1 do begin
    cont := AResource.Contacts.GetContact(i);
    contNode := ADoc.CreateElement('Contact');
    node.AppendChild(contNode);
    WriteContact(ADoc, contNode, cont);
  end;
end;

procedure TVpXmlDatastore.WriteEvent(ADoc: TDOMDocument; AEventNode: TDOMNode;
  AEvent: TVpEvent);
var
  child, txt: TDOMNode;
begin
  with TDOMElement(AEventNode) do begin
    SetAttribute('RecordID', IntToStr(AEvent.RecordID));
    SetAttribute('Category', IntToStr(AEvent.Category));
    if AEvent.StartTime <> 0 then
      SetAttribute('StartTime', DateTimeToStr(AEvent.StartTime, FXmlSettings));
    if AEvent.EndTime <> 0 then
      SetAttribute('EndTime', DateTimeToStr(AEvent.EndTime, FXmlSettings));
    SetAttribute('AllDayEvent', BoolToStr(AEvent.AllDayEvent, strTRUE, strFALSE));
    SetAttribute('RepeatCode', GetEnumName(TypeInfo(TVpRepeatType), ord(AEvent.RepeatCode)));
    if AEvent.RepeatRangeEnd <> 0 then
      SetAttribute('RepeatRangeEnd', DateTimeToStr(AEvent.RepeatRangeEnd, FXmlSettings));
    SetAttribute('AlarmSet', BoolToStr(AEvent.AlarmSet, strTRUE, strFALSE));
    SetAttribute('CustomInterval', IntToStr(AEvent.CustomInterval));
    if AEvent.SnoozeTime <> 0 then
      SetAttribute('SnoozeTime', TimeToStr(AEvent.SnoozeTime, FXmlSettings));
    SetAttribute('AlarmAdvanceType', GetEnumName(TypeInfo(TVpAlarmAdvType), ord(AEvent.AlarmAdvanceType)));
    SetAttribute('AlarmAdvance', IntToStr(AEvent.AlarmAdvance));
    SetAttribute('AlertDisplayed', BoolToStr(AEvent.AlertDisplayed, strTRUE, strFALSE));
  end;

  if AEvent.DingPath <> '' then begin
    child := ADoc.CreateElement('DingPath');
    txt := ADoc.CreateTextNode(AEvent.DingPath);
    child.AppendChild(txt);
    AEventNode.AppendChild(child);
  end;

  if AEvent.Description <> '' then begin
    child := ADoc.CreateElement('Description');
    txt := ADoc.CreateTextNode(AEvent.Description);
    child.AppendChild(txt);
    AEventNode.Appendchild(child);
  end;

  if AEvent.Notes <> '' then begin
    child := ADoc.CreateElement('Notes');
    txt := ADoc.CreateTextNode(AEvent.Notes);
    child.AppendChild(txt);
    AEventNode.AppendChild(child);
  end;

  if AEvent.Location <> '' then begin
    child := ADoc.CreateElement('Location');
    txt := ADoc.CreateTextNode(AEvent.Location);
    child.AppendChild(txt);
    AEventNode.AppendChild(child);
  end;

  if AEvent.UserField0 <> '' then begin
    child := ADoc.CreateElement('UserField0');
    txt := ADoc.CreateTextNode(AEvent.UserField0);
    child.AppendChild(txt);
    AEventNode.AppendChild(child);
  end;

  if AEvent.UserField1 <> '' then begin
    child := ADoc.CreateElement('UserField1');
    txt := ADoc.CreateTextNode(AEvent.UserField1);
    child.AppendChild(txt);
    AEventNode.AppendChild(child);
  end;

  if AEvent.UserField2 <> '' then begin
    child := ADoc.CreateElement('UserField2');
    txt := ADoc.CreateTextNode(AEvent.UserField2);
    child.AppendChild(txt);
    AEventNode.AppendChild(child);
  end;

  if AEvent.UserField3 <> '' then begin
    child := ADoc.CreateElement('UserField3');
    txt := ADoc.CreateTextNode(AEvent.UserField3);
    child.AppendChild(txt);
    AEventNode.AppendChild(child);
  end;

  if AEvent.UserField4 <> '' then begin
    child := ADoc.CreateElement('UserField4');
    txt := ADoc.CreateTextNode(AEvent.UserField4);
    child.AppendChild(txt);
    AEventNode.AppendChild(child);
  end;

  if AEvent.UserField5 <> '' then begin
    child := ADoc.CreateElement('UserField5');
    txt := ADoc.CreateTextNode(AEvent.UserField5);
    child.AppendChild(txt);
    AEventNode.AppendChild(child);
  end;

  if AEvent.Userfield6 <> '' then begin
    child := ADoc.CreateElement('UserField6');
    txt := ADoc.CreateTextNode(AEvent.UserField6);
    child.AppendChild(txt);
    AEventNode.AppendChild(child);
  end;

  if AEvent.UserField7 <> '' then begin
    child := ADoc.CreateElement('UserField7');
    txt := ADoc.CreateTextNode(AEvent.UserField7);
    child.AppendChild(txt);
    AEventNode.AppendChild(child);
  end;

  if AEvent.UserField8 <> '' then begin
    child := ADoc.CreateElement('UserField8');
    txt := ADoc.CreateTextNode(AEvent.UserField8);
    child.AppendChild(txt);
    AEventNode.AppendChild(child);
  end;

  if AEvent.UserField9 <> '' then begin
    child := ADoc.CreateElement('UserField9');
    txt := ADoc.CreateTextNode(AEvent.UserField9);
    child.AppendChild(txt);
    AEventNode.AppendChild(child);
  end;
end;

procedure TVpXmlDatastore.WriteEvents(ADoc: TDOMDocument; AParentNode: TDOMNode;
  AResource: TVpResource);
var
  i: Integer;
  node, evNode: TDOMNode;
  ev: TVpEvent;
begin
  node := ADoc.CreateElement('Events');
  TDOMElement(node).SetAttribute('Count', IntToStr(AResource.Schedule.EventCount));
  AParentNode.AppendChild(node);

  for i:=0 to AResource.Schedule.EventCount-1 do begin
    ev := AResource.Schedule.GetEvent(i);
    evNode := ADoc.CreateElement('Event');
    node.AppendChild(evNode);
    WriteEvent(ADoc, evNode, ev);
  end;
end;

procedure TVpXmlDatastore.WriteResources(ADoc: TDOMDocument; AParentNode: TDOMNode);
var
  i: Integer;
  node, resnode, child, txt: TDOMNode;
  res: TVpResource;
begin
  node := ADoc.CreateElement('Resources');
  TDOMElement(node).SetAttribute('Count', IntToStr(Resources.Count));
  AParentNode.AppendChild(node);

  for i:=0 to Resources.Count-1 do begin
    res := Resources.Items[i];

    resNode := ADoc.CreateElement('Resource');
    with TDOMElement(resNode) do begin
      SetAttribute('ResourceID', IntToStr(res.ResourceID));
      SetAttribute('ResourceActive', BoolToStr(res.ResourceActive, strTRUE, strFALSE));
    end;
    node.AppendChild(resnode);

    if res.Description <> '' then begin
      child := ADoc.CreateElement('Description');
      txt := ADoc.CreateTextNode(res.Description);
      child.AppendChild(txt);;
      resnode.AppendChild(child);
    end;

    if res.Notes <> '' then begin
      child := ADoc.CreateElement('Notes');
      txt := ADoc.CreateTextNode(res.Notes);
      child.AppendChild(txt);
      resnode.AppendChild(child);
    end;

    if res.UserField0 <> '' then begin
      child := ADoc.CreateElement('UserField0');
      txt := ADoc.CreateTextNode(res.UserField0);
      child.AppendChild(txt);
      resnode.AppendChild(child);
    end;

    if res.UserField1 <> '' then begin
      child := ADoc.CreateElement('UserField1');
      txt := ADoc.CreateTextNode(res.UserField1);
      child.AppendChild(txt);
      resnode.AppendChild(child);
    end;

    if res.UserField2 <> '' then begin
      child := ADoc.CreateElement('UserField2');
      txt := ADoc.CreateTextNode(res.UserField2);
      child.AppendChild(txt);
      resnode.AppendChild(child);
    end;

    if res.UserField3 <> '' then begin
      child := ADoc.CreateElement('UserField3');
      txt := ADoc.CreateTextNode(res.UserField3);
      child.AppendChild(txt);
      resnode.AppendChild(child);
    end;

    if res.UserField4 <> '' then begin
      child := ADoc.CreateElement('UserField4');
      txt := ADoc.CreateTextNode(res.UserField4);
      child.AppendChild(txt);
      resnode.AppendChild(child);
    end;

    if res.UserField5 <> '' then begin
      child := ADoc.CreateElement('UserField5');
      txt := ADoc.CreateTextNode(res.UserField5);
      child.AppendChild(txt);
      resnode.AppendChild(child);
    end;

    if res.UserField6 <> '' then begin
      child := ADoc.CreateElement('UserField6');
      txt := ADoc.CreateTextNode(res.UserField6);
      child.AppendChild(txt);
      resnode.AppendChild(child);
    end;

    if res.UserField7 <> '' then begin
      child := ADoc.CreateElement('UserField7');
      txt := ADoc.CreateTextNode(res.UserField7);
      child.AppendChild(txt);
      resnode.AppendChild(child);
    end;

    if res.UserField8 <> '' then begin
      child := ADoc.CreateElement('UserField8');
      txt := ADoc.CreateTextNode(res.UserField8);
      child.AppendChild(txt);
      resnode.AppendChild(child);
    end;

    if res.UserField9 <> '' then begin
      child := ADoc.CreateElement('UserField9');
      txt := ADoc.CreateTextNode(res.UserField9);
      child.AppendChild(txt);
      resnode.AppendChild(child);
    end;

    WriteContacts(ADoc, resnode, res);
    WriteEvents(ADoc, resnode, res);
    WriteTasks(ADoc, resNode, res);
  end;
end;

procedure TVpXmlDatastore.WriteTask(ADoc: TDOMDocument; ATaskNode: TDOMNode;
  ATask: TVpTask);
var
  child, txt: TDOMNode;
begin
  with TDOMElement(ATaskNode) do begin
    SetAttribute('RecordID', IntToStr(ATask.RecordID));
    SetAttribute('Category', IntToStr(ATask.Category));
    SetAttribute('Priority', IntToStr(ATask.Priority));
    SetAttribute('DueDate', DateToStr(ATask.DueDate, FXmlSettings));
    SetAttribute('Complete', BoolToStr(ATask.Complete, strTRUE, strFALSE));
    if ATask.CreatedOn > 0 then
      SetAttribute('CreatedOn', DateToStr(ATask.CreatedOn, FXmlSettings));
    if ATask.CompletedOn > 0 then
      SetAttribute('CompletedOn', DateToStr(ATask.CompletedOn, FXmlSettings));
  end;

  if ATask.Description <> '' then begin
    child := ADoc.CreateElement('Description');
    txt := ADoc.CreateTextNode(ATask.Description);
    child.AppendChild(txt);;
    ATaskNode.AppendChild(child);
  end;

  if ATask.Details <> '' then begin
    child := ADoc.CreateElement('Details');
    txt := ADoc.CreateTextNode(ATask.Details);
    child.AppendChild(txt);
    ATaskNode.AppendChild(child);
  end;

  if ATask.UserField0 <> '' then begin
    child := ADoc.CreateElement('UserField0');
    txt := ADoc.CreateTextNode(ATask.UserField0);
    child.AppendChild(txt);
    ATaskNode.AppendChild(child);
  end;

  if ATask.UserField1 <> '' then begin
    child := ADoc.CreateElement('UserField1');
    txt := ADoc.CreateTextNode(ATask.UserField1);
    child.AppendChild(txt);
    ATaskNode.AppendChild(child);
  end;

  if ATask.UserField2 <> '' then begin
    child := ADoc.CreateElement('UserField2');
    txt := ADoc.CreateTextNode(ATask.UserField2);
    child.AppendChild(txt);
    ATaskNode.AppendChild(child);
  end;

  if ATask.UserField3 <> '' then begin
    child := ADoc.CreateElement('UserField3');
    txt := ADoc.CreateTextNode(ATask.UserField3);
    child.AppendChild(txt);
    ATaskNode.AppendChild(child);
  end;

  if ATask.UserField4 <> '' then begin
    child := ADoc.CreateElement('UserField4');
    txt := ADoc.CreateTextNode(ATask.UserField4);
    child.AppendChild(txt);
    ATaskNode.AppendChild(child);
  end;

  if ATask.UserField5 <> '' then begin
    child := ADoc.CreateElement('UserField5');
    txt := ADoc.CreateTextNode(ATask.UserField5);
    child.AppendChild(txt);
    ATaskNode.AppendChild(child);
  end;

  if ATask.UserField6 <> '' then begin
    child := ADoc.CreateElement('UserField6');
    txt := ADoc.CreateTextNode(ATask.UserField6);
    child.AppendChild(txt);
    ATaskNode.AppendChild(child);
  end;

  if ATask.UserField7 <> '' then begin
    child := ADoc.CreateElement('UserField7');
    txt := ADoc.CreateTextNode(ATask.UserField7);
    child.AppendChild(txt);
    ATaskNode.AppendChild(child);
  end;

  if ATask.UserField8 <> '' then begin
    child := ADoc.CreateElement('UserField8');
    txt := ADoc.CreateTextNode(ATask.UserField8);
    child.AppendChild(txt);
    ATaskNode.AppendChild(child);
  end;

  if ATask.UserField9 <> '' then begin
    child := ADoc.CreateElement('UserField9');
    txt := ADoc.CreateTextNode(ATask.UserField9);
    child.AppendChild(txt);
    ATaskNode.AppendChild(child);
  end;
end;

procedure TVpXmlDatastore.WriteTasks(ADoc: TDOMDocument; AParentNode: TDOMNode;
  AResource: TVpResource);
var
  i: Integer;
  node, tnode: TDOMNode;
  t: TVpTask;
begin
  node := ADoc.CreateElement('Tasks');
  TDOMElement(node).SetAttribute('Count', IntToStr(AResource.Tasks.Count));
  AParentNode.AppendChild(node);

  for i:=0 to AResource.Tasks.Count-1 do begin
    t := AResource.Tasks.GetTask(i);
    tNode := ADoc.CreateElement('Task');
    node.AppendChild(tNode);
    WriteTask(ADoc, tNode, t);
  end;
end;

procedure TVpXmlDatastore.WriteToXML;
var
  doc: TXMLDocument;
  storeNode: TDOMNode;
begin
  if FFileName = '' then
    exit;

  doc := nil;
  try
    if FileExists(FFileName) then begin
      // Read existing file and find the node containing the store data
      ReadXMLFile(doc, FFileName);
      storeNode := FindStoreNode(doc);
      // If the file does not contain a store node create a new subtree
      if storeNode = nil then
        storeNode := CreateStoreNode(doc);
      // Remove any pre-existing store data to be replaced by new data.
      CleanNode(storeNode);
    end else begin
      // If file does not exist then create a new xml document
      doc := TXMLDocument.Create;
      storeNode := CreateStoreNode(doc); //doc.CreateElement(STORE_NODE_NAME);
    end;

    WriteResources(doc, storeNode);
    WriteXMLFile(doc, FFileName);
  finally
    doc.Free;
  end;
end;

end.
