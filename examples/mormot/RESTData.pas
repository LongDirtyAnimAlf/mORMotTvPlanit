unit RESTData;

interface

{$I Synopse.inc}

uses
  Types,
  SynCommons,
  mORMot;

type
  TVpEventRec = packed record
    Rec: TRect;
    IconRect: TRect;
    Event: Pointer;
  end;

  TVpEventArray = array of TVpEventRec;

  TVpAlarmAdvanceType = (atMinutes, atHours, atDays);

  TVpRepeatType = (rtNone, rtDaily, rtWeekly, rtMonthlyByDay, rtMonthlyByDate,
    rtYearlyByDay, rtYearlyByDate, rtCustom);

  TVpContactSort = (csLastFirst, csFirstLast);

  TSQLRecordWithUserFields = class(TSQLRecord)
  //TSQLRecordWithUserFields = class(TSQLRecordTimed)
  protected
    FUserField0: string;
    FUserField1: string;
    FUserField2: string;
    FUserField3: string;
    FUserField4: string;
    FUserField5: string;
    FUserField6: string;
    FUserField7: string;
    FUserField8: string;
    FUserField9: string;
  published
    property UserField0: string read FUserField0 write FUserField0;
    property UserField1: string read FUserField1 write FUserField1;
    property UserField2: string read FUserField2 write FUserField2;
    property UserField3: string read FUserField3 write FUserField3;
    property UserField4: string read FUserField4 write FUserField4;
    property UserField5: string read FUserField5 write FUserField5;
    property UserField6: string read FUserField6 write FUserField6;
    property UserField7: string read FUserField7 write FUserField7;
    property UserField8: string read FUserField8 write FUserField8;
    property UserField9: string read FUserField9 write FUserField9;
  end;


  TSQLVpResource = class(TSQLRecordWithUserFields)
  protected
    fResourceID: TID;
    fDescription: string;
    fNotes: string;
    fImageIndex: integer;
    fResourceActive: boolean;
  public
    class procedure InitializeTable(Server: TSQLRestServer; const FieldName: RawUTF8;
      Options: TSQLInitializeTableOptions); override;
  published
    property ResourceID: TID read fResourceID write fResourceID;
    property Description: string read fDescription write fDescription;
    property Notes: string read fNotes write fNotes;
    property ImageIndex: integer read fImageIndex write fImageIndex;
    property ResourceActive: boolean read fResourceActive write fResourceActive;
  end;

  TSQLRecordWithUserFieldsAndID = class(TSQLRecordWithUserFields)
  protected
    fResourceID:integer;
  published
    property RecordID: TID read fID;
    property ResourceID: integer read fResourceID write fResourceID;
  end;


  TSQLVpEvent = class(TSQLRecordWithUserFieldsAndID)
  protected
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FPrivateEvent: boolean;
    FAlarmSet: boolean;
    FDingPath: string;
    FAllDayEvent: boolean;
    FCategory: integer;
    FAlarmAdvance: integer;
    FAlertDisplayed: boolean;
    FAlarmAdvanceType: TVpAlarmAdvanceType;
    FLocation: string;
    FNotes: string;
    FDescription: string;
    FSnoozeTime: TDateTime;
    FRepeatCode: TVpRepeatType;
    FRepeatRangeEnd: TDateTime;
    FCustomInterval: integer;
  published
    property StartTime: TDateTime read FStartTime write FStartTime;
    property EndTime: TDateTime read FEndTime write FEndTime;
    property Description: string read FDescription write FDescription;
    property Location: string read FLocation write FLocation;
    property Notes: string read FNotes write FNotes;
    property Category: integer read FCategory write FCategory;
    property DingPath: string read FDingPath write FDingPath;
    property AllDayEvent: boolean read FAllDayEvent write FAllDayEvent;
    property AlarmSet: boolean read FAlarmSet write FAlarmSet;
    property AlarmAdvance: integer read FAlarmAdvance write FAlarmAdvance;
    property AlarmAdvanceType: TVpAlarmAdvanceType read FAlarmAdvanceType write FAlarmAdvanceType;
    property SnoozeTime: TDateTime read FSnoozeTime write FSnoozeTime;
    property RepeatCode: TVpRepeatType read FRepeatCode write FRepeatCode;
    property RepeatRangeEnd: TDateTime read FRepeatRangeEnd write FRepeatRangeEnd;
    property CustomInterval: integer read FCustomInterval write FCustomInterval;
  end;

  TVpPhoneType = (ptAssistant, ptCallback, ptCar, ptCompany, ptHome, ptHomeFax,
                    ptISDN, ptMobile, ptOther, ptOtherFax, ptPager, ptPrimary,
                    ptRadio, ptTelex, ptTTYTDD, ptWork, ptWorkFax);

  TVpCategoryType = (ctBusiness, ctClients, ctFamily, ctOther, ctPersonal);

  TSQLVpContact = class(TSQLRecordWithUserFieldsAndID)
  protected
    fFirstName:string;
    fLastName:string;
    fBirthdate:TDateTime;
    fAnniversary:TDateTime;
    fTitle:string;
    fCompany:string;
    fJob_Position:string;
    fEMail:string;
    fAddress:string;
    fCity:string;
    fState:string;
    fZip:string;
    fCountry:string;
    fNotes:string;
    fPhone1:string;
    fPhone2:string;
    fPhone3:string;
    fPhone4:string;
    fPhone5:string;
    fPhoneType1:TVpPhoneType;
    fPhoneType2:TVpPhoneType;
    fPhoneType3:TVpPhoneType;
    fPhoneType4:TVpPhoneType;
    fPhoneType5:TVpPhoneType;
    fCategory:TVpCategoryType;
    fCustom1:string;
    fCustom2:string;
    fCustom3:string;
    fCustom4:string;
  published
    property FirstName:string read fFirstName write fFirstName;
    property LastName:string read fLastName write fLastName;
    property Birthdate:TDateTime read fBirthdate write fBirthdate;
    property Anniversary:TDateTime read fAnniversary write fAnniversary;
    property Title:string read fTitle write fTitle;
    property Company:string read fCompany write fCompany;
    property Job_Position:string read fJob_Position write fJob_Position;
    property EMail:string read fEMail write fEMail;
    property Address:string read fAddress write fAddress;
    property City:string read fCity write fCity;
    property State:string read fState write fState;
    property Zip:string read fZip write fZip;
    property Country:string read fCountry write fCountry;
    property Notes:string read fNotes write fNotes;
    property Phone1:string read fPhone1 write fPhone1;
    property Phone2:string read fPhone2 write fPhone2;
    property Phone3:string read fPhone3 write fPhone3;
    property Phone4:string read fPhone4 write fPhone4;
    property Phone5:string read fPhone5 write fPhone5;
    property PhoneType1:TVpPhoneType read fPhoneType1 write fPhoneType1;
    property PhoneType2:TVpPhoneType read fPhoneType2 write fPhoneType2;
    property PhoneType3:TVpPhoneType read fPhoneType3 write fPhoneType3;
    property PhoneType4:TVpPhoneType read fPhoneType4 write fPhoneType4;
    property PhoneType5:TVpPhoneType read fPhoneType5 write fPhoneType5;
    property Category:TVpCategoryType read fCategory write fCategory;
    property Custom1:string read fCustom1 write fCustom1;
    property Custom2:string read fCustom2 write fCustom2;
    property Custom3:string read fCustom3 write fCustom3;
    property Custom4:string read fCustom4 write fCustom4;
  end;

  TSQLVpTask = class(TSQLRecordWithUserFieldsAndID)
  protected
    fDescription:String;
    fDetails:String;
    fComplete:Boolean;
    fDueDate:TDateTime;
    fCreatedOn:TDateTime;
    fCompletedOn:TDateTime;
    fPriority:Integer;
    fCategory:Integer;
  published
    property Description:String read fDescription write fDescription;
    property Details:String read fDetails write fDetails;
    property Complete:Boolean read fComplete write fComplete;
    property DueDate:TDateTime read fDueDate write fDueDate;
    property CreatedOn:TDateTime read fCreatedOn write fCreatedOn;
    property CompletedOn:TDateTime read fCompletedOn write fCompletedOn;
    property Priority:Integer read fPriority write fPriority;
    property Category:Integer read fCategory write fCategory;
  end;

function DataModel: TSQLModel;

const
  {$ifdef Windows}
  HTTP_PORT = '888';
  {$else}
  HTTP_PORT = '8888';
  {$endif}

  
implementation

function DataModel: TSQLModel;
begin
  result := TSQLModel.Create([
  //{$ifdef USEAUTHENTICATION}
  TSQLAuthGroup,
  TSQLAuthUser,
  //{$endif}
  TSQLVpResource,
  TSQLVpEvent,
  TSQLVpContact,
  TSQLVpTask
  ]);
end;

class procedure TSQLVpResource.InitializeTable(Server: TSQLRestServer; const FieldName: RawUTF8;
      Options: TSQLInitializeTableOptions);
var
  LocalResource:TSQLVpResource;
  aNewID:TID;
begin
  inherited InitializeTable(Server,FieldName,Options);
  if FieldName='' then
  begin
    LocalResource:=TSQLVpResource.Create;
    try
      LocalResource.Description := 'MyFirstResource';
      LocalResource.Notes := 'This is an automatically created resource only when the database was non existent';
      LocalResource.ResourceActive := True;
      aNewID:=Server.Add(LocalResource,true);
      // do we have a new resource ?
      if aNewID>0 then
      begin
        LocalResource.ResourceID:=aNewID;
        Server.Update(LocalResource,'ResourceID',true);
      end;
    finally
      LocalResource.Free;
    end;
  end;
end;


initialization
   {$ifndef ISDELPHI2010}
   {$ifndef HASINTERFACERTTI} // circumvent a old FPC bug
   TTextWriter.RegisterCustomJSONSerializerFromTextSimpleType(TypeInfo(TVpAlarmAdvanceType));
   TTextWriter.RegisterCustomJSONSerializerFromTextSimpleType(TypeInfo(TVpRepeatType));
   TTextWriter.RegisterCustomJSONSerializerFromTextSimpleType(TypeInfo(TVpContactSort));
   TTextWriter.RegisterCustomJSONSerializerFromTextSimpleType(TypeInfo(TVpPhoneType));
   TTextWriter.RegisterCustomJSONSerializerFromTextSimpleType(TypeInfo(TVpCategoryType));
   {$endif}
   {$endif}
end.
