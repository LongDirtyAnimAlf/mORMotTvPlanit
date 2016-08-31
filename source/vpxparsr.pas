{*********************************************************}
{*                   VPXPARSR.PAS 1.03                   *}
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

unit VpXParsr;

interface

uses
  {$IFDEF LCL}
  LCLProc, LCLType,
  {$ELSE}
  Windows,
  {$ENDIF}
  Graphics, Controls, SysUtils, Classes,
  VpConst, VpSR, VpBase, VpXBase, VpXChrFlt;

type
  StringIds = array[0..1] of DOMString;

{== Event types ======================================================}
  TVpDocTypeDeclEvent = procedure(oOwner : TObject;
                                  sDecl,
                                  sId0,
                                  sId1   : DOMString) of object;
  TVpValueEvent = procedure(oOwner : TObject;
                            sValue : DOMString) of object;
  TVpAttributeEvent = procedure(oOwner     : TObject;
                                sName,
                                sValue     : DOMString;
                                bSpecified : Boolean) of object;
  TVpProcessInstrEvent = procedure(oOwner : TObject;
                                   sName,
                                   sValue : DOMString) of object;
  TVpResolveEvent = procedure(oOwner     : TObject;
                        const sName,
                              sPublicId,
                              sSystemId  : DOMString;
                          var sValue     : DOMString) of object;
  TVpNonXMLEntityEvent = procedure(oOwner        : TObject;
                                   sEntityName,
                                   sPublicId,
                                   sSystemId,
                                   sNotationName : DOMString) of object;
  TVpPreserveSpaceEvent = procedure(oOwner       : TObject;
                                    sElementName : DOMString;
                                var bPreserve    : Boolean) of object;
{== Class types ======================================================}
  TVpParser = class(TVpComponent)
  protected
    { Private declarations }
    FAttrEnum: TStringList;
    FAttributeType: TStringList;
    FBufferSize: Integer;
    FCDATA: Boolean;
    FContext: Integer;
    FCurrentElement: DOMString;
    FCurrentElementContent: Integer;
    FCurrentPath: string;
    FDataBuffer: DOMString;
    FDocStack: TList;
    FElementInfo: TStringList;
    FEntityInfo: TStringList;
    FErrors: TStringList;
    FFilter: TVpInCharFilter;
    FInCharSet: TVpCharEncoding;
    FNormalizeData: Boolean;
    FNotationInfo: TStringList;
    FOnAttribute: TVpAttributeEvent;
    FOnCDATASection: TVpValueEvent;
    FOnCharData: TVpValueEvent;
    FOnComment: TVpValueEvent;
    FOnDocTypeDecl: TVpDocTypeDeclEvent;
    FOnEndDocument: TNotifyEvent;
    FOnEndElement: TVpValueEvent;
    FOnIgnorableWhitespace: TVpValueEvent;
    FOnNonXMLEntity: TVpNonXMLEntityEvent;
    FOnPreserveSpace: TVpPreserveSpaceEvent;
    FOnProcessingInstruction: TVpProcessInstrEvent;
    FOnResolveEntity: TVpResolveEvent;
    FOnStartDocument: TNotifyEvent;
    FOnStartElement: TVpValueEvent;
    FOnBeginElement: TVpValueEvent;
    FPreserve: Boolean;
    FRaiseErrors: Boolean;
    FTagAttributes: TStringList;
    FTempFiles: TStringList;
    FUrl: DOMString;
    FIsStandAlone: Boolean;
    FHasExternals: Boolean;
    FXMLDecParsed: Boolean;

    procedure Cleanup;
    procedure CheckParamEntityNesting(const aString: DOMString);
    procedure DataBufferAppend(const sVal: DOMString);
    procedure DataBufferFlush;
    procedure DataBufferNormalize;
    function DataBufferToString: DOMString;
    function DeclaredAttributes(const sName: DOMString; aIdx : Integer): TStringList;
    function GetAttributeDefaultValueType(const sElemName, sAttrName: DOMString): Integer;
    function GetAttributeExpandedValue(const sElemName, sAttrName: DOMString; aIdx: Integer): DOMString;
    function GetElementContentType(const sName: DOMString; aIdx: Integer): Integer;
    function GetElementIndexOf(const sElemName: DOMString): Integer;
    function GetEntityIndexOf(const sEntityName: DOMString; aPEAllowed: Boolean): Integer;
    function GetEntityNotationName(const sEntityName: DOMString): DOMString;
    function GetEntityPublicId(const sEntityName: DOMString): DOMString;
    function GetEntitySystemId(const sEntityName: DOMString): DOMString;
    function GetEntityType(const sEntityName: DOMString; aPEAllowed: Boolean): Integer;
    function GetEntityValue(const sEntityName: DOMString; aPEAllowed: Boolean): DOMString;
    function GetErrorCount: Integer;
    function GetExternalTextEntityValue(const sName, sPublicId: DOMString;
      sSystemId: DOMString): DOMString;
    function GetInCharSet: TVpCharEncoding;
    procedure Initialize;
    function IsEndDocument: Boolean;
    function IsWhitespace(const cVal: DOMChar): Boolean;
    function LoadDataSource(sSrcName: string; oErrors: TStringList): Boolean;
    function ParseAttribute(const sName: DOMString): DOMString;
    function ParseEntityRef(bPEAllowed: Boolean): DOMString;
    procedure ParseCDSect;
    function  ParseCharRef: DOMChar;
    procedure ParseComment;
    procedure ParseContent;
    procedure ParseDocTypeDecl;
    procedure ParseDocument;
    procedure ParseEndTag;
    procedure ParseEq;
    procedure ParseElement;
    procedure ParseMisc;
    function ParseParameterEntityRef(aPEAllowed: Boolean; bSkip: Boolean): DOMString;
    procedure ParsePCData(aInEntityRef: Boolean);
    procedure ParsePI;
    function ParsePIEx : Boolean;
      { Returns true if an XML declaration was found }
    procedure ParsePrim;
    procedure ParseProlog;
    procedure ParseUntil(const S : array of Longint);
    procedure ParseWhitespace;
    procedure ParseXMLDeclaration;
    procedure PopDocument;
    procedure PushDocument;
    procedure PushString(const sVal: DOMString);
    function ReadChar(const UpdatePos: Boolean): DOMChar;
    procedure ReadExternalIds(bInNotation: Boolean; var sIds: StringIds);
    function ReadLiteral(wFlags: Integer; var HasEntRef: Boolean): DOMString;
    function ReadNameToken(aValFirst: Boolean): DOMString;
    procedure Require(const S: array of Longint);
    procedure RequireWhitespace;
    procedure SetAttribute(const sElemName, sName: DOMString; wType: Integer;
      const sEnum, sValue: DOMString; wValueType: Integer);
    procedure SetElement(const sName: DOMString; wType: Integer;
      const sContentModel: DOMString);
    procedure SetEntity(const sEntityName: DOMString; wClass: Integer;
      const sPublicId, sSystemId, sValue, sNotationName: DOMString; aIsPE: Boolean);
    procedure SetInternalEntity(const sName, sValue: DOMString; aIsPE: Boolean);
    procedure SetNotation(const sNotationName, sPublicId, sSystemId: DOMString);
    procedure SkipChar;
    procedure SkipWhitespace(aNextDoc: Boolean);
    function TryRead(const S: array of Longint): Boolean;
    procedure ValidateAttribute(const aValue: DOMString; HasEntRef: Boolean);
    procedure ValidateCData(const CDATA: DOMString);
    procedure ValidateElementName(const aName: DOMString);
    procedure ValidateEncName(const aValue: string);
    procedure ValidateEntityValue(const aValue: DOMString; aQuoteCh: DOMChar);
    function ValidateNameChar(const First: Boolean; const Char: DOMChar): Boolean;
    procedure ValidatePCData(const aString: DOMString; aInEntityRef: Boolean);
    procedure ValidatePublicID(const aString: DOMString);
    procedure ValidateVersNum(const aString: string);

  protected
    { Protected declarations }
    property OnIgnorableWhitespace: TVpValueEvent
      read FOnIgnorableWhitespace
      write FOnIgnorableWhitespace;

  public
    { Public declarations }
    constructor Create(oOwner : TComponent); override;
    destructor Destroy; override;

    function GetErrorMsg(wIdx : Integer) : DOMString;
    function ParseDataSource(const sSource : string) : Boolean;

    property ErrorCount : Integer
      read GetErrorCount;

    property Errors : TStringList
      read FErrors;

    property InCharSet : TVpCharEncoding
      read GetInCharSet;

    property IsStandAlone : Boolean
      read FIsStandAlone;

    property HasExternals : Boolean
      read FHasExternals;

    { Published declarations }
    property BufferSize : Integer
      read FBufferSize
      write FBufferSize
      default 8192;

    property NormalizeData : Boolean
      read FNormalizeData
      write FNormalizeData
      default True;

    property RaiseErrors : Boolean
      read FRaiseErrors
      write FRaiseErrors
      default False;

    property OnAttribute : TVpAttributeEvent
      read FOnAttribute
      write FOnAttribute;

    property OnCDATASection : TVpValueEvent
      read FOnCDATASection
      write FOnCDATASection;

    property OnCharData : TVpValueEvent
      read FOnCharData
      write FOnCharData;

    property OnComment : TVpValueEvent
      read FOnComment
      write FOnComment;

    property OnDocTypeDecl : TVpDocTypeDeclEvent
      read FOnDocTypeDecl
      write FOnDocTypeDecl;


    property OnEndDocument : TNotifyEvent
      read FOnEndDocument
      write FOnEndDocument;

    property OnEndElement : TVpValueEvent
      read FOnEndElement
      write FOnEndElement;

    property OnNonXMLEntity : TVpNonXMLEntityEvent
      read FOnNonXMLEntity
      write FOnNonXMLEntity;

    property OnPreserveSpace : TVpPreserveSpaceEvent
      read FOnPreserveSpace
      write FOnPreserveSpace;

    property OnProcessingInstruction : TVpProcessInstrEvent
      read FOnProcessingInstruction
      write FOnProcessingInstruction;

    property OnResolveEntity : TVpResolveEvent
      read FOnResolveEntity
      write FOnResolveEntity;

    property OnStartDocument : TNotifyEvent
      read FOnStartDocument
      write FOnStartDocument;

    property OnStartElement : TVpValueEvent
      read FOnStartElement
      write FOnStartElement;

    property OnBeginElement : TVpValueEvent
      read FOnBeginElement
      write FOnBeginElement;
  end;

implementation

{.$R *.RES}

uses
 {$IFDEF FPC}
  LazUtf8,
 {$ENDIF}
  VpMisc;


{== TVpEntityInfo ====================================================}
type
  TVpEntityInfo = class(TObject)
  private
    FEntityClass  : Integer;
    FIsPE         : Boolean;
    FPublicId     : DOMString;
    FSystemId     : DOMString;
    FValue        : DOMString;
    FNotationName : DOMString;
  public
    property EntityClass : Integer
      read FEntityClass
      write FEntityClass;

    property IsParameterEntity : Boolean
      read FIsPE
      write FIsPE;

    property NotationName : DOMString
      read FNotationName
      write FNotationName;

    property PublicId : DOMString
      read FPublicId
      write FPublicId;

    property SystemId : DOMString
      read FSystemId
      write FSystemId;

    property Value : DOMString
      read FValue
      write FValue;
  end;
{== TVpNotationInfo ==================================================}
  TVpNotationInfo = class(TObject)
  private
    FPublicId: DOMString;
    FSystemId: DOMString;
  public
    property PublicId: DOMString read FPublicId write FPublicId;
    property SystemId: DOMString read FSystemId write FSystemId;
  end;

{== TVpAttributeInfo =================================================}
  TVpAttributeInfo = class(TObject)
  private
    FType: Integer;
    FValue: DOMString;
    FValueType: Integer;
    FEnum: DOMString;
    FLookup: DOMString;
  public
    property AttrType: Integer read FType write FType;
    property Enum: DOMString read FEnum write FEnum;
    property Lookup: DOMString read FLookup write FLookup;
    property Value: DOMString read FValue write FValue;
    property ValueType: Integer read FValueType write FValueType;
  end;

{== TVpElementInfo ===================================================}
  TVpElementInfo = class(TObject)
  private
    FAttributeList: TStringList;
    FContentType: Integer;
    FContentModel: DOMString;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetAttribute(const sName: DOMString; oAttrInfo: TVpAttributeInfo);

    property AttributeList: TStringList read FAttributeList;
    property ContentModel: DOMString read FContentModel write FContentModel;
    property ContentType: Integer read FContentType write FContentType;
  end;

{=== TVpElementInfo ==================================================}
constructor TVpElementInfo.Create;
begin
  inherited Create;
  FAttributeList := nil;
  FContentModel := '';
  FContentType := 0;
end;
{--------}
destructor TVpElementInfo.Destroy;
var
  i : Integer;
begin
  if FAttributeList <> nil then begin
    for i := 0 to FAttributeList.Count - 1 do
      TVpAttributeInfo(FAttributeList.Objects[i]).Free;
    FAttributeList.Free;
  end;
  inherited Destroy;
end;
{--------}
procedure TVpElementInfo.SetAttribute(const sName: DOMString;
  oAttrInfo: TVpAttributeInfo);
var
  wIdx: Integer;
begin
  if FAttributeList = nil then begin
    FAttributeList := TStringList.Create;
    FAttributeList.Sorted := True;
    wIdx := -1
  end else
   {$IFDEF DELPHI}
    wIdx := FAttributeList.IndexOf(sName);
   {$ELSE}
    wIdx := FAttributeList.IndexOf(UTF8Encode(sName));
   {$ENDIF}

  if wIdx < 0 then
   {$IFDEF DELPHI}
    FAttributeList.AddObject(sName, oAttrInfo)
   {$ELSE}
    FAttributeList.AddObject(UTF8Encode(sName), oAttrInfo)
   {$ENDIF}
  else begin
    TVpAttributeInfo(FAttributeList.Objects[wIdx]).Free;
    FAttributeList.Objects[wIdx] := oAttrInfo;
  end;
end;

{=== TVpParser =======================================================}
constructor TVpParser.Create(oOwner : TComponent);
begin
  inherited Create(oOwner);

  FErrors := TStringList.Create;
  FAttributeType := TStringList.Create;
  FAttributeType.AddObject('CDATA', Pointer(ATTRIBUTE_CDATA));
  FAttributeType.AddObject('ID', Pointer(ATTRIBUTE_ID));
  FAttributeType.AddObject('IDREF', Pointer(ATTRIBUTE_IDREF));
  FAttributeType.AddObject('IDREFS', Pointer(ATTRIBUTE_IDREFS));
  FAttributeType.AddObject('ENTITY', Pointer(ATTRIBUTE_ENTITY));
  FAttributeType.AddObject('ENTITIES', Pointer(ATTRIBUTE_ENTITIES));
  FAttributeType.AddObject('NMTOKEN', Pointer(ATTRIBUTE_NMTOKEN));
  FAttributeType.AddObject('NMTOKENS', Pointer(ATTRIBUTE_NMTOKENS));
  FAttributeType.AddObject('NOTATION', Pointer(ATTRIBUTE_NOTATION));
  FElementInfo := TStringList.Create;
  FElementInfo.Sorted := True;
  FEntityInfo := TStringList.Create;
  FInCharSet := ceUnknown;
  FNotationInfo := TStringList.Create;
  FNotationInfo.Sorted := true;
  FNotationInfo.Duplicates := dupIgnore;
  FTagAttributes := TStringList.Create;
  FAttrEnum := TStringList.Create;
  FDocStack := TList.Create;
  FNormalizeData := True;
  FCDATA := False;
  FPreserve := False;
  FUrl := '';
  FRaiseErrors := False;
  FFilter := nil;
  FBufferSize := 8192;
  FCurrentPath := '';
  FTempFiles := TStringList.Create;
  FIsStandAlone := False;
  FHasExternals := False;
  FXMLDecParsed := False;
end;
{--------}
destructor TVpParser.Destroy;
var
  TempFilter : TVpInCharFilter;
  i          : Integer;
begin
  Cleanup;
  FTagAttributes.Free;
  FNotationInfo.Free;
  FEntityInfo.Free;
  FElementInfo.Free;
  FAttributeType.Free;
  FErrors.Free;
  if Assigned(FTempFiles) then begin
    for i := 0 to Pred(FTempFiles.Count) do
      DeleteFile(FTempFiles[i]);
    FTempFiles.Free;
  end;
  FAttrEnum.Free;
  if FDocStack.Count > 0 then begin
    for i := Pred(FDocStack.Count) to 0 do begin
      TempFilter := FDocStack[i];
      TempFilter.Free;
      FDocStack.Delete(i);
    end;
  end;
  FDocStack.Free;
  inherited Destroy;
end;
{--------}
procedure TVpParser.CheckParamEntityNesting(const aString : DOMString);
var
  OpenPos: Integer;
  ClosePos: Integer;
  errMsg: DOMString;
begin
  OpenPos := VpPos('(', aString);
  ClosePos := VpPos(')', aString);
  if ((OpenPos <> 0) and (ClosePos = 0)) or
     ((ClosePos <> 0) and (OpenPos = 0)) then
  begin
   {$IFDEF DELPHI}
    errMsg := sBadParamEntNesting + aString;
   {$ELSE}
    errMsg := UTF8Decode(sBadParamEntNesting) + aString;
   {$ENDIF}
    raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, errMsg);
   end;
end;
{--------}
procedure TVpParser.Cleanup;
var
  i : Integer;
begin
  if FElementInfo <> nil then begin
    for i := 0 to FElementInfo.Count - 1 do
      TVpElementInfo(FElementInfo.Objects[i]).Free;
    FElementInfo.Clear;
  end;

  if FEntityInfo <> nil then begin
    for i := 0 to FEntityInfo.Count - 1 do
      TVpEntityInfo(FEntityInfo.Objects[i]).Free;
    FEntityInfo.Clear;
  end;

  if FNotationInfo <> nil then begin
    for i := 0 to FNotationInfo.Count - 1 do
      TVpNotationInfo(FNotationInfo.Objects[i]).Free;
    FNotationInfo.Clear;
  end;
end;
{--------}
procedure TVpParser.DataBufferAppend(const sVal : DOMString);
begin
  FDataBuffer := FDataBuffer + sVal;
end;
{--------}
procedure TVpParser.DataBufferFlush;
begin
  if FNormalizeData and
     not FCDATA and
     not FPreserve then
    DataBufferNormalize;
  if FDataBuffer <> '' then begin
    case FCurrentElementContent of
      CONTENT_MIXED, CONTENT_ANY :
        if FCDATA then begin
          ValidateCData(FDataBuffer);
          if Assigned(FOnCDATASection) then
            FOnCDATASection(self, FDataBuffer);
        end else begin
          if Assigned(FOnCharData) then
            FOnCharData(self, FDataBuffer);
        end;
      CONTENT_ELEMENTS :
        if Assigned(FOnIgnorableWhitespace) then
          FOnIgnorableWhitespace(self, FDataBuffer);
    end;
    FDataBuffer := '';
  end;
end;
{--------}
procedure TVpParser.DataBufferNormalize;
var
  BuffLen     : Integer;
  j           : Integer;
  CharDeleted : Boolean;
begin
  while (Length(FDataBuffer) > 0) and
        IsWhiteSpace(FDataBuffer[1]) do
    Delete(FDataBuffer, 1, 1);
  while (Length(FDataBuffer) > 0) and
        IsWhiteSpace(FDataBuffer[Length(FDataBuffer)]) do
    Delete(FDataBuffer, Length(FDataBuffer), 1);

  j := 1;
  BuffLen := Length(FDataBuffer);
  CharDeleted := False;
  while j < BuffLen do begin
    if IsWhiteSpace(FDataBuffer[j]) then begin
      { Force whitespace to a single space }
      FDataBuffer[j] := ' ';

      { Remove additional whitespace }
      j := j + 1;
      while (j <= Length(FDataBuffer)) and
            IsWhiteSpace(FDataBuffer[j]) do begin
        Delete(FDataBuffer, j, 1);
        CharDeleted := True;
      end;
      if (CharDeleted) then begin
        BuffLen := Length(FDataBuffer);
        CharDeleted := False;
      end;
    end;
    j := j + 1;
  end;
end;
{--------}
function TVpParser.DataBufferToString : DOMString;
begin
  Result := FDataBuffer;
  FDataBuffer := '';
end;
{--------}
function TVpParser.GetErrorCount : Integer;
begin
  Result := FErrors.Count;
end;
{--------}
function TVpParser.GetErrorMsg(wIdx: Integer): DOMString;
begin
 {$IFDEF DELPHI}
  Result := sIndexOutOfBounds;
  if (wIdx >= 0) and (wIdx < FErrors.Count) then
    Result := FErrors[wIdx];
 {$ELSE}
  Result := UTF8Decode(sIndexOutOfBounds);
  if (wIdx >= 0) and (wIdx < FErrors.Count) then
    Result := UTF8Decode(FErrors[wIdx]);
 {$ENDIF}
end;
{--------}
function TVpParser.DeclaredAttributes(const sName: DOMString;
  aIdx: Integer): TStringList;
begin
  Unused(sName);
  if aIdx < 0 then
    Result := nil
  else
    Result := TVpElementInfo(FElementInfo.Objects[aIdx]).AttributeList;
end;
{--------}
function TVpParser.GetAttributeDefaultValueType(
  const sElemName, sAttrName: DOMString): Integer;
var
  wIdx: Integer;
  oAttrList: TStringList;
  oAttr: TVpAttributeInfo;
begin
  Result := ATTRIBUTE_DEFAULT_UNDECLARED;
  wIdx := GetElementIndexOf(sElemName);
  if wIdx >= 0 then begin
    oAttrList := TVpElementInfo(FElementInfo.Objects[wIdx]).AttributeList;
    if oAttrList <> nil then begin
     {$IFDEF DELPHI}
      wIdx := oAttrList.IndexOf(sAttrName);
     {$ELSE}
      wIdx := oAttrList.IndexOf(UTF8Encode(sAttrName));
     {$ENDIF}
      if wIdx >= 0 then begin
        oAttr := TVpAttributeInfo(oAttrList.Objects[wIdx]);
        Result := oAttr.AttrType;
      end;
    end;
  end;
end;
{--------}
function TVpParser.GetAttributeExpandedValue(const sElemName, sAttrName: DOMString;
  aIdx: Integer): DOMString;
var
  wIdx: Integer;
  oAttrList: TStringList;
  oAttr: TVpAttributeInfo;
  HasEntRef: Boolean;
begin
  Unused(sElemName);

  SetLength(Result, 0);
  HasEntRef := False;
  if aIdx >= 0 then begin
    oAttrList := TVpElementInfo(FElementInfo.Objects[aIdx]).AttributeList;
    if oAttrList <> nil then begin
     {$IFDEF DELPHI}
      wIdx := oAttrList.IndexOf(sAttrName);
     {$ELSE}
      wIdx := oAttrList.IndexOf(UTF8Encode(sAttrName));
     {$ENDIF}
      if wIdx >= 0 then begin
        oAttr := TVpAttributeInfo(oAttrList.Objects[wIdx]);
        if (oAttr.Lookup = '') and (oAttr.Value <> '') then
        begin
          PushString('"' + oAttr.Value + '"');
          oAttr.Lookup := ReadLiteral(LIT_NORMALIZE or LIT_CHAR_REF or LIT_ENTITY_REF, HasEntRef);
          SkipWhitespace(True);
        end;
        Result := oAttr.Lookup;
      end;
    end;
  end;
end;
{--------}
function TVpParser.GetElementContentType(const sName: DOMString;
  aIdx: Integer): Integer;
begin
  Unused(sName);
  if aIdx < 0 then
    Result := CONTENT_UNDECLARED
  else
    Result := TVpElementInfo(FElementInfo.Objects[aIdx]).ContentType;
end;
{--------}
function TVpParser.GetElementIndexOf(const sElemName: DOMString): Integer;
begin
 {$IFDEF DELPHI}
  Result := FElementInfo.IndexOf(sElemName);
 {$ELSE}
  Result := FElementInfo.IndexOf(UTF8Encode(sElemName));
 {$ENDIF}
end;
{--------}
function TVpParser.GetEntityIndexOf(const sEntityName: DOMString;
  aPEAllowed: Boolean): Integer;
begin
  for Result := 0 to FEntityInfo.Count - 1 do
    if FEntityInfo[Result] = {$IFDEF DELPHI}sEntityName{$ELSE}UTF8Encode(sEntityName){$ENDIF}
    then begin
      if (not aPEAllowed) then begin
        if (not TVpEntityInfo(FEntityInfo.Objects[Result]).IsParameterEntity) then
          Exit;
      end else
        Exit;
    end;
  Result := -1;
end;
{--------}
function TVpParser.GetEntityNotationName(const sEntityName : DOMString)
                                                           : DOMString;
var
  wIdx    : Integer;
  oEntity : TVpEntityInfo;
begin
  Result := '';
  wIdx := GetEntityIndexOf(sEntityName, False);
  if wIdx >= 0 then begin
    oEntity := TVpEntityInfo(FEntityInfo.Objects[wIdx]);
    Result := oEntity.NotationName;
  end;
end;
{--------}
function TVpParser.GetEntityPublicId(const sEntityName : DOMString)
                                                       : DOMString;
var
  wIdx    : Integer;
  oEntity : TVpEntityInfo;
begin
  Result := '';
  wIdx := GetEntityIndexOf(sEntityName, False);
  if wIdx >= 0 then begin
    oEntity := TVpEntityInfo(FEntityInfo.Objects[wIdx]);
    Result := oEntity.PublicId;
  end;
end;
{--------}
function TVpParser.GetEntitySystemId(const sEntityName : DOMString)
                                                       : DOMString;
var
  wIdx    : Integer;
  oEntity : TVpEntityInfo;
begin
  Result := '';
  wIdx := GetEntityIndexOf(sEntityName, False);
  if wIdx >= 0 then begin
    oEntity := TVpEntityInfo(FEntityInfo.Objects[wIdx]);
    Result := oEntity.SystemId;
  end;
end;
{--------}
function TVpParser.GetEntityType(const sEntityName : DOMString;
                                       aPEAllowed  : Boolean)
                                                   : Integer;
var
  wIdx    : Integer;
  oEntity : TVpEntityInfo;
begin
  Result := ENTITY_UNDECLARED;
  wIdx := GetEntityIndexOf(sEntityName, aPEAllowed);
  if wIdx >= 0 then begin
    oEntity := TVpEntityInfo(FEntityInfo.Objects[wIdx]);
    Result := oEntity.EntityClass;
  end;
end;
{--------}
function TVpParser.GetEntityValue(const sEntityName : DOMString;
                                        aPEAllowed  : Boolean)
                                                    : DOMString;
var
  wIdx    : Integer;
  oEntity : TVpEntityInfo;
begin
  Result := '';
  wIdx := GetEntityIndexOf(sEntityName, aPEAllowed);
  if wIdx >= 0 then begin
    oEntity := TVpEntityInfo(FEntityInfo.Objects[wIdx]);
    Result := oEntity.Value;
  end;
end;
{--------}
function TVpParser.GetExternalTextEntityValue(const sName, sPublicId: DOMString;
  sSystemId: DOMString): DOMString;
var
  CompletePath: string;
begin
  DataBufferFlush;
  Result := '';

  FHasExternals := True;

  if Assigned(FOnResolveEntity) then
    FOnResolveEntity(self, sName, sPublicId, sSystemId, sSystemId);

  if sSystemId = '' then
    exit;

  PushDocument;
  {$IFDEF DELPHI}
   CompletePath := sSystemID;
  {$ELSE}
   CompletePath := UTF8Encode(sSystemID);
  {$ENDIF}
  if (VpPos('/', sSystemID) = 0) and (VpPos('\', sSystemID) = 0) then
    CompletePath := FCurrentPath + CompletePath;

  {TODO:: Need to check return value of LoadDataSource? }
  try
    LoadDataSource(CompletePath, FErrors);
  except
    PopDocument;
    raise;
  end;
end;
{--------}
function TVpParser.GetInCharSet : TVpCharEncoding;
begin
  if FFilter <> nil then
    Result := ceUTF8
  else
    { If no current filter then return last known value. }
    Result := FInCharSet;
end;
{--------}
procedure TVpParser.Initialize;
begin
  FDataBuffer := '';

  SetInternalEntity('amp', '&#38;', False);
  SetInternalEntity('lt', '&#60;', False);
  SetInternalEntity('gt', '&#62;', False);
  SetInternalEntity('apos', '&#39;', False);
  SetInternalEntity('quot', '&#34;', False);
end;
{--------}
function TVpParser.IsEndDocument : Boolean;
var
  TheStream : TStream;
  DocCount  : Integer;
begin
  DocCount := FDocStack.Count;
  if (DocCount = 0) then
    Result := FFilter.Eof
  else begin
    Result := False;
    while FFilter.EOF do begin
      if (DocCount > 0) then begin
        TheStream := FFilter.Stream;
        FFilter.Free;
        TheStream.Free;
      end;
      PopDocument;
      DocCount := FDocStack.Count;
    end;
  end;
end;
{--------}
function TVpParser.IsWhitespace(const cVal : DOMChar) : Boolean;
begin
  Result := (cVal = #$20) or (cVal = #$09) or
            (cVal = #$0D) or (cVal = #$0A);
end;
{--------}
function TVpParser.LoadDataSource(sSrcName: string; oErrors: TStringList): Boolean;
var
  aFileStream: TVpFileStream;
begin
  begin
    { Must be a local or network file. Eliminate file:// prefix. }
    if StrLIComp(PChar(sSrcName), 'file://', 7) = 0 then
      Delete(sSrcName, 1, 7);

    if FileExists(sSrcName) then begin
      FCurrentPath := ExtractFilePath(sSrcName);
      {the stream and filter are destroyed after the document is parsed}
      aFileStream := TVpFileStream.CreateEx(fmOpenRead, sSrcName);
      aFileStream.Position := 0;
      Result := True;
    end else begin
      oErrors.Add(format(sFileNotFound, [sSrcName]));
      raise EVpParserError.CreateError (0,
                                        0,
                                        format(sFileNotFound, [sSrcName]));
    end;
  end;

  if Result then
    try
      aFileStream.Position := 0;
      FFilter := TVpInCharFilter.Create(aFileStream, FBufferSize);
    except
      aFileStream.Free;
      raise;
    end;
end;
{--------}
function TVpParser.ParseAttribute(const sName: DOMString): DOMString;
var
  sAttrName, sValue: DOMString;
  wType: Integer;
  HasEntRef: Boolean;
begin
  Result := '';
  HasEntRef := False;
  sAttrName := ReadNameToken(True);
  wType := GetAttributeDefaultValueType(sName, sAttrName);

  ParseEq;

  {we need to validate production 10 - 1st letter in quotes}

  if (wType = ATTRIBUTE_CDATA) or (wType = ATTRIBUTE_UNDECLARED) then
    sValue := ReadLiteral(LIT_CHAR_REF or LIT_ENTITY_REF, HasEntRef)
  else
    sValue := ReadLiteral(LIT_CHAR_REF or LIT_ENTITY_REF or LIT_NORMALIZE, HasEntRef);
  if not HasEntRef then
    ValidateAttribute(sValue, HasEntRef);

  if Assigned(FOnAttribute) then
    FOnAttribute(self, sAttrName, sValue, True);
  FDataBuffer := '';

 {$IFDEF DELPHI}
  FTagAttributes.Add(sAttrName);
 {$ELSE}
  FTagAttributes.Add(UTF8Encode(sAttrName));
 {$ENDIF}

  if sAttrName = 'xml:space' then
    Result := sValue;
end;
{--------}
procedure TVpParser.ParseCDSect;
{conditional section}
begin
  ParseUntil(Xpc_ConditionalEnd);
end;
{--------}
function TVpParser.ParseCharRef : DOMChar;
var
  TempChar: DOMChar;
  Ucs4Chr: TVpUcs4Char;
  msg: DOMString;
begin
  Ucs4Chr := 0;
  if TryRead(Xpc_CharacterRefHex) then begin
    Ucs4Chr := 0;
    while True do begin
      TempChar := ReadChar(True);
      if (TempChar = '0') or (TempChar = '1') or (TempChar = '2') or
         (TempChar = '3') or (TempChar = '4') or (TempChar = '5') or
         (TempChar = '6') or (TempChar = '7') or (TempChar = '8') or
         (TempChar = '9') or (TempChar = 'A') or (TempChar = 'B') or
         (TempChar = 'C') or (TempChar = 'D') or (TempChar = 'E') or
         (TempChar = 'F') or (TempChar = 'a') or (TempChar = 'b') or
         (TempChar = 'c') or (TempChar = 'd') or (TempChar = 'e') or
         (TempChar = 'f') then
      begin
        Ucs4Chr := Ucs4Chr shl 4;
       {$IFDEF DELPHI}
        Ucs4Chr := Ucs4Chr + StrToIntDef(TempChar, 0);
       {$ELSE}
        Ucs4Chr := Ucs4Chr + StrToIntDef(UTF16ToUTF8(TempChar), 0);
       {$ENDIF}
      end else
      if (TempChar = ';') then
        Break
      else begin
       {$IFDEF DELPHI}
        msg := sIllCharInRef + QuotedStr(TempChar);
       {$ELSE}
        msg := UTF8Decode(sIllCharInRef + QuotedStr(UTF16ToUTF8(TempChar)));
       {$ENDIF}
        raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, msg);
      end;
    end;
  end else begin
    while True do begin
      TempChar := ReadChar(True);
      if (TempChar = '0') or (TempChar = '1') or (TempChar = '2') or
         (TempChar = '3') or (TempChar = '4') or (TempChar = '5') or
         (TempChar = '6') or (TempChar = '7') or (TempChar = '8') or
         (TempChar = '9') then
      begin
        Ucs4Chr := Ucs4Chr * 10;
       {$IFDEF DELPHI}
        Ucs4Chr := Ucs4Chr + StrToIntDef(TempChar, 0);
       {$ELSE}
        Ucs4Chr := Ucs4Chr + StrToIntDef(UTF16ToUTF8(TempChar), 0);
       {$ENDIF}
      end else
      if (TempChar = ';') then
        Break
      else begin
       {$IFDEF DELPHI}
        msg := sIllCharInRef + QuotedStr(TempChar);
       {$ELSE}
        msg := UTF8Decode(sIllCharInRef + QuotedStr(UTF16ToUTF8(TempChar)));
       {$ENDIF}
        raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, msg);
      end;
    end;
  end;
  VpUcs4ToWideChar(Ucs4Chr, Result);
  DataBufferAppend(Result);
end;
{--------}
procedure TVpParser.ParseComment;
var
  TempComment : DOMString;
begin
  ParseUntil(Xpc_CommentEnd);
  TempComment := DataBufferToString;
  { Did we find '--' within the comment? }
  if (TempComment <> '') and
     ((VpPos('--', TempComment) <> 0) or
      (TempComment[Length(TempComment)] = '-')) then
    { Yes. Raise an error. }
    raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, sInvalidCommentText);
  if Assigned(FOnComment) then
    FOnComment(self, TempComment);
end;
{--------}
procedure TVpParser.ParseContent;
var
  TempChar: DOMChar;
  TempStr: DOMString;
  EntRefs: TStringList;
  OldLine: Integer;
  OldPos: Integer;
  TempInt: Integer;
  StackLevel: Integer;
  LastCharAmp: Boolean;
  msg: DOMString;
begin
  LastCharAmp := False;
  StackLevel := 0;
  TempChar := #0;
  EntRefs := nil;
  while True do begin
    OldLine := FFilter.Line;
    OldPos := FFilter.LinePos;
    case FCurrentElementContent of
      CONTENT_ANY, CONTENT_MIXED :
        begin
          if Assigned(EntRefs) then begin
            if (FDataBuffer <> '&') or (LastCharAmp) then begin
              ParsePCData(True);
              LastCharAmp := False;
            end;
            { Reset the last ent ref if we parsed something.}
            if (FFilter.Line <> OldLine) and (FFilter.LinePos <> OldPos) then
            begin
              EntRefs.Free;
              EntRefs := nil;
            end;
          end else
            ParsePCData(TempChar <> '');
        end;
      CONTENT_ELEMENTS:
        ParseWhitespace;
    end;
    TempChar := ReadChar(False);
    if IsEndDocument then
      raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, sUnexpectedEof);
    if (TempChar = '&') then begin
      SkipChar;
      TempChar := ReadChar(False);
      if TempChar = '#' then begin
        SkipChar;
        TempChar := ParseCharRef;
        if TempChar = '&' then
          LastCharAmp := True;
        if (FCurrentElementContent <> CONTENT_ANY) and (FCurrentElementContent <> CONTENT_MIXED) then
          PushString(TempChar);
      end else begin
        if (not Assigned(EntRefs)) then begin
          StackLevel := Succ(FDocStack.Count);
          EntRefs := TStringList.Create;
          TempStr := ParseEntityRef(False);
        end else begin
          {Check for circular references}
          TempStr := ParseEntityRef(False);
          StackLevel := FDocStack.Count;
         {$IFDEF DELPHI}
          TempInt := EntRefs.IndexOf(TempStr);
         {$ELSE}
          TempInt := EntRefs.IndexOf(UTF8Encode(TempStr));
         {$ENDIF}
          if TempInt <> -1 then begin
           {$IFDEF DELPHI}
            msg := sCircularEntRef + TempStr;
           {$ELSE}
            msg := UTF8Decode(sCircularEntRef) + TempStr;
           {$ENDIF}
            raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, msg);
          end;
        end;
       {$IFDEF DELPHI}
        EntRefs.Add(TempStr);
       {$ELSE}
        EntRefs.Add(UTF8Encode(TempStr));
       {$ENDIF}
      end;
      if (FCurrentElementContent <> CONTENT_ANY) and
         (FCurrentElementContent <> CONTENT_MIXED) and
         (TempChar = '<') then
      begin
        DataBufferFlush;
        ParseElement;
      end else
        TempChar := ReadChar(False);
    end else if (TempChar = '<') then begin
      EntRefs.Free;
      EntRefs := nil;
      SkipChar;
      TempChar := ReadChar(False);
      if (TempChar = '!') then begin
        SkipChar;
        DataBufferFlush;
        TempChar := ReadChar(True);
        if (TempChar = '-') then begin
          Require(Xpc_Dash);
          ParseComment;
        end else if (TempChar = '[') then begin
          Require(Xpc_CDATAStart);
          FCDATA := True;
          ParseCDSect;
          ValidateCData(FDataBuffer);
          DataBufferFlush;
          FCDATA := False;
        end else begin
         {$IFDEF DELPHI}
          msg := sExpCommentOrCDATA + '(' + TempChar + ')';
         {$ELSE}
          msg := UTF8Decode(sExpCommentOrCDATA) + '(' + TempChar + ')';
         {$ENDIF}
          raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, msg);
        end;
      end else if (TempChar = '?') then begin
        EntRefs.Free;
        EntRefs := nil;
        SkipChar;
        DataBufferFlush;
        ParsePI;
      end else if (TempChar = '/') then begin
        SkipChar;
        DataBufferFlush;
        ParseEndTag;
        Exit;
      end else begin
        EntRefs.Free;
        EntRefs := nil;
        DataBufferFlush;
        ParseElement;
      end;
    end; {if..else}
    if (Assigned(EntRefs)) and
       (FDocStack.Count < StackLevel) then begin
      EntRefs.Clear;
      StackLevel := FDocStack.Count;
    end;
  end;
  EntRefs.Free;
end;
{--------}
function TVpParser.ParseDataSource(const sSource : string) : Boolean;
begin
  FErrors.Clear;
  FIsStandAlone := False;
  FHasExternals := False;
 {$IFDEF DELPHI}
  FUrl := sSource;
 {$ELSE}
  FUrl := UTF8Decode(sSource);
 {$ENDIF}
  Result := LoadDataSource(sSource, FErrors);
  if Result then begin
    FFilter.FreeStream := True;
    ParsePrim;
  end
  else
    FErrors.Add(sSrcLoadFailed + sSource);
  FUrl := '';
  Result := FErrors.Count = 0;
end;
{--------}
procedure TVpParser.ParseDocTypeDecl;
var
  sDocTypeName : DOMString;
  sIds         : StringIds;
begin
  RequireWhitespace;
  sDocTypeName := ReadNameToken(True);
  SkipWhitespace(True);
  ReadExternalIds(False, sIds);
  SkipWhitespace(True);

  // Parse external DTD
  if sIds[1] <> '' then begin
  end;

  if sIds[1] <> '' then begin
    while True do begin
      FContext := CONTEXT_DTD;
      SkipWhitespace(True);
      FContext := CONTEXT_NONE;
      if TryRead(Xpc_BracketAngleRight) then
        Break
      else begin
        FContext := CONTEXT_DTD;
        FContext := CONTEXT_NONE;
      end;
    end;
  end else begin
    SkipWhitespace(True);
    Require(Xpc_BracketAngleRight);
  end;

  if Assigned(FOnDocTypeDecl) then
    FOnDocTypeDecl(self, sDocTypeName, sIds[0], sIds[1]);
end;
{--------}
procedure TVpParser.ParseDocument;
begin
  FXMLDecParsed := False;
  ParseProlog;
  Require(Xpc_BracketAngleLeft);
  ParseElement;
  try
    ParseMisc;
  except
  end;
  SkipWhiteSpace(True);
  if (not IsEndDocument) then
    raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, sDataAfterValDoc);

  if Assigned(FOnEndDocument) then
    FOnEndDocument(self);
end;
{--------}
procedure TVpParser.ParseElement;
var
  wOldElementContent,
  i                  : Integer;
  sOldElement        : DOMString;
  sGi, sTmp, sTmp2   : DOMString;
  oTmpAttrs          : TStringList;
  bOldPreserve       : Boolean;
  TempChar           : DOMChar;
  aList              : TStringList;
  ElemIdx            : Integer;
begin
  wOldElementContent := FCurrentElementContent;
  sOldElement := FCurrentElement;
  bOldPreserve := FPreserve;

  FTagAttributes.Clear;
  sGi := ReadNameToken(True);

  ValidateElementName(sGi);

  if Assigned(FOnBeginElement) then
    FOnBeginElement(self, sGi);

  FCurrentElement := sGi;
  ElemIdx := GetElementIndexOf(sGi);
  FCurrentElementContent := GetElementContentType(sGi, ElemIdx);
  if FCurrentElementContent = CONTENT_UNDECLARED then
    FCurrentElementContent := CONTENT_ANY;

  SkipWhitespace(True);
  sTmp := '';
  TempChar := ReadChar(False);
  while (TempChar <> '/') and
        (TempChar <> '>') do begin
    sTmp2 := ParseAttribute(sGi);
    if sTmp2 <> '' then
      sTmp := sTmp2;
    SkipWhitespace(True);          
    TempChar := ReadChar(False);
    { check for duplicate attributes }
    if FTagAttributes.Count > 1 then begin
      aList := TStringList.Create;
      try
        aList.Sorted := True;
        aList.Duplicates := dupIgnore;
        aList.Assign(FTagAttributes);
        if (aList.Count <> FTagAttributes.Count) then
          raise EVpParserError.CreateError (FFilter.Line,
                                             FFilter.LinePos,
                                             sRedefinedAttr);
      finally
        aList.Free;
      end;
    end;
  end;

  oTmpAttrs := DeclaredAttributes(sGi, ElemIdx);
  if oTmpAttrs <> nil then begin
    for i := 0 to oTmpAttrs.Count - 1 do begin
      if FTagAttributes.IndexOf(oTmpAttrs[i]) <> - 1 then
        Continue;

      if Assigned(FOnAttribute) then begin
       {$IFDEF DELPHI}
        sTmp2 := GetAttributeExpandedValue(sGi, oTmpAttrs[i], ElemIdx);
        if sTmp2 <> '' then
          FOnAttribute(self, oTmpAttrs[i], sTmp2, False);
       {$ELSE}
        sTmp2 := GetAttributeExpandedValue(sGi, UTF8Decode(oTmpAttrs[i]), ElemIdx);
        if sTmp2 <> '' then
          FOnAttribute(self, UTF8Decode(oTmpAttrs[i]), sTmp2, False);
       {$ENDIF}
      end;
    end;
  end;

  if sTmp = '' then
    sTmp := GetAttributeExpandedValue(sGi, 'xml:space', ElemIdx);
  if sTmp = 'preserve' then
    FPreserve := True
  else if sTmp = 'default' then
    FPreserve := not FNormalizeData;

  if Assigned(FOnPreserveSpace) then
    FOnPreserveSpace(self, sGi, FPreserve);

  TempChar := ReadChar(True);
  if (TempChar = '>') then begin
    if Assigned(FOnStartElement) then
      FOnStartElement(self, sGi);
    ParseContent;
  end else if (TempChar = '/') then begin
    Require(Xpc_BracketAngleRight);
    if Assigned(FOnStartElement) then
      FOnStartElement(self, sGi);
    if Assigned(FOnEndElement) then
      FOnEndElement(self, sGi);
  end;

  FPreserve := bOldPreserve;
  FCurrentElement := sOldElement;
  FCurrentElementContent := wOldElementContent;
end;
{--------}
procedure TVpParser.ParseEndTag;
var
  sName : DOMString;
  msg: DOMString;
begin
  sName := ReadNameToken(True);
  if sName <> FCurrentElement then begin
    {$IFDEF DELPHI}
    msg := sMismatchEndTag + 'Start tag = "' + FCurrentElement + '" End tag = "' + sName + '"';
    {$ELSE}
    msg := UTF8Decode(sMismatchEndTag) + 'Start tag = "' + FCurrentElement + '" End tag = "' + sName + '"';
    {$ENDIF}
    raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, msg);
  end;
  SkipWhitespace(True);
  Require(Xpc_BracketAngleRight);
  if Assigned(FOnEndElement) then
    FOnEndElement(self, FCurrentElement);
end;
{--------}
function TVpParser.ParseEntityRef(bPEAllowed: Boolean): DOMString;
var
  msg: DOMString;
begin
  Result := ReadNameToken(True);
  Require(Xpc_GenParsedEntityEnd);
  case GetEntityType(Result, bPEAllowed) of
    ENTITY_UNDECLARED :
      begin
       {$IFDEF DELPHI}
        msg := sUndeclaredEntity + QuotedStr(Result);
       {$ELSE}
        msg := UTF8Decode(sUndeclaredEntity + QuotedStr(UTF8Encode(Result)));
       {$ENDIF}
        raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, msg);
      end;
    ENTITY_INTERNAL :
      PushString(GetEntityValue(Result, False));
    ENTITY_TEXT :
      GetExternalTextEntityValue(Result, GetEntityPublicId(Result), GetEntitySystemId(Result));
    ENTITY_NDATA :
      begin
        FHasExternals := True;
        if Assigned(FOnNonXMLEntity) then
          FOnNonXMLEntity(self, Result, GetEntityPublicId(Result), GetEntitySystemId(Result), GetEntityNotationName(Result));
      end;
  end;
end;
{--------}
procedure TVpParser.ParseEq;
begin
  SkipWhitespace(True);
  Require(Xpc_Equation);
  SkipWhitespace(True);
end;
{--------}
procedure TVpParser.ParseMisc;
var
  ParsedComment : Boolean;
begin
  ParsedComment := False;
  while True do begin
    SkipWhitespace(True);
    if TryRead(Xpc_ProcessInstrStart) then begin
      if ParsePIEx and ParsedComment then
        raise EVpParserError.CreateError (FFilter.Line,
                                           FFilter.LinePos,
                                           sCommentBeforeXMLDecl)
      else
        FXMLDecParsed := True;
    end else if TryRead(Xpc_CommentStart) then begin
      FXMLDecParsed := True;
      ParsedComment := True;
      ParseComment;
    end else
      Exit;
  end;
end;
{--------}
function TVpParser.ParseParameterEntityRef(aPEAllowed: Boolean;
  bSkip: Boolean): DOMString;
var
  sName, sValue : DOMString;
  msg: DOMString;
begin
  sName := ReadNameToken(True);
  Require(Xpc_GenParsedEntityEnd);
  case GetEntityType(sName, aPEAllowed) of
    ENTITY_UNDECLARED :
      begin
       {$IFDEF DELPHI}
        msg := sUndeclaredEntity + sName;
       {$ELSE}
        msg := UTF8Decode(sUndeclaredEntity) + sName;
       {$ENDIF}
        raise EVpParserError.CreateError (FFilter.Line, FFilter.LinePos - 3, msg);
       end;
    ENTITY_INTERNAL :
      begin
        sValue := GetEntityValue(sName, aPEAllowed);
        if bSkip then
          DataBufferAppend(sValue)
        else
          PushString(sValue);
        Result := sValue;
      end;
    ENTITY_TEXT :
      begin
        sValue := GetExternalTextEntityValue(sName, GetEntityPublicId(sName), GetEntitySystemId(sName));
        if bSkip then
          DataBufferAppend(sValue);
        Result := sValue;
      end;
    ENTITY_NDATA :
      begin
        FHasExternals := True;
        if Assigned(FOnNonXMLEntity) then
          FOnNonXMLEntity(self, sName, GetEntityPublicId(sName), GetEntitySystemId(sName), GetEntityNotationName(sName));
      end;
  end;
end;
{--------}
procedure TVpParser.ParsePCData(aInEntityRef : Boolean);
var
  TempBuff   : DOMString;
  TempChar   : DOMChar;
  CurrLength : Longint;
  BuffLength : Longint;
  Added      : Boolean;
begin
  Added := False;
  CurrLength := 0;
  BuffLength := 50;
  SetLength(TempBuff, BuffLength);
  while True do begin
    TempChar := ReadChar(False);
    if (TempChar = '<') or
       (TempChar = '&') or
       (FFilter.EOF) then
      Break
    else begin
      if ((CurrLength + 2) > BuffLength) then begin
        BuffLength := BuffLength * 2;
        SetLength(TempBuff, BuffLength);
      end;
      Move(TempChar,
           PByteArray(Pointer(TempBuff))[CurrLength],
           2);
      Inc(CurrLength, 2);
      SkipChar;
      Added := True;
    end;
  end;
  if Added then begin
    SetLength(TempBuff, CurrLength div 2);
    ValidatePCData(TempBuff, aInEntityRef);
    DataBufferAppend(TempBuff);
  end;
end;
{--------}
procedure TVpParser.ParsePI;
begin
  ParsePIEx;
end;
{--------}
function TVpParser.ParsePIEx : Boolean;
var
  sName : DOMString;
begin
  Result := False;
  sName := ReadNameToken(True);
  if sName <> 'xml' then begin
    FXMLDecParsed := True;
    if not TryRead(Xpc_ProcessInstrEnd) then begin
      RequireWhitespace;
      ParseUntil(Xpc_ProcessInstrEnd);
    end;
  end else begin
    Result := True;
    ParseXMLDeclaration;
  end;
  if Assigned(FOnProcessingInstruction) then
    FOnProcessingInstruction(self, sName, DataBufferToString)
  else
    DataBufferToString;
end;
{--------}
procedure TVpParser.ParsePrim;
begin
  try
    Initialize;

    if Assigned(FOnStartDocument) then
      FOnStartDocument(self);

    try
      ParseDocument;
    except
      on E: EVpFilterError do begin
        FErrors.Add(Format(sFmtErrorMsg,
                           [E.Line, E.LinePos, E.Message]));
        if FRaiseErrors then begin
          if Assigned(FOnEndDocument) then
            FOnEndDocument(self);
          Cleanup;
          raise;
        end;
      end;
    end;

    if Assigned(FOnEndDocument) then
      FOnEndDocument(self);

    Cleanup;
  finally
    FInCharSet := ceUTF8;
    FFilter.Free;
    FFilter := nil;
  end;
end;
{--------}
procedure TVpParser.ParseProlog;
begin
  ParseMisc;
  if TryRead(Xpc_DTDDocType) then begin
    FXMLDecParsed := True;
    ParseDocTypeDecl;
    ParseMisc;
  end;
end;
{--------}
procedure TVpParser.ParseUntil(const S : array of Longint);
var
  TempStr  : AnsiString;
  TempChar : AnsiChar;
  i        : Integer;
  Found    : Boolean;
begin
  Found := TryRead(s);
  while (not Found) and
        (not FFilter.EOF) do begin
    DataBufferAppend(ReadChar(True));
    Found := TryRead(s);
  end;
  if (not Found) then begin
    {$IFDEF DCC4OrLater}
    SetLength(TempStr, Length(S));
    {$ENDIF}
    for i := 0 to High(S) do begin
      VpUcs4ToIso88591(s[i], TempChar);
      TempStr[Succ(i)] := TempChar;
    end;
    raise EVpParserError.CreateError (FFilter.Line,
                                       FFilter.LinePos,
                                       sUnexpEndOfInput +
                                       QuotedStr(TempStr));
  end;
end;
{--------}
procedure TVpParser.ParseWhitespace;
var
  TempChar : DOMChar;
begin
  TempChar := ReadChar(False);
  while IsWhitespace(TempChar) do begin
    SkipChar;
    DataBufferAppend(TempChar);
    TempChar := ReadChar(False);
  end;
end;
{--------}
procedure TVpParser.ParseXMLDeclaration;
var
  sValue: DOMString;
  s: String;
  Buffer: DOMString;
  HasEntRef: Boolean;
begin
  if FXMLDecParsed then
    raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, sXMLDecNotAtBeg);

  HasEntRef := False;
  SkipWhitespace(True);
  Require(Xpc_Version);
  DatabufferAppend('version');
  ParseEq;
  DatabufferAppend('="');
  Buffer := DatabufferToString;
  sValue := ReadLiteral(0, HasEntRef);
 {$IFDEF DELPHI}
  ValidateVersNum(sValue);
 {$ELSE}
  ValidateVersNum(UTF8Encode(sValue));
 {$ENDIF}
  Buffer := Buffer + sValue + '"';
  if (sValue <> VpXMLSpecification) then
    raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, Format(sInvalidXMLVersion, [VpXMLSpecification]));
  SkipWhitespace(True);
  if TryRead(Xpc_Encoding) then begin
    DatabufferAppend('encoding');
    ParseEq;
    DataBufferAppend('="');
    Buffer := Buffer + ' ' + DataBufferToString;
    sValue := ReadLiteral(LIT_CHAR_REF or LIT_ENTITY_REF, HasEntRef);
   {$IFDEF DELPHI}
    ValidateEncName(sValue);
    if CompareText(sValue, 'ISO-8859-1') = 0 then
      FFilter.Format := sfISO88591;
   {$ELSE}
    s := UTF8Encode(sValue);
    ValidateEncName(s);
    if CompareText(s, 'ISO-8859-1') = 0 then
      FFilter.Format := sfISO88591;
   {$ENDIF}
    Buffer := Buffer + sValue + '"';
    SkipWhitespace(True);
  end;

  if TryRead(Xpc_Standalone) then begin
    DatabufferAppend('standalone');
    ParseEq;
    DatabufferAppend('="');
    Buffer := Buffer + ' ' + DataBufferToString;
    sValue := ReadLiteral(LIT_CHAR_REF or LIT_ENTITY_REF, HasEntRef);
    if (not ((sValue = 'yes') or (sValue = 'no'))) then
      raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, sInvStandAloneVal);
    Buffer := Buffer + sValue + '"';
    FIsStandalone := sValue = 'yes';
    SkipWhitespace(True)
  end;

  Require(Xpc_ProcessInstrEnd);
  DatabufferToString;
  DatabufferAppend(Buffer);
end;
{--------}
procedure TVpParser.PopDocument;
begin
  Assert(FDocStack.Count > 0);

  if FDocStack.Count > 0 then begin
    FFilter := FDocStack[Pred(FDocStack.Count)];
    FDocStack.Delete(Pred(FDocStack.Count));
  end;
end;
{--------}
procedure TVpParser.PushDocument;
begin
  Assert(Assigned(FFilter));

  FDocStack.Add(Pointer(FFilter));
  FFilter := nil;
end;
{--------}
procedure TVpParser.PushString(const sVal: DOMString);
var
  MemStream: TVpMemoryStream;
  TempString: string;
begin
  if Length(sVal) > 0 then begin
    PushDocument;
    MemStream := TVpMemoryStream.Create;
   {$IFDEF DELPHI}
    TempString := WideCharLenToString(Pointer(sVal), Length(sVal));
   {$ELSE}
    WideCharLenToStrVar(PWideChar(sVal), Length(sVal), TempString);
   {$ENDIF}
    MemStream.Write(TempString[1], Length(TempString));
    MemStream.Position := 0;
    FFilter := TVpInCharFilter.Create(MemStream, BufferSize);
  end;
end;
{--------}
function TVpParser.ReadChar(const UpdatePos: Boolean) : DOMChar;
begin
  Result := FFilter.ReadChar;
  if (Result = VpEndOfStream) and (not IsEndDocument) then
    Result := FFilter.ReadChar;
  if UpdatePos then
    FFilter.SkipChar;
end;
{--------}
procedure TVpParser.ReadExternalIds(bInNotation : Boolean;
                                var sIds        : StringIds);
var
  HasEntRef : Boolean;
  TempChar  : DOMChar;
begin
  HasEntRef := False;
  if TryRead(Xpc_ExternalPublic) then begin
    RequireWhitespace;
    sIds[0] := ReadLiteral(LIT_NORMALIZE, HasEntRef);
    ValidatePublicID(sIds[0]);
    if bInNotation then begin
      SkipWhitespace(True);
      TempChar := ReadChar(False);
      if (TempChar = '''') or
         (TempChar = '"') then
        sIds[1] := ReadLiteral(0, HasEntRef);
    end else begin
      RequireWhitespace;
      sIds[1] := ReadLiteral(0, HasEntRef);
    end;
  end else if TryRead(Xpc_ExternalSystem) then begin
    RequireWhitespace;
    sIds[1] := ReadLiteral(0, HasEntRef);
  end;
end;
{--------}
function TVpParser.ReadLiteral(wFlags: Integer; var HasEntRef: Boolean): DOMString;
var
  TempStr: DOMString;
  cDelim, TempChar: DOMChar;
  EntRefs: TStringList;
  StackLevel: Integer;
  CurrCharRef: Boolean;
  msg: DOMString;
begin
  StackLevel := 0;
  CurrCharRef := False;
  Result := '';
  EntRefs := nil;
  cDelim := ReadChar(True);
  if (cDelim <> '"') and
     (cDelim <> #39) and
     (cDelim <> #126) and
     (cDelim <> #0)
  then
    raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, sQuoteExpected);
  TempChar := ReadChar(False);
  while (not IsEndDocument) and ((CurrCharRef) or (TempChar <> cDelim)) do begin
    if (TempChar = #$0A) then begin
      TempChar := ' ';
    end else if (TempChar = #$0D) then
      TempChar := ' '
    else if (TempChar = '&') then begin
      if wFlags and LIT_CHAR_REF <> 0 then begin
        if wFlags and LIT_ENTITY_REF <> 0 then
          CurrCharRef := True;
        HasEntRef := True;
        SkipChar;
        TempChar := ReadChar(False);
        if TempChar = '#' then begin
          SkipChar;
          ParseCharRef;
          TempChar := ReadChar(False);
          CurrCharRef := False;
          Continue;
        end else if wFlags and LIT_ENTITY_REF <> 0 then begin
          TempStr := ParseEntityRef(False);
          if (TempStr <> 'lt') and
             (TempStr <> 'gt') and
             (TempStr <> 'amp') and
             (TempStr <> 'apos') and
             (TempStr <> 'quot') then
          begin
            if (not Assigned(EntRefs)) then begin
              EntRefs := TStringList.Create;
              EntRefs.Sorted := True;
              EntRefs.Duplicates := dupError;
              StackLevel := FDocStack.Count;
            end else
              StackLevel := Succ(FDocStack.Count);
            try
              if FDocStack.Count = StackLevel then begin
                EntRefs.Clear;
                StackLevel := FDocStack.Count;
              end;
             {$IFDEF DELPHI}
              EntRefs.Add(TempStr);
             {$ELSE}
              EntRefs.Add(UTF8Encode(TempStr));
             {$ENDIF}
            except
              on E:EStringListError do begin
                EntRefs.Free;
               {$IFDEF DELPHI}
                msg := sCircularEntRef + TempChar;
               {$ELSE}
                msg := UTF8Decode(sCircularEntRef) + TempChar;
               {$ENDIF}
                raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, msg);
              end;
              on E:EVpParserError do
                raise;
            end;
          end else
            HasEntRef := False;
          TempChar := ReadChar(False);
          Continue;
        end else if wFlags and LIT_PE_REF <> 0 then begin
          ParseParameterEntityRef(False, True);
          Continue;
        end else
          DataBufferAppend('&');
          if (not Assigned(EntRefs)) then begin
            StackLevel := FDocStack.Count;
            EntRefs := TStringList.Create;
            EntRefs.Sorted := True;
            EntRefs.Duplicates := dupError;
          end;
          try
            if StackLevel = FDocStack.Count then begin
              EntRefs.Clear;
              StackLevel := FDocStack.Count;
            end;
           {$IFDEF DELPHI}
            EntRefs.Add('&' + DOMString(TempChar));
           {$ELSE}
            EntRefs.Add('&' + UTF16ToUTF8(TempChar));
           {$ENDIF}
          except
            on E:EStringListError do begin
              EntRefs.Free;
             {$IFDEF DELPHI}
              msg := sCircularEntRef + TempChar;
             {$ELSE}
              msg := UTF8Decode(sCircularEntRef) + TempChar;
             {$ENDIF}
              raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, msg);
            end;
            on E:EVpParserError do
              raise;
          end;
      end;
    end;
    DataBufferAppend(TempChar);
    SkipChar;
    TempChar := ReadChar(False);
    CurrCharRef := False;
  end;
  if TempChar <> cDelim then
    raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, 'Expected: ' + cDelim);

  SkipChar;

  if wFlags and LIT_NORMALIZE <> 0 then
    DataBufferNormalize;

  Result := DataBufferToString;

  EntRefs.Free;
end;
{--------}
function TVpParser.ReadNameToken(aValFirst: Boolean): DOMString;
var
  TempChar: DOMChar;
  First: Boolean;
  ResultLen: Integer;
  CurrLen: Integer;
  msg: DOMString;
begin
  if TryRead(Xpc_ParamEntity) then begin
    ParseParameterEntityRef(True, False);
    SkipWhiteSpace(True);
  end;
  First := aValFirst;
  Result := '';
  CurrLen := 0;
  ResultLen := 20;
  SetLength(Result, ResultLen);
  while True do begin
    TempChar := ReadChar(False);
    if (TempChar = '%') or (TempChar = '<') or (TempChar = '>') or
       (TempChar = '&') or (TempChar = ',') or (TempChar = '|') or
       (TempChar = '*') or (TempChar = '+') or (TempChar = '?') or
       (TempChar = ')') or (TempChar = '=') or (TempChar = #39) or
       (TempChar = '"') or (TempChar = '[') or (TempChar = ' ') or
       (TempChar = #9) or (TempChar = #$0A) or (TempChar = #$0D) or
       (TempChar = ';') or (TempChar = '/') or (TempChar = '') or
       (TempChar = #1)
    then
      Break
    else
      if ValidateNameChar(First, TempChar) then begin
        if (CurrLen + 2 > ResultLen) then begin
          ResultLen := ResultLen * 2;
          SetLength(Result, ResultLen);
        end;
        SkipChar;
        Move(TempChar, PByteArray(Pointer(Result))^[CurrLen], 2);
        Inc(CurrLen, 2);
      end else begin
       {$IFDEF DELPHI}
        msg := sInvalidName + QuotedStr(TempChar);
       {$ELSE}
        msg := UTF8Decode(sInvalidName + QuotedStr(UTF16ToUTF8(TempChar)));
       {$ENDIF}
        raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, msg);
      end;
    First := False;
  end;
  SetLength(Result, CurrLen div 2);
end;
{--------}
procedure TVpParser.Require(const S : array of Longint);
var
  TempStr  : AnsiString;
  TempChar : AnsiChar;
  i        : Integer;
begin
  if not TryRead(S) then begin
    SetLength(TempStr, High(S) + 1);
    for i := 0 to High(S) do begin
      VpUcs4ToIso88591(s[i], TempChar);
      TempStr[i + 1] := TempChar;
    end;
    if ReadChar(False) = '&' then begin
      SkipChar;
      if ReadChar(False) = '#' then begin
        SkipChar;
       {$IFDEF DELPHI}
        if ParseCharRef = TempStr then
          Exit;
       {$ELSE}
        if UTF16ToUTF8(ParseCharRef) = TempStr then
          Exit;
       {$ENDIF}
      end;
    end;
    TempStr := sExpectedString + QuotedStr(TempStr);
    raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, TempStr);
  end;
end;
{--------}
procedure TVpParser.RequireWhitespace;
begin
  if IsWhitespace(ReadChar(False)) then
    SkipWhitespace(True)
  else
    raise EVpParserError.CreateError (FFilter.Line,
                                       FFilter.LinePos,
                                       sSpaceExpectedAt +
                                       'Line: ' + IntToStr(FFilter.Line) +
                                       ' Position: ' + IntToStr(FFilter.LinePos));
end;
{--------}
procedure TVpParser.SetAttribute(const sElemName, sName : DOMString;
                                       wType            : Integer;
                                 const sEnum, sValue    : DOMString;
                                       wValueType       : Integer);
var
  wIdx      : Integer;
  oElemInfo : TVpElementInfo;
  oAttrInfo : TVpAttributeInfo;
begin
  wIdx := GetElementIndexOf(sElemName);
  if wIdx < 0 then begin
    SetElement(sElemName, CONTENT_UNDECLARED, '');
    wIdx := GetElementIndexOf(sElemName);
  end;

  oElemInfo := TVpElementInfo(FElementInfo.Objects[wIdx]);
  oAttrInfo := TVpAttributeInfo.Create;
  oAttrInfo.AttrType := wType;
  oAttrInfo.Value := sValue;
  oAttrInfo.ValueType := wValueType;
  oAttrInfo.Enum := sEnum;
  oElemInfo.SetAttribute(sName, oAttrInfo);
end;
{--------}
procedure TVpParser.SetElement(const sName: DOMString; wType: Integer;
  const sContentModel: DOMString);
var
  oElem: TVpElementInfo;
  wIdx: Integer;
begin
  wIdx := GetElementIndexOf(sName);
  if wIdx < 0 then begin
    oElem := TVpElementInfo.Create;
   {$IFDEF DELPHI}
    FElementInfo.AddObject(sName, oElem);
   {$ELSE}
    FElementInfo.AddObject(UTF8Encode(sName), oElem);
   {$ENDIF}
  end else
    oElem := TVpElementInfo(FElementInfo.Objects[wIdx]);

  if wType <> CONTENT_UNDECLARED then
    oElem.ContentType := wType;

  if sContentModel <> '' then
    oElem.ContentModel := sContentModel;
end;
{--------}
procedure TVpParser.SetEntity(const sEntityName: DOMString; wClass: Integer;
  const sPublicId, sSystemId, sValue, sNotationName: DOMString; aIsPE: Boolean);
var
  wIdx: Integer;
  oEntity: TVpEntityInfo;
begin
  wIdx := GetEntityIndexOf(sEntityName, aIsPE);
  if wIdx < 0 then begin
    oEntity := TVpEntityInfo.Create;
    oEntity.EntityClass := wClass;
    oEntity.PublicId := sPublicId;
    oEntity.SystemId := sSystemId;
    oEntity.Value := sValue;
    oEntity.NotationName := sNotationName;
    oEntity.IsParameterEntity := aIsPE;
   {$IFDEF DELPHI}
    FEntityInfo.AddObject(sEntityName, oEntity);
   {$ELSE}
    FEntityInfo.AddObject(UTF8Encode(sEntityName), oEntity);
   {$ENDIF}
  end;
end;
{--------}
procedure TVpParser.SetInternalEntity(const sName, sValue : DOMString;
                                            aIsPE         : Boolean);
begin
  SetEntity(sName, ENTITY_INTERNAL, '', '', sValue, '', aIsPE);
end;
{--------}
procedure TVpParser.SetNotation(const sNotationName, sPublicId, sSystemId: DOMString);
var
  oNot : TVpNotationInfo;
  wIdx : Integer;
begin
 {$IFDEF DELPHI}
  if not FNotationInfo.Find(sNotationName, wIdx) then begin
 {$ELSE}
  if not FNotationInfo.Find(UTF8Encode(sNotationName), wIdx) then begin
 {$ENDIF}
    oNot := TVpNotationInfo.Create;
    oNot.PublicId := sPublicId;
    oNot.SystemId := sSystemId;
   {$IFDEF DELPHI}
    FNotationInfo.AddObject(sNotationName, oNot);
   {$ELSE}
    FNotationInfo.AddObject(UTF8Encode(sNotationName), oNot);
   {$ENDIF}
  end;
end;
{--------}
procedure TVpParser.SkipChar;
begin
  FFilter.SkipChar;
end;
{--------}
procedure TVpParser.SkipWhitespace(aNextDoc : Boolean);
begin
  while (not FFilter.Eof) and
        (IsWhitespace(ReadChar(False))) do
    SkipChar;
 if aNextDoc then begin
   IsEndDocument;
   while (not FFilter.Eof) and
         (IsWhitespace(ReadChar(False))) do
     SkipChar;
 end;
end;
{--------}
function TVpParser.TryRead(const S : array of Longint) : Boolean;
begin
  Result := False;
  if (not IsEndDocument) then begin
    Result := FFilter.TryRead(S);
    IsEndDocument;
  end;                                                                 
end;
{--------}
procedure TVpParser.ValidateAttribute(const aValue: DOMString; HasEntRef: Boolean);
begin
  if (not HasEntRef) then
    if (VpPos('<', aValue) <> 0) then
      raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, sInvAttrChar + '''<''')
    else
    if (VpPos('&', aValue) <> 0) then
      raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, sInvAttrChar + '''&''')
    else
    if (VpPos('"', aValue) <> 0) then
      raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, sInvAttrChar + '''"''');
end;
{--------}
procedure TVpParser.ValidateCData(const CDATA: DOMString);
begin
  if (VpPos(']]>', CDATA) <> 0) then
    raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, sInvalidCDataSection);
end;
{--------}
procedure TVpParser.ValidateElementName(const aName: DOMString);
var
  msg: DOMString;
begin
  if (aName = '') or (aName = ' ') then begin
   {$IFDEF DELPHI}
    msg := sInvalidElementName + QuotedStr(aName);
   {$ELSE}
    msg := UTF8Decode(sInvalidElementName + QuotedStr(UTF8Encode(aName)));
   {$ENDIF}
    raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, msg);
  end;
end;
{--------}
procedure TVpParser.ValidateEncName(const aValue: string);
var
  i: Integer;
  Good: Boolean;
begin
  { Production [81]}
  for i := 1 to Length(aValue) do begin
    Good := False;
    if ((aValue[i] >= 'A') and
        (aValue[i] <= 'z')) then
      Good := True
    else if i > 1 then
      if (aValue[i] >= '0') and
         (aValue[i] <= '9') then
        Good := True
      else if aValue[i] = '.' then
        Good := True
      else if aValue[i] = '_' then
        Good := True
      else if aValue[i] = '-' then
        Good := True
      else if aValue[i] = '=' then
        Good := True;
    if not Good then
      raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, sInvEncName + QuotedStr(aValue));
  end;
end;
{--------}
procedure TVpParser.ValidateEntityValue(const aValue: DOMString; aQuoteCh: DOMChar);
var
  TempChr: DOMChar;
  i: Integer;
  msg: String;
begin
  for i := 1 to Length(aValue) do begin
    TempChr := aValue[i];
    if (TempChr = '%') or (TempChr = '&') or (TempChr = aQuoteCh) then begin
     {$IFDEF DELPHI}
      msg := sInvEntityValue + QuotedStr(TempChr));
     {$ELSE}
      msg := sInvEntityValue + QuotedStr(UTF16ToUTF8(TempChr));
     {$ENDIF}
      raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, msg);
    end;
  end;
end;
{--------}
function TVpParser.ValidateNameChar(const First: Boolean;
  const Char: DOMChar): Boolean;
var
  BothUsed: Boolean;
  UCS4: TVpUCS4Char;
begin
  { Naming rules -  from sect 2.3 of spec}
  { Names cannot be an empty string }
  { Names must begin with 1 letter or one of the following
    punctuation characters ['_',':']}
  { Names should not begin with 'XML' or any case derivitive}
  { Except for the first character, names can contain
    [letters, digits,'.', '-', '_', ':'}

  VpUtf16ToUcs4(
    DOMChar(PByteArray(@Char)^[0]),
    DOMChar(PByteArray(@Char)^[1]),
    UCS4,
    BothUsed
  );
  if not First then
    Result := VpIsNameChar(UCS4)
  else
    Result := VpIsNameCharFirst(UCS4);
end;
{--------}
procedure TVpParser.ValidatePCData(const aString      : DOMString;
                                         aInEntityRef : Boolean);
begin
  if (not aInEntityRef) then
    if (VpRPos('<', aString) <> 0) then
      raise EVpParserError.CreateError (FFilter.Line,
                                         FFilter.LinePos,
                                         sInvPCData + '''<''')
    else if (VpRPos('&', aString) <> 0) and
            (VpRPos(';', aString) = 0) then
      raise EVpParserError.CreateError (FFilter.Line,
                                         FFilter.LinePos,
                                         sInvPCData + '''&''')
    else if (VpRPos(']]>', aString) <> 0) then
      raise EVpParserError.CreateError (FFilter.Line,
                                         FFilter.LinePos,
                                         sInvPCData + ''']]>''');
end;
{--------}
procedure TVpParser.ValidatePublicID(const aString: DOMString);
var
  Ucs4Char: TVpUcs4Char;
  i: Integer;
  msg: DOMString;
begin
  for i := 1 to Length(aString) do begin
    VpIso88591ToUcs4(AnsiChar(aString[i]), Ucs4Char);
    if (not VpIsPubidChar(Ucs4Char)) then
    begin
     {$IFDEF DELPHI}
      msg := sInvPubIDChar + QuotedStr(aString[i]);
     {$ELSE}
      msg := UTF8Decode(sInvPubIDChar + QuotedStr(UTF16ToUTF8(aString[i])));
     {$ENDIF}
      raise EVpParserError.CreateError(FFilter.Line, FFilter.LinePos, msg);
    end;
  end;
end;
{--------}
procedure TVpParser.ValidateVersNum(const aString : string);
var
  i       : Integer;
  TempChr : char;
  Good    : Boolean;
begin
  for i := 1 to Length(aString) do begin
    Good := False;
    TempChr := aString[i];
    if (TempChr >= 'A') and
       (TempChr <= 'z') then
      Good := True
    else if (TempChr >= '0') and
            (TempChr <= '9') then
      Good := True
    else if (TempChr = '.') then
      Good := True
    else if (TempChr = '_') then
      Good := True
    else if (TempChr = ':') then
      Good := True
    else if (TempChr = '-') then
      Good := True;
    if not Good then
      raise EVpParserError.CreateError (FFilter.Line,
                                         FFilter.LinePos,
                                         sInvVerNum +
                                         QuotedStr(aString));
  end;
end;

end.



