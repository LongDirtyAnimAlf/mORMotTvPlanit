{*********************************************************}
{*                  VPREG.PAS 1.03                       *}
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

{$I Vp.INC}    { Compiler Version Defines }
{$R vpreg.res} { Palette Glyphs           }

unit VpReg;
  {Registration unit for the Visual PlanIt design-time interface}

interface

uses
  {$IFDEF LCL}
  LCLProc, LCLType, LCLIntf, LazFileUtils,
  {$ELSE}
  Windows,
  {$ENDIF}
  Dialogs,
  {$IFDEF VERSION6}
  {$IFNDEF LCL}
  DesignIntf, DesignEditors, VCLEditors,
  {$ELSE}
  PropEdits, LazarusPackageIntf, FieldsEditor,  ComponentEditors,
  {$ENDIF}
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  Classes, Controls, TypInfo, Forms, SysUtils,
  VpDatePropEdit;

type
  {TDBStringProperty}
  TDBStringProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  {$IFDEF DELPHI}
  {TAliasNameProperty}
  TAliasNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  {TDriverNameProperty}
  TDriverNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  { TDataStoreProperty }
  TDataStoreProperty = class(TComponentProperty)
  private
    FCheckProc: TGetStrProc;
    procedure CheckComponent(const Value: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;
 {$ENDIF}

  TVpDateProperty = class (TFloatProperty)
    public
      procedure Edit; override;
      function  GetAttributes : TPropertyAttributes; override;
      function  GetValue : string; override;
      procedure SetValue (const Value : string); override;
  end;

  TVpGeneralFileNameProperty = class (TStringProperty)
    protected
      function GetDefaultExt: String; virtual;
      function GetFilter: String; virtual;
      function GetInitialDir: String; virtual;
    public
      function GetAttributes: TPropertyAttributes; override;
      function GetValue: string; override;
      procedure Edit; override;
      procedure SetValue(const NewValue: String); override;
  end;

  TVpLocalizeFileNameProperty = class (TVpGeneralFileNameProperty)
  protected
    function GetDefaultExt: String; override;
    function GetFilter: String; override;
  end;

  TVpWavFilenameProperty = class(TVpGeneralFilenameProperty)
  protected
    function GetDefaultExt: String; override;
    function GetFilter: String; override;
    function GetInitialDir: String; override;
  end;
  {
  public
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: string; override;
    procedure Edit; override;
    procedure SetValue(const Value: string); override;
  end;
   }

  TVpMediaFolderProperty = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: string; override;
    procedure Edit; override;
    procedure SetValue(const Value: string); override;
  end;

procedure Register;

implementation

uses
 {$IFDEF DELPHI}
  DbTables,                   { VCL - BDE runtime unit                       }
  VpWavPE,                    { Wav File Finder - Property Editor            }
 {$ENDIF}
  VpMisc,

  { Component Units                                                          }
  VpBase,                     { Base classes for Vp                          }
  VpClock,                    { Clock Component                              }
  VpDlg,                      { Dialog components ancestor                   }
  VpLEDLabel,                 { LEDLabel Component                           }
  VpCalendar,                 { Calendar Component                           }
  VpNavBar,                   { Navigation Bar Component                     }
  VpBaseDS,                   { Base DataStore Classes                       }
  VpDayView,                  { Day View Component                           }
  VpWeekView,                 { Week View Component                          }
  VpMonthView,                { Month View Component                         }
  VpContactGrid,              { ContactGrid Component                        }
  VpTaskList,                 { Task List Component                          }
 {$IFDEF DELPHI}
  VpBDEDS,                    { DataStore Component                          }
  VpDateEdit,                 { DateEdit Component                           }
 {$ENDIF}
 {$IFDEF LCL}
  VpIniDS,                    { IniFile datastore                            }
  VpXmlDS,                    { XML file datastore                           }
  VpBufDS,                    { Datastore for TBufDataset                    }
  VpFBDS,                     { Datastore for Firebird database              }
  VpSqlite3DS,                { Datastore for sqlite3                        }
  //  VpSdfDS                     { Datastore for TSdfDataset                    }
  //  VpDbfDS,                    { Datastore for dbase files                    }
 {$ENDIF}
  VpFlxDS,                    { Flexible DataStore                           }
  VpContactEditDlg,           { Contact Edit Dialog Component                }
  VpTaskEditDlg,              { Task Edit Dialog Component                   }
  VpEvntEditDlg,              { Event Edit Dialog Component                  }
  VpAlarmDlg,                 { Alarm Notification Dialog                    }
  VpResEditDlg,               { Resource Edit Dialog                         }
  VpPrtPrv,                   { Print Preview Component                      }
  VpPrtFmtCBox,               { Print Format Combo Box Component             }
  VpPrtPrvDlg,                { Print Preview Dialog                         }
  VpPrtFmtDlg,                { Print Format Dialog                          }
  VpPrtFmtEd,                 { Print Format Property editor                 }
  VpContactButtons,           { - New contact grid button bar component }    
  { Designtime Interfaces (Property and Component Editors)                   }
  VpAbout,                    { About form for the About property editor     }
 {$IFDEF DELPHI}
  VpNabEd,                    { component editor for the VpNavBar            }
 {$ENDIF}
  VpFlxDSEd1;                 { Field mapper component editor for the FlexDS }


(*****************************************************************************)
{ TDBStringProperty }

function TDBStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;
{=====}

procedure TDBStringProperty.GetValueList(List: TStrings);
begin
  Unused(List);
end;
{=====}

procedure TDBStringProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;
{=====}

{$IFDEF DELPHI}
(*****************************************************************************)
{ TAliasNameProperty }

procedure TAliasNameProperty.GetValueList(List: TStrings);
begin
  (GetComponent(0) as TVpBDEDataStore).Database.Session.GetAliasNames(List);
end;

(*****************************************************************************)
{ TDriverNameProperty }

procedure TDriverNameProperty.GetValueList(List: TStrings);
begin
  (GetComponent(0) as TVpBDEDataStore).Database.Session.GetDriverNames(List);
end;

(*****************************************************************************)
{ TDataStoreProperty }
procedure TDataStoreProperty.CheckComponent(const Value: string);
var
  J: Integer;
  DataStore: TVpCustomDataStore;
begin
  DataStore := TVpCustomDataStore(Designer.GetComponent(Value));
  for J := 0 to PropCount - 1 do
    if TVpDayView(GetComponent(J)).DataStore = DataStore then
      Exit;
  FCheckProc(Value);
end;

procedure TDataStoreProperty.GetValues(Proc: TGetStrProc);
begin
  FCheckProc := Proc;
  inherited GetValues(CheckComponent);
end;
{=====}
{$ENDIF}

(*****************************************************************************)
{ TVpDateProperty }
procedure TVpDateProperty.Edit;
var
  frmDatePropertyEditor: TfrmDatePropertyEditor;
begin
  frmDatePropertyEditor := TfrmDatePropertyEditor.Create (Application);
  try
    frmDatePropertyEditor.VpCalendar1.Date := GetFloatValue;
    if frmDatePropertyEditor.Execute then
      SetFloatValue(Trunc(frmDatePropertyEditor.VpCalendar1.Date));
  finally
    frmDatePropertyEditor.Free;
  end;
end;
{=====}

function TVpDateProperty.GetAttributes : TPropertyAttributes;
begin
  Result := [paDialog, paMultiSelect];
end;
{=====}

function TVpDateProperty.GetValue : string;
begin
  Result := FormatDateTime('ddddd', GetFloatValue);
end;
{=====}

procedure TVpDateProperty.SetValue (const Value : string);
begin
  SetFloatValue(StrToDate (Value));
end;
{=====}

(*****************************************************************************)
{ TVpGeneralFileNameProperty }

function TVpGeneralFileNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TVpGeneralFileNameProperty.Edit;
var
  dlg: TOpenDialog;
begin
  dlg := TOpenDialog.Create(Application);
  try
    dlg.DefaultExt := GetDefaultExt;
    dlg.Filter := GetFilter;
    dlg.FilterIndex := 1;
    dlg.InitialDir := GetForcedPathDelims(GetInitialDir);
    dlg.Options := [ofHideReadOnly];
    dlg.Filename := GetValue;
    if dlg.Execute then
      SetValue(dlg.Filename);
  finally
    dlg.Free;
  end;
end;

function TVpGeneralFilenameProperty.GetDefaultExt: String;
begin
  Result := '*.*';
end;

function TVpGeneralFilenameProperty.GetFilter: String;
begin
  Result := 'All files (*.*)|*.*';
end;

function TVpGeneralFilenameProperty.GetInitialDir: String;
var
  filename: String;
begin
  filename := GetValue;
  if filename <> '' then
    Result := ExtractFileDir(filename) else
    Result := '';
end;

function TVpGeneralFileNameProperty.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TVpGeneralFilenameProperty.SetValue(const NewValue: string);
begin
  SetStrValue(NewValue);
end;


{ TVpLocalizeFilenameProperty }

function TVpLocalizeFilenameProperty.GetDefaultExt: String;
begin
  Result := '*.xml';
end;

function TVpLocalizeFilenameProperty.GetFilter: String;
begin
  Result := 'Localization files (*.xml)|*.xml|' + inherited;
end;

{
procedure TVpGenericFileNameProperty.Edit;
const
  VpRegLocalizeFilter = 'Localization Files (*.XML)|*.XML';
  VpRegDefFilter      = 'All Files (*.*)|*.*';

var
  Dlg    : TOpenDialog;
  Filter : string;

begin
  Filter := '';
  if Self is TVpLocalizeFileNameProperty then
    Filter := VpRegLocalizeFilter;

  if Filter = '' then
    Filter := VpRegDefFilter
  else
    Filter := Filter + '|' + VpRegDefFilter;

  Dlg := TOpenDialog.Create (Application);
  try
    Dlg.DefaultExt := '*.*';
    Dlg.Filter := Filter;
    Dlg.FilterIndex := 0;
    Dlg.Options := [ofHideReadOnly];
//    Dlg.FileName := Value;
//    if Dlg.Execute then
//      Value := Dlg.FileName;
  finally
    Dlg.Free;
  end;
end;
                               }

{ TVpWavFilenameProperty }

function TVpWavFilenameProperty.GetDefaultExt: String;
begin
  Result := '*.wav';
end;

function TVpWavFilenameProperty.GetFilter: String;
begin
  Result := 'Wav files (*.wav)|*.wav|All files (*.*)|*.*';
end;

function TVpWavFilenameProperty.GetInitialDir: String;
var
  ds: TVpCustomDatastore;
begin
  ds := GetComponent(0) as TVpCustomDatastore;
  if Assigned(ds) then begin
    if ds.DefaultEventSound = '' then
      Result := ds.MediaFolder
    else
      Result := ExtractFilePath(ds.DefaultEventSound);
    Result := GetForcedPathDelims(Result);
  end else
    Result := '';
end;


{ TVpWavFileProperty }
            {
function TVpWavFileProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TVpWavFileProperty.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TVpWavFileProperty.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

procedure TVpWavFileProperty.Edit;
var
  dlg: TOpenDialog;
  ds: TVpCustomDatastore;
begin
  ds := GetComponent(0) as TVpCustomDatastore;
  if Assigned(ds) then
  begin
    dlg := TOpenDialog.Create(nil);
    try
      dlg.Filter := 'Wav files (*.wav)|*.wav|All files (*.*)|*.*';
      dlg.FilterIndex := 1;
      dlg.DefaultExt := '*.wav';
      if ds.DefaultEventSound = '' then
        dlg.InitialDir := ds.MediaFolder
      else
        dlg.InitialDir := ExtractFilePath(ds.DefaultEventSound);
      if dlg.Execute then
        ds.DefaultEventSound := dlg.FileName;
    finally
      dlg.Free;
    end;
  end
  else
    inherited;
end;
     }

{ TVpMediaFolderProperty }

function TVpMediaFolderProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TVpMediaFolderProperty.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TVpMediaFolderProperty.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

procedure TVpMediaFolderProperty.Edit;
var
  dlg: TSelectDirectoryDialog;
  ds: TVpCustomDatastore;
begin
  ds := GetComponent(0) as TVpCustomDatastore;
  if Assigned(ds) then
  begin
    dlg := TSelectDirectoryDialog.Create(nil);
    try
      dlg.Filter := 'Wav files (*.wav)|*.wav|All files (*.*)|*.*';
      dlg.FilterIndex := 1;
      dlg.DefaultExt := '*.wav';
      if ds.MediaFolder <> '' then
        dlg.InitialDir := ds.MediaFolder
      else
        dlg.InitialDir := ExtractFilePath(ds.DefaultEventSound);
      if dlg.Execute then
        ds.DefaultEventSound := dlg.FileName;
    finally
      dlg.Free;
    end;
  end
  else
    inherited;
end;


{*** component registration ***}
procedure Register;
begin
  {----------------------------------------------------------------------------}
  {                   register component editors                               }
  {----------------------------------------------------------------------------}
 {$IFDEF DELPHI}
  RegisterComponentEditor(TVpNavBar, TVpNavBarEditor);
 {$ENDIF}
  RegisterComponentEditor(TVpControlLink, TVpPrtFmtPropertyEditor);
  RegisterComponentEditor(TVpFlexDataStore, TVpFlexDSEditor);

  {----------------------------------------------------------------------------}
  {                    register property editors                               }
  {----------------------------------------------------------------------------}

  { register the About Box property editor for the Version properties }
  RegisterPropertyEditor(TypeInfo(string), TVpCollectionItem,
    'Version', TVpAboutProperty);
  RegisterPropertyEditor(TypeInfo(string), TVpComponent,
    'Version', TVpAboutProperty);
  RegisterPropertyEditor(TypeInfo(string), TVpNavBar,
    'Version', TVpAboutProperty);
  RegisterPropertyEditor(TypeInfo(string), TVpCalendar,
    'Version', TVpAboutProperty);
  RegisterPropertyEditor(TypeInfo(string), TVpLEDLabel,
    'Version', TVpAboutProperty);
  RegisterPropertyEditor(TypeInfo(string), TVpClock,
    'Version', TVpAboutProperty);
  RegisterPropertyEditor(TypeInfo(string), TVpResourceCombo,
    'Version', TVpAboutProperty);
  RegisterPropertyEditor(TypeInfo(string), TVpCustomControl,
    'Version', TVpAboutProperty);
  RegisterPropertyEditor(TypeInfo(string), TVpControlLink,
    'Version', TVpAboutProperty);

  { Other property editors }
{$IFDEF DELPHI}
  RegisterPropertyEditor(TypeInfo(string), TVpBDEDataStore,
    'Version', TVpAboutProperty);
  RegisterPropertyEditor(TypeInfo(string), TVpDateEdit,
    'Version', TVpAboutProperty);
{$ENDIF}
  RegisterPropertyEditor(TypeInfo(string), TVpFlexDataStore,
    'Version', TVpAboutProperty);
  RegisterPropertyEditor(TypeInfo(string), TVpBaseDialog,
    'Version', TVpAboutProperty);
  RegisterPropertyEditor(TypeInfo(string), TVpPrintFormatComboBox,
    'Version', TVpAboutProperty);

{$IFDEF DELPHI}
  {register the BDE Alias and Driver properties                             }

  RegisterPropertyEditor(TypeInfo(string), TVpBDEDataStore,
    'AliasName', TAliasNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TVpBDEDataStore,
    'DriverName', TDriverNameProperty);

  {register the DayView properties                                          }

  // LCL: Registering next property editor inhibits that the DataStore
  // property combo of the DayView lists the available datastores.
  RegisterPropertyEditor(TypeInfo(TVpCustomDataStore), TVpDayView,
    'DataStore', TDataStoreProperty);

  {register the property editor for the DataStore's DefaultAlarmWav         }
  // NO - not useful in design mode because there is not platform-independent way
  // to play the sound }
  RegisterPropertyEditor(TypeInfo(string), TVpCustomDataStore,
    'DefaultEventSound', TWavFilenameProperty);
 {$ENDIF}

  RegisterPropertyEditor(TypeInfo(String), TVpCustomDatastore,
    'DefaultEventSound', TVpWavFilenameProperty);

  RegisterPropertyEditor(TypeInfo(String), TVpCustomDatastore,
   'MediaFolder', TVpMediaFolderProperty);

  RegisterPropertyEditor(TypeInfo(TDateTime), TVpPrintPreview,
    'StartDate', TVpDateProperty);

  RegisterPropertyEditor(TypeInfo(TDateTime), TVpPrintPreview,
    'EndDate', TVpDateProperty);

  RegisterPropertyEditor(TypeInfo(TDateTime), TVpPrintPreviewDialog,
    'StartDate', TVpDateProperty);

  RegisterPropertyEditor(TypeInfo(TDateTime), TVpPrintPreviewDialog,
    'EndDate', TVpDateProperty);

  RegisterPropertyEditor(TypeInfo(string), TVpControlLink,
    'LocalizationFile', TVpLocalizeFileNameProperty);


  {----------------------------------------------------------------------------}
  {               register Visual PlanIt components with the IDE               }
  {----------------------------------------------------------------------------}
  RegisterComponents('Visual PlanIt', [
    TVpLEDLabel,
    TVpClock,
    TVpCalendar,
    TVpNavBar,
    TVpFlexDataStore,
{$IFDEF DELPHI}
    TVpBDEDataStore,    // BDE is not available in Lazarus
    TVpDateEdit,        // Does not work in Lazarus
{$ENDIF}
{$IFDEF LCL}
    TVpIniDatastore,
    TVpXmlDatastore,
    TVpBufDSDatastore,
    TVpSqlite3Datastore,
    TVpFirebirdDatastore,
    //TVpSdfDatastore,       // to do (maybe)...
    //TVpDbfDatastore,       // to do...
{$ENDIF}
    TVpControlLink,
    TVpPrintPreview,
    TVpPrintFormatComboBox,
    TVpResourceCombo,
    TVpDayView,
    TVpWeekView,
    TVpMonthView,
    TVpContactGrid,
    TVpContactButtonBar,
    TVpTaskList,
    TVpNotificationDialog,
    TVpResourceEditDialog,
    TVpEventEditDialog,
    TVpContactEditDialog,
    TVpTaskEditDialog,
    TVpPrintFormatEditDialog,
    TVpPrintPreviewDialog]);
end;

end.
