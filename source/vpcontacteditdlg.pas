{*********************************************************}
{*              VPCONTACTEDITDLG.PAS 1.03                *}
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

unit VpContactEditDlg;

interface

uses
  {$IFDEF LCL}
  LCLProc, LCLType, LCLIntf, LResources,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils,
  {$IFDEF VERSION6} Variants, {$ENDIF}
  Classes, Graphics, Controls, Forms, Dialogs, VpData, ExtCtrls, StdCtrls,
  VpException, VpMisc, VpBase, VpSR, VpDlg, VpBaseDS, ComCtrls, EditBtn;

type
  { forward declarations }
  TVpContactEditDialog = class;

  { TContactEditForm }

  TContactEditForm = class(TForm)
    BirthdateEdit: TDateEdit;
    BirthdateLbl: TLabel;
    FirstNameEdit: TEdit;
    FirstNameLbl: TLabel;
    tsContacts: TPageControl;
    tabMain: TTabSheet;
    LastNameLbl: TLabel;
    AddrLbl: TLabel;
    CityLbl: TLabel;
    StateLbl: TLabel;
    ZipLbl: TLabel;
    CountryLbl: TLabel;
    PositionLbl: TLabel;
    TitleLbl: TLabel;
    CompanyLbl: TLabel;
    LastNameEdit: TEdit;
    AddressEdit: TEdit;
    CityEdit: TEdit;
    StateEdit: TEdit;
    ZipCodeEdit: TEdit;
    PositionEdit: TEdit;
    TitleEdit: TEdit;
    CompanyEdit: TEdit;
    cboxCategory: TComboBox;
    cboxCountry: TComboBox;
    cboxState: TComboBox;
    edtCountry: TEdit;
    tabContact: TTabSheet;
    tabCustom: TTabSheet;
    pnlBottom: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    tabNotes: TTabSheet;
    NoteEdit: TMemo;
    CustomLbl1: TLabel;
    CustomLbl2: TLabel;
    CustomLbl3: TLabel;
    CustomLbl4: TLabel;
    Custom1Edit: TEdit;
    Custom2Edit: TEdit;
    Custom3Edit: TEdit;
    Custom4Edit: TEdit;
    cboxPhoneLbl1: TComboBox;
    cboxPhoneLbl2: TComboBox;
    cboxPhoneLbl3: TComboBox;
    cboxPhoneLbl4: TComboBox;
    Phone4Edit: TEdit;
    Phone3Edit: TEdit;
    Phone2Edit: TEdit;
    Phone1Edit: TEdit;
    cboxPhoneLbl5: TComboBox;
    Phone5Edit: TEdit;
    EMailLbl: TLabel;
    EMailEdit: TEdit;
    CategoryLbl: TLabel;
    procedure cboxCountryChange(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure ItemChanged(Sender: TObject);
    procedure tsContactsChange(Sender: TObject);
  private
    procedure SetCaptions;
    procedure DisplayCurrentCountry;
    procedure ResizeControls;
  public
    Resource: TVpResource;
    Contact: TVpContact;
    ReturnCode: TVpEditorReturnCode;
    ControlLink: TVpControlLink;
    procedure PopulateSelf;
    procedure DePopulateSelf;
    procedure ArrangeControls;
  end;

  TVpContactEditDialog = class(TVpBaseDialog)
  protected {private}
    ceEditDlg: TContactEditForm;
    ceContact: TVpContact;
    ceResource: TVpResource;
  public
    function Execute(Contact: TVpContact): Boolean; reintroduce;
    function AddNewContact: Boolean;
  published
    {properties}
    property ControlLink;
    property DataStore;
    property Placement;                  
  end;


implementation

{$IFDEF LCL}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

uses
  VpConst;

{ Utility functions }

function Max(const a, b: Integer): Integer;
begin
  if a >= b then
    Result := a
  else
    Result := b;
end;


{ TContactEditForm }

procedure TContactEditForm.FormCreate(Sender: TObject);
begin
  ReturnCode := rtAbandon;
  SetCaptions;
end;

procedure TContactEditForm.SetCaptions;
begin
  tabMain.Caption := RSMasterData;
  tabContact.Caption := RSDlgContactEdit;
  tabCustom.Caption := RSCustom;
  tabNotes.Caption := RSNotes;

  OkBtn.Caption := RSOKBtn;
  CancelBtn.Caption := RSCancelBtn;
  LastNameLbl.Caption := RSLastNameLbl;
  FirstNameLbl.Caption := RSFirstNameLbl;
  TitleLbl.Caption := RSTitleLbl;
  AddrLbl.Caption := RSAddressLbl;
  CityLbl.Caption := RSCityLbl;
  StateLbl.Caption := RSStateLbl;
  ZipLbl.Caption := RSZipCodeLbl;
  CountryLbl.Caption := RSCountryLbl;
  CompanyLbl.Caption := RSCompanyLbl;
  PositionLbl.Caption := RSPositionLbl;
  CategoryLbl.Caption := RSCategoryLbl;
  BirthdateLbl.Caption := RSBirthDateLbl;
  EmailLbl.Caption := RSEmail;
  CustomLbl1.Caption := RSCustom1;
  CustomLbl2.Caption := RSCustom2;
  CustomLbl3.Caption := RSCustom3;
  CustomLbl4.Caption := RSCustom4;
end;

procedure TContactEditForm.OKBtnClick(Sender: TObject);
begin
  if LastNameEdit.Text = '' then begin
    raise EVpContactEditError.Create(RSNameIsRequired);
    exit;
  end;
  ReturnCode := rtCommit;
  Close;
end;

procedure TContactEditForm.CancelBtnClick(Sender: TObject);
begin
  ReturnCode := rtAbandon;
  Close;
end;

procedure TContactEditForm.DePopulateSelf;
begin
//  ParseName(Contact, LastNameEdit.Text);
  Contact.LastName := LastNameEdit.Text;
  Contact.FirstName := FirstNameEdit.Text;
  Contact.Address := AddressEdit.Text;
  Contact.City := CityEdit.Text;
  if cboxState.Visible then
    Contact.State := cboxState.Text
  else
    Contact.State := StateEdit.Text;
  Contact.Zip := ZipCodeEdit.Text;
  Contact.Job_Position := PositionEdit.Text;
  Contact.Title := TitleEdit.Text;
  Contact.EMail := EMailEdit.Text;
  Contact.Company := CompanyEdit.Text;
  Contact.Birthdate := BirthdateEdit.Date;
  Contact.Phone1 := Phone1Edit.Text;
  Contact.Phone2 := Phone2Edit.Text;
  Contact.Phone3 := Phone3Edit.Text;
  Contact.Phone4 := Phone4Edit.Text;
  Contact.Phone5 := Phone5Edit.Text;
  if cboxCountry.Visible then
    Contact.Country := cboxCountry.Text
  else
    Contact.Country := edtCountry.Text;
  Contact.Notes := NoteEdit.Text;
  Contact.Category := cboxCategory.ItemIndex;
  Contact.Custom1 := Custom1Edit.Text;
  Contact.Custom2 := Custom2Edit.Text;
  Contact.Custom3 := Custom3Edit.Text;
  Contact.Custom4 := Custom4Edit.Text;

  Contact.PhoneType1 := cboxPhoneLbl1.ItemIndex;
  Contact.PhoneType2 := cboxPhoneLbl2.ItemIndex;
  Contact.PhoneType3 := cboxPhoneLbl3.ItemIndex;
  Contact.PhoneType4 := cboxPhoneLbl4.ItemIndex;
  Contact.PhoneType5 := cboxPhoneLbl5.ItemIndex;

  Contact.Category := cboxCategory.ItemIndex;
end;

procedure TContactEditForm.PopulateSelf;
var
  CurCountry: Integer;
  pt: TVpPhoneType;
  ct: TVpCategoryType;

begin
  LastNameEdit.Text := Contact.LastName;
  FirstNameEdit.Text := Contact.FirstName; //AssembleName(Contact);
  AddressEdit.Text := Contact.Address;
  CityEdit.Text := Contact.City;
  ZipCodeEdit.Text := Contact.Zip;
  PositionEdit.Text := Contact.Job_Position;
  TitleEdit.Text := Contact.Title;
  EMailEdit.Text := Contact.EMail;
  CompanyEdit.Text := Contact.Company;
  Phone1Edit.Text := Contact.Phone1;
  Phone2Edit.Text := Contact.Phone2;
  Phone3Edit.Text := Contact.Phone3;
  Phone4Edit.Text := Contact.Phone4;
  Phone5Edit.Text := Contact.Phone5;
  NoteEdit.Text := Contact.Notes;
  cboxCategory.ItemIndex := Contact.Category;
  Custom1Edit.Text := Contact.Custom1;
  Custom2Edit.Text := Contact.Custom2;
  Custom3Edit.Text := Contact.Custom3;
  Custom4Edit.Text := Contact.Custom4;
  cboxCountry.Text := Contact.Country;
  edtCountry.Text := Contact.Country;
  if (Contact.Country = '') and (Assigned (ControlLink)) then begin
    if ControlLink.DefaultCountry <> '' then begin
      cboxCountry.Text := ControlLink.DefaultCountry;
      edtCountry.Text := ControlLink.DefaultCountry;
    end else begin
      CurCountry := ControlLink.Localization.GetCurrentCountry;
      if CurCountry >= 0 then begin
        cboxCountry.Text := ControlLink.Localization.Countries.Items[CurCountry].Name;
        edtCountry.Text := ControlLink.Localization.Countries.Items[CurCountry].Name;
      end;
    end;
  end;
  StateEdit.Text := Contact.State;
  cboxState.Text := Contact.State;
  if Contact.Birthdate = 0.0 then
    BirthdateEdit.Clear else
    BirthdateEdit.Date := Contact.Birthdate;

  for pt := Low (TVpPhoneType) to High (TVpPhoneType) do begin
    cboxPhoneLbl1.Items.Add(PhoneLabel(pt));
    cboxPhoneLbl2.Items.Add(PhoneLabel(pt));
    cboxPhoneLbl3.Items.Add(PhoneLabel(pt));
    cboxPhoneLbl4.Items.Add(PhoneLabel(pt));
    cboxPhoneLbl5.Items.Add(PhoneLabel(pt));
  end;
  cboxPhoneLbl1.ItemIndex := Contact.PhoneType1;
  cboxPhoneLbl2.ItemIndex := Contact.PhoneType2;
  cboxPhoneLbl3.ItemIndex := Contact.PhoneType3;
  cboxPhoneLbl4.ItemIndex := Contact.PhoneType4;
  cboxPhoneLbl5.ItemIndex := Contact.PhoneType5;

  for ct := Low (TVpCategoryType) to High (TVpCategoryType) do
    cboxCategory.Items.Add(CategoryLabel(ct));
  cboxCategory.ItemIndex := Contact.Category;

  DisplayCurrentCountry;
end;

procedure TContactEditForm.ItemChanged(Sender: TObject);
begin
  Contact.Changed := true;

  { if there is a comma in the LastNameEdit, then it is assumed that the name is  }
  { formatted as last, first. Since the comma & space aren't actually part of }
  { the name, we need to allow two extra characters in the namefield's width. }
  if Pos(',', LastNameEdit.Text) > 0 then
    LastNameEdit.MaxLength := 102
  else
    LastNameEdit.MaxLength := 100;
end;

procedure TContactEditForm.ArrangeControls;
begin
  edtCountry.Left := cboxCountry.Left;
  StateEdit.Left := cboxState.Left;

  if (not Assigned (ControlLink)) or
     (ControlLink.Localization.Countries.Count = 0) then begin
    edtCountry.Visible := True;
    CountryLbl.FocusControl := edtCountry;
    cboxCountry.Visible := False;
    StateEdit.Visible := True;
    StateLbl.FocusControl := StateEdit;
    cboxState.Visible := False;
  end
  else begin
    ControlLink.Localization.CountriesToTStrings(cboxCountry.Items);
    CountryLbl.FocusControl := cboxCountry;
    cboxCountry.Visible := True;
    edtCountry.Visible := False;
    StateLbl.FocusControl := cboxState;
    cboxState.Visible := True;
    StateEdit.Visible := False;
  end;
  tsContacts.ActivePage := tabMain;
end;

procedure TContactEditForm.ResizeControls;
const
  ComboArrowWidth  = 32;
//  FieldVertSep     = 25;
//  FormRightBorder  = 20;
//  MinFormWidth     = 265;
//  FormHeightOffset = 103;
//  MinFormHeight    = 250;
  TopField         = 8;

type
  TLabelArray = array of TLabel;
  TComboboxArray = array of TCombobox;
  TEditArray = array of TEdit;

var
  Labels: TLabelArray;
  Comboboxes: TComboboxArray;
  Edits: TEditArray;
  LargestLabelWidth: Integer;
  WidestField: Integer;
  i, j: Integer;
  OldFont: TFont;
  FieldTop: Integer;
  delta: Integer;
  corr: Integer;         // difference between form's client width and tabsheet width
  editHeight: Integer;   // DPI-aware height of an edit control
  btnHeight: Integer;    // DPI-aware height of a button control
  vDist: Integer = 4;    // Vertical distance between edits
  hBorder: Integer = 8;  // Distance between container border and label
  dist: Integer = 4;     // Distance between label and edit/combo

begin
  dist := ScaleX(dist , DesignTimeDPI);
  vdist := ScaleY(vdist, DesignTimeDPI);
  hBorder := ScaleX(hBorder, DesignTimeDPI);
  editHeight := ScaleY(LastNameEdit.Height, DesignTimeDPI);
  btnHeight := ScaleY(OKBtn.Height, DesignTimeDPI);

  BirthdateEdit.ButtonWidth := editHeight;

  { Note: The resizing algorithm is dependent upon the labels having their
    FocusControl property set to the corresponding edit field or combobox. }

  SetLength(Labels, 12);
  Labels[0] := LastNameLbl;
  Labels[1] := FirstNameLbl;
  Labels[2] := TitleLbl;
  Labels[3] := AddrLbl;
  Labels[4] := CityLbl;
  Labels[5] := StateLbl;
  Labels[6] := ZipLbl;
  Labels[7] := CountryLbl;
  Labels[8] := CompanyLbl;
  Labels[9] := PositionLbl;
  Labels[10] := CategoryLbl;
  Labels[11] := BirthdateLbl;

  LargestLabelWidth := 0;
  for i := Low(Labels) to High(Labels) do
    LargestLabelWidth := Max(LargestLabelWidth, GetLabelWidth(Labels[i]));

  { Determine width of label based upon dpi of screen }
  for i := Low(Labels) to High(Labels) do begin
    Labels[i].FocusControl.Left := HBorder + LargestLabelWidth + Dist;
    Labels[i].Left := Labels[i].FocusControl.Left - DIST - GetLabelWidth(Labels[i]);
    Labels[i].Top := Labels[i].FocusControl.Top + (Labels[i].FocusControl.Height - Labels[i].Height) div 2;
  end;

  widestField := 250;

  { If localization file is loaded determine the width of the country and state
    comboboxes }
  if cboxCountry.Visible or cboxState.Visible then begin
    OldFont := TFont.Create;
    try
      OldFont.Assign(Canvas.Font);
      if cboxCountry.Visible then begin
        Canvas.Font.Assign(cboxCountry.Font);
        for j := 0 to cboxCountry.Items.Count - 1 do
          widestField := Max(widestField, Canvas.TextWidth(cboxCountry.Items[j]) + ComboArrowWidth);
      end;
      if cboxState.Visible then begin
        Canvas.Font.Assign(cboxCountry.Font);
        for j := 0 to cboxState.Items.Count - 1 do
          widestField := Max(widestfield, Canvas.TextWidth(cboxState.Items[j]) + ComboArrowWidth);
      end;
    finally
      Canvas.Font.Assign(OldFont);
      OldFont.Free;
    end;
  end;

  { Set form width according to widest field }
  corr := ClientWidth - tabMain.ClientWidth;
  ClientWidth := LastNameEdit.Left + widestfield + HBorder + corr;

  { Set edit and combo widths }
  for i:= Low(Labels) to High(Labels) do
    if (Labels[i].FocusControl <> ZipCodeEdit) and
       (Labels[i].FocusControl <> BirthdateEdit)
    then
      Labels[i].FocusControl.Width := widestfield;
  cboxCountry.Width := widestField;
  cboxState.Width := widestField;

  { Vertically arrange the fields. }
  delta := (Labels[0].FocusControl.Height - labels[0].Height) div 2;
  FieldTop := TopField;
  for i := Low(Labels) to High(Labels) do
    if Labels[i].Visible then begin
      Labels[i].FocusControl.Top := FieldTop;
      Labels[i].Top := FieldTop + delta;
      inc(FieldTop, editHeight + VDist);
    end;

  OKBtn.Top := vDist;
  OKBtn.Height := btnHeight;
  CancelBtn.Top := OKBtn.Top;
  CancelBtn.Height := OKBtn.Height;
  pnlBottom.ClientHeight := btnHeight + vDist*2;

  { Set form height such that first tab is filled completely by controls }
  ClientHeight := BirthDateEdit.Top + editHeight + TopField + VDist + // required height of tab sheet
    pnlBottom.Height +  // height of button panel
    tsContacts.Height - tabMain.ClientHeight;   // Height of tab + border

  { Page "Contact" }
  SetLength(Comboboxes, 5);
  Comboboxes[0] := cboxPhoneLbl1;
  Comboboxes[1] := cboxPhoneLbl2;
  Comboboxes[2] := cboxPhoneLbl3;
  Comboboxes[3] := cboxPhoneLbl4;
  Comboboxes[4] := cboxPhoneLbl5;

  SetLength(Edits, 5);
  Edits[0] := Phone1Edit;
  Edits[1] := Phone2Edit;
  Edits[2] := Phone3Edit;
  Edits[3] := Phone4Edit;
  Edits[4] := Phone5Edit;

  largestLabelWidth := GetLabelWidth(EMailLbl);
  OldFont := TFont.Create;
  try
    OldFont.Assign(Canvas.Font);
    Canvas.Font.Assign(cboxPhoneLbl1.Font);
    for i:=0 to cboxPhoneLbl1.Items.Count-1 do
      largestLabelWidth := Max(cboxPhoneLbl1.Canvas.TextWidth(cboxPhoneLbl1.Items[i]) + ComboArrowWidth, largestlabelWidth);
  finally
    Canvas.Font.Assign(OldFont);
    OldFont.Free;
  end;

  FieldTop := TopField;
  for i:=Low(Comboboxes) to High(Comboboxes) do begin
    Comboboxes[i].Left := HBorder;
    Comboboxes[i].Width := largestLabelWidth;
    Comboboxes[i].Top := FieldTop;
    inc(FieldTop, editHeight + VDist);
  end;

  for i:= Low(Edits) to High(Edits) do begin
    Edits[i].Left := cboxPhoneLbl1.Left + cboxPhoneLbl1.Width + DIST;
    Edits[i].Width := ClientWidth - Edits[i].Left - HBorder - corr;
    Edits[i].Top := Comboboxes[i].Top;
  end;

  EMailEdit.Left := Phone1Edit.Left;
  EMailEdit.Width := Phone1Edit.Width;
  EMailEdit.Top := Phone5Edit.Top + editHeight + VDist;
  EMailLbl.Left := EMailEdit.Left - GetLabelWidth(EMailLbl) - Dist;
  EMailLbl.Top := EMailEdit.Top + delta;

  { Page "User-defined" }
  SetLength(Labels, 4);
  Labels[0] := CustomLbl1;
  Labels[1] := CustomLbl2;
  Labels[2] := CustomLbl3;
  Labels[3] := CustomLbl4;

  largestLabelWidth := 0;
  for i := Low(Labels) to High(Labels) do
    largestLabelWidth := Max(largestLabelWidth, GetLabelWidth(Labels[i]));

  FieldTop := TopField;
  for i := Low(Labels) to High(Labels) do begin
    Labels[i].FocusControl.Left := HBorder + LargestLabelWidth + Dist;
    Labels[i].FocusControl.Top := FieldTop;
    Labels[i].FocusControl.Width := ClientWidth - Labels[i].FocusControl.Left - HBorder - corr;
    Labels[i].Width := LargestLabelWidth;
    Labels[i].Left := Labels[i].FocusControl.Left - GetLabelWidth(Labels[i]) - Dist;
    Labels[i].Top := FieldTop + delta;
    inc(FieldTop, editHeight + VDist);
  end;

  OKBtn.Width := Max(GetButtonWidth(OKBtn), GetButtonWidth(CancelBtn));
  CancelBtn.Width := OKBtn.Width;
 {$IFDEF MSWINDOWS}   // button order: OK - Cancel
  CancelBtn.Left := pnlBottom.ClientWidth - HBorder - CancelBtn.Width;
  OKBtn.Left := CancelBtn.Left - Dist - OKBtn.Width;
 {$ELSE}              // button order: Cancel - OK
  OKBtn.Left := pnlBottom.ClientWidth - HBorder - OKBtn.Width;
  CancelBtn.Left := OKBtn.Left - Dist - CancelBtn.Width;
  OKBtn.TabOrder := 1;
 {$ENDIF}
end;

procedure TContactEditForm.DisplayCurrentCountry;
var
  Idx : Integer;
begin
  if not Assigned(ControlLink) then
    Exit;

  Idx := ControlLink.Localization.CountryNameToIndex(cboxCountry.Text);
  if Idx > -1 then begin
    ControlLink.Localization.StatesToTStrings(Idx, cboxState.Items);

    if ControlLink.Localization.Countries.Items[Idx].Address1Visible then begin
      AddrLbl.Visible := True;
      AddressEdit.Visible := True;
      if ControlLink.Localization.Countries.Items[Idx].Address1Caption <> '' then
        AddrLbl.Caption := ControlLink.Localization.Countries.Items[Idx].Address1Caption
      else
        AddrLbl.Caption := RSAddressLbl;
    end else begin
      AddrLbl.Visible := False;
      AddressEdit.Visible := False;
    end;

    if ControlLink.Localization.Countries.Items[Idx].CityVisible then begin
      CityLbl.Visible := True;
      CityEdit.Visible := True;
      if ControlLink.Localization.Countries.Items[Idx].CityCaption <> '' then
        CityLbl.Caption := ControlLink.Localization.Countries.Items[Idx].CityCaption
      else
        CityLbl.Caption := RSCityLbl;
    end else begin
      CityLbl.Visible := False;
      CityEdit.Visible := False;
    end;

    if ControlLink.Localization.Countries.Items[Idx].StatesVisible then begin
      StateLbl.Visible := True;
      if ControlLink.Localization.Countries.Items[Idx].States.Count > 0 then begin
        StateLbl.FocusControl := cboxState;
        cboxState.Visible := True;
        StateEdit.Visible := False;
      end else begin
        StateLbl.FocusControl := StateEdit;
        StateEdit.Visible := True;
        cboxState.Visible := False;
        StateEdit.Left := cboxState.Left;
      end;
      if ControlLink.Localization.Countries.Items[Idx].StateCaption <> '' then
        StateLbl.Caption := ControlLink.Localization.Countries.Items[Idx].StateCaption
      else
        StateLbl.Caption := RSStateLbl;
    end else begin
      StateLbl.Visible := False;
      StateEdit.Visible := False;
      cboxState.Visible := False;
    end;

    if ControlLink.Localization.Countries.Items[Idx].ZipVisible then begin
      ZipLbl.Visible := True;
      ZipCodeEdit.Visible := True;
      if ControlLink.Localization.Countries.Items[Idx].ZipCaption <> '' then
        ZipLbl.Caption := ControlLink.Localization.Countries.Items[Idx].ZipCaption
      else
        ZipLbl.Caption := RSZipCodeLbl;
    end else begin
      ZipLbl.Visible := False;
      ZipCodeEdit.Visible := False;
    end;

  end else begin
    cboxState.Items.Clear;

  end;
  ResizeControls;
end;

procedure TContactEditForm.cboxCountryChange(Sender: TObject);
begin
  StateEdit.Text := '';
  cboxState.Text := '';
  DisplayCurrentCountry;
end;

procedure TContactEditForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Unused(Shift);
  if Key = VK_ESCAPE then begin
    ReturnCode := rtAbandon;
    Close;
  end;
end;

procedure TContactEditForm.tsContactsChange(Sender: TObject);
begin
  if Visible then
    if tsContacts.ActivePage = tabMain then
      LastNameEdit.SetFocus
    else if tsContacts.ActivePage = tabContact then
      Phone1Edit.SetFocus
    else if tsContacts.ActivePage = tabCustom then
      Custom1Edit.SetFocus
    else if tsContacts.ActivePage = tabNotes then
      NoteEdit.SetFocus;
end;

procedure TContactEditForm.FormShow(Sender: TObject);
begin
  if tsContacts.ActivePage = tabMain then
    LastNameEdit.SetFocus;
               (*
  {$IFDEF LCL}
  ScaleDPI(Self, DesigntimeDPI);
  {$ENDIF}
  *)
end;


{ TVpContactEditDialog }

function TVpContactEditDialog.Execute(Contact: TVpContact): Boolean;
var
  EditForm: TContactEditForm;
begin
  ceContact := Contact;
  Result := false;
  Application.CreateForm(TContactEditForm, EditForm);
  try
    DoFormPlacement(EditForm);
    SetFormCaption(EditForm, Contact.FullName, RSDlgContactEdit);
    EditForm.Contact := ceContact;
    EditForm.Resource := DataStore.Resource;
    EditForm.ControlLink := ControlLink;
    EditForm.ArrangeControls;
    EditForm.PopulateSelf;
    EditForm.ShowModal;
    if EditForm.ReturnCode = rtCommit then begin
      EditForm.DePopulateSelf;
      Result := true;
    end;
  finally
    EditForm.Release;
  end;

  if Result then begin
    ceContact.Changed := true;
    DataStore.PostContacts;
    DataStore.NotifyDependents;
  end;
end;

function TVpContactEditDialog.AddNewContact: Boolean;
begin
  result := false;
  if DataStore <> nil then begin
    if DataStore.Resource = nil then
      Exit;
    ceContact := DataStore.Resource.Contacts.AddContact(
      DataStore.GetNextID(ContactsTableName));
    if ceContact <> nil then begin
      Result := Execute(ceContact);
      if Result then
        DataStore.PostContacts
      else
        ceContact.Free;
    end;
  end;
end;

end.

  
