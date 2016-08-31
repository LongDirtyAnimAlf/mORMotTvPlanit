{*********************************************************}
{*                VPEDFMT.PAS 1.03                       *}
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

{$I vp.inc}

unit VpEdFmt;

interface

uses
  {$IFDEF LCL}
  LCLProc, LCLType, LCLIntf,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, TypInfo, ComCtrls,
  VpPrtFmt;

type

  { TfrmEditFormat }

  TfrmEditFormat = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    edDescription: TEdit;
    edName: TEdit;
    LblIncrement: TLabel;
    LblDescription: TLabel;
    LblName: TLabel;
    rgDayIncrement: TRadioGroup;
    udIncrement: TUpDown;
    edIncrement: TEdit;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure PositionControls;
    procedure SetCaptions;
  protected
    procedure SaveData(AFormat: TVpPrintFormatItem);
    procedure SetData(AFormat: TVpPrintFormatItem);
    function Validate: Boolean;
  public
    function Execute(AFormat: TVpPrintFormatItem) : Boolean;
  end;


implementation

{$IFDEF LCL}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

uses
  Math, VpMisc, VpSR;

{ TfrmEditLayout }

procedure TfrmEditFormat.FormShow(Sender: TObject);
begin
  PositionControls;
  edName.SetFocus;
end;
{=====}
procedure TfrmEditFormat.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;
{=====}
procedure TfrmEditFormat.btnOkClick(Sender: TObject);
begin
  if Validate then
    ModalResult := mrOk
  else begin
    ShowMessage('Please supply a Format Name');
    edName.SetFocus;
    Exit;
  end;
end;
{=====}
function TfrmEditFormat.Execute(AFormat: TVpPrintFormatItem) : Boolean;
begin
  SetData(AFormat);
  Result := ShowModal = mrOk;
  if Result then
    SaveData(AFormat);
end;

procedure TfrmEditFormat.FormCreate(Sender: TObject);
begin
  SetCaptions;
end;

{=====}
procedure TfrmEditFormat.SaveData(AFormat: TVpPrintFormatItem);
var
  EnumVal : Integer;
begin
  AFormat.FormatName := edName.Text;
  AFormat.Description := edDescription.Text;
  AFormat.DayInc := udIncrement.Position;

  EnumVal := GetEnumValue(TypeInfo(TVpDayUnits), 'du' + rgDayIncrement.Items[rgDayIncrement.ItemIndex]);
  if EnumVal > -1 then
    AFormat.DayIncUnits := TVpDayUnits(EnumVal)
  else
    AFormat.DayIncUnits := duDay;
end;

procedure TfrmEditFormat.SetCaptions;
begin
  Caption := RSEditFormatCaption;
  LblName.Caption := RSNameLbl;
  LblDescription.Caption := RSDescriptionLbl;
  LblIncrement.Caption := RsTimeIncLbl;
  rgDayIncrement.Caption := RsTimeIncUnits;
  rgDayIncrement.Items[0] := RSDays;
  rgDayIncrement.Items[1] := RSWeeks;
  rgDayIncrement.Items[2] := RSMonths;
  rgDayIncrement.Items[3] := RSYears;
  btnOK.Caption := RSOKBtn;
  btnCancel.Caption := RSCancelBtn;
end;

procedure TfrmEditFormat.PositionControls;
var
  DELTA: integer = 8;
  margin: Integer = 8;
  vdist: Integer = 4;
var
  w, h: Integer;
  dummyRB: TRadioButton;
  editHeight: Integer;
  btnHeight: Integer;
begin
  // Fix edit and button heights at higher dpi
  with TEdit.Create(self) do
  try
    Parent := self;
    editHeight := Height;
  finally
    Free;
  end;

  btnHeight := ScaleY(btnOK.Height, DesignTimeDPI);

  DELTA := ScaleX(DELTA, DesignTimeDPI);
  MARGIN := ScaleX(MARGIN, DesignTimeDPI);
  VDIST := ScaleY(VDIST, DesignTimeDPI);

  w := MaxValue([GetLabelWidth(LblName), GetLabelWidth(LblDescription), GetLabelWidth(LblIncrement)]);
  edName.Left := margin + w + DELTA;
  edDescription.Left := edName.Left;
  edDescription.Width := edName.Width;
  edIncrement.Left := edName.Left;
  udIncrement.Left := edIncrement.Left + edIncrement.Width;
  LblName.Left := edName.Left - GetLabelWidth(LblName) - DELTA;
  LblDescription.Left := edDescription.Left - GetLabelWidth(lblDescription) - DELTA;
  lblIncrement.Left := edIncrement.Left - GetLabelWidth(lblIncrement) - DELTA;

  ClientWidth := MARGIN + w + DELTA + edName.Width + MARGIN;
  rgDayIncrement.Left := MARGIN;
  rgDayIncrement.Width := ClientWidth - 2*MARGIN;

  w := Max(GetButtonWidth(btnOK), GetButtonWidth(btnCancel));
  btnOK.Width := w;
  btnCancel.Width := w;
 {$IFDEF MSWINDOWS}
  btnCancel.Left := RightOf(rgDayIncrement) - btnCancel.Width;
  btnOK.Left := btnCancel.Left - DELTA - btnOK.Width;
  btnOK.TabOrder := rgDayIncrement.TabOrder + 1;
  btnCancel.TabOrder := btnOK.TabOrder + 1;
 {$ELSE}
  btnOK.Left := RightOf(rgDayIncrement) - btnOK.Width;
  btnCancel.Left := btnOK.Left - DELTA - btnCancel.Width;
  btnCancel.TabOrder := rgDayIncrement.TabOrder + 1;
  btnOK.TabOrder := btnCancel.TabOrder + 1;
 {$ENDIF}

  edName.Height := editHeight;
  edDescription.Height := editHeight;
  edIncrement.Height := editHeight;
  udIncrement.Height := editHeight;

  edDescription.Top := BottomOf(edName) + VDIST;
  lblDescription.Top := edDescription.Top + (edDescription.Height - lblDescription.Height) div 2;
  edIncrement.Top := BottomOf(edDescription) + VDIST;
  udIncrement.Top := edIncrement.Top;
  lblIncrement.top := edIncrement.Top + (edIncrement.Height - lblIncrement.Height) div 2;
  rgDayIncrement.Top := BottomOf(edIncrement) + VDISt + VDIST;

  DummyRB := TRadioButton.Create(self);
  DummyRB.Parent := self;
  h := DummyRB.Height;
  DummyRB.Free;

  rgdayIncrement.Height := h + 2*LblName.Height;
  btnOK.Height := btnHeight;
  btnCancel.Height := btnHeight;
  btnOK.Top := Bottomof(rgDayIncrement) + MARGIN;
  btnCancel.Top := btnOK.Top;

  ClientHeight := Bottomof(btnOK) + VDIST*2;
end;

procedure TfrmEditFormat.SetData(AFormat: TVpPrintFormatItem);
var
  IncName : string;
begin
  edName.Text := AFormat.FormatName;
  edDescription.Text := AFormat.Description;
  udIncrement.Position := AFormat.DayInc;

  IncName := GetEnumName(TypeInfo(TVpDayUnits), Ord(AFormat.DayIncUnits));
  if IncName <> '' then begin
    rgDayIncrement.ItemIndex := rgDayIncrement.Items.IndexOf(Copy(IncName, 3, Length(IncName) - 2));
  end
  else
    rgDayIncrement.ItemIndex := 0;
end;
{=====}
function TfrmEditFormat.Validate : Boolean;
begin
  Result := edName.Text <> '';
end;
{=====}



end.
  
