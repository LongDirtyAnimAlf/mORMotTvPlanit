{*********************************************************}
{*                VPRESEDITDLG.PAS 1.03                  *}
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

unit VpResEditDlg;

interface

uses
  {$IFDEF LCL}
  LCLProc, LCLType, LCLIntf, LResources,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils,
  {$IFDEF VERSION6} Variants, {$ENDIF}
  Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  VpDlg, VpBase, VpData, ComCtrls, VpConst;

type
  { forward declarations }
  TVpResourceEditDialog = class;

  TResEditForm = class(TForm)
    pnlBottom: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    pgResource: TPageControl;
    tabResource: TTabSheet;
    DescriptionEdit: TEdit;
    lblDescription: TLabel;
    lblNotes: TLabel;
    NotesMemo: TMemo;
    imgResources: TImage;
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure PositionControls;
    procedure SetControls;
  public
    ReturnCode: TVpEditorReturnCode;
    ResourceChanged: Boolean;
    Resource: TVpResource;
    procedure PopulateSelf;
    procedure DePopulateSelf;
  end;

  TVpResourceEditDialog = class(TVpBaseDialog)
  protected {private}
    reEditDlg: TResEditForm;
    reResource: TVpResource;
    function Show: Boolean;
  public
    function Execute: Boolean; reintroduce;
    function AddNewResource: Boolean;
  published
    {properties}
    property DataStore;
    property Options;
    property Placement;
  end;

function ExecuteResourceDlg(Resource: TVpResource): Boolean;

implementation

{$IFDEF LCL}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

uses
  Math,
  vpSR, vpMisc;

function ExecuteResourceDlg(Resource: TVpResource): Boolean;
var
  EditForm: TResEditForm;
begin
  result := false;

  if Resource = nil then
    Exit;

  Application.CreateForm(TResEditForm, EditForm);
  EditForm.Resource := Resource;
  EditForm.PopulateSelf;
  EditForm.ShowModal;
  if EditForm.ReturnCode = rtCommit then begin
    EditForm.DePopulateSelf;
    result := true;
  end;
  EditForm.Release;
end;
{=====}

{ TVpResourceEditDialog }

function TVpResourceEditDialog.AddNewResource: Boolean;
var
  Res: TVpResource;
  ResName: string;
begin
  result := false;
  if DataStore <> nil then begin
    Res := DataStore.Resources.AddResource(DataStore.GetNextID(ResourceTableName));
    if Res <> nil then begin
      Res.Changed := true;
      reResource := Res;
      result := Show;

      if Result then begin
        ResName := Res.Description;
        DataStore.PostResources;
        DataStore.Load;
        DataStore.SetResourceByName(ResName);
      end else
        Res.Free;
    end;
  end;
end;
{=====}

function TVpResourceEditDialog.Show: Boolean;
var
  EditForm: TResEditForm;
begin
  result := false;
  Application.CreateForm(TResEditForm, EditForm);
  try
    DoFormPlacement(EditForm);
    EditForm.Resource := reResource;
    EditForm.PopulateSelf;
    EditForm.ShowModal;
    if EditForm.ReturnCode = rtCommit then begin
      EditForm.DePopulateSelf;
      DataStore.NotifyDependents;
      result := true;
    end;
  finally
    EditForm.Release;
  end;
end;
{=====}

function TVpResourceEditDialog.Execute: Boolean;
begin
  result := false;
  if (DataStore <> nil) and (DataStore.Resource <> nil) then begin
    reResource := DataStore.Resource;

    result := Show;

    if result then begin
      reResource.Changed := true;
      DataStore.PostResources;
    end;
  end;
end;
{=====}

{ TResEditForm }

procedure TResEditForm.DePopulateSelf;
begin
  Resource.Description := DescriptionEdit.Text;
  Resource.Notes := NotesMemo.Text;
end;
{=====}

procedure TResEditForm.PopulateSelf;
begin
  DescriptionEdit.Text := Resource.Description;
  NotesMemo.Text := Resource.Notes;

  Caption := RSDlgResEditCaption;
  tabResource.Caption := RSResource;
  lblDescription.Caption := RSDlgResEditDescription;
  lblNotes.Caption := RSNotes;
  OKBtn.Caption := RSOKBtn;
  CancelBtn.Caption := RSCancelBtn;

  PositionControls;
end;
{=====}

procedure TResEditForm.PositionControls;
var
  HDelta: Integer = 8;
  VDelta: Integer = 8;
begin
  HDelta := ScaleX(HDelta, DesignTimeDPI);
  VDelta := ScaleY(VDelta, DesignTimeDPI);

  DescriptionEdit.Left := lblDescription.Left + GetLabelWidth(lblDescription) + HDelta;
  DescriptionEdit.Width := imgResources.Left - 2*HDelta - DescriptionEdit.Left;
  DescriptionEdit.Top := imgResources.Top; // + (imgResources.Height - DescriptionEdit.Height) div 2;
  lblDescription.Top := DescriptionEdit.Top + (DescriptionEdit.Height - lblDescription.Height) div 2;

  lblNotes.Top := BottomOf(DescriptionEdit) + VDelta;
  NotesMemo.Top := BottomOf(lblNotes) + VDelta;
  ClientHeight := ClientHeight + ScaleY(NotesMemo.Height, DesignTimeDPI) - NotesMemo.Height;
  NotesMemo.Height := tabResource.ClientHeight - NotesMemo.Top - VDelta;

  OKBtn.Height := ScaleY(OKBtn.Height, DesignTimeDPI);
  CancelBtn.Height := OKBtn.Height;
  pnlBottom.Height := VDelta + OKBtn.Height + VDelta;
  OKBtn.Top := VDelta;
  CancelBtn.Top := VDelta;

  OKBtn.Width := Max(GetButtonWidth(OKBtn), GetButtonWidth(CancelBtn));
  CancelBtn.Width := OKBtn.Width;
 {$IFDEF MSWINDOWS}
  CancelBtn.Left := pnlBottom.ClientWidth - lblDescription.Left - CancelBtn.Width;
  OKBtn.Left := CancelBtn.Left - OKBtn.Width - HDelta; // - (ClientWidth - tabResource.ClientWidth);
  OKBtn.TabOrder := 0;
  CancelBtn.TabOrder := 1;
 {$ELSE}
  OKBtn.Left := pnlBottom.ClientWidth - lblDescription.Left - OKBtn.Width;
  CancelBtn.Left := OKBtn.Left - CancelBtn.Width - HDelta; // - (ClientWidth - tabResource.ClientWidth);
  CancelBtn.TabOrder := 0;
  OKBtn.TabOrder := 1;
 {$ENDIF}
end;

procedure TResEditForm.OKBtnClick(Sender: TObject);
begin
  if ResourceChanged then
    ReturnCode := rtCommit;
  Close;
end;
{=====}

procedure TResEditForm.FormCreate(Sender: TObject);
begin
  ReturnCode := rtAbandon;
  ResourceChanged := false;
end;
{=====}

procedure TResEditForm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;
{=====}

procedure TResEditForm.Change(Sender: TObject);
begin
  ResourceChanged := true;
  SetControls;
end;
{=====}

{=====}

procedure TResEditForm.FormShow(Sender: TObject);
begin
  DescriptionEdit.SetFocus;
  SetControls;
end;
{=====}

procedure TResEditForm.SetControls;
begin
  OKBtn.Enabled := (DescriptionEdit.Text <> '');
end;

end.
  
