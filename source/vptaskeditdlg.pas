{*********************************************************}
{*                VPTASKEDITDLG.PAS 1.03                 *}
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

unit VpTaskEditDlg;
  { default task editing dialog }

interface

uses
  {$IFDEF LCL}
  LCLProc, LCLType, LCLIntf, LResources,
  {$ELSE}
  Windows, Messages, VpEdPop, VpDateEdit,
  {$ENDIF}
  SysUtils,
  {$IFDEF VERSION6} Variants, {$ENDIF}
  Classes, Graphics, Controls, Forms, Dialogs, VpData, StdCtrls, ExtCtrls,
  VpBase, VpSR, VpDlg, ComCtrls, EditBtn;

type
  { forward declarations }
  TVpTaskEditDialog = class;

  { TTaskEditForm }

  TTaskEditForm = class(TForm)
    ButtonPanel: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    PageControl1: TPageControl;
    tabTask: TTabSheet;
    DescriptionEdit: TEdit;
    DueDateLbl: TLabel;
    DueDateEdit: TDateEdit;
    CompleteCB: TCheckBox;
    CreatedOnLbl: TLabel;
    CompletedOnLbl: TLabel;
    DetailsMemo: TMemo;
    ResourceNameLbl: TLabel;
    Bevel1: TBevel;
    imgCalendar: TImage;
    imgCompleted: TImage;
    procedure FormCreate(Sender: TObject);
    procedure OnChange(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FReturnCode: TVpEditorReturnCode;
    FTask: TVpTask;
    FResource: TVpResource;
    procedure PositionControls;
  public
    procedure PopulateSelf;
    procedure DePopulateSelf;
    property Task: TVpTask
      read FTask write FTask;
    property Resource: TVpResource
      read FResource write FResource;
    property ReturnCode: TVpEditorReturnCode
      read FReturnCode;
  end;

  TVpTaskEditDialog = class(TVpBaseDialog)
  protected {private}
    teEditDlg: TTaskEditForm;
    teTask: TVpTask;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute(Task: TVpTask): Boolean; reintroduce;
    function AddNewTask: Boolean;
  published
    {properties}
    property DataStore;
    property Options;
    property Placement;
  end;

implementation

uses
  Math, VpMisc;

{$IFDEF LCL}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

{ TTaskEditForm }

procedure TTaskEditForm.FormCreate(Sender: TObject);
begin
  FReturnCode := rtAbandon;
end;
{=====}

procedure TTaskEditForm.DePopulateSelf;
begin
  Task.Description := DescriptionEdit.Text;
  Task.DueDate := DueDateEdit.Date;
  Task.Details := DetailsMemo.Text;
  Task.Complete := CompleteCB.Checked;
  DueDateLbl.Caption := RSDueDate;
end;
{=====}

procedure TTaskEditForm.PopulateSelf;
begin
  ResourceNameLbl.Caption := Resource.Description;
  CompleteCB.Caption := RSTaskComplete;
  DueDateLbl.Caption := RSDueDate;
  OKBtn.Caption := RSOKBtn;
  CancelBtn.Caption := RSCancelBtn;
  TabTask.Caption := RSDlgTaskEdit;

  DescriptionEdit.Text := Task.Description;
  DueDateEdit.Date := Task.DueDate;
  DetailsMemo.Text := Task.Details;
  CompleteCB.Checked := Task.Complete;
  if Task.CompletedOn <> 0 then
    CompletedOnLbl.Caption := RSCompletedOn + ' ' + FormatDateTime(DefaultFormatSettings.ShortDateFormat, Task.CompletedOn)
  else
    CompletedOnLbl.Visible := False;
  CompletedOnLbl.Visible := CompleteCB.Checked;
  CreatedOnLbl.Caption := RSCreatedOn + ' ' + FormatDateTime(DefaultFormatSettings.ShortDateFormat, Task.CreatedOn);

  PositionControls;
end;

procedure TTaskEditForm.PositionControls;
var
  VBevelDist: Integer = 8;  // Distance bevel-to-control
  VDist: Integer = 8;       // Vertical distance between controls
  HDist: Integer = 8;       // Horizontal distance between controls:
  w: Integer;
  cnv: TControlCanvas;
  cb: TCheckbox;
  editHeight: Integer;
begin
  VBevelDist := ScaleY(VBevelDist, DesignTimeDPI);
  VDist := ScaleY(VDist, DesignTimeDPI);
  HDist := ScaleX(HDist, DesignTimeDPI);
  editHeight := ScaleY(DueDateEdit.Height, DesignTimeDPI);

  OKBtn.Height := ScaleY(OKBtn.Height, DesignTimeDPI);
  OKBtn.Top := VDist;
  CancelBtn.Height := OKBtn.Height;
  CancelBtn.Top := OKBtn.Top;
  ButtonPanel.Height := OKBtn.Height + VDIST*2;
  ResourceNameLbl.Font.Size := ScaleY(ResourceNameLbl.Font.Size, DesignTimeDPI);
  ResourceNameLbl.Top := OKBtn.Top + (OKBtn.Height - ScaleY(ResourceNameLbl.Height, DesignTimeDPI)) div 2;

  DueDateEdit.ButtonWidth := ScaleX(DueDateEdit.Height, DesigntimeDPI);
  DueDateEdit.Left := DueDateLbl.Left + GetLabelWidth(DueDateLbl) + HDist;
  cnv := TControlCanvas.Create;
  try
    cnv.Control := DueDateEdit;
    cnv.Font.Assign(DueDateEdit.Font);
    w := cnv.TextWidth(' 99-99-9999 ') + DueDateEdit.ButtonWidth + 10;
  finally
    cnv.Free;
  end;
  DueDateEdit.Width := w;

  if RightOf(DueDateEdit) + 3*HDist > ImgCompleted.Left then begin
    ImgCompleted.Left := RightOf(DueDateEdit) + 3*HDist;
    CompleteCB.Left := RightOf(ImgCompleted) + HDist;
    CompletedOnLbl.Left := CompleteCB.Left;

    cnv := TControlCanvas.Create;
    try
      cnv.Control := CompleteCB;
      cnv.Font.Assign(CompleteCB.Font);
      w := cnv.TextWidth(CompleteCB.Caption) + GetSystemMetrics(SM_CXMENUCHECK);
    finally
      cnv.Free;
    end;
    w := Max(GetlabelWidth(CompletedOnLbl), w);
    ClientWidth := ClientWidth - tabTask.ClientWidth + CompleteCB.Left + w + HDist*2;
  end;

  OKBtn.Width := Max(GetButtonWidth(OKBtn), GetButtonWidth(CancelBtn));
  CancelBtn.Width := OKBtn.Width;
 {$IFDEF MSWINDOWS}
  CancelBtn.Left := ButtonPanel.ClientWidth - ResourcenameLbl.Left - CancelBtn.Width;
  OKBtn.Left := CancelBtn.Left - HDist - OKBtn.Width;
  OKBtn.TabOrder := 0;
  CancelBtn.TabOrder := 1;
 {$ELSE}
  OKBtn.Left := ButtonPanel.ClientWidth - ResourcenameLbl.Left - OKBtn.Width;
  CancelBtn.Left := OKBtn.Left - HDist - CancelBtn.Width;
  CancelBtn.TabOrder := 0;
  OKBtn.TabOrder := 1;
 {$ENDIF}

  Bevel1.Top := DescriptionEdit.Top + editHeight + VBevelDist; //BottomOf(DescriptionEdit) + VBevelDist;

  ImgCalendar.Top := Bevel1.Top + 2 + VBevelDist;
  ImgCompleted.Top := ImgCalendar.Top;
  DueDateEdit.Top := ImgCalendar.Top; // + (ImgCalendar.Height - DueDateEdit.Height) div 2;
  DueDateLbl.Top := DueDateEdit.Top + (DueDateEdit.Height - DueDateLbl.Height) div 2;
  CompleteCB.Top := ImgCompleted.Top; // + (ImgCompleted.Height - CompleteCB.Height) div 2;

  CreatedOnLbl.Top := DueDateEdit.Top + editHeight + VDist; //BottomOf(DueDateEdit) + VDist;
  CompletedOnLbl.Top := CreatedOnLbl.Top;

  DetailsMemo.Top := BottomOf(CreatedOnLbl) + VBevelDist;
  DetailsMemo.Height :=  tabTask.ClientHeight - DetailsMemo.Top - DescriptionEdit.Top;
end;
{=====}

procedure TTaskEditForm.OnChange(Sender: TObject);
begin
  Task.Changed := true;
end;

{=====}

procedure TTaskEditForm.OKBtnClick(Sender: TObject);
begin
  FReturnCode := rtCommit;
  Close;
end;
{=====}

procedure TTaskEditForm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;
{=====}

procedure TTaskEditForm.FormShow(Sender: TObject);
begin
  DescriptionEdit.SetFocus;
end;
{=====}

{ TVpTaskEditDialog }

constructor TVpTaskEditDialog.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FPlacement.Height := 340;
  FPlacement.Width := 545;
end;

function TVpTaskEditDialog.Execute(Task: TVpTask): Boolean;
var
  TaskEditForm: TTaskEditForm;
begin
  Result := false;
  teTask := Task;
  if (teTask <> nil) and (DataStore <> nil) and (DataStore.Resource <> nil) then
  begin
    Application.CreateForm(TTaskEditForm, TaskEditForm);
    try
      DoFormPlacement(TaskEditForm);
      SetFormCaption(TaskEditForm, Task.Description, RSDlgTaskEdit);
      TaskEditForm.Task := Task;
      TaskEditForm.Resource := DataStore.Resource;
      TaskEditForm.PopulateSelf;
      TaskEditForm.ShowModal;
      Result := (TaskEditForm.ReturnCode = rtCommit);
      Task.Changed := Result;
      if Result then begin
        TaskEditForm.DePopulateSelf;
//        DataStore.PostTasks;
//        DataStore.NotifyDependents;
      end;
    finally
      TaskEditForm.Release;
    end;
  end;
end;
{=====}

function TVpTaskEditDialog.AddNewTask: Boolean;
begin
  result := false;
  if DataStore <> nil then begin
    teTask := DataStore.Resource.Tasks.AddTask(DataStore.GetNextID('Tasks'));
    if teTask <> nil then begin
      Result := Execute(teTask);
      if not Result then
        teTask.Free;
    end;
  end;
end;
{=====}

end.
  
