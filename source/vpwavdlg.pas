{*********************************************************}
{*                   VPWAVDLG.PAS 1.03                   *}
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

unit VpWavDlg;

{$I vp.inc}

interface

{$WARNINGS OFF} {Some of this stuff in here isn't platform friendly}

uses
  {$IFDEF LCL}
  LCLProc, LCLType, LCLIntf, LResources,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, VpBase, ComCtrls, ShellCtrls;

type

  { TFrmSoundDialog }

  TFrmSoundDialog = class(TForm)
    Label3: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    ButtonPanel: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    RightPanel: TPanel;
    Label4: TLabel;
    ShellListView: TShellListView;
    ShellTreeView: TShellTreeView;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    PlayButton: TSpeedButton;
    CBDefault: TCheckBox;
    OkBtn: TButton;
    CancelBtn: TButton;
    procedure CancelBtnClick(Sender: TObject);
    procedure CBDefaultClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure PlayButtonClick(Sender: TObject);
  private
    FOnPlaySound: TVpPlaySoundEvent;
    function FindFileItem(AFilename: String): TListItem;
    procedure PlaySound;
    procedure StopSound;
  public
    DingPath: string;
    MediaFolder: String;
    function GetSelectedFileName: String;
    procedure Populate;
    property OnPlaySound: TVpPlaySoundEvent read FOnPlaySound write FOnPlaySound;
  end;


implementation

uses
  Math,
  VpSR, VpMisc;

{$IFDEF LCL}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

procedure TFrmSoundDialog.CancelBtnClick(Sender: TObject);
begin
  StopSound;
end;

procedure TFrmSoundDialog.CBDefaultClick(Sender: TObject);
begin
  ShellTreeview.Visible := not CBDefault.Checked;
  ShellListview.Visible := not CBDefault.Checked;
  Panel3.Visible := CBDefault.Checked;
  Label4.Visible := CBDefault.Checked;
  PlayButton.Visible := not CBDefault.Checked;
end;
{=====}

function TFrmSoundDialog.FindFileItem(AFileName: String): TListItem;
var
  i: Integer;
begin
  AFileName := ExtractFileName(AFileName);
  for i:=0 to ShellListview.Items.Count-1 do
    if ShellListview.Items[i].Caption = AFilename then begin
      Result := ShellListview.Items[i];
      exit;
    end;
  Result := nil;
end;

procedure TFrmSoundDialog.FormCreate(Sender: TObject);
begin
  Panel3.Align := alClient;
  Panel4.Align := alLeft;
  ShellTreeView.Align := alClient;
  Label4.Align := alClient;
end;
{=====}

function TFrmSoundDialog.GetSelectedFileName: String;
begin
  if ShellListview.Selected <> nil then
    Result := IncludeTrailingPathDelimiter(ShellTreeView.Path) + ShellListview.Selected.Caption
  else
    Result := '';
end;

procedure TFrmSoundDialog.OkBtnClick(Sender: TObject);
begin
  StopSound;
end;

procedure TFrmSoundDialog.PlayButtonClick(Sender: TObject);
begin
  DingPath := GetSelectedFileName;
  PlaySound;
end;
{=====}

procedure TFrmSoundDialog.PlaySound;
begin
  if Assigned(FOnPlaySound) then
    FOnPlaySound(self, DingPath, psmAsync);
end;

procedure TFrmSoundDialog.Populate;
var
  DIST: Integer = 8;
  VDIST: Integer = 8;
  HBORDER: Integer = 8;
begin
  TabSheet1.Caption := RSSelectASound;
  Self.Caption := RSSoundFinder;
  CBDefault.Caption := RSDefaultSound;
  OkBtn.Caption := RSOkBtn;
  CancelBtn.Caption := RSCancelBtn;
  Label3.Caption := RSNothingToSelectFrom;
  Label4.Caption := RSNothingToSelectFrom;

  DIST := ScaleX(DIST, DesignTimeDPI);
  VDist := ScaleY(VDist, DesignTimeDPI);
  HBORDER := ScaleX(HBORDER, DesignTimeDPI);

  OKBtn.Height := ScaleX(OKBtn.Height, DesignTimeDPI);
  CancelBtn.Height := OKBtn.Height;
  ButtonPanel.Height := VDist + OKBtn.Height + VDist;
  OKBtn.Top := VDist;
  CancelBtn.Top := VDist;
  PlayButton.Top := (ButtonPanel.Height - PlayButton.Height) div 2;

  OKBtn.Width := Max(GetButtonWidth(OKBtn), GetButtonWidth(CancelBtn));
  CancelBtn.Width := OKBtn.Width;
 {$IFDEF MSWINDOWS}
  CancelBtn.Left := ButtonPanel.ClientWidth - HBORDER - CancelBtn.Width;
  OKBtn.Left := CancelBtn.Left - DIST - OKBtn.Width;
  OKBtn.TabOrder := 0;
  CancelBtn.TabOrder := 1;
 {$ELSE}
  OKBtn.Left := ButtonPanel.ClientWidth - HBORDER - OKBtn.Width;
  CancelBtn.Left := OKBtn.Left - DIST - CancelBtn.Width;
  CancelBtn.TabOrder := 0;
  OKBtn.TabOrder := 1;
 {$ENDIF}
  if DingPath = '' then begin
    CBDefault.Checked := true;
    if (MediaFolder <> '') and DirectoryExists(MediaFolder) then
      ShellTreeView.Path := MediaFolder;
  end else
  if FileExists(DingPath) then begin
    ShellTreeview.Path := ExtractFileDir(DingPath);
    ShellListview.Selected := FindFileItem(DingPath);
  end else begin
    ShellTreeView.Path := MediaFolder;
  end;
  CBDefaultClick(nil);
end;
{=====}

procedure TFrmSoundDialog.StopSound;
begin
  if Assigned(FOnPlaySound) then
    FOnPlaySound(self, '', psmStop);
end;

end.
  
