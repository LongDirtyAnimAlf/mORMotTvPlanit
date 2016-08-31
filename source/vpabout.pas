{*********************************************************}
{*                  VPABOUT.PAS 1.03                     *}
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

unit VpAbout;

interface

uses
{$IFDEF LCL}
 LCLProc, LCLType, LCLIntf,
{$ELSE}
 Windows, Messages,
{$ENDIF}
 Forms, Graphics, Controls, Dialogs, StdCtrls, ExtCtrls,
{$IFDEF VERSION6}
 {$IFDEF DELPHI}
 DesignIntf, DesignEditors,
 {$ELSE}
 PropEdits, LazarusPackageIntf,
 {$ENDIF}
{$ELSE}
 DsgnIntf,
{$ENDIF}
 Classes, SysUtils;

type

  { TfrmAbout }
  TfrmAbout = class(TForm)
    Bevel3: TBevel;
    Label4: TLabel;
    Label5: TLabel;
    lblLazForumLink: TLabel;
    lblLazPortLink: TLabel;
    ImagePanel: TPanel;
    Image1: TImage;
    Panel1: TPanel;
    TextPanel: TPanel;
    SupportPanel: TPanel;
    ButtonPanel: TPanel;
    ProgramName: TLabel;
    GeneralNewsgroupsLabel: TLabel;
    lblTurboLink: TLabel;
    lblHelp: TLabel;
    CopyrightLabel: TLabel;
    OKButton: TButton;
    lblGeneralDiscussion: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label1: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure lblLinkMouseEnter(Sender: TObject);
    procedure lblLinkMouseLeave(Sender: TObject);
    procedure lblLinkClick(Sender: TObject);
  private
    { Private declarations }
    procedure PositionControls;
  public
    { Public declarations }
    IsServer : boolean;
  end;

  TVpAboutProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes;
      override;
    procedure Edit;
      override;
  end;

implementation

{$IFDEF LCL}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

uses
{$IFNDEF LCL}
  ShellAPI,
{$ENDIF}
  VpConst, VpMisc, VpSR;

const
  TURBO_LINK_URL = 'http://sourceforge.net/projects/tpvplanit/';
  HELP_URL = 'http://sourceforge.net/forum/forum.php?forum_id=241880';
  {%H-}NEWS_SPECIFIC_URL = 'news://news.turbopower.com/turbopower.public.support.visualplanit';
  GENERAL_DISCUSSION_URL = 'http://sourceforge.net/forum/forum.php?forum_id=241879';
  LAZARUS_PORT_URL = 'http://sourceforge.net/p/lazarus-ccr/svn/HEAD/tree/components/tvplanit';
  LAZARUS_FORUM_URL = 'http://forum.lazarus.freepascal.org';


{*** TVpAboutProperty ***}

function TVpAboutProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

procedure TVpAboutProperty.Edit;
begin
  with TfrmAbout.Create(Application) do begin
    try
      ShowModal;
    finally
      Free;
    end;
  end;
end;


{ FrmAbout }

procedure TfrmAbout.FormActivate(Sender: TObject);
const
{$IFDEF LCL}
  COPYRIGHT = '©';
{$ELSE}
  COPYRIGHT = #169
{$ENDIF}
var
  Year, Junk: Word;
begin
  ProgramName.Caption := VpProductName + ' ' + VpVersionStr;
  DecodeDate(Now, Year, junk, junk);
  CopyrightLabel.Caption := Format('%s Copyright 2000 - %d, TurboPower Software Company and Lazarus team.' +
    LineEnding + 'All rights reserved.',
    [COPYRIGHT, Year]);

  lblTurboLink.Cursor := crHandPoint;
  lblHelp.Cursor := crHandPoint;
  lblGeneralDiscussion.Cursor := crHandPoint;
  lblLazPortLink.Cursor := crHandPoint;
  lblLazForumLink.Cursor := crHandpoint;

  PositionControls;
end;

procedure TfrmAbout.lblLinkMouseEnter(Sender: TObject);
begin
  (Sender as TLabel).Font.Style := [fsUnderline];
end;

procedure TfrmAbout.lblLinkMouseLeave(Sender: TObject);
begin
  (Sender as TLabel).Font.Style := [];
end;

procedure TfrmAbout.lblLinkClick(Sender: TObject);
var
  url: String;
begin
  //  if Sender = lblNewsSpecific then url := NEWS_SPECIFIC_URL else
  if Sender = lblHelp then
    url := HELP_URL
  else
  if Sender = lblGeneralDiscussion then
    url := GENERAL_DISCUSSION_URL
  else
  if Sender = lblTurboLink then
    url := TURBO_LINK_URL
  else
  if Sender = lblLazPortLink then
    url := LAZARUS_PORT_URL
  else
  if Sender = lblLazForumLink then
    url := LAZARUS_FORUM_URL
  else
    exit;
{$IFDEF LCL}
  if not OpenUrl(url)
{$ELSE}
  if ShellExecute(0, 'open', PChar(url), '', '', SW_SHOWNORMAL) <= 32
{$ENDIF}
  then
    ShowMessage(RSBrowserError);
end;

procedure TfrmAbout.PositionControls;
var
  VDIST: Integer = 8;
begin
  VDIST := ScaleY(VDIST, DesignTimeDPI);

  ProgramName.Font.Size := ScaleY(Programname.Font.Size, DesignTimeDPI);
  ClientWidth := GetLabelWidth(LblLazPortLink) + Panel1.Width + 48; //ScaleX(ClientWidth, DesignTimeDPI);
//  ClientHeight := ScaleY(ClientHeight, DesignTimeDPI);

  //SupportPanel.ClientHeight := BottomOf(lblLazForumLink) + VDIST;
  ButtonPanel.ClientHeight :=  CopyRightLabel.Height + 2 * VDIST;

  OKButton.Height := ScaleY(OKButton.Height, DesignTimeDPI);
//  OKButton.Top := (ButtonPanel.Height - OKButton.Height) div 2;
//  CopyrightLabel.Top := (ButtonPanel.Height - CopyrightLabel.Height) div 2;

  //ClientHeight := Max(BottomOf(ImagePanel), BottomOf(SupportPanel)) + VDIST + ButtonPanel.Height;
end;

end.
  
