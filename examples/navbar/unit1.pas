unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ColorBox, EditBtn, VpNavBar;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    BtnAddFolder: TButton;
    BtnAddItem: TButton;
    BkColor: TColorBox;
    BtnLoadBkImage: TButton;
    CbPlaySounds: TCheckBox;
    EdBkImage: TFileNameEdit;
    EdSoundFile: TFileNameEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    IconsLbl: TLabel;
    IconsLink: TLabel;
    Panel2: TPanel;
    RgBorderStyle: TRadioGroup;
    RbBkColor: TRadioButton;
    RbBkImage: TRadioButton;
    RbBkImageTile: TRadioButton;
    RbBkImageStretch: TRadioButton;
    RbBkImageNormal: TRadioButton;
    RgIconSize: TRadioGroup;
    Label1: TLabel;
    Images: TImageList;
    Panel1: TPanel;
    RgDrawingStyle: TRadioGroup;
    VpNavBar1: TVpNavBar;
    procedure BackgroundColorChange(Sender: TObject);
    procedure BtnAddFolderClick(Sender: TObject);
    procedure BtnAddItemClick(Sender: TObject);
    procedure BtnLoadBkImageClick(Sender: TObject);
    procedure CbPlaySoundsChange(Sender: TObject);
    procedure EdSoundFileAcceptFileName(Sender: TObject; var Value: String);
    procedure EdSoundFileEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure IconsLinkClick(Sender: TObject);
    procedure IconsLinkMouseEnter(Sender: TObject);
    procedure IconsLinkMouseLeave(Sender: TObject);
    procedure RgBorderStyleClick(Sender: TObject);
    procedure RgDrawingStyleClick(Sender: TObject);
    procedure RgIconSizeClick(Sender: TObject);
    procedure RbBkColorChange(Sender: TObject);
    procedure VpNavBar1FolderChanged(Sender: TObject; Index: Integer);
    procedure VpNavBar1ItemClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; Index: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  LCLIntf,
  VpMisc;

{ TForm1 }

procedure TForm1.BackgroundColorChange(Sender: TObject);
begin
  VpNavBar1.BackgroundColor := BkColor.Selected;
end;

procedure TForm1.BtnAddFolderClick(Sender: TObject);
var
  s: String;
begin
  s := InputBox('Add folder', 'Folder name:', '');
  if s <> '' then
    VpNavBar1.AddFolder(s);
end;

procedure TForm1.BtnAddItemClick(Sender: TObject);
var
  folder: TVpNavFolder;
  item: TVpNavBtnItem;
  s: String;
  idx: Integer;
begin
  if VpNavBar1.ActiveFolder = -1 then
    exit;
  s := InputBox('Add item', 'Item name:', '');
  if s <> '' then begin
    folder := VpNavBar1.Folders[VpNavBar1.ActiveFolder];
    idx := folder.ItemCount;
    VpNavBar1.AddItem(s, VpNavBar1.ActiveFolder, idx);
    item := folder.Items[idx];
    item.IconIndex := Random(VpNavBar1.Images.Count);
  end;
end;

procedure TForm1.BtnLoadBkImageClick(Sender: TObject);
var
  ext: String;
  bmp: TBitmap;
  jpg: TJpegImage;
  png: TPortableNetworkGraphic;
begin
  if EdBkImage.FileName = '' then
    exit;
  if not FileExists(EdBkImage.FileName) then
    exit;
  ext := Lowercase(ExtractFileExt(EdBkImage.FileName));
  case ext of
    '.bmp':
      VpNavBar1.BackgroundImage.LoadFromFile(EdBkImage.FileName);
    '.png':
      begin
        png := TPortableNetworkGraphic.Create;
        try
          png.LoadFromFile(EdBkImage.FileName);
          VpNavBar1.BackgroundImage.Assign(png);
        finally
          png.Free;
        end;
      end;
    '.jpg', '.jpeg':
      begin
        jpg := TJpegImage.Create;
        try
          jpg.LoadFromFile(EdBkImage.FileName);
          VpNavBar1.BackgroundImage.Assign(jpg);
        finally
          jpg.Free;
        end;
      end;
  end;
  {
  pic := TPicture.Create;
  try
    pic.LoadFromFile(EdBkImage.FileName);
    bmp := TBitmap.Create;
    bmp.Assign(pic);
    VpNavBar1.BackgroundImage.Assign(bmp);
  finally
    pic.Free;
  end;
  }
end;

procedure TForm1.CbPlaySoundsChange(Sender: TObject);
begin
  VpNavBar1.PlaySounds := CbPlaySounds.Checked;
end;

procedure TForm1.EdSoundFileAcceptFileName(Sender: TObject; var Value: String);
begin
  VpNavBar1.SoundAlias := ExpandFilename(Value);
end;

procedure TForm1.EdSoundFileEditingDone(Sender: TObject);
begin
  VpNavBar1.SoundAlias := EdSoundFile.FileName;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RandSeed := 1;
  IconsLink.Left := IconsLbl.Left + GetLabelWidth(IconsLbl);
  RgDrawingStyle.ItemIndex := ord(VpNavBar1.DrawingStyle);
  RgBorderStyle.ItemIndex := ord(VpNavBar1.BorderStyle);
  BkColor.Selected := VpNavBar1.BackgroundColor;
  case VpNavBar1.BackgroundMethod of
    bmNone:
      RbBkColor.Checked := true;
    bmNormal:
      begin
        RbBkImage.Checked := true;
        RbBkImageNormal.Checked := true;
      end;
    bmStretch:
      begin
        RbBkImage.Checked := true;
        RbBkImageStretch.Checked := true;
      end;
    bmTile:
      begin
        RbBkImage.Checked := true;
        RbBkImageTile.Checked := true;
      end;
  end;
  BtnLoadBkImageClick(nil);

  EdSoundFile.InitialDir := ExtractFileDir(VpNavBar1.SoundAlias);
  EdSoundFile.FileName := VpNavBar1.SoundAlias;
  CbPlaySounds.Checked := VpNavBar1.PlaySounds;
end;

procedure TForm1.IconsLinkClick(Sender: TObject);
begin
  OpenDocument(IconsLink.Caption);
end;

procedure TForm1.IconsLinkMouseEnter(Sender: TObject);
begin
  IconsLink.Font.style := IconsLink.Font.Style + [fsUnderline];
end;

procedure TForm1.IconsLinkMouseLeave(Sender: TObject);
begin
  IconsLink.Font.style := IconsLink.Font.Style - [fsUnderline];
end;

procedure TForm1.RgDrawingStyleClick(Sender: TObject);
begin
  VpNavBar1.DrawingStyle := TVpFolderDrawingStyle(RgDrawingStyle.ItemIndex);
end;

procedure TForm1.RgIconSizeClick(Sender: TObject);
var
  folder: TVpNavFolder;
begin
  RgIconSize.OnClick := nil;
  folder := VpNavBar1.Folders[VpNavBar1.ActiveFolder];
  folder.IconSize := TVpIconSize(RgDrawingStyle.ItemIndex);
  RgIconSize.OnClick := @RgIconSizeClick;
end;

procedure TForm1.RbBkColorChange(Sender: TObject);
begin
  if RbBkColor.Checked then
    VpNavBar1.BackgroundMethod := bmNone
  else
  if RbBkImage.Checked then begin
    if RbBkImageNormal.Checked then
      VpNavBar1.BackgroundMethod := bmNormal
    else
    if RbBkImageStretch.Checked then
      VpNavBar1.BackgroundMethod := bmStretch
    else
    if RbBkImageTile.Checked then
      VpNavBar1.BackgroundMethod := bmTile;
  end;;
  VpNavBar1.Invalidate;
end;

procedure TForm1.RgBorderStyleClick(Sender: TObject);
begin
  VpNavBar1.BorderStyle := TBorderStyle(RgBorderStyle.ItemIndex);
end;

procedure TForm1.VpNavBar1FolderChanged(Sender: TObject; Index: Integer);
var
  folder: TVpNavFolder;
begin
  RgIconSize.OnClick := nil;
  folder := VpNavBar1.Folders[Index];
  RgIconSize.ItemIndex := ord(folder.IconSize);
  RgIconSize.OnClick := @RgIconSizeClick;
end;

procedure TForm1.VpNavBar1ItemClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; Index: Integer);
var
  folder: TVpNavFolder;
  item: TVpNavBtnItem;
begin
  folder := VpNavBar1.Folders[VpNavBar1.ActiveFolder];
  item := folder.Items[Index];
  Label1.Caption := Format('Item "%s" clicked', [item.Caption]);
end;

end.

