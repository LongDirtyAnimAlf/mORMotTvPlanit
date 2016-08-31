{*********************************************************}
{*                VPEDSHAPE.PAS 1.03                     *}
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

unit VpEdShape;

interface

uses
  {$IFDEF LCL}
  LCLProc, LCLType, LCLIntf, ColorBox,
  {$ELSE}
  Windows, Messages, ColorGrd,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, TypInfo, ComCtrls, Buttons,
  VpPrtFmt;

type

  { TfrmEditShape }

  TfrmEditShape = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    cbBrushStyle: TComboBox;
    cbBrushColor: TColorBox;
    cbPenMode: TComboBox;
    cbPenStyle: TComboBox;
    cbPenColor: TColorBox;
    gbBrush: TGroupBox;
    gbPen: TGroupBox;
    gbShapes: TGroupBox;
    lblBrushStyle: TLabel;
    lblBrushColor: TLabel;
    lblPenStyle: TLabel;
    lblPenColor: TLabel;
    lblPenWidth: TLabel;
    lblPenMode: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    udPenWidth: TUpDown;
    edPenWidth: TEdit;
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure cbBrushColorChange(Sender: TObject);
    procedure cbBrushStyleChange(Sender: TObject);
    procedure cbBrushStyleDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cbPenColorChange(Sender: TObject);
    procedure cbPenStyleChange(Sender: TObject);
    procedure cbPenStyleDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure edPenWidthChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    FShapeButtons: array[TVpShapeType] of TSpeedButton;
    FShapeBitmaps: array[TVpShapeType] of TBitmap;
    procedure FillBrushStyleList;
    procedure FillPenStyleList;
    procedure FillPenModeList;
    procedure PositionControls;
    procedure SetCaptions;

  protected
    procedure CreateBitmaps;
    procedure DestroyBitmaps;
    procedure SaveData(AShape: TVpPrintShape);
    procedure SetData(AShape: TVpPrintShape);
    procedure UpdateBitmap(AShape: TVpShapeType);
    procedure UpdateBitmaps;

  public
    function Execute(AShape : TVpPrintShape) : Boolean;
  end;


implementation

{$IFDEF LCL}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

uses
  Math, VpMisc, VpSr;


{ TfrmEditShape }

procedure TfrmEditShape.CreateBitmaps;
var
  shape: TVpShapeType;
  w, h: Integer;
begin
  w := SpeedButton1.Width div 2;
  h := SpeedButton1.Height div 2;
  for shape := Low(TVpShapeType) to High(TVpShapeType) do begin
    FShapeBitmaps[shape] := TBitmap.Create;
    with FShapeBitmaps[shape] do begin
      PixelFormat := pf24Bit;
      SetSize(w, h);
      Transparent := true;
    end;
    FShapeButtons[shape].Glyph.Assign(FShapeBitmaps[shape]);
    case shape of
      ustRectangle  : FShapeButtons[shape].Hint := RSRectangle;
      ustTopLine    : FShapeButtons[shape].Hint := RSTopLine;
      ustBottomLine : FShapeButtons[shape].Hint := RSBottomLine;
      ustLeftLine   : FShapeButtons[shape].Hint := RSLeftLine;
      ustRightLine  : FShapeButtons[shape].Hint := RSRightLine;
      ustTLToBRLine : FShapeButtons[shape].Hint := RSTLToBRLine;
      ustBLToTRLine : FShapeButtons[shape].Hint := RSBLToTRLine;
      ustEllipse    : FShapeButtons[shape].Hint := RSEllipse;
    end;
  end;
end;

procedure TfrmEditShape.DestroyBitmaps;
var
  shape: TVpShapeType;
begin
  for shape := Low(TVpShapeType) to High(TVpShapeType) do
    FShapeBitmaps[shape].Free;
end;

procedure TfrmEditShape.edPenWidthChange(Sender: TObject);
begin
  UpdateBitmaps;
end;

procedure TfrmEditShape.UpdateBitmap(AShape: TVpShapeType);
var
  pw: Integer;
  bkcol, pcol, bcol: TColor;
begin
  pw := StrToInt(edPenWidth.Text);
  pcol := cbPenColor.Selected;
  bcol := cbBrushColor.Selected;
  bkcol := clWhite;
  while (bkcol = pcol) or (bkcol = bcol) do
    bkcol := rgb(random(256), random(256), random(256));
  with FShapeBitmaps[AShape] do begin
    TransparentColor := bkcol;
    Canvas.Brush.Color := bkCol;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(0, 0, Width, Height);
    Canvas.Pen.Width := pw;
    Canvas.Pen.Style := TPenStyle(cbPenStyle.ItemIndex);
    Canvas.Pen.Color := pcol;
    Canvas.Brush.Style := TBrushStyle(cbBrushStyle.ItemIndex);
    Canvas.Brush.Color := bcol;
    case AShape of
      ustRectangle  : Canvas.Rectangle(pw, pw, Width-pw, Height-pw);
      ustTopLine    : Canvas.Line(pw, pw, Width-pw, pw);
      ustBottomLine : Canvas.Line(pw, Height-pw, Width, Height-pw);
      ustLeftLine   : Canvas.Line(pw, pw, pw, Height-pw);
      ustRightLine  : Canvas.Line(Width-pw, pw, Width-pw, Height);
      ustTLToBRLine : Canvas.Line(pw, pw, Width-pw, Height-pw);
      ustBLToTRLine : Canvas.Line(pw, Height-pw, Width-pw, pw);
      ustEllipse    : Canvas.Ellipse(pw, pw, Width-pw, Height-pw);
    end;
  end;
  FShapeButtons[AShape].Glyph.Assign(FShapeBitmaps[AShape]);
end;

procedure TfrmEditShape.UpdateBitmaps;
var
  shape: TVpShapeType;
begin
  for shape := Low(TVpShapeType) to High(TVpShapeType) do
    UpdateBitmap(shape);
end;

procedure TfrmEditShape.FormCreate(Sender: TObject);
begin
  FShapeButtons[ustRectangle] := SpeedButton1;
  FShapeButtons[ustTopLine] := SpeedButton2;
  FShapeButtons[ustBottomLine] := SpeedButton3;
  FShapeButtons[ustLeftLine] := SpeedButton4;
  FShapeButtons[ustRightLine] := SpeedButton5;
  FShapeButtons[ustTLToBRLine] := SpeedButton6;
  FShapeButtons[ustBLToTRLine] := SpeedButton7;
  FShapeButtons[ustEllipse] := SpeedButton8;

  FillBrushStyleList;
  FillPenStyleList;
  FillPenModeList;

  CreateBitmaps;
  UpdateBitmaps;

  SetCaptions;
end;

procedure TfrmEditShape.FormDestroy(Sender: TObject);
begin
  DestroyBitmaps;
end;

procedure TfrmEditShape.FormShow(Sender: TObject);
begin
  PositionControls;
end;

{=====}
function TfrmEditShape.Execute(AShape: TVpPrintShape): Boolean;
begin
  SetData(AShape);
  Result := ShowModal = mrOk;
  if Result then
    SaveData(AShape);
end;
{=====}
procedure TfrmEditShape.btnOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmEditShape.cbBrushColorChange(Sender: TObject);
begin
  UpdateBitmaps;
end;

procedure TfrmEditShape.cbBrushStyleChange(Sender: TObject);
begin
  UpdateBitmaps;
end;

{=====}
procedure TfrmEditShape.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;
{=====}
procedure TfrmEditShape.FillBrushStyleList;
var
  Style: TBrushStyle;
  StyleName: string;
begin
  for Style := Low(TBrushStyle) to High(TBrushStyle) do begin
    StyleName := GetEnumName(TypeInfo(TBrushStyle), Ord(Style));
    cbBrushStyle.Items.Add(StyleName);
  end;
end;
{=====}
procedure TfrmEditShape.FillPenModeList;
var
  Mode: TPenMode;
  ModeName: string;
begin
  for Mode := Low(TPenMode) to High(TPenMode) do begin
    ModeName := GetEnumName(TypeInfo(TPenMode), Ord(Mode));
    cbPenMode.Items.Add(ModeName);
  end;
end;
{=====}
procedure TfrmEditShape.FillPenStyleList;
var
  Style: TPenStyle;
  StyleName: string;
begin
  for Style := Low(TPenStyle) to High(TPenStyle) do begin
    StyleName := GetEnumName(TypeInfo(TPenStyle), Ord(Style));
    cbPenStyle.Items.Add(StyleName);
  end;
end;
{=====}
procedure TfrmEditShape.PositionControls;
var
  w, hc, hb: Integer;
  shape: TVpShapeType;
  DELTA: Integer = 8;
  VDIST: Integer = 4;
  cnv: TControlCanvas;
  i: Integer;
begin
  AutoSize := false;
  gbPen.AutoSize := false;
  gbBrush.AutoSize := false;

  // A workaround for the combobox height issue at higher dpi values:
  // Create a combobox at runtime, it has the correct height, and apply its
  // ItemHeight to the other comboboxes.
  with TCombobox.Create(self) do
  try
    Parent := self;
    hc := ItemHeight;
  finally
    Free;
  end;
  cbPenStyle.ItemHeight := hc;
  cbPenColor.ItemHeight := hc;
  cbBrushStyle.ItemHeight := hc;
  cbBrushColor.ItemHeight := hc;

  // Fix button hight at higher dpi.
  hb := ScaleY(btnOK.Height, DesignTimeDPI);

  DELTA := ScaleX(DELTA, DesignTimeDPI);
  VDIST := ScaleY(VDIST, DesignTimeDPI);

  { gsShapes - vert }
  gbShapes.ClientHeight := SpeedButton1.Height + 3 * VDIST;
  for shape := Low(TVpShapeType) to High(TVpShapeType) do
    FShapeButtons[shape].Top := VDIST;

  { gbPen - hor }
  w := MaxValue([GetLabelWidth(lblPenColor), GetLabelWidth(lblPenStyle),
    GetLabelWidth(lblPenWidth), GetLabelWidth(lblPenMode)]) + 2 * DELTA;
  cbPenColor.Left := w;
  cbPenStyle.Left := w;
  edPenWidth.Left := w;
  cbPenMode.Left := w;
  lblPenColor.Left := cbPenColor.Left - GetLabelWidth(lblPenColor) - DELTA;
  lblPenStyle.Left := cbPenColor.Left - GetLabelWidth(lblPenStyle) - DELTA;
  lblPenWidth.Left := cbPenColor.Left - GetLabelWidth(lblPenWidth) - DELTA;
  lblPenMode.Left := cbPenColor.Left - GetLabelWidth(lblPenMode) - DELTA;
  udPenWidth.Left := RightOf(edPenWidth);

  { gbPen - Width }
  cnv := TControlCanvas.Create;
  try
    cnv.Control := cbPenStyle;
    cnv.Font.Assign(cbPenStyle.Font);
    w := 0;
    for i:=0 to cbPenStyle.Items.Count-1 do
      w := Max(w, cnv.TextWidth(cbPenStyle.Items[i]));
    w := w + 10 + 2*cbPenStyle.Height;
  finally
    cnv.Free;
  end;

  cbPenColor.Width := w;
  cbPenStyle.Width := w;
  cbPenMode.Width := w;

  { gbPen - vert }
  lblPenColor.Top := cbPenColor.Top + (cbPenColor.Height - lblPenColor.Height) div 2;
  cbPenStyle.Top := BottomOf(cbPenColor) + VDIST;
  lblPenstyle.Top := cbPenStyle.Top + (cbPenStyle.Height - lblPenStyle.Height) div 2;
  edPenWidth.Top := BottomOf(cbPenStyle) + VDIST;
  udPenWidth.Top := edPenWidth.Top;
  lblPenWidth.Top := edPenWidth.Top + (edPenWidth.Height - lblPenWidth.Height) div 2;
  cbPenMode.Top := BottomOf(edPenWidth) + VDIST;
  lblPenMode.Top := cbPenMode.Top + (cbPenMode.Height - lblPenMode.Height) div 2;

  { gpPen - set size }
  gbPen.AutoSize := true;
  gbPen.Top := BottomOf(gbShapes) + VDIST*2;

  { gbBrush - hor }
  w := MaxValue([GetLabelWidth(lblBrushColor), GetLabelWidth(lblBrushStyle)]) + 2*DELTA;
  cbBrushColor.Left := w;
  cbBrushStyle.Left := w;
  cbBrushColor.Width := cbPenColor.Width;
  cbBrushStyle.Width := cbPenStyle.Width;
  lblBrushColor.Left := cbBrushColor.Left - GetLabelWidth(lblBrushColor) - DELTA;
  lblBrushStyle.Left := cbBrushColor.Left - GetLabelWidth(lblBrushStyle) - DELTA;
  gbBrush.Left := RightOf(gbPen) + 16;

  { gbBrush - ver }
  lblBrushColor.Top := lblPenColor.Top;
  cbBrushStyle.Top := cbPenStyle.Top;
  lblBrushStyle.Top := lblPenStyle.Top;

  { gbBrush - set size }
  gbBrush.AutoSize := true;
  gbBrush.Top := gbPen.Top;

  { Buttons - hor }
  btnOK.Width := Max(GetButtonWidth(btnOK), GetButtonWidth(btnCancel));
  btnCancel.Width := btnOK.Width;
  if btnOK.Width + DELTA + btnCancel.Width > gbBrush.Width then begin
    cbBrushColor.Width := cbBrushColor.Width + btnOK.Width + DELTA + btnCancel.Width - gbBrush.Width;
    cbBrushStyle.Width := cbBrushColor.Width;
  end;
 {$IFDEF MSWINDOWS}
  btnCancel.Left := RightOf(gbBrush) - btnCancel.Width;
  btnOK.Left := btnCancel.Left - DELTA - btnOK.Width;
  btnOK.TabOrder := gbBrush.TabOrder + 1;
  btnCancel.TabOrder := btnOK.TabOrder + 1;
 {$ELSE}
  btnOK.Left := RightOf(gbBrush) - btnOK.Width;
  btnCancel.Left := btnOK.Left - DELTA - btnCancel.Width;
  btnCancel.TabOrder := gbBrush.TabOrder + 1;
  btnOK.TabOrder := btnCancel.TabOrder + 1;
 {$ENDIF}

  { Buttons - vert }
  btnOK.Height := hb;
  btnCancel.Height := hb;
  btnOK.Top := BottomOf(gbPen) - btnOK.Height;
  btnCancel.Top := btnOK.Top;

  { shapes - hor }
  gbShapes.Width := RightOf(gbBrush) - gbShapes.Left;
  w := (gbShapes.ClientWidth - 11 * DELTA) div 8;
  for shape := Low(TVpShapeType) to High(TVpShapeType) do begin
    if shape = Low(TVpShapeType) then
      FShapeButtons[shape].Left := DELTA * 2 else
      FShapeButtons[shape].Left := RightOf(FShapeButtons[pred(shape)]) + DELTA;
    FShapeButtons[shape].Width := w;
  end;

  AutoSize := true;
end;

procedure TfrmEditShape.SaveData(AShape: TVpPrintShape);
var
  shape: TVpShapeType;
begin
  for shape := Low(TVpShapeType) to High(TVpShapeType) do
    if FShapeButtons[shape].Down then begin
      AShape.Shape := shape;
      break;
    end;
  AShape.Pen.Style := TPenStyle(cbPenStyle.ItemIndex);
  AShape.Pen.Width := udPenWidth.Position;
  AShape.Pen.Color := cbPenColor.Selected;
  AShape.Pen.Mode := TPenMode(cbPenMode.ItemIndex);

  AShape.Brush.Style := TBrushStyle(cbBrushStyle.ItemIndex);
  AShape.Brush.Color := cbBrushColor.Selected;
end;
{=====}
procedure TfrmEditShape.SetCaptions;
begin
  Caption := RSEditShapeCaption;
  gbShapes.Caption := RsShapeCaption;
  gbPen.Caption := RSPenCaption;
  lblPenColor.Caption := RSColorLbl;
  lblPenStyle.Caption := RSStyleLbl;
  lblPenWidth.Caption := RSWidthLbl;
  lblPenMode.Caption := RSModeLbl;
  gbBrush.Caption := RSBrushCaption;
  lblBrushColor.Caption := RSColorLbl;
  lblBrushStyle.Caption := RSStyleLbl;
  btnOK.Caption := RSOKBtn;
  btnCancel.Caption := RSCancelBtn;
end;

procedure TfrmEditShape.SetData(AShape: TVpPrintShape);
begin
  FShapeButtons[AShape.Shape].Down := true;

  { pen settings }
  cbPenColor.Selected := AShape.Pen.Color;
  udPenWidth.Position := AShape.Pen.Width;
  cbPenStyle.ItemIndex := ord(AShape.Pen.Style);
  cbPenMode.ItemIndex := ord(AShape.Pen.Mode);

  { brush settings }
  cbBrushColor.Selected := AShape.Brush.Color;
  cbBrushStyle.ItemIndex := ord(AShape.Brush.Style);
end;
{=====}

procedure TfrmEditShape.cbBrushStyleDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
const
  DIST = 2;
var
  SavePenColor, SaveBrushColor: TColor;
  x: Integer;
  SaveBrushStyle: TBrushStyle;
  Item : string;
  TxtRect : TRect;
  R: TRect;
  bs: TBrushStyle;
begin
  Unused(Control, State);

  Item := cbBrushStyle.Items[Index];
  with cbBrushStyle.Canvas do
  try
    { keep old settings }
    SavePenColor := Pen.Color;
    SaveBrushColor := Brush.Color;
    SaveBrushStyle := Brush.Style;

    R := Rect;
    InflateRect(R, -1, -1);
    x := Rect.Left + HeightOf(Rect);
    R.Right := x;
   {$IFDEF LINUX}
    InflateRect(R, -2, -2);
    x := Rect.Left + HeightOf(Rect);
    R.Right := x - 2;
   {$ENDIF}
    bs := TBrushStyle(GetEnumValue(TypeInfo(TBrushStyle), Item));

    { draw background }
    FillRect(Rect);     // Brush is already set

    { draw frame }
    if bs <> bsClear then begin
      Brush.Color := clWindow;
      Rectangle(R);
    end;

    if bs <> bsClear then begin
      { set up for drawing sample }
      Brush.Style := bs;
      Brush.Color := clWindowText; //cbBrushStyle.Font.Color;
      Pen.Color := clWindowText; //txtColor; //cbBrushStyle.Font.Color;

      { Draw sample }
      Rectangle(R);
    end;

    { draw the item text }
    inc(x, ScaleX(DIST, DesignTimeDPI));
    TxtRect := Classes.Rect(x, Rect.Top, Rect.Right, Rect.Bottom);
    TextRect(TxtRect, TxtRect.Left, TxtRect.Top + 1, Item);  // Font color already set

  finally
    { restore settings }
    Brush.Color := SaveBrushColor;
    Brush.Style := SaveBrushStyle;
    Pen.Color := SavePenColor;
  end;
end;

procedure TfrmEditShape.cbPenColorChange(Sender: TObject);
begin
  UpdateBitmaps;
end;

procedure TfrmEditShape.cbPenStyleChange(Sender: TObject);
begin
  UpdateBitmaps;
end;

{=====}

procedure TfrmEditShape.cbPenStyleDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
const
  DIST = 2;
var
  SavePenColor, SaveBrushColor: TColor;
  SavePenStyle: TPenStyle;
  Item: string;
  R, TxtRect : TRect;
  x, y: Integer;
begin
  Unused(Control, State);

  Item := cbPenStyle.Items[Index];
  x := Rect.Left + HeightOf(Rect) * 2;
  y := Rect.Top + HeightOf(Rect) div 2;

  with cbPenStyle.Canvas do
  try
    { Keep old settings }
    SavePenColor := Pen.Color;
    SaveBrushColor := Brush.Color;
    SavePenStyle := Pen.Style;

    { Draw background }
    FillRect(Rect);     // Brush already set by caller

    { Set up for drawing sample }
    Brush.Color := cbPenStyle.Brush.Color;
    Pen.Color := cbPenStyle.Font.Color;
    R := Rect;
    InflateRect(R, -1, -1);
   {$IFDEF LINUX}
    InflateRect(R, -2, -2);
   {$ENDIF}
    Rectangle(R.Left, R.Top, x - 1, R.Bottom);

    { Draw sample }
    Pen.Style := TPenStyle(GetEnumValue(TypeInfo(TPenStyle), Item));
    Pen.Color := cbPenStyle.Font.Color;

    MoveTo(R.Left + 1, y);
    LineTo(x - 1, y);
    MoveTo(R.Left + 1, y + 1);
    LineTo(x - 1, y + 1);

    { Draw the item text }
    inc(x, ScaleX(DIST, DesignTimeDPI));
    TxtRect := Classes.Rect(x, Rect.Top, Rect.Right, Rect.Bottom);
    TextRect(TxtRect, TxtRect.Left, TxtRect.Top + 1, Item);   // Color already set

  finally
    { restore settings }
    Brush.Color := SaveBrushColor;
    Pen.Style := SavePenStyle;
    Pen.Color := SavePenColor;
  end;
end;
{=====}

end.
  
