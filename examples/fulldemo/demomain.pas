unit demoMain;

{$mode objfpc}{$H+}

interface

uses
 {$IFDEF UNIX}
  clocale,
 {$ENDIF}
  Classes, SysUtils, FileUtil, PrintersDlgs, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, LCLTranslator, Menus, LCLVersion,
 VpBaseDS, VpDayView,
  VpWeekView, VpTaskList, VpAbout, VpContactGrid, VpMonthView, VpResEditDlg,
  VpContactButtons, VpBufDS, VpNavBar, VpData, VpPrtPrvDlg, VpPrtFmtDlg, Types,
  VpBase, VpCalendar;

type

  { TMainForm }

  TMainForm = class(TForm)
    BtnNewRes: TButton;
    BtnEditRes: TButton;
    BtnDeleteRes: TButton;
    CbLanguages: TComboBox;
    CbGranularity: TComboBox;
    CbTimeFormat: TComboBox;
    CbFirstDayOfWeek: TComboBox;
    CbAllowInplaceEditing: TCheckBox;
    CbAddressBuilder: TComboBox;
    CbDrawingStyle: TComboBox;
    CbAllowDragAndDrop: TCheckBox;
    CbDragDropTransparent: TCheckBox;
    Img: TImage;
    ImageList1: TImageList;
    LblDrawingStyle: TLabel;
    LblAddressBuilder: TLabel;
    LblFirstDayOfWeek: TLabel;
    LblTimeFormat: TLabel;
    LblGranularity: TLabel;
    LblLanguage: TLabel;
    LblVisibleDays: TLabel;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MnuLoadPrintFormats: TMenuItem;
    MnuPrint: TMenuItem;
    MnuEditPrintFormats: TMenuItem;
    MnuPrintPreview: TMenuItem;
    Notebook: TNotebook;
    Events: TPage;
    OpenDialog: TOpenDialog;
    Splitter1: TSplitter;
    Tasks: TPage;
    Contacts: TPage;
    Resources: TPage;
    Settings: TPage;
    PrintDialog1: TPrintDialog;
    TitleLbl: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MnuSettings: TMenuItem;
    MnuAbout: TMenuItem;
    MnuMaintenance: TMenuItem;
    MnuQuit: TMenuItem;
    MnuResources: TMenuItem;
    Panel1: TPanel;
    LeftPanel: TPanel;
    DaySelectorPanel: TPanel;
    HeaderPanel: TPanel;
    Panel6: TPanel;
    RbAllTasks: TRadioButton;
    RbHideCompletedTasks: TRadioButton;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    DaysTrackBar: TTrackBar;
    VpBufDSDataStore1: TVpBufDSDataStore;
    VpContactButtonBar1: TVpContactButtonBar;
    VpContactGrid1: TVpContactGrid;
    VpControlLink1: TVpControlLink;
    VpDayView1: TVpDayView;
    VpMonthView1: TVpMonthView;
    VpNavBar1: TVpNavBar;
    VpPrintFormatEditDialog1: TVpPrintFormatEditDialog;
    VpPrintPreviewDialog1: TVpPrintPreviewDialog;
    VpResourceCombo1: TVpResourceCombo;
    VpResourceEditDialog1: TVpResourceEditDialog;
    VpTaskList1: TVpTaskList;
    VpWeekView1: TVpWeekView;
    procedure BtnDeleteResClick(Sender: TObject);
    procedure BtnNewResClick(Sender: TObject);
    procedure BtnEditResClick(Sender: TObject);
    procedure Cb3DChange(Sender: TObject);
    procedure CbAddressBuilderChange(Sender: TObject);
    procedure CbAllowDragAndDropChange(Sender: TObject);
    procedure CbAllowInplaceEditingChange(Sender: TObject);
    procedure CbDragDropTransparentChange(Sender: TObject);
    procedure CbDrawingStyleChange(Sender: TObject);
    procedure CbFirstDayOfWeekChange(Sender: TObject);
    procedure CbGranularityChange(Sender: TObject);
    procedure CbLanguagesChange(Sender: TObject);
    procedure CbTimeFormatChange(Sender: TObject);
    procedure DaysTrackBarChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure MnuAboutClick(Sender: TObject);
    procedure MnuEditPrintFormatsClick(Sender: TObject);
    procedure MnuLoadPrintFormatsClick(Sender: TObject);
    procedure MnuPrintClick(Sender: TObject);
    procedure MnuPrintPreviewClick(Sender: TObject);
    procedure MnuQuitClick(Sender: TObject);
    procedure MnuResourcesClick(Sender: TObject);
    procedure MnuSettingsClick(Sender: TObject);
    procedure RbAllTasksChange(Sender: TObject);
    procedure RbHideCompletedTasksChange(Sender: TObject);
    procedure VpBufDSDataStore1PlaySound(Sender: TObject;
      const AWavFile: String; AMode: TVpPlaySoundMode);
    procedure VpNavBar1ItemClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; Index: Integer);
  private
    { private declarations }
    FLang: String;
    FActiveView: Integer;
    FVisibleDays: Integer;
    procedure PopulateLanguages;
    procedure PositionControls;
    procedure SetActiveView(AValue: Integer);
    procedure SetLanguage(ALang: String); overload;
    procedure SetLanguage(AIndex: Integer); overload;
    procedure ShowAllEvents;
    procedure ShowContacts;
    procedure ShowEventsPerDay;
    procedure ShowEventsPerMonth;
    procedure ShowEventsPerWeek;
    procedure ShowResources;
    procedure ShowSettings;
    procedure ShowTasks;

    procedure ReadIni;
    procedure WriteIni;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{$UNDEF UTF8_CALLS}
{$IFDEF LCL}
  {$DEFINE UTF8_CALLS}
  {$IF (lcl_major=1) and (lcl_minor<=6)}
     {$UNDEF UTF8_CALLS}
  {$ENDIF}
{$ENDIF}


uses
 {$IFDEF WINDOWS}
  Windows,
 {$ENDIF}
  LResources, LazFileUtils, LazUTF8, StrUtils, DateUtils, Translations,
 {$IFNDEF UTF8_CALLS}
  lConvEncoding,
 {$ENDIF}
  IniFiles, Math, Printers,
  VpMisc, VpPrtFmt,
  sound, ExVpRptSetup;

const
  LANGUAGE_DIR = '..\..\languages\';

resourcestring
  RSConfirmDeleteRes = 'Do you really want to delete resource %s?';
  RSEventsOverview = 'Events overview';
  RSEventsPerMonth = 'Events per month';
  RSEventsPerWeek = 'Events per week';
  RSEventsPerDay = 'Events per day';
  RSTasks = 'Tasks';
  RSContacts = 'Contacts';
  RSResources = 'Resources';
  RSSettings = 'Program settings';
  RSSettings_short = 'Settings';
  RSPlanner = 'Planner';
  RSMaintenance = 'Maintenance';
  RS24Hours = '24 hours';
  RS12Hours = '12 hours AM/PM';
  RS5Min = '5 min';
  RS6Min = '6 min';
  RS10Min = '10 min';
  RS15Min = '15 min';
  RS20Min = '20 min';
  RS30Min = '30 min';
  RS60Min = '60 min';
  RSSunday = 'Sunday';
  RSMonday = 'Monday';
  RSTuesday = 'Tuesday';
  RSWednesday = 'Wednesday';
  RSThursday = 'Thursday';
  RSFriday = 'Friday';
  RSSaturday = 'Saturday';
  RSFlat = 'flat';
  RS3d = '3D';
  RSBorderless = 'no border';
  RSXMLFiles = 'XML files (*.xml)';

{$IFDEF WINDOWS}
{ This function determines the LCID from the language code.
  Works only for Windows. }
function LangToLCID(ALang: String): Integer;
begin
 case lowercase(ALang) of
   ''     : Result := $0409;    // Default = englisch
   'ar'   : Result := $0401;    // Arabic
   'bg'   : Result := $0403;    // Bulgarian
   'ca'   : Result := $0403;    // Catalan
   'cs'   : Result := $0405;    // Czech
   'de'   : Result := $0407;    // German
   'en'   : Result := $0409;    // English (US)
   'es'   : Result := $040A;    // Spanisch
   'fi'   : Result := $040B;    // Finnish
   'fr'   : Result := $040C;    // French
   'he'   : Result := $040D;    // Hebrew
   'hu'   : Result := $040E;    // Hungarian
   'it'   : Result := $0410;    // Italian
   'jp'   : Result := $0411;    // Japanese
   'nl'   : Result := $0413;    // Netherlands (Dutch)
   'pl'   : Result := $0415;    // Polish
   'pt'   : Result := $0816;    // Portuguese (Portugal)
   'ru'   : Result := $0419;    // Russian
   'tr'   : Result := $041F;    // Turkish
   'zh_cn', 'zh-cn': Result := $0804;    // Chinese (China)
   'zh_tw', 'zh-tw': Result := $0404;    // Chinese (Taiwan)
   // please complete if necessary. Language code and LCIDs can be found at
   // http://www.science.co.il/Language/Locale-codes.asp
   else  raise Exception.CreateFmt('Language "%s" not supported. Please add to GetLCIDFromLangCode.',[ALang]);
 end;
end;

function GetLocaleStr(LCID, LT: Longint; const Def: string): ShortString;
// borrowed from SysUtils
var
  L: Integer;
  Buf: array[0..255] of Char;
begin
  L := GetLocaleInfo(LCID, LT, Buf, SizeOf(Buf));
  if L > 0 then
    SetString(Result, @Buf[0], L - 1)
  else
    Result := Def;
end;
{$ENDIF}

procedure UpdateFormatSettings(ALang: String);
{$IFDEF WINDOWS}
var
  LCID: Integer;
  i: Integer;
  codepage: String;
{$ENDIF}
begin
 {$IFDEF WINDOWS}
  // Determine the LCID for the requested language
  LCID := LangToLCID(ALang);

  // Now we update the format settings to the new language
  {$IFDEF UTF8_CALLS}
  GetLocaleFormatSettingsUTF8(LCID, DefaultFormatSettings);
  {$ELSE}
  GetLocaleFormatSettings(LCID, DefaultFormatSettings);
  codepage := 'cp' + GetLocaleStr(LCID, LOCALE_IDEFAULTANSICODEPAGE, '');
  with DefaultFormatSettings do begin
    for i:=1 to 12 do begin
      LongMonthNames[i] := ConvertEncoding(LongMonthNames[i], codepage, 'utf8');
      ShortMonthNames[i] := ConvertEncoding(ShortMonthNames[i], codepage, 'utf8');
    end;
    for i:=1 to 7 do begin
      LongDayNames[i] := ConvertEncoding(LongDayNames[i], codepage, 'utf8');
      ShortDayNames[i] := ConvertEncoding(ShortDayNames[i], codepage, 'utf8');
    end;
  end;
  {$ENDIF}
 {$ENDIF}
end;

function GetFirstDayOfWeek(ALang: String): TVpDayType;
// Don't know how to determine this from the OS
begin
   Result := dtSunday;
end;


{ TMainForm }

procedure TMainForm.BtnDeleteResClick(Sender: TObject);
var
  res: TVpResource;
begin
  res := VpControlLink1.Datastore.Resource;
  if res = nil then
    exit;

  if MessageDlg(Format(RSConfirmDeleteRes, [res.Description]), mtConfirmation, [mbYes, mbNo], 0) = mrOK then
    VpControlLink1.Datastore.Resources.RemoveResource(res);
end;

// Edits the currently selected resource
procedure TMainForm.BtnEditResClick(Sender: TObject);
begin
  // Open the resource editor dialog, everything is done here.
  VpResourceEditDialog1.Execute;
end;

// Adds a new resource
procedure TMainForm.BtnNewResClick(Sender: TObject);
begin
  VpResourceEditDialog1.AddNewResource;
end;

procedure TMainForm.Cb3DChange(Sender: TObject);
var
  ds: TVpDrawingStyle;
begin
 ds := TVpDrawingStyle(CbDrawingStyle.ItemIndex);
 VpTaskList1.DrawingStyle := ds;
 VpContactGrid1.DrawingStyle := ds;
 VpDayView1.DrawingStyle := ds;
 VpWeekView1.DrawingStyle := ds;
 VpMonthView1.DrawingStyle := ds;
end;

procedure TMainForm.CbAddressBuilderChange(Sender: TObject);
begin
 if CbAddressBuilder.ItemIndex <= 0 then
   VpControlLink1.CityStateZipFormat := ''
 else
   VpControlLink1.CityStateZipFormat := CbAddressBuilder.Items[CbAddressBuilder.ItemIndex];
end;

procedure TMainForm.CbAllowDragAndDropChange(Sender: TObject);
begin
  VpDayView1.AllowDragAndDrop := CbAllowDragAndDrop.Checked;
end;

procedure TMainForm.CbAllowInplaceEditingChange(Sender: TObject);
begin
  VpContactGrid1.AllowInplaceEditing := CbAllowInplaceEditing.Checked;
  VpDayView1.AllowInplaceEditing := CbAllowInplaceEditing.Checked;
  VpWeekView1.AllowInplaceEditing := CbAllowInplaceEditing.Checked;
  VpTaskList1.AllowInplaceEditing := CbAllowInplaceEditing.Checked;
end;

procedure TMainForm.CbDragDropTransparentChange(Sender: TObject);
begin
  VpDayView1.DragDropTransparent := CbDragDropTransparent.Checked;
end;

procedure TMainForm.CbDrawingStyleChange(Sender: TObject);
var
  ds: TVpDrawingStyle;
begin
  ds := TVpDrawingStyle(CbDrawingStyle.ItemIndex);
  VpTaskList1.DrawingStyle := ds;
  VpContactGrid1.DrawingStyle := ds;
  VpDayView1.DrawingStyle := ds;
  VpWeekView1.DrawingStyle := ds;
  VpMonthView1.DrawingStyle := ds;
end;

procedure TMainForm.CbFirstDayOfWeekChange(Sender: TObject);
begin
  VpWeekView1.WeekStartsOn := TVpDayType(CbFirstDayOfWeek.ItemIndex);
  VpMonthView1.WeekStartsOn := TVpDayType(CbFirstDayOfWeek.ItemIndex);
end;

procedure TMainForm.CbGranularityChange(Sender: TObject);
begin
  VpDayView1.Granularity := TVpGranularity(CbGranularity.ItemIndex);
end;

procedure TMainForm.CbLanguagesChange(Sender: TObject);
begin
  SetLanguage(CbLanguages.ItemIndex);
end;

procedure TMainForm.CbTimeFormatChange(Sender: TObject);
begin
  VpDayView1.TimeFormat := TVpTimeFormat(CbTimeFormat.ItemIndex);
  VpWeekView1.TimeFormat := TVpTimeFormat(CbTimeFormat.ItemIndex);
  VpMonthView1.TimeFormat := TVpTimeFormat(CbTimeFormat.ItemIndex);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if CanClose then
    try
      WriteIni;
    except
    end;
end;

// Load the last resource.
procedure TMainForm.FormCreate(Sender: TObject);
var
  lastRes: TVpResource;
  ds: TVpCustomDataStore;
begin
  PopulateLanguages;
  ReadIni;

  ds := VpControlLink1.Datastore;
  if ds.Resources.Count > 0 then
  begin
    lastRes := ds.Resources.Items[ds.Resources.Count-1];
    ds.ResourceID := lastRes.ResourceID;
  end;
end;

procedure TMainForm.MnuAboutClick(Sender: TObject);
var
  F: TfrmAbout;
begin
  F := TfrmAbout.Create(nil);
  try
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TMainForm.MnuEditPrintFormatsClick(Sender: TObject);
begin
  VpPrintFormatEditDialog1.DrawingStyle := VpWeekView1.DrawingStyle;
  VpPrintFormatEditDialog1.Execute;
end;

procedure TMainForm.MnuLoadPrintFormatsClick(Sender: TObject);
begin
  if OpenDialog.Filename = '' then
    OpenDialog.InitialDir := Application.Location else
    OpenDialog.InitialDir := ExtractFileDir(OpenDialog.FileName);
  if OpenDialog.Execute then
    VpControlLink1.Printer.LoadFromFile(Opendialog.Filename, false);
end;

procedure TMainForm.MnuPrintClick(Sender: TObject);
var
  F: TfrmReportSetup;
begin
  if ReportData.StartDate = 0 then
    ReportData.StartDate := VpMonthView1.Date;
  if ReportData.EndDate = 0 then
    ReportData.EndDate := VpMonthView1.Date;
  if ReportData.Format = '' then
    ReportData.Format := VpControlLink1.Printer.PrintFormats.Items[0].FormatName;

  F := TfrmReportSetup.Create(nil);
  try
    F.ControlLink := VpControlLink1;
    if not F.Execute(ReportData) then
      exit;

    if PrintDialog1.Execute then begin
      Printer.BeginDoc;
      VpControlLink1.Printer.CurFormat := VpControlLink1.Printer.Find(ReportData.Format);
      VpControlLink1.Printer.Print(Printer, ReportData.StartDate, ReportData.EndDate);
      Printer.EndDoc;
    end;
  finally
    F.Free;
  end;
end;

procedure TMainForm.MnuPrintPreviewClick(Sender: TObject);
var
  t1, t2: TDateTime;
  fmt: TVpPrintFormatItem;
  fmtidx: Integer;
begin
  fmtidx := VpPrintPreviewDialog1.ControlLink.Printer.CurFormat;
  fmt := VpPrintPreviewDialog1.ControlLink.Printer.PrintFormats.Items[fmtidx];
  case fmtidx of
    0: begin // current week in DayView
         t1 := StartOfTheWeek(now);
         t2 := t1 + 7 - VpDayView1.NumDays mod 7; // + 7;
         fmt.DayInc := VpDayView1.NumDays;
         VpControlLink1.Printer.Granularity := gr30Min;
         VpControlLink1.Printer.DayStart := h_08;
         VpControlLink1.Printer.DayEnd := h_18;
       end;
    1: begin  // current week in WeekView
         t1 := StartOfTheWeek(now);
         t2 := t1;     // it all fits on one single page
       end;
    2: begin  // Tasks of current week
         t1 := StartOfTheWeek(now);
         t2 := t1;
       end;
  end;
  VpPrintPreviewDialog1.ControlLink := VpControlLink1;
  VpPrintPreviewDialog1.Printer := Printer;
  VpPrintPreviewDialog1.StartDate := t1;
  VpPrintPreviewDialog1.EndDate := t2;
  VpPrintPreviewDialog1.DrawingStyle := VpDayView1.DrawingStyle;
  if VpPrintPreviewDialog1.Execute then
    if PrintDialog1.Execute then begin
      Printer.BeginDoc;
      try
        t1 := VpPrintPreviewDialog1.StartDate;
        t2 := VpPrintPreviewDialog1.EndDate;
        VpPrintPreviewDialog1.ControlLink.Printer.Print(Printer, t1, t2);
      finally
        Printer.EndDoc;
       end;
    end;
end;

procedure TMainForm.MnuSettingsClick(Sender: TObject);
begin
  ShowSettings;
end;

procedure TMainForm.MnuQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.MnuResourcesClick(Sender: TObject);
begin
  ShowResources;
end;

procedure TMainForm.PopulateLanguages;

  function ExtractLanguage(s: String): String;
  var
    p: Integer;
  begin
    s := ChangeFileExt(s, '');
    p := RPos('.', s);
    if p > 0 then
      Result := Copy(s, p+1, Length(s))
    else
      Result := '';
  end;

var
  L: TStrings;
  po: TStringList;
  lang: String;
  i: Integer;
  langdir: String;
begin
  L := TStringList.Create;
  po := TStringList.Create;
  try
    langdir := ExpandFileName(AppendPathDelim(Application.Location) + LANGUAGE_DIR);
    FindAllFiles(L, langdir, '*.po');
    po.Sorted := true;
    po.Duplicates := dupIgnore;
    for i := 0 to L.Count-1 do begin
      lang := ExtractLanguage(L[i]);
      case lang of
        'de': po.Add('de - Deutsch');
        '',
        'en': po.Add('en - English');
        'es': po.Add('es - Español');
        'fr': po.Add('fr - Français');
        'he': po.Add('he - Hebrew');
        'hu': po.Add('hu - magyar');
        'it': po.Add('it - Italian');
        'nl': po.Add('nl - Dutch');
        'ru': po.Add('ru - русский');
      end;
    end;

    CbLanguages.Items.Assign(po);
    SetLanguage(lang);
//    SetLanguage(GetDefaultLang);

  finally
    po.Free;
    L.Free;
  end;
end;

procedure TMainForm.PositionControls;
var
  w: Integer;
begin
  // DayView page
  DaySelectorPanel.Height := 2*CbGranularity.Top + CbGranularity.Height;

  // Tasks page
  Panel6.ClientHeight := 2*RbAllTasks.Top + ScaleY(RbAllTasks.Height, DesignTimeDPI);

  // Resources page
  w := MaxValue([
    GetButtonWidth(BtnNewRes),
    GetButtonWidth(BtnEditRes),
    GetButtonWidth(BtnDeleteRes)
  ]);
  BtnNewRes.Width := w;
  BtnEditRes.Width := w;
  BtnDeleteRes.Width := w;
  BtnEditRes.Left := RightOf(BtnNewRes) + 8;
  BtnDeleteRes.Left := RightOf(BtnEditRes) + 8;
  VpResourceCombo1.Width := RightOf(BtnDeleteRes) - BtnNewRes.Left;

  // Settings page
  w := MaxValue([
    GetLabelWidth(LblLanguage),
    GetLabelWidth(LblTimeFormat),
    GetLabelWidth(LblFirstDayOfWeek),
    GetLabelWidth(LblAddressBuilder)
  ]);
  CbLanguages.Left := 24 + w + 8;
  CbTimeFormat.Left := CbLanguages.Left;
  CbFirstDayOfWeek.Left := CbLanguages.Left;
  CbAddressBuilder.Left := CbLanguages.Left;
  CbDrawingStyle.Left := CbLanguages.Left;;
  LblLanguage.Left :=  CbLanguages.Left - 8 - GetLabelWidth(LblLanguage);
  LblTimeFormat.Left := CbTimeFormat.Left - 8 - GetLabelWidth(LblTimeFormat);
  LblFirstDayOfWeek.Left := CbFirstDayOfWeek.Left - 8 - GetLabelWidth(LblFirstDayOfWeek);
  LblAddressBuilder.Left := CbAddressBuilder.Left - 8 - GetLabelWidth(LblAddressBuilder);
  LblDrawingStyle.Left := CbDrawingStyle.Left - 8 - GetlabelWidth(LblDrawingStyle);

  CbAllowInplaceEditing.Left := CbLanguages.Left + CbLanguages.Width + 32;
  CbAllowDragAndDrop.Left := CbAllowInplaceEditing.Left;
  CbDragDropTransparent.Left := CbAllowInplaceEditing.Left;
  w := GetLabelWidth(LblDrawingStyle);

  // Planner pages
  DaysTrackbar.Left := GetLabelWidth(LblVisibleDays) + LblVisibleDays.Left + 8;
  LblGranularity.Left := DaysTrackbar.Left + DaysTrackbar.Width + 32;
  CbGranularity.Left := LblGranularity.Left + GetLabelWidth(LblGranularity) + 8;
  RbHideCompletedTasks.Left := RbAllTasks.Left + RbAllTasks.Width + 48;
end;

procedure TMainForm.RbAllTasksChange(Sender: TObject);
begin
  VpTaskList1.DisplayOptions.ShowAll := RbAllTasks.Checked;
end;

procedure TMainForm.RbHideCompletedTasksChange(Sender: TObject);
begin
  VpTaskList1.DisplayOptions.ShowAll := not RbHideCompletedTasks.Checked;
end;

procedure TMainForm.ReadIni;
var
  ini: TCustomIniFile;
  lang: String;
  L,T, W,H: Integer;
  R: TRect;
begin
  ini := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    WindowState := wsNormal;
    R := Screen.WorkAreaRect;
    L := ini.ReadInteger('Form', 'Left', Left);
    T := ini.ReadInteger('Form', 'Top', Top);
    W := ini.ReadInteger('Form', 'Width', Width);
    H := ini.ReadInteger('Form', 'Height', Height);
    if L < R.Left then L := R.Left;
    if L + W > R.Right then L := R.Right - W;
    if L < R.Left then W := R.Right - R.Left;
    if T < R.Top then T := R.Top;
    if T + H > R.Bottom then T := R.Bottom - H;
    if T < R.Top then H := R.Bottom - R.Top;
    SetBounds(L, T, W, H);

    w := ini.ReadInteger('Form', 'LeftPanel_Width', LeftPanel.Width);
    if w < 200 then w := 200;
    LeftPanel.Width := w;

    h := ini.ReadInteger('Form', 'BottomPanel_Height', VpMonthView1.Height);
    if h < 160 then h := 160;
    VpMonthView1.Height := h;

    lang := ini.ReadString('Settings', 'Language', ''); //GetDefaultLang);
    SetLanguage(lang);

    SetActiveView(ini.ReadInteger('Settings', 'ActiveView', 0));
    VpNavBar1.ActiveFolder := FActiveView div 1000;

    CbTimeFormat.ItemIndex := ini.ReadInteger('Settings', 'TimeFormat', ord(VpDayView1.TimeFormat));
    CbTimeFormatChange(nil);

    CbGranularity.ItemIndex := ini.ReadInteger('Settings', 'Granularity', ord(VpDayView1.Granularity));
    CbGranularityChange(nil);

    CbFirstDayOfWeek.ItemIndex := ini.ReadInteger('Settings', 'FirstDayOfWeek', ord(VpWeekView1.WeekStartsOn));
    CbFirstDayOfWeekChange(nil);

    if ini.ReadBool('Settings', 'AllTasks', VpTaskList1.DisplayOptions.ShowAll) then
      RbAllTasks.Checked := true else
      RbHideCompletedTasks.Checked := true;
    RbAllTasksChange(nil);

    FVisibleDays := ini.ReadInteger('Settings', 'VisibleDays', DaysTrackbar.Position);
    if FActiveView = 3 then begin  // DayView
      DaysTrackbar.Position := FVisibleDays;
      DaysTrackbarChange(nil);
    end;

    VpControlLink1.CityStateZipFormat := ini.ReadString('Settings', 'CityStateZip', '');
    if VpControlLink1.CityStateZipFormat = '' then
      CbAddressBuilder.ItemIndex := 0 else
      CbAddressBuilder.ItemIndex := CbAddressBuilder.Items.Indexof(VpControlLink1.CityStateZipFormat);

    CbDrawingStyle.ItemIndex := ini.ReadInteger('Settings', 'DrawingStyle',
      ord(dsFlat));
    CbDrawingStyleChange(nil);

    CbAllowInplaceEditing.Checked := ini.ReadBool('Settings', 'AllowInplaceEditing',
      CbAllowInplaceEditing.Checked);
    CbAllowInplaceEditingChange(nil);

    CbAllowDragAndDrop.Checked := ini.ReadBool('Settings', 'AllowDragAndDrop',
      CbAllowDragAndDrop.Checked);
    CbAllowDragAndDropChange(nil);

    CbDragDropTransparent.Checked := ini.ReadBool('Settings', 'DragAndDropTransparent',
      CbDragDropTransparent.Checked);
    CbDragDropTransparentChange(nil);

  finally
    ini.Free;
  end;
end;

procedure TMainForm.WriteIni;
var
  ini: TCustomIniFile;
begin
  ini := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    if WindowState = wsNormal then begin
      ini.WriteInteger('Form', 'Width', Width);
      ini.WriteInteger('Form', 'Height', Height);
      ini.WriteInteger('Left', 'Left', Left);
      ini.WriteInteger('Form', 'Top', Top);
    end;
    if FActiveView = 0 then begin
      ini.WriteInteger('Form', 'LeftPanel_Width', LeftPanel.Width);
      ini.WriteInteger('Form', 'BottomPanel_Height', VpMonthView1.Height);
    end;

    ini.WriteString('Settings', 'Language', FLang);
    ini.WriteInteger('Settings', 'ActiveView', FActiveView);
    ini.WriteInteger('Settings', 'TimeFormat', ord(VpDayView1.TimeFormat));
    ini.WriteInteger('Settings', 'Granularity', ord(VpDayView1.Granularity));
    ini.WriteInteger('Settings', 'FirstDayOfWeek', ord(VpWeekView1.WeekStartsOn));
    ini.WriteString('Settings', 'CityStateZip', VpControlLink1.CityStateZipFormat);
    ini.WriteInteger('Settings', 'DrawingStyle', CbDrawingStyle.ItemIndex);
    ini.WriteInteger('Settings', 'VisibleDays', FVisibleDays);
    ini.WriteBool('Settings', 'AllTasks', VpTaskList1.DisplayOptions.ShowAll);
    ini.WriteBool('Settings', 'AllowInplaceEditing', CbAllowInplaceEditing.Checked);
    ini.WriteBool('Settings', 'AllowDragAndDrop', CbAllowDragAndDrop.Checked);
    ini.WriteBool('Settings', 'DragAndDropTransparent', CbDragDropTransparent.Checked);
  finally
    ini.Free;
  end;
end;

procedure TMainForm.SetLanguage(AIndex: Integer);
var
  p: Integer;
  lang: String;
begin
  p := pos(' - ', CbLanguages.Items[AIndex]);
  if p > 0 then
    lang := Copy(CbLanguages.Items[AIndex], 1, p-1)
  else
    raise Exception.Create('Incorrect structure of language combobox.');
  SetLanguage(lang);
end;

procedure TMainForm.SetLanguage(ALang: String);
var
  i: Integer;
  langdir: String;
  found: Boolean;
  tfmt: TVpTimeFormat;
  firstWeekDay: TVpDayType;
  translator: TUpdateTranslator;
  nf: TVpNavFolder;
begin
  langdir := ExpandFileName(AppendPathDelim(Application.Location) + LANGUAGE_DIR);

  // Select new language
  if ALang = 'en' then
    FLang := '' else
    FLang := ALang;

  // Translate VisualPlanIt strings.
  if FLang = '' then begin
    TranslateUnitResourceStrings('vpsr', langdir + 'vpsr.po');

    { NOTE: Translation of app strings back to english not working }

    TranslateUnitResourceStrings('demoMain', langDir + 'demo.po');
    TranslateUnitResourceStrings('lclstrconsts', langDir + 'lclstrconsts.po');

    if FileExistsUTF8(langdir + 'demo.po') then begin
      translator := TPOTranslator.Create(langdir + 'demo.po');
      if Assigned(LRSTranslator) then
        LRSTranslator.Free;
      LRSTranslator := translator;
      for i := 0 to Screen.CustomFormCount-1 do
        translator.UpdateTranslation(Screen.CustomForms[i]);
    end;
  end
  else
  begin
    SetDefaultLang(FLang, langdir);
    TranslateUnitResourceStrings('vpsr', langdir + 'vpsr.' + FLang + '.po');
  end;
                        {
  VpDayView1.LoadLanguage;
  VpWeekView1.LoadLanguage;
  VpMonthView1.LoadLanguage;
  VpTaskList1.LoadLanguage;
  VpContactGrid1.LoadLanguage;
  //VpCalendar1.LoadLanguage;
                         }
  // Select language in language combobox.
  if ALang = '' then ALang := 'en';
  found := false;
  for i:=0 to CbLanguages.Items.Count-1 do
    if pos(ALang + ' ', CbLanguages.Items[i]) = 1 then begin
      CbLanguages.ItemIndex := i;
      found := true;
      break;
    end;
  if not found then
    CbLanguages.ItemIndex := 0;

  // Update UI strings
  nf := TVpNavFolder(VpNavBar1.FolderCollection.ItemByName('NFPlanner'));
  nf.Caption := RSPlanner;
  nf.ItemByName('NIEvents').Caption := RSEventsOverview;
  nf.ItemByName('NIEventsByMonth').Caption := RSEventsPerMonth;
  nf.ItemByName('NIEventsByWeek').Caption := RSEventsPerWeek;
  nf.ItemByName('NIEventsByDay').Caption := RSEventsPerDay;
  nf.ItemByName('NITasks').Caption := RSTasks;
  nf.ItemByName('NIContacts').Caption := RSContacts;

  nf := TVpNavFolder(VpNavBar1.FolderCollection.ItemByName('NFMaintenance'));
  nf.Caption := RSMaintenance;
  nf.ItemByname('NIResources').Caption := RSResources;
  nf.ItembyName('NISettings').Caption := RSSettings_short;

  CbTimeFormat.Items.Clear;
  CbTimeFormat.Items.Add(RS24hours);
  CbTimeFormat.Items.Add(RS12hours);

  CbGranularity.Items.Clear;
  CbGranularity.Items.Add(RS5Min);
  CbGranularity.Items.Add(RS6Min);
  CbGranularity.Items.Add(RS10Min);
  CbGranularity.Items.Add(RS15Min);
  CbGranularity.Items.Add(RS20Min);
  CbGranularity.Items.Add(RS30Min);
  CbGranularity.Items.Add(RS60Min);

  CbFirstDayOfWeek.Items.Clear;
  CbFirstDayOfWeek.Items.Add(RSSunday);
  CbFirstDayOfWeek.Items.Add(RSMonday);
  CbFirstDayOfWeek.Items.Add(RSTuesday);
  CbFirstDayOfWeek.Items.Add(RSWednesday);
  CbFirstDayOfWeek.Items.Add(RSThursday);
  CbFirstDayOfWeek.Items.Add(RSFriday);
  CbFirstDayOfWeek.Items.Add(RSSaturday);

  CbDrawingStyle.Items.Clear;
  CbDrawingStyle.Items.Add(RSFlat);
  CbDrawingStyle.Items.Add(RS3d);
  CbDrawingStyle.Items.Add(RSBorderless);

  OpenDialog.Filter := rsXMLFiles + '|*.xml';

  // Next settings work correctly only for Windows.
 {$IFDEF WINDOWS}
  UpdateFormatSettings(ALang);
  VpDayView1.DateLabelFormat := FormatSettings.LongDateFormat;
  VpWeekView1.DayHeadAttributes.DateFormat := FormatSettings.LongDateFormat;
  VpWeekView1.DateLabelFormat := FormatSettings.LongDateFormat;
  VpMonthView1.DateLabelFormat := 'mmmm yyyy';
  VpTaskList1.DisplayOptions.DueDateFormat := FormatSettings.ShortDateFormat;
  tfmt := GetTimeFormat;
  VpDayView1.TimeFormat := tfmt;
  VpWeekView1.TimeFormat := tfmt;
  VpMonthView1.TimeFormat := tfmt;
 {$ENDIF}
  firstWeekDay := GetFirstDayofWeek(ALang);   // not correct at the moment
  VpMonthView1.WeekStartsOn := firstWeekDay;
  VpWeekView1.WeekStartsOn := firstWeekDay;
  //VpCalendar1.WeekStarts := firstWeekDay;

  PositionControls;

  SetActiveView(1001);
  Invalidate;
end;

procedure TMainForm.ShowAllEvents;
begin
  Notebook.PageIndex := 0;
  VpDayView1.Parent := LeftPanel;
  VpMonthView1.Parent := LeftPanel;
  VpMonthView1.Align := alBottom;
  VpDayview1.Show;
  VpMonthView1.Show;
  Splitter2.Top := 0;
  LeftPanel.Show;
  Splitter3.Show;
  Splitter3.Left := Width;
  VpWeekView1.Show;
  DaySelectorPanel.Hide;
  VpDayView1.NumDays := 1;
  TitleLbl.Caption := RSEventsOverview;
  ImageList1.GetBitmap(0, Img.Picture.Bitmap);
end;

procedure TMainform.ShowEventsPerMonth;
begin
  Notebook.PageIndex := 0;
  LeftPanel.Hide;
  Splitter3.Hide;
  VpDayView1.Hide;
  VpWeekView1.Hide;
  VpMonthView1.Parent := Notebook.Page[0];
  VpMonthView1.Align := alClient;
  VpMonthView1.Show;
  DaySelectorPanel.Hide;
  TitleLbl.Caption := RSEventsPerMonth;
  ImageList1.GetBitmap(5, Img.Picture.Bitmap);
end;

procedure TMainForm.ShowEventsPerWeek;
begin
  Notebook.PageIndex := 0;
  LeftPanel.Hide;
  Splitter3.Hide;
  VpMonthView1.Hide;
  VpDayView1.Hide;
  VpWeekView1.Show;
  DaySelectorPanel.Hide;
  TitleLbl.Caption := RSEventsPerWeek;
  ImageList1.GetBitmap(4, Img.Picture.Bitmap);
end;

procedure TMainform.ShowEventsPerDay;
begin
  Notebook.PageIndex := 0;
  LeftPanel.Hide;
  Splitter3.Hide;
  VpMonthView1.Hide;
  VpWeekView1.Hide;
  VpDayView1.Parent := Notebook.Page[Notebook.PageIndex];
  VpDayView1.Align := alClient;
  VpDayView1.Show;
  DaySelectorPanel.Parent := Notebook.Page[Notebook.PageIndex];
  DaySelectorPanel.Show;
  DaysTrackbar.Position := FVisibleDays;
  VpDayView1.NumDays := DaysTrackBar.Position;
  TitleLbl.Caption := RSEventsPerDay;
  ImageList1.GetBitmap(3, Img.Picture.Bitmap);
end;

procedure TMainForm.ShowTasks;
begin
  Notebook.PageIndex := 1;
  titleLbl.Caption := RSTasks;
  ImageList1.GetBitmap(1, Img.Picture.Bitmap);
end;

procedure TMainForm.VpBufDSDataStore1PlaySound(Sender: TObject;
  const AWavFile: String; AMode: TVpPlaySoundMode);
begin
  sound.PlaySound(AWavFile, AMode);
end;

procedure TMainForm.ShowContacts;
begin
  Notebook.PageIndex := 2;
  TitleLbl.Caption := RSContacts;
  ImageList1.GetBitmap(2, Img.Picture.Bitmap);
end;

procedure TMainForm.ShowResources;
begin
  Notebook.PageIndex := 3;
  TitleLbl.Caption := RSResources;
  ImageList1.GetBitmap(7, Img.Picture.Bitmap);
end;

procedure TMainForm.ShowSettings;
begin
  Notebook.PageIndex := 4;
  TitleLbl.Caption := RSSettings;
  ImageList1.GetBitmap(8, Img.Picture.Bitmap);
end;

procedure TMainForm.DaysTrackBarChange(Sender: TObject);
begin
  if FActiveView = 3 then
    FVisibleDays := DaysTrackbar.Position;
  VpDayView1.NumDays := DaysTrackBar.Position;
end;

procedure TMainForm.VpNavBar1ItemClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; Index: Integer);
begin
 SetActiveView(VpNavBar1.ActiveFolder * 1000 + Index);
end;

procedure TMainForm.SetActiveView(AValue: Integer);
var
  folderIndex, itemIndex: Integer;
begin
  FActiveView := AValue;
  folderIndex := AValue div 1000;
  itemIndex := AValue mod 1000;
  case folderIndex of
   0: case itemIndex of          // All planner items
        0: ShowAllEvents;        // show all
        1: ShowEventsPerMonth;   // Month view only
        2: ShowEventsPerWeek;    // Week view only
        3: ShowEventsPerDay;     // Day view only
        4: ShowTasks;            // Tasks
        5: ShowContacts;         // Contacts
      end;
   1: case itemIndex of
        0: ShowResources;
        1: ShowSettings;
      end;
  end;
end;

end.

