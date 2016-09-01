{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit laz_visualplanit;

{$warn 5023 off : no warning about unused units}
interface

uses
  VpAlarmDlg, VpBase, VpBaseDS, VpCalendar, VpCanvasUtils, VpConst, 
  VpContactButtons, VpContactEditDlg, VpContactGrid, VpData, VpDatePropEdit, 
  VpDayView, VpDBDS, VpDlg, VpEdElem, VpEdFmt, VpEdShape, VpEvntEditDlg, 
  VpException, VpLEDLabel, VpLocalize, VpMisc, VpMonthView, VpNavBar, 
  VpPrtFmt, VpPrtFmtCBox, VpPrtFmtDlg, VpPrtFmtEd, VpPrtPrv, VpPrtPrvDlg, 
  VpResEditDlg, VpSelResDlg, VpSR, VpTaskEditDlg, VpTaskList, VpTimerPool, 
  VpWavDlg, VpWavPE, VpWeekView, VpXBase, VpXChrFlt, VpXParsr, VpFlxDS, 
  VpAbout, VpFlxDsEd1, VpNabEd, VpReg, VpBufDS, VpDayViewPainter, 
  VpWeekViewPainter, VpMonthViewPainter, VpBasePainter, VpContactGridPainter, 
  VpTasklistPainter, VpCalendarPainter, VpIniDs, VpSQLite3DS, VpXmlDs, 
  VpNavBarPainter, VpFBDS, VpEdFmtLst, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('VpReg', @VpReg.Register);
end;

initialization
  RegisterPackage('laz_visualplanit', @Register);
end.
