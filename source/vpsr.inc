{*********************************************************}
{*                    VPSR.INC 1.03                      *}
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
{* Portions created by TurboPower Software Inc. are Copyright (c) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{Visual PlanIt String Resources - To create language specific versions of
 Visual PlanIt controls, translate these strings to the desired language
 and re-compile your application.}

resourcestring
  RSNoneStr                 = '(None)';
  RSYes                     = 'Y';
  RSNo                      = 'N';
  RSTrue                    = 'T';
  RSFalse                   = 'F';
  RSDelete                  = 'Delete';
  RSNotDoneYet              = 'This feature is not implemented at this time.';
  RSNoTimersAvail           = 'No Window''s timers are available.';
  RSBadTriggerHandle        = 'Invalid trigger handle.';
  RSEditingItems            = 'Folder Items Editor.';
  RSEditingFolders          = 'Folder Editor.';
  RSExclusiveEventConflict  = 'Conflicts with another exclusive event.';
  RSBackwardTimesError      = 'The end time cannot precede the start time.';
  RSDBPostError             = 'Error posting data to the database.';
  RSMonthConvertError       = 'Error converting the month number.';
  RSInvalidDay              = 'Error: Invalid Day.';
  RSInvalidDate             = 'Error: Invalid Date.';
  RSInvalidMonth            = 'Error: Invalid Month.';
  RSInvalidMonthName        = 'Error: Invalid Month Name.';
  RSInvalidYear             = 'Error: Invalid Year.';
  RSDayIsRequired           = 'Error: Day is required.';
  RSMonthIsRequired         = 'Error: Month Is Required.';
  RSYearIsRequired          = 'Error: Year is required.';
  RSNameIsRequired          = 'Error: Name cannot be empty.';
  RSFailToCreateTask        = 'Error: Failure while creating Task.';
  RSFailToCreateEvent       = 'Error: Failure while creating Event.';
  RSFailToCreateContact     = 'Error: Failure while creating Contact.';
  RSFailToCreateResource    = 'Error: Failure while creating Resource.';
  RSDuplicateResource       = 'Error: Duplicate Resource.';
  RSInvalidTableSpecified   = 'Error: Invalid table specified.';
  RSUnableToOpen            = 'Error: Unable to open ';
  RSSQLUpdateError          = 'Error: Unable to update ';                {!!.01}

  RSPhoneTypeLabel1         = 'Assistant';
  RSPhoneTypeLabel2         = 'Callback';
  RSPhoneTypeLabel3         = 'Car';
  RSPhoneTypeLabel4         = 'Company';
  RSPhoneTypeLabel5         = 'Home';
  RSPhoneTypeLabel6         = 'Home Fax';
  RSPhoneTypeLabel7         = 'ISDN';
  RSPhoneTypeLabel8         = 'Mobile';
  RSPhoneTypeLabel9         = 'Other';
  RSPhoneTypeLabel10        = 'Other Fax';
  RSPhoneTypeLabel11        = 'Pager';
  RSPhoneTypeLabel12        = 'Primary';
  RSPhoneTypeLabel13        = 'Radio';
  RSPhoneTypeLabel14        = 'Telex';
  RSPhoneTypeLabel15        = 'TTY/TDD';
  RSPhoneTypeLabel16        = 'Work';
  RSPhoneTypeLabel17        = 'Work Fax';

  RSCategoryLabel1          = 'Business';
  RSCategoryLabel2          = 'Clients';
  RSCategoryLabel3          = 'Family';
  RSCategoryLabel4          = 'Personal';
  RSCategoryLabel5          = 'Other';

  RSWeekOf                  = 'Week of';
  RSThrough                 = 'Through';
  RSSunday                  = 'Sunday';
  RSMonday                  = 'Monday';
  RSTuesday                 = 'Tuesday';
  RSWednesday               = 'Wednesday';
  RSThursday                = 'Thursday';
  RSFriday                  = 'Friday';
  RSSaturday                = 'Saturday';
  RSASunday                 = 'Sun';
  RSAMonday                 = 'Mon';
  RSATuesday                = 'Tue';
  RSAWednesday              = 'Wed';
  RSAThursday               = 'Thu';
  RSAFriday                 = 'Fri';
  RSASaturday               = 'Sat';
  RSLSunday                 = 'S';
  RSLMonday                 = 'M';
  RSLTuesday                = 'T';
  RSLWednesday              = 'W';
  RSLThursday               = 'T';
  RSLFriday                 = 'F';
  RSLSaturday               = 'S';
  RSNone                    = 'None';
  RSDaily                   = 'Daily';
  RSWeekly                  = 'Weekly';
  RSMonthlyByDay            = 'Monthly By Day';
  RSMonthlyByDate           = 'Monthly By Date';
  RSYearlyByDay             = 'Yearly By Day';
  RSYearlyByDate            = 'Yearly By Date';
  RSCustom                  = 'Custom';
  RSMinutes                 = 'Minutes';
  RSHours                   = 'Hours';
  RSDays                    = 'Days';
  RSWeeks                   = 'Weeks';
  RSMonths                  = 'Months';
  RSYears                   = 'Years';
  RSCalendarWeekAbbr        = 'CW';   // Abbreviation of "calendar week"

  {WARNINGS}
  RSPermanent               = 'This operation cannot be undone!';

  {Contact Specific}
  RSContactPopupAdd         = 'Add Contact...';
  RSContactPopupEdit        = 'Edit Contact...';
  RSContactPopupDelete      = 'Delete Contact...';
  RSConfirmDeleteContact    = 'Delete contact %s?';

  {Event Specific}
  RSFromSchedule            = 'from your schedule?';
  RSConfirmDeleteEvent      = 'Delete event from schedule?';
  RSStartEndTimeError       = 'Incorrect order of start and end times. ' +
                              'Do you want to flip them?';

  {Task Specific}
  RSConfirmDeleteTask       = 'Delete this task from your list?';
  RSTaskPopupAdd            = 'Add Task...';
  RSTaskPopupEdit           = 'Edit Task...';
  RSTaskPopupDelete         = 'Delete Task...';
  RSTaskTitleResource       = 'Task List - ';                            {!!.01}
  RSTaskTitleNoResource     = 'Task List';                               {!!.01}

  {Month Specific}
  RSMonthPopupToday         = 'Today';
  RSMonthPopupNextMonth     = 'Next Month';
  RSMonthPopupPrevMonth     = 'Previous Month';
  RSMonthPopupNextYear      = 'Next Year';
  RSMonthPopupPrevYear      = 'Previous Year';

  {Week Specific}
  RSWeekPopupAdd            = 'Add Event...';
  RSWeekPopupEdit           = 'Edit Event...';
  RSWeekPopupDelete         = '&Delete Event...';
  RSWeekPopupNav            = 'Change Date';
  RSWeekPopupNavToday       = 'Today';
  RSWeekPopupNavNextWeek    = 'Next Week';
  RSWeekPopupNavPrevWeek    = 'Previous Week';
  RSWeekPopupNavNextMonth   = 'Next Month';
  RSWeekPopupNavPrevMonth   = 'Previous Month';
  RSWeekPopupNavNextYear    = 'Next Year';
  RSWeekPopupNavPrevYear    = 'Previous Year';

  { Print Preview Specific }
  RSPrintPrvPrint           = '&Print';
  RSPrintPrvPrintHint       = 'Print';
  RSPrintPrvPrevPage        = 'Pre&vious page';
  RSPrintPrvNextPage        = '&Next page';
  RSPrintPrvFirstPage       = '&First page';
  RSPrintPrvLastPage        = '&Last page';
  RSPrintPrvFirstPageHint   = 'Go to first page';
  RSPrintPrvPrevPageHint    = 'Go to previous page';
  RSPrintPrvNextPageHint    = 'Go to next page';
  RSPrintPrvLastPageHint    = 'Go to last page';
  RSPrintPrvCancel          = 'Cancel';
  RSPrintPrvCancelHint      = 'Cancel preview';

  { DayView Specific }
  RSDayPopupAdd             = 'Add Event...';
  RSDayPopupEdit            = 'Edit Event...';
  RSDayPopupDelete          = 'Delete Event...';
  RSDayPopupNav             = 'Change Date';
  RSDayPopupNavToday        = 'Today';
  RSDayPopupNavTomorrow     = 'Tomorrow';
  RSDayPopupNavYesterday    = 'Yesterday';
  RSDayPopupNavNextDay      = 'Next Day';
  RSDayPopupNavPrevDay      = 'Previous Day';
  RSDayPopupNavNextWeek     = 'Next Week';
  RSDayPopupNavPrevWeek     = 'Previous Week';
  RSDayPopupNavNextMonth    = 'Next Month';
  RSDayPopupNavPrevMonth    = 'Previous Month';
  RSDayPopupNavNextYear     = 'Next Year';
  RSDayPopupNavPrevYear     = 'Previous Year';
  RSHintToday               = 'Today';                                   {!!.01}
  RSHintTomorrow            = 'Tomorrow';                                {!!.01}
  RSHintYesterday           = 'Yesterday';                               {!!.01}
  RSHintNextWeek            = 'Next week';                               {!!.01}
  RSHintPrevWeek            = 'Previous week';                           {!!.01}
  RSHintPrevDay             = 'Previous day';
  RsHintNextDay             = 'Next day';

  { field names }
  RSPosition                = 'Position';
  RSCompany                 = 'Company';
  RSTitle                   = 'Title';
  RSEMail                   = 'E-Mail';
  RSCountry                 = 'Country';
  RSCategory                = 'Category';
  RSNotes                   = 'Notes';

  RSCustom1                 = 'Custom 1';
  RSCustom2                 = 'Custom 2';
  RSCustom3                 = 'Custom 3';
  RSCustom4                 = 'Custom 4';

  { Generic Dialog Captions }
  RSOKBtn                   = 'OK';
  RSCancelBtn               = 'Cancel';
  RSCloseBtn                = '&Close';
  RSPrintBtn                = '&Print';
  RSUntitled                = 'Untitled';

  {Sound Selection Dialog}
  RSSelectASound            = 'Select A Sound';
  RSSoundFinder             = 'Sound Finder';
  RSDefaultSound            = 'Use the default sound';
  RSNothingToSelectFrom     = 'There is nothing to select if default sound is used.';

  { Event Edit Dialog Captions }
  RSDlgEventEdit            = 'Event';
  RSAppointmentGroupBox     = 'Appointment';
  RSDescriptionLbl          = 'Description:';
  RSLocationLbl             = 'Location:';
  RSCategoryLbl             = 'Category:';
  RSStartTimeLbl            = 'Start time:';
  RSEndTimeLbl              = 'End time:';
  RSAlarmSet                = '&Reminder';
  RSRecurringLbl            = 'Appointment recurrence:';
  RSIntervalLbl             = 'Interval (days):';
  RSRecurrenceEndsLbl       = 'Repeat until:';
  RSAllDayEvent             = '&All Day Event';
  RSNotesLbl                = 'Notes:';

  { Contact Edit Dialog Captions }
  RSDlgContactEdit          = 'Contact';
  RSLastNameLbl             = 'Last name:';
  RSFirstNameLbl            = 'First name:';
  RSTitleLbl                = 'Title:';
  RSAddressLbl              = 'Address:';
  RSCityLbl                 = 'City:';
  RSStateLbl                = 'State:';
  RSCountryLbl              = 'Country:';
  RSZipCodeLbl              = 'Zip code:';
  RSCompanyLbl              = 'Company:';
  RSPositionLbl             = 'Position:';
  RSBirthDateLbl            = 'Birth date:';
  RSMasterData              = 'Master data';

  { Print Preview dialog captions }
  RSDlgPrintPreview        = 'Print preview';

  { Task Edit Dialog Captions }
  RSDlgTaskEdit            = 'Task';
  RSDueDate                = 'Due Date:';
  RSDetails                = 'Details:';
  RSTaskComplete           = 'Task complete';
  RSCreatedOn              = 'Created on';
  RSCompletedOn            = 'Completed on';

  { Resource Edit Dialog Captions }
  RSDlgResEditCaption      = 'Resource Edit';
  RSDlgResEditDescription  = 'Description';
  RSResource               = 'Resource';

  { Reminder Dialog Captions}
  RSReminder               = 'Reminder';
  RSOverdue                = 'OVERDUE!';
  RSSnoozeCaption          = 'Click &Snooze to be reminded again in:';
  RSSubjectCaption         = 'Subject:';
  RSNotesCaption           = 'Notes:';
  RSDismissBtn             = '&Dismiss';
  RSSnoozeBtn              = '&Snooze';
  RSOpenItemBtn            = '&Open Item';
  RS1Minute                = '1 minute';
  RSXMinutes               = '%d minutes';
  RS1Hour                  = '1 hour';
  RSXHours                 = '%d hours';
  RS1Day                   = '1 day';
  RSXDays                  = '%d days';
  RS1Week                  = '1 week';

  { Calendar }
  RSCalendarPrevMonth      = 'Previous Month';
  RSCalendarNextMonth      = 'Next Month';
  RSCalendarPrevYear       = 'Previous Year';
  RSCalendarNextYear       = 'Next Year';
  RSCalendarToday          = 'Today';
  RSCalendarRevert         = 'Revert';

  RSCalendarPopupToday     = 'Today';
  RSCalendarPopupNextMonth = 'Next Month';
  RSCalendarPopupPrevMonth = 'Previous Month';
  RSCalendarPopupNextYear  = 'Next Year';
  RSCalendarPopupPrevYear  = 'Previous Year';
  RSCalendarPopupRevert    = 'Revert';

  { XML }
  sIENotInstalled    = 'Cannot open WININET, Microsoft IE required';
  sOpenFileFailed    = 'Unable to open file ';
  sFileNotFound      = 'File %s could not be found';
  sAllocSrcMemFailed = 'Unable to allocate memory for XML source';
  sHttpReadReqFailed = 'Http read request failed';
  sHttpDataNotAvail  = 'Http data not available';
  sHttpReqSendFailed = 'Unable to send http request';
  sHttpReqOpenFailed = 'Unable to open http request';
  sInetConnectFailed = 'Unable to make Internet connection';
  sInetOpenFailed    = 'Unable to open Internet';
  sInvalidFtpLoc     = 'Invalid ftp location';
  sInvalidFtpDir     = 'Invalid ftp directory';
  sFtpReadReqFailed  = 'Ftp read request failed';
  sFtpDataNotAvail   = 'Ftp data not available';
  sFtpOpenFileFailed = 'Unable to open ftp file';
  sFtpPutFileFailed  = 'Could not save file via ftp to %s';
  sSrcLoadFailed     = 'Unable to load source ';
  sInvalidMemPtr     = 'Invalid memory Pointer';
  sFmtErrorMsg       = 'Line: %d Col: %d Error: %s';
  sIndexOutOfBounds  = 'ERROR INDEX OUT OF BOUNDS';
  sExpMarkupDecl     = 'Expected markup declaration, but found: ';
  sIllAttrType       = 'Illegal attribute type: ';
  sIllAttrDefKeyw    = 'Illegal keyword for attribute default value: ';
  sSysIdMissing      = 'System identifier missing ';
  sExtModifMissing   = 'External modifier missing: ';
  sIllCondSectStart  = 'Conditional section must begin with INCLUDE or IGNORE';
  sBadSepInModel     = 'Bad separator in content model: ';
  sExpCommentOrCDATA = 'Expected comment or CDATA section ';
  sUnexpectedEof     = 'Unexpected end of file ';
  sMismatchEndTag    = 'Mismatched end tag: ';
  sIllCharInRef      = 'Illegal character in reference';
  sUndeclaredEntity  = 'Reference to undeclared entity: ';
  sExpectedString    = 'Expected String: ';
  sSpaceExpectedAt   = 'Whitespace expected at byte ';
  sUnexpEndOfInput   = 'End of input while looking for delimiter: ';
  sQuoteExpected     = 'Expected " or ' + Chr (39);
  sInvalidXMLVersion = 'XMLPartner does not support XML specification greater than %s';
  sUnableCreateStr   = 'Unable to create stream for input.';
  sInvalidName       = 'Invalid XML name: ';
  sInvalidCommentText= 'Invalid comment text';
  sCommentBeforeXMLDecl = 'Document cannot start with a comment if it also contains an XML declaration';
  sInvalidCDataSection = 'Invalid characters in CDATA section';
  sRedefinedAttr     = 'Attributes cannot be redefined in a start tag';
  sCircularEntRef    = 'Circular reference to: ';
  sInvAttrChar       = 'Invalid character in attribute value: ';
  sInvPCData         = 'Invalid characters in element''s character data: ';
  sDataAfterValDoc   = 'There is invalid data after valid XML document';
  sNoIntConditional  = 'Conditional sections not allowed in internal subset of document type declaration';
  sNotationNotDeclared = 'Notation not declared: ';
  sInvPubIDChar      = 'Invalid PublicID character: ';
  sNoNDATAInPeDecl   = 'NDATA not allowed in parameter entity declaration';
  sInvStandAloneVal  = 'Standalone value must equal ''yes'' or ''no''';
  sInvEncName        = 'Invalid encoding declaration: ';
  sInvVerNum         = 'Invalid XML version number: ';
  sInvEntityValue    = 'Invalid character in entity value: ';
  sNoCommentInMarkup = 'Comments can not be placed within other markup';
  sNoPEInIntDTD      = 'Parameter entities not allowed in DTD internal subset';
  sXMLDecNotAtBeg   =  'The XML declaration must appear before the first element';
  sInvalidElementName = 'Invalid element name: ';
  sBadParamEntNesting = 'Parameter-entity text must be properly nested: ';
  sInvalidCharEncoding = 'Invalid character encoding specified.';
  sAttrNotNum        = 'Attribute %s of element %s does not have an integer value.';
  sUnknownAxis       = 'Unknown axis specifier: %s';

  {xpchrstm related errors}
  sInvalidXMLChar    = 'Invalid XML Character found';
  sInvalidBEChar     = 'Invalid (big-endian) UTF-16 character encoding';
  sInvalidLEChar     = 'Invalid (little-endian) UTF-16 character encoding';
  sBadUTF8Char       = 'Badly formed UTF-8 character in stream';
  sErrEndOfDocument  = 'Unexpected end of document stream';
  sUCS_ISOConvertErr = 'Cannot convert UCS-4 character to ISO-8859-1';
  sUCS_U16ConvertErr = 'Cannot convert UCS-4 character to UTF-16';
  sUCS_U8ConverErr   = 'Cannot convert UCS-4 character to UTF-8';

  { Misc Exceptions and Errors }
  RSOutOfRange       = 'Out of range';
  RSNotSupported     = 'not supported';
  RSNeedElementName  = 'Please supply an Element Name';
  RSNeedFormatName   = 'FormatName cannot be blank';
  RSPrtControlOwner  = 'Print controller is not owned by a TVpControlLink!';
  RSBadPrintFormat   = 'Invalid print format ';
  RSBadItemType      = 'Invalid item type ';
  RSBadMeasurement   = 'Invalid measurement';
  RSOwnerNotWinCtrl  = 'Owner must be a TWinControl descendent';
  RSNoControlLink    = 'Component must be linked to a TVpControlLink';
  RSNoPrintFormats   = 'No print formats have been defined';             {!!.01}
  RSNoCanvas         = 'TCanvas not assigned';                           {!!.01}
  RSNoLocalizationFile = 'Localization file not found.';                 {!!.02}

  { Misc strings }
  RSCategoryDesc     = 'Category %d';

  { Print Format Editor }
  RSEditPrintFormat  = 'Edit Print Formats...';
  RSPrintFormatDesigner = 'Print format designer';
  RSFormatLbl        = 'Format:';
  RSFormats          = 'Formats:';
  RSElements         = 'Elements:';
  RSNewBtn           = 'New';
  RSEditBtn          = 'Edit';
  RSDeleteBtn        = 'Delete';
  RSPrintOrder       = 'Print order';
  RSNewFileBtn       = 'New file';
  RSLoadFileBtn      = 'Load file...';
  RSSaveFileBtn      = 'Save file...';

  RSReportSetup      = 'Report setup';

  { Print Format Item Editor }
  RSEditFormatCaption= 'Edit format';
  RSNameLbl          = 'Name:';
  RSTimeIncLbl       = 'Time increment:';
  RSTimeIncUnits     = 'Time increment units:';

  { Format element editor }
  RSEditElementCaption = 'Edit element';
  RSElementTypeLbl   = 'Element type';
  RSDayViewElement   = 'Day view';
  RSWeekViewElement  = 'Week view';
  RSMonthViewElement = 'Month view';
  RSCalendarElement  = 'Calendar';
  RSShapeElement     = 'Shape';
  RSCaptionElement   = 'Caption';
  RSTasksElement     = 'Tasks';
  RSContactsElement  = 'Contacts';
  RSVisualCaption    = 'Visual';
  RSRotationCaption  = 'Rotation';
  RSMeasurementCaption = 'Measurement';
  RSPixels           = 'Pixels';
  RSPercent          = 'Percent';
  RSInches           = 'Inches';
  RSLeft             = 'Left';
  RSTop              = 'Top';
  RSWidth            = 'Width';
  RSHeight           = 'Height';
  RSVisible          = 'Visible';
  RSCaption          = 'Caption';
  RSTextCaption      = 'Text';
  RSShapeBtn         = 'Shape...';
  RSFontBtn          = 'Font...';

  { Shape editor }
  RSEditShapeCaption = 'Edit shape';
  RSShapeCaption     = 'Shape';
  RSRectangle        = 'rectangle';
  RSTopLine          = 'top line';
  RSBottomLine       = 'bottom line';
  RSLeftLine         = 'left line';
  RSRightLine        = 'right line';
  RSTLToBRLine       = 'top-left to bottom-right line';
  RSBLToTRLine       = 'bottom-left to top-right line';
  RSEllipse          = 'ellipse';
  RSPenCaption       = 'Pen';
  RSBrushCaption     = 'Brush';
  RSStyleLbl         = 'Style:';
  RSWidthLbl         = 'Width:';
  RSModeLbl          = 'Mode:';
  RSColorLbl         = 'Color:';

  { Automatic resource adding/selection}                               {!!.01}
  RSAddNewResource   = 'No resources have been defined. Would you ' +  {!!.01}
                       'like to add one now?';                         {!!.01}
  RSSelectResource   = 'No resource has been selected. Would you ' +   {!!.01}
                       'like to select one now?';

  { AboutBox }
  RSBrowserError     = 'Unable to start web browser. Make sure you have ' +
                       'it properly setup on your system.';

  { Ini storage }
  RSIniFileStructure = 'Incorrect structure of ini file.';

