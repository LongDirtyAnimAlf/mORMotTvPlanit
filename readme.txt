TurboPower Visual PlanIt


Table of contents

1.  Introduction
2.  Package names
3.  Installation
4.  Version history
4.1   Release 1.03
4.2   Release 1.04

==============================================


1. Introduction

Visual PlanIt is a set of synchronized, data-aware components for
adding time, task, & contact management capabilities to applications
written in Borland Delphi & C++Builder. Get that Outlook look & feel
without the hassle.

This is a source-only release of TurboPower Visual PlanIt. It includes
designtime packages for Delphi 4 through 7 and C++Builder 4 through 6.

==============================================

2. Package names


TurboPower Visual PlanIt package names have the following form:

  VNNNKKVV.*
   |  | |
   |  | +------ VV  VCL version (30=Delphi 3, 40=Delphi 4, 70=Delphi 7)
   |  +-------- K   Kind of package (_D=designtime, AD = Advantage DataStore,
   |                                 IS = DBISAM DataStore, F2 = FlashFiler 2 DataStore)
   |
   +----------- NNN Product version number (e.g., 403=version 4.03)


For example, the Visual PlanIt designtime package files for Delphi 7 have
the filename V103_D70.*.

==============================================

3. Installation


To install TurboPower Visual PlanIt into your IDE, take the following
steps:

  1. Unzip the release files into a directory (e.g., d:\vplanit).

  2. Start Delphi or C++Builder.

  3. Add the source subdirectory (e.g., d:\vplanit\source) to the
     IDE's library path.

  4. Open & install the designtime package specific to the IDE being
     used. The IDE should notify you the components have been
     installed.

  5. Make sure the PATH environmental variable contains the directory
     in which the compiled packages (i.e., BPL or DPL files) were
     placed.

==============================================

4. Version history


4.1 Release 1.03

  Please note that the following issue #s are from Bugzilla. These
  bugs were not exported to SourceForge.

  Bug fixes
  -------------------------------------------------------------
  3547 - List Index out of Bounds error
  3589 - Needs OnDblClick Event
  3877 - ContactGrid won't scroll to a newly selected contact if it is
         out of view.
  3979 - FlexDataStore bug
  4021 - TVpTask.SetChanged marks Events dirty instead of taks. (duh!)
  4076 - VPDBISAMDataStore needs an AfterPost event.
  4078 - 12 and 24 hour display backward in the Events
  4079 - Using the DBIsamDataStore, recurring events show up under all
         resources.
  4080 - De Piggify the DBIsamDataStore component.

  
4.2 Release 1.04

  This release is Lazarus-only. It won't compile under Delphi any more.
  
  misc - Replace hard-coded MS Sans Serif font by "default" to use the system 
         font
  misc - Fix crashes due to inplace editors being destroyed.
  misc - Replace lrs resources by res resources. FPC 2.4 or greater required!
  misc - Add FlexDatastore and its component editor to package.
  4710 - Rename package to laz_visualplanit.
  4717 - Add new event field "Location". Rename some fields for more consistency. 
  misc - Add datastores for TBufDataset, Sqlite3, ZEOS, Firebird
  misc - Add non-database datastores for xml and ini files
  misc - Fix NavBar, PrintPreview, VpClock etc.
  misc - Translate user interface in po files
  misc - DPI-aware and translation-tolerant form layout
  misc - Add new sample projects

  

