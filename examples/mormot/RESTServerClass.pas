unit RESTServerClass;

interface

{$I Synopse.inc}

{.$define USEWRAPPERS}

uses
  SysUtils,
  Classes,
  SynCommons,
  SynLog,
  mORMot,
  mORMotSQLite3,
  SynSQLite3Static,
  RESTData;

type
  EVpServer = class(EORMException);
  
  //TVpServer = class(TSQLRestServerFullMemory)
  TVpServer = class(TSQLRestServerDB)
  protected
    fRootFolder: TFileName;
  public
    constructor Create(const aRootFolder: TFileName; const aRootURI: RawUTF8); reintroduce;
    destructor Destroy; override;
    property RootFolder: TFileName read fRootFolder;
  end;

implementation

uses
  {$ifdef USEWRAPPERS}
  mORMotWrappers,      // <= allow cross-platform client wrappers
  SynMustache,
  {$ifdef Windows}
  Windows, // needed for RT_RCDATA
  {$endif}
  {$endif}
  SynSQLite3;

{ TVpServer }

constructor TVpServer.Create(const aRootFolder: TFileName;
  const aRootURI: RawUTF8);
{$ifdef USEWRAPPERS}
procedure SaveWrappersFromResource(filename,resourcename:string);
var
  fs:Tfilestream;
begin
  if (NOT FileExists(filename)) then
  with TResourceStream.Create(hInstance, resourcename, RT_RCDATA) do
  try
    try
      fs:=Tfilestream.Create(Filename,fmCreate);
      Savetostream(fs);
    finally
      fs.Free;
    end;
  finally
    Free;
  end;
end;
var
  aDirName:string;
{$endif}
begin
  fRootFolder := EnsureDirectoryExists(ExpandFileName(aRootFolder),true);

  // define the log level
  with TSQLLog.Family do begin
    Level := LOG_VERBOSE; // LOG_STACKTRACE;
    DestinationPath := fRootFolder+'..\log\';
    if not FileExists(DestinationPath) then
      CreateDir(DestinationPath);
    PerThreadLog := ptIdentifiedInOnFile;
  end;


  // prepare the simple server for in-memory storage
  // for TSQLRestServerFullMemory
  //inherited Create(DataModel(aRootURI),fRootFolder+'data.json',false,false);
  //UpdateToFile;



  // prepare the SQLite3 server with authentication
  // for TSQLRestServerDB
  inherited Create(DataModel,fRootFolder+'data.db3',True);

  // make it fast !!
  DB.Synchronous := smOff;
  DB.LockingMode := lmExclusive;

  // create the tables
  CreateMissingTables;

  // create indexes
  CreateSQLIndex(TSQLVpResource, ['ResourceID'], False);
  CreateSQLIndex(TSQLVpEvent, ['RecordID'], False);
  CreateSQLIndex(TSQLVpContact, ['RecordID'], False);
  CreateSQLIndex(TSQLVpTask, ['RecordID'], False);

  TSQLVpResource.AddFilterNotVoidText(['Description']);
  TSQLVpEvent.AddFilterNotVoidText(['Description','ResourceID']);
  TSQLVpContact.AddFilterNotVoidText(['FirstName','LastName','ResourceID']);
  TSQLVpTask.AddFilterNotVoidText(['Description','ResourceID']);

  {
  Cache.SetCache(TSQLVpResource);
  Cache.SetCache(TSQLVpEvent);
  Cache.SetTimeOut(TSQLVpResource,60000);
  Cache.SetTimeOut(TSQLVpEvent,60000);
  }

  {$ifdef USEWRAPPERS}

  if Self.InheritsFrom(TSQLRestServer) then
  begin
    aDirName := fRootFolder+DirectorySeparator+'templates';
    if not FileExists(aDirName) then  CreateDir(aDirName);
    SaveWrappersFromResource(aDirName+DirectorySeparator+'API.adoc.mustache','API.ADOC');
    SaveWrappersFromResource(aDirName+DirectorySeparator+'CrossPlatform.pas.mustache','CROSSPLATFORM.PAS');
    SaveWrappersFromResource(aDirName+DirectorySeparator+'Delphi.pas.mustache','DELPHI.PAS');
    SaveWrappersFromResource(aDirName+DirectorySeparator+'FPC-mORMotInterfaces.pas.mustache','FPC-MORMOTINTERFACES.PAS');
    SaveWrappersFromResource(aDirName+DirectorySeparator+'FPCServer-mORMotServer.pas.mustache','FPCSERVER-MORMOTSERVER.PAS');
    SaveWrappersFromResource(aDirName+DirectorySeparator+'SmartMobileStudio.pas.mustache','SMARTMOBILESTUDIO.PAS');
    AddToServerWrapperMethod(Self,[aDirName]);
    TSQLLog.Add.Log(sllInfo,'Wrapper in: '+aDirName);
  end;
  {$endif}

end;

destructor TVpServer.Destroy;
begin
  inherited;
  fModel.Free;
end;

end.
