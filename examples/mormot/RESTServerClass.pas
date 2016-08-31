unit RESTServerClass;

interface

{$I Synopse.inc}

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
  SynSQLite3;

{ TVpServer }

constructor TVpServer.Create(const aRootFolder: TFileName;
  const aRootURI: RawUTF8);
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

end;

destructor TVpServer.Destroy;
begin
  inherited;
  fModel.Free;
end;

end.
