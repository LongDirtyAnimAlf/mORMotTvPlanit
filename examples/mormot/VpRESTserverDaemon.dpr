program VpRESTserverDaemon;

{$ifdef Linux}
  {$ifdef FPC_CROSSCOMPILING}
    {$linklib libc_nonshared.a}
  {$endif}
{$endif}

{$I Synopse.inc} // define HASINLINE WITHLOG USETHREADPOOL ONLYUSEHTTPSOCKET
uses
  {$I SynDprUses.inc} // <--- has cthreads if needed
  Classes,
  SysUtils,
  SynLog,
  mORMotHTTPServer,
  RESTData,
  RESTServerClass,
  daemonapp;

Type
  TTestDaemon = Class(TCustomDaemon)
  Private
    fVpServer: TVpServer;
    fHTTPServer: TSQLHttpServer;
  public
    Function Start : Boolean; override;
    Function Stop : Boolean; override;
    Function Pause : Boolean; override;
    Function Continue : Boolean; override;
    Function Execute : Boolean; override;
    Function ShutDown : Boolean; override;
    Function Install : Boolean; override;
    Function UnInstall: boolean; override;
  end;

{ TTestThread }

Procedure AWriteln(MSg : String; B : Boolean);
begin
  Application.Log(etcustom,Msg+BoolToStr(B));
end;

{ TTestDaemon }

function TTestDaemon.Start: Boolean;
begin
  Result:=inherited Start;
  AWriteln('Daemon Start',Result);
  AWriteln('Dir: '+GetAppConfigDir(False)+'data',True);
  if not DirectoryExists(GetAppConfigDir(False))
     then CreateDir(GetAppConfigDir(False));
  fVpServer := TVpServer.Create(GetAppConfigDir(False)+'data','root');
  fHTTPServer := TSQLHttpServer.Create(HTTP_PORT,fVpServer{$ifndef ONLYUSEHTTPSOCKET},'+',useHttpApiRegisteringURI{$endif});
  fHTTPServer.AccessControlAllowOrigin := '*'; // allow cross-site AJAX queries
end;

function TTestDaemon.Stop: Boolean;
begin
  Result:=inherited Stop;
  AWriteln('Daemon Stop: ',Result);
  FreeAndNil(fHTTPServer);
  FreeAndNil(fVpServer);
end;

function TTestDaemon.Pause: Boolean;
begin
  Result:=inherited Pause;
  AWriteln('Daemon pause: ',Result);
end;

function TTestDaemon.Continue: Boolean;
begin
  Result:=inherited Continue;
  AWriteln('Daemon continue: ',Result);
end;

function TTestDaemon.Execute: Boolean;
begin
  Result:=inherited Execute;
  AWriteln('Daemon execute: ',Result);
end;

function TTestDaemon.ShutDown: Boolean;
begin
  Result:=inherited ShutDown;
  AWriteln('Daemon Shutdown: ',Result);
  try
    try
      FreeAndNil(fHTTPServer);
    except
    end;
  finally
    FreeAndNil(fVpServer);
  end;
end;

function TTestDaemon.Install: Boolean;
begin
  Result:=inherited Install;
  AWriteln('Daemon Install: ',Result);
end;

function TTestDaemon.UnInstall: boolean;
begin
  Result:=inherited UnInstall;
  AWriteln('Daemon UnInstall: ',Result);
end;

Type

  { TTestDaemonMapper }

  TTestDaemonMapper = Class(TCustomDaemonMapper)
    Constructor Create(AOwner : TComponent); override;
  end;

{ TTestDaemonMapper }

constructor TTestDaemonMapper.Create(AOwner: TComponent);

Var
  D : TDaemonDef;

begin
  inherited Create(AOwner);
  D:=DaemonDefs.Add as TDaemonDef;
  D.DisplayName:='Test daemon';
  D.Name:='TestDaemon';
  D.DaemonClassName:='TTestDaemon';
  //D.WinBindings.ServiceType:=stWin32;
end;

{$R *.res}

begin
  RegisterDaemonClass(TTestDaemon);
  RegisterDaemonMapper(TTestDaemonMapper);
  Application.Run;
end.


