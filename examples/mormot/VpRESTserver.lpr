/// RESTful ORM server
program VpRESTserver;

{$ifndef FPC}
{$APPTYPE CONSOLE}
{$endif}

{$I Synopse.inc} // define HASINLINE WITHLOG USETHREADPOOL ONLYUSEHTTPSOCKET

// first line after uses clause should be  {$I SynDprUses.inc}  for FastMM4
uses
  {$I SynDprUses.inc}
  Classes,
  SysUtils,
  SynCommons,
  SynLog,
  mORMot,
  SynCrtSock,
  mORMotHTTPServer,
  RESTData,
  RESTServerClass;

var ORMServer: TVpServer;
    HTTPServer: TSQLHttpServer;

{$R *.res}

begin
  ORMServer := TVpServer.Create(ExeVersion.ProgramFilePath+'data','root');
  try
    TSQLLog.Family.EchoToConsole := LOG_VERBOSE;
    HTTPServer := TSQLHttpServer.Create(HTTP_PORT,ORMServer{$ifndef ONLYUSEHTTPSOCKET},'+',useHttpApiRegisteringURI{$endif});
    try
      sleep(300); // let the HTTP server start (for the console log refresh)
      writeln(#13#10'Background server is running at http://localhost:'+HTTP_PORT+#13#10+
              // #13#10'Wrappers at http://localhost:'+HTTP_PORT+'/root/wrapper'+#13#10+
              #13#10'Press [Enter] to close the server.');
      readln;
    finally
      HTTPServer.Free;
    end;
  finally
    ORMServer.Free;
  end;
end.

