program Project14Client;

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  Forms,
  {$ifdef FPC}
  Interfaces,
  {$endif}
  Project14ClientMain in 'Project14ClientMain.pas' {Form1},
  Project14Interface in 'Project14Interface.pas';

{$ifndef FPC}
{$R *.res}
{$endif}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
