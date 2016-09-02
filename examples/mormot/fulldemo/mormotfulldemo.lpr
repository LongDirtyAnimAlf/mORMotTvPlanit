program mormotfulldemo;

{$mode objfpc}{$H+}

{$ifdef Linux}
  {$ifdef FPC_CROSSCOMPILING}
    {$ifdef CPUARM}
      {$linklib GLESv2}
    {$endif}
    {$linklib libc_nonshared.a}
  {$endif}
{$endif}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, demoMain, laz_visualplanit, LCLTranslator, DefaultTranslator,
  printer4lazarus;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

