unit VpRegZEOS;
  {-Registration unit for the ZEOS database components}

{$I Vp.INC}        // Compiler version defines
{$R vpregzeos.res} // Palette glyphs

interface

uses
 {$IFDEF DELPHI}
 // Windows,
  {$IFDEF VERSION6} DesignIntf, DesignEditors, {$ELSE} DsgnIntf, {$ENDIF}
 {$ENDIF}
  Classes;

procedure Register;

implementation

uses
  VpZeosDS;

procedure Register;
begin
  RegisterComponents('Visual PlanIt', [TVpZeosDatastore]);
end;

end.
