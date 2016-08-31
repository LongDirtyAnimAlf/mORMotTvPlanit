{ This unit enables playing of sound for
  - all versions of Windows
  - most versions of Linux
  - maybe Macintosh OSX
  Adapted from PlayWavePackage by Gordon Bamber.
  https://sourceforge.net/p/lazarus-ccr/svn/HEAD/tree/components/playsoundpackage/
}

unit sound;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VpBase;

procedure PlaySound(const ASoundFilename: string; APlaySoundMode: TVpPlaySoundMode);


implementation

uses
{$IFDEF WINDOWS}
  mmsystem,
{$ELSE}
  process, asyncprocess,
{$ENDIF}
  FileUtil;

CONST
  C_UnableToPlay = 'Unable to play ';

{$IFNDEF WINDOWS}
var
  SoundPlayerAsyncProcess: TAsyncProcess = nil;
  SoundPlayerSyncProcess: TProcess = nil;
{$ENDIF}

procedure PlaySound(const ASoundFilename: string; APlaySoundMode: TVpPlaySoundMode);
{$IFNDEF WINDOWS}
 var
  playCommand: string;
  L: TStrings;
  i: Integer;
{$ENDIF}
begin
{$IFDEF WINDOWS}
  try
    case APlaySoundMode of
      psmAsync : sndPlaySound(PChar(ASoundFilename), SND_ASYNC or SND_NODEFAULT);
      psmSync  : sndPlaySound(PChar(ASoundFilename), SND_SYNC or SND_NODEFAULT);
      psmStop  : sndPlaySound(nil, 0);
    end;
  except
    On E: Exception do
      E.CreateFmt(C_UnableToPlay +
      '%s Message:%s', [ASoundFilename, E.Message]);
  end;
{$ELSE}
  if (APlaySoundMode = psmStop) then begin
    if SoundPlayerAsyncProcess <> nil then
      SoundPlayerAsyncProcess.Terminate(1);
    exit;
  end;

  // How to play in Linux? Use generic Linux commands
  // Use asyncprocess to play sound as SND_ASYNC
  // Try play
  playCommand := '';
  if (FindDefaultExecutablePath('play') <> '') then
    playCommand := 'play';
  // Try aplay
  if (playCommand = '') then
    if (FindDefaultExecutablePath('aplay') <> '') then
      playCommand := 'aplay -q ';
  // Try paplay
  if (playCommand = '') then
    if (FindDefaultExecutablePath('paplay') <> '') then
      playCommand := 'paplay';
  // Try mplayer
  if (playCommand = '') then
    if (FindDefaultExecutablePath('mplayer') <> '') then
      playCommand := 'mplayer -really-quiet ';
  // Try CMus
  if (playCommand = '') then
    if (FindDefaultExecutablePath('CMus') <> '') then
      playCommand := 'CMus ';
  // Try pacat
  if (playCommand = '') then
    if (FindDefaultExecutablePath('pacat') <> '') then
      playCommand := 'pacat -p ';
  // Try ffplay
  if (playCommand = '') then
    if (FindDefaultExecutablePath('ffplay') <> '') then
      playCommand := 'ffplay -autoexit -nodisp ';
  // Try cvlc
  if (playCommand = '') then
    if (FindDefaultExecutablePath('cvlc') <> '') then
      playCommand := 'cvlc -q --play-and-exit ';
  // Try canberra-gtk-play
  if (playCommand = '') then
    if (FindDefaultExecutablePath('canberra-gtk-play') <> '') then
      playCommand := 'canberra-gtk-play -c never -f ';
  // Try Macintosh command?
  if (playCommand = '') then
    if (FindDefaultExecutablePath('afplay') <> '') then
      playCommand := 'afplay';

  // proceed if we managed to find a valid command
  if (playCommand <> '') then
  begin
    // Since the playcommand found above can contain parameters we must
    // separate executable and parameters.
    L := TStringList.Create;
    try
      L.Delimiter := ' ';;
      L.DelimitedText := playCommand;
      if APlaySoundMode = psmAsync then
      begin
        if SoundPlayerAsyncProcess = nil then
          SoundPlayerAsyncProcess := TAsyncProcess.Create(nil);
        SoundPlayerAsyncProcess.CurrentDirectory := ExtractFileDir(ASoundFilename);
        SoundPlayerAsyncProcess.Executable := FindDefaultExecutablePath(L[0]);
        SoundPlayerAsyncProcess.Parameters.Clear;
        for i:=1 to L.Count-1 do
          SoundPlayerAsyncProcess.Parameters.Add(L[i]);
        SoundPlayerAsyncProcess.Parameters.Add(ASoundFilename);
        try
          SoundPlayerAsyncProcess.Execute;
        except
          on E: Exception do
            E.CreateFmt('PlaySoundMode = psmAsync: ' + C_UnableToPlay +
              '%s Message:%s', [ASoundFilename, E.Message]);
        end;
      end
      else
      begin
        if SoundPlayerSyncProcess = nil then
          SoundPlayerSyncProcess := TProcess.Create(nil);
        SoundPlayerSyncProcess.CurrentDirectory := ExtractFileDir(ASoundFilename);
        SoundPlayerSyncProcess.Executable := FindDefaultExecutablePath(L[0]);
        SoundPlayersyncProcess.Parameters.Clear;
        for i:=1 to L.Count-1 do
          SoundPlayerSyncProcess.Parameters.Add(L[i]);
        SoundPlayerSyncProcess.Parameters.Add(ASoundFilename);
        try
          SoundPlayerSyncProcess.Execute;
          SoundPlayersyncProcess.WaitOnExit;
        except
          On E: Exception do
            E.CreateFmt('PlaySoundMode = psmSync: ' + C_UnableToPlay +
              '%s Message:%s', [ASoundFilename, E.Message]);
        end;
      end;

    finally
      L.Free;
    end;
  end
  else
    raise Exception.Create('No sound support found on this system.');
{$ENDIF}
end;


{$IFNDEF WINDOWS}
initialization

finalization
  FreeAndNil(SoundPlayerSyncProcess);
  FreeAndNil(SoundPlayerAsyncProcess);
{$ENDIF}

end.

