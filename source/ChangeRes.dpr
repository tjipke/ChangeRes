program ChangeRes;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Windows,
  SysUtils,
  Classes,
  RTLConsts,
  Registry,
  VersionResources in '.\SharedCode\VersionResources.pas',
  ResFiles in '.\SharedCode\ResFiles.pas',
  // TrsChgResPro in 'TrsChgResPro.pas',
  BdsProjSupport in 'BdsProjSupport.pas',
  LibXmlParser in '.\XMLParser\LibXmlParser.pas',
  ChangeResApp in 'ChangeResApp.pas';

// {$I CheckLicenseChangeRes.inc}
// {$I CheckLicense4a.inc}

const
  MyRegistryKey: string = '\Software\Tiriss\ChangeRes\1.4';
var
  I: Integer;
  S, FileName, SVersion: string;
  TheApp: TChangeResApp;
  Settings: TStringList;
begin
  {$IFDEF TRIAL}
  SVersion := 'trial';
  {$ELSE}
  {$IFDEF PREVIEW}
  SVersion := 'preview';
  {$ELSE}
  SVersion := 'retail';
  {$ENDIF}
  {$ENDIF}
  {$IFDEF BETA}
  SVersion := '(Beta) '+ SVersion;
  {$ENDIF}

  WriteLn('ChangeRes v1.43 ('+SVersion+') (c) MMX by Tiriss');
  WriteLn('');
  try
    WriteLn('');
    if ParamCount < 1 then
    begin
      WriteLn('Usage: ChangeRes Filename (-U) (-F filename) ([VERSIONINFO]) (Settings)');
      WriteLn('');
      WriteLn('Filename                  The .res file to update (.res extension is optional)');
      WriteLn('-U (optional)             Update option file (.dof, .bdsproj, dproj, .cbproj, .bpk)');
      WriteLn('-F filename (optional)    Get settings from filename');
      WriteLn('[VERSIONINFO] (optional)  Tells changeres to change version info (is default)');
      WriteLn('Settings (optional):      The things you wish to set');
      WriteLn('');
      WriteLn('For VersionInfo: ');
      WriteLn('  FileVersion=?.?.?.?(/s):       set FileVersion (/s: set stringvalue also)');
      WriteLn('  ProductVersion=?.?.?.?(/s):    set ProductVersion (/s: see above)');
      WriteLn('  FileFlags=+-Flags:             set FileFlags (example: +VS_FF_SPECIALBUILD)');
      WriteLn('  FileDate=yyyymmddhhnnss:       set FileDate (example: 20033101000000)');
      WriteLn('  FileDate=now|today:            set FileDate to now (incl. time) or today');
      WriteLn('  \StringFileInfo\\String=Value: set value of a String');
      WriteLn('  -\StringFileInfo\\String:      remove stringvalue');
      WriteLn('');
      WriteLn('Examples:');
      WriteLn(' ChangeRes Project1.Res -U [VERSIONINFO] FileVersion=1.2.3.4/s FileFlags=+VS_FFSPECIALBUILD');
      WriteLn(' ChangeRes Project1 -U [VERSIONINFO] FileVersion=+0.+0.+0.+1/s FileDate=Today');
      WriteLn(' ChangeRes Project1 -U FileVersion=.yyddd.hhnn.ss/s ProductVersion=.yyddd.0.0/s');
      WriteLn(' ChangeRes Project1.Res -U -F ChangeResVersionInfo.ini');
      WriteLn('');
      WriteLn('For more info or new versions see: http://www.tiriss.com/');
      WriteLn('');
      Sleep(1000);
      Exit;
    end;

    TheApp := TChangeResApp.Create;
    try
      TheApp.LoadResFile(ParamStr(1));

      Settings := TStringList.Create;
      try
        for I := 2 to ParamCount do
          Settings.Add(ParamStr(I));

        TChangeResApp.AddDefSectionIfNeeded(Settings);

        TheApp.ApplySettings(Settings);
        TheApp.SaveResFile();
      finally
        Settings.Free;
      end;
    finally
      TheApp.Free;
    end;
{$IFDEF TRIALORPREVIEW}
    WriteLn('If you keep using ChangeRes, please buy it at http://www.tiriss.com/changeres');
    sleep(500);
{$ENDIF}
  except
    on E: Exception do
    begin
      {$IFDEF TRIALORPREVIEW}
      if E.ClassNameIs(ExpiredException.ClassName) or ((E.ClassParent <> nil) and E.ClassParent.ClassNameIs(ExpiredException.ClassName))
         or (CompareText(E.Message, 'EXPIRED') = 0) then
      begin
        Beep;
        E.Message := 'This trial version is expired! (code:'+E.ClassName[1]+')'#13#10'If you want to continue using this product, get the retail version from http://www.tiriss.com/changeres/';
      end;
      {$ELSE}
      if E.ClassNameIs(LicException.ClassName) or ((E.ClassParent <> nil) and E.ClassParent.ClassNameIs(LicException.ClassName)) then
        E.Message := 'This retail version needs a license file (.lic).' + E.ClassName + ': ' + E.Message;
      {$ENDIF}
      WriteLn(ErrOutput, E.Message);
      ExitCode := 1;
      Sleep(500);
    end;
  end;

end.

