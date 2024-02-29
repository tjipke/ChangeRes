unit ChangeResApp;

interface

uses ResFiles, Classes;

type
  TChangeResApp = class(TObject)
  private
    FResFile: TResourceFile;
    FResFileName: string;
    function GetResType(aStr: string): Integer;
    function GetResTypeStr(aResType: Integer): string;
  protected
    class procedure AddResourceSection(aSettings: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    class procedure AddDefSectionIfNeeded(aSettings: TStrings);
    procedure ApplySettings(aSettings: TStrings);
    procedure LoadResFile(const Value: string);
    procedure SaveResFile;
    property ResFileName: string read FResFileName;
  end;

implementation

uses SysUtils, RTLConsts, Windows, BdsProjSupport, VersionResources;

constructor TChangeResApp.Create;
begin
  inherited Create;
  FResFile := TResourceFile.Create();
{$IFDEF DEBUG}
  FResFile.Log.Logging := TStringList.Create;
{$ENDIF}
end;

destructor TChangeResApp.Destroy;
begin
{$IFDEF DEBUG}
  WriteLn(FResFile.Log.Logging.Text);
  sleep(500);
  FResFile.Log.Logging.Free;
{$ENDIF}
  FreeAndNil(FResFile);
  inherited Destroy;
end;

class procedure TChangeResApp.AddDefSectionIfNeeded(aSettings: TStrings);
var
  bResourceSectionFound: Boolean;
  I: Integer;
  S: string;
begin
  // search for [] if not in there add it ourself
  bResourceSectionFound := False;
  for I := 0 to aSettings.Count-1 do
  begin
    S := aSettings[I];
    if (S <> '') and (S[1] = '[') then
    begin
      bResourceSectionFound := True;
      Break;
    end;
    if (S[1] in ['/', '\', '-']) then
    begin
      if (Length(S) = 2)  and (Upcase(S[2])='F') then // when we use a file, we need don't add resource section
      begin
        bResourceSectionFound := True;
        Break;
      end;
    end
  end;

  if not bResourceSectionFound then
    AddResourceSection(aSettings);
end;

class procedure TChangeResApp.AddResourceSection(aSettings: TStrings);
var
  I: Integer;
  S: string;
  UseSettingsFile: Boolean;
begin
  // find position to insert [VERSIONINFO]
  I := 0;
  UseSettingsFile := False;
  while I < aSettings.Count do
  begin
    S := aSettings[I];
    if S = '' then
    begin
      ; // nop
    end
    else
    if (S[1] in ['/', '\', '-']) then
    begin
      if Length(S) < 2 then
        raise Exception.Create('unknown param: '+S);
      case Upcase(S[2]) of
        'F': UseSettingsFile := True;
        'U': ;
        else
        begin
          //add before this setting
          Break;
        end;
      end;
    end
    else
    if UseSettingsFile then // we should have a filename
    begin
      ;// just skip filename
    end
    else
    begin
      //add before this setting
      Break;
    end;
    Inc(I);
  end;
  aSettings.Insert(I, '[VERSIONINFO]');
end;

procedure TChangeResApp.ApplySettings(aSettings: TStrings);
var
  I: Integer;
  ResType: Integer;
  S: string;
  InOptions: Boolean;
  UpdateBorlandOptionsFile, UseSettingsFile, Handled: Boolean;
  TempStrings: TStringList;
  j: Integer;
begin
  UpdateBorlandOptionsFile := False;
  UseSettingsFile := False;
  InOptions := True;
  ResType := -1;
  I := 0;
  while I < aSettings.Count do
  begin
    S := aSettings[I];
    if S = '' then
    begin
      ; // nop
    end
    else
    if InOptions and (S[1] in ['/', '\', '-']) then
    begin
      if Length(S) < 2 then
        raise Exception.Create('unknown param: '+S);
      case Upcase(S[2]) of
        'F': UseSettingsFile := True;
        'U': UpdateBorlandOptionsFile := True;
        else
          raise Exception.Create('unknown param: '+S);
      end;
    end
    else
    if InOptions and UseSettingsFile then // we should have a filename
    begin
      TempStrings := TStringList.Create;
      try
        TempStrings.LoadFromFile(S);
        aSettings.AddStrings(TempStrings);
        UseSettingsFile := False;
      finally
        TempStrings.Free;
      end;
    end
    else
    if s[1] = '[' then
    begin
      InOptions := False;
      ResType := GetResType(Copy(s, 2, Length(s)-2));
    end
    else
    begin
      if ResType = -1 then
        raise Exception.Create('No resource part specified');
      Handled := False;
      for j := 0 to FResFile.ResourceParts.Count-1 do
      begin
        if FResFile.ResourceParts[j].ResType = ResType then
        begin
          Handled := True;
          FResFile.ResourceParts[j].PartData.ChangeByParam(s);
        end;
      end;
      if not Handled then Raise Exception.CreateFmt('No %s resource part found', [GetResTypeStr(ResType)]);
    end;
    Inc(I);
  end;

  if UpdateBorlandOptionsFile then
    TOptionFileUpdater.UpdateBorlandOptionFile(FResFile, ResFileName);
end;

function TChangeResApp.GetResType(aStr: string): Integer;
begin
  if CompareText(aStr, 'VersionInfo') = 0 then
    Result := rtVersionInfo
  else
    raise Exception.Create('Unknown Resource part type: '+aStr);
end;

function TChangeResApp.GetResTypeStr(aResType: Integer): string;
begin
  case aResType of
    rtVersionInfo: Result := 'VersionInfo'
    else
      raise Exception.Create('Unknown Resource part type:' + IntToStr(aResType));
  end;
end;

procedure TChangeResApp.LoadResFile(const Value: string);
begin
  FResFileName := Value;
  if not FileExists(FResFileName) then
    FResFileName := ChangeFileExt(FResFileName, '.res');
  if not FileExists(FResFileName) then
    raise EFOpenError.CreateResFmt(@SFOpenErrorEx, [ExpandFileName(FResFileName), SysErrorMessage(ERROR_FILE_NOT_FOUND)]);
  FResFile.LoadFromFile(FResFileName);
end;

procedure TChangeResApp.SaveResFile;
begin
  FResFile.SaveToFile(ResFileName);
end;

end.
