unit ResFiles;

interface

uses
  Windows, Classes, Contnrs, IniFiles;

type
  TLog = class(TObject)

  private
    FLogging: TStrings;
  public
    procedure AddToLog(aLogging: string); overload;
    procedure AddToLog(aLogging: string; const Args: array of const); overload;
    property Logging: TStrings read FLogging write FLogging;
  end;
  
  TResourceTypeHandler = class;

  TResourcePart = class (TObject)
  private
    FCharacteristics: LongWord;
    FDataSize: Integer;
    FDataversion: Integer;
    FHeaderSize: Integer;
    FLanguageid: Word;
    FMemoryflags: Word;
    FPartData: TResourceTypeHandler;
    FResName: Integer;
    FResNameStr: WideString;
    FResourceData: TStream;
    FResType: Integer;
    FResTypeStr: WideString;
    FVersion: LongWord;
    FLog: TLog;
    procedure SetResName(Value: Integer);
    procedure SetResNameStr(Value: WideString);
    procedure SetResType(Value: Integer);
    procedure SetResTypeStr(Value: WideString);
  public
    destructor Destroy; override;
    procedure LoadFromStream(aStream: TStream);
    procedure SaveToStream(aStream: TStream);
    property Characteristics: LongWord read FCharacteristics write
       FCharacteristics;
    property DataSize: Integer read FDataSize;
    property Dataversion: Integer read FDataversion write FDataversion;
    property HeaderSize: Integer read FHeaderSize;
    property Languageid: Word read FLanguageid write FLanguageid;
    property Memoryflags: Word read FMemoryflags write FMemoryflags;
    property PartData: TResourceTypeHandler read FPartData write FPartData;
    property ResName: Integer read FResName write SetResName;
    property ResNameStr: WideString read FResNameStr write SetResNameStr;
    property ResourceData: TStream read FResourceData write FResourceData;
    property ResType: Integer read FResType write SetResType;
    property ResTypeStr: WideString read FResTypeStr write SetResTypeStr;
    property Version: LongWord read FVersion write FVersion;
    property Log: TLog read FLog write FLog;
  end;
  
  TResourceParts = class (TObjectList)
  private
    function GetItems(Index: Integer): TResourcePart;
    procedure SetItems(Index: Integer; Value: TResourcePart);
  public
    property Items[Index: Integer]: TResourcePart read GetItems write SetItems; 
       default;
  end;
  
  TResourceFile = class (TObject)
  private
    FResourceParts: TResourceParts;
    FLog: TLog;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const aFilename: string);
    procedure SaveToFile(const aFilename: string);
    property ResourceParts: TResourceParts read FResourceParts write
       FResourceParts;
    property Log: TLog read FLog;
  end;

  TResourceTypeHandler = class (TObject)
  protected
    function ExtractParamNameAndValue(const aParam: string; out aName, aValue:
       string): Boolean;
  public
    constructor Create; virtual;
    procedure ChangeByParam(aParam: string); virtual; abstract;
    procedure UpdateBorlandOptionFile(aIniFile: TCustomIniFile); virtual; abstract;
    procedure LoadResourcePartData(aSender: TResourcePart; aPart: TStream);
       virtual; abstract;
    procedure SaveResourcePartData(aSender: TResourcePart; aPart: TStream);
       virtual; abstract;
  end;
  
  TResourceTypeHandlerClass = class of TResourceTypeHandler;

function NextAlignBoundary(aPosition: Int64; aAlignOn: Byte=4): Int64;
procedure AlignStream(aStream: TStream; aWrite: Boolean=False; aAlignOn: Byte=4);
function ReadUniStringFromStream(aStream: TStream; aAlign: Boolean): WideString;
procedure WriteUniStringToStream(aStream: TStream; aString: WideString; aAlign: Boolean);
function WideLength(aWideString: WideString): Integer;

procedure RegisterResourceTypeHandler(aResType: Integer; aResTypeStr: string; aHandler: TResourceTypeHandlerClass);
procedure UnregisterResourceTypeHandler(aResType: Integer; aResTypeStr: string; aHandler: TResourceTypeHandlerClass);
function FindResourceTypeHandler(aResType: Integer; aResTypeStr: string; out aHandler: TResourceTypeHandler): Boolean;

implementation

uses SysUtils{$IFDEF TRIAL}, Registry{$ENDIF};

{$I CheckLicenseChangeRes.inc}

var
  FResTypeStrHandlers: TStringList;
  FResTypeHandlers: TStringList;

procedure RegisterResourceTypeHandler(aResType: Integer; aResTypeStr: string;
    aHandler: TResourceTypeHandlerClass);
begin
  if aResTypeStr <> '' then
  begin
    if not Assigned(FResTypeStrHandlers) then
      FResTypeStrHandlers := TStringList.Create;
    FResTypeStrHandlers.AddObject(aResTypeStr, TObject(aHandler));
  end
  else
  begin
    if not Assigned(FResTypeHandlers) then
      FResTypeHandlers := TStringList.Create;
    FResTypeHandlers.AddObject(IntToStr(aResType), TObject(aHandler));
  end
end;

procedure UnregisterResourceTypeHandler(aResType: Integer; aResTypeStr: string;
    aHandler: TResourceTypeHandlerClass);
var
  I: Integer;
begin
  if aResTypeStr <> '' then
  begin
    if not Assigned(FResTypeStrHandlers) then
      Exit;
    I := FResTypeStrHandlers.IndexOf(aResTypeStr);
    if I = -1 then
      Exit;
    while (I < FResTypeStrHandlers.Count) and ((FResTypeStrHandlers.Objects[I] <> TObject(aHandler)) or (FResTypeStrHandlers[I] <> aResTypeStr)) do
      Inc(I);
    if I < FResTypeStrHandlers.Count then
      FResTypeStrHandlers.Delete(I);
  end
  else
  begin
    if not Assigned(FResTypeHandlers) then
      Exit;
    I := FResTypeHandlers.IndexOf(IntToStr(aResType));
    if I = -1 then
      Exit;
    while (I < FResTypeHandlers.Count) and ((FResTypeHandlers.Objects[I] <> TObject(aHandler)) or (FResTypeHandlers[I] <> IntToStr(aResType))) do
      Inc(I);
    if I < FResTypeHandlers.Count then
      FResTypeHandlers.Delete(I);
  end;
end;

function FindResourceTypeHandler(aResType: Integer; aResTypeStr: string; out aHandler: TResourceTypeHandler): Boolean;
var
  I: Integer;
begin
  Result := False;// remove warnign
  aHandler := nil;
  try
    if aResTypeStr <> '' then
    begin
      if not Assigned(FResTypeStrHandlers) then Exit;
      I := FResTypeStrHandlers.IndexOf(aResTypeStr);
      if I <> -1 then
        aHandler := TResourceTypeHandlerClass(FResTypeStrHandlers.Objects[i]).Create;
    end
    else
    begin
      if not Assigned(FResTypeHandlers) then Exit;
      I := FResTypeHandlers.IndexOf(IntToStr(aResType));
      if I <> -1 then
        aHandler := TResourceTypeHandlerClass(FResTypeHandlers.Objects[i]).Create;
    end;
  finally
    Result := aHandler <> nil;
  end;
end;

function NextAlignBoundary(aPosition: Int64; aAlignOn: Byte=4): Int64;
begin
  Result := ((aPosition+(aAlignOn-1)) div aAlignOn) * aAlignOn;
end;

procedure AlignStream(aStream: TStream; aWrite: Boolean=False; aAlignOn:
    Byte=4);
var
  TmpByte: Byte;
  I: Int64;
begin
  I := NextAlignBoundary(aStream.Position);
  if aWrite and (I > aStream.Position) then
  begin
    TmpByte := 0;
    while I > aStream.Position do
      aStream.Write(TmpByte, 1);
  end
  else
    aStream.Position := I;
end;

function ReadUniStringFromStream(aStream: TStream; aAlign: Boolean): WideString;
var
  TmpChar: WideChar;
begin
  Result := '';
  while aStream.Read(TmpChar, 2) = 2 do
  begin
    if TmpChar = #0 then Break;
    Result := Result + TmpChar;
  end;
  if aAlign then AlignStream(aStream);
end;

procedure WriteUniStringToStream(aStream: TStream; aString: WideString; aAlign: Boolean);
begin
  aStream.WriteBuffer(Pointer(aString)^, WideLength(aString));
  if aAlign then AlignStream(aStream, {aWrite=}True);
end;

function WideLength(aWideString: WideString): Integer;
begin
  Result := Length(aWideString)*2;
  if Result > 0 then
    Result := Result + 2{#0000};
end;

{
******************************** TResourcePart *********************************
}
destructor TResourcePart.Destroy;
begin
  if Assigned(FResourceData) then
    FResourceData.Free;
  inherited;
end;

procedure TResourcePart.LoadFromStream(aStream: TStream);
var
  TmpWord: Word;
  iPos: int64;
begin
  iPos := aStream.Position;
  Log.AddToLog('Loading Part from position: %dx', [iPos, iPos]);
  aStream.Read(FDatasize, 4);
  aStream.Read(FHeadersize, 4);
  Log.AddToLog('DataSize: %dx HeaderSize: %dx', [FDataSize, FDataSize, FHeaderSize, FHeaderSize]);
  aStream.Read(TmpWord, 2);
  if TmpWord = $ffff then
  begin
    aStream.Read(TmpWord, 2);
    FResType := TmpWord;
    FResTypeStr := '';
  end
  else
  begin
    FResType := -1;
    FResTypeStr := WideChar(TmpWord) + ReadUniStringFromStream(aStream, True);
  end;
  aStream.Read(TmpWord, 2);
  if TmpWord = $ffff then
  begin
    aStream.Read(TmpWord, 2);
    FResName := TmpWord;
    FResNameStr := '';
  end
  else
  begin
    FResName := -1;
    FResNameStr := WideChar(TmpWord) + ReadUniStringFromStream(aStream, True);
  end;
  Log.AddToLog('ResType: $%x %s ResName: $%x %s', [FResType, string(FResTypeStr), FResName, string(FResNameStr)]);
  aStream.Read(FDataversion, 4);
  aStream.Read(FMemoryflags, 2);
  aStream.Read(FLanguageid, 2);
  aStream.Read(FVersion, 4);
  aStream.Read(FCharacteristics, 4);
  Log.AddToLog('Dataversion: %d MemFlags: $%x LangId: $%x-%x Version %d Characteristics: %d', [FDataversion, FMemoryflags, FLanguageid and $3FF, FLanguageid shr 10, FVersion, FCharacteristics]);
  if aStream.Position <> int64(iPos + HeaderSize) then
  begin
    Log.AddToLog('!Current Position (%dx) <> Position+Headersize (%dx) -> changed', [aStream.Position, aStream.Position, iPos+HeaderSize, iPos+HeaderSize]);
    aStream.Position := iPos + HeaderSize;
  end;
  if Datasize > 0 then
  begin
    if Assigned(FResourceData) then
      FResourceData.Free;
    FResourceData := TMemoryStream.Create;
    FResourceData.Size := DataSize;
    Log.AddToLog('Loading data');
    aStream.ReadBuffer(TMemoryStream(FResourceData).Memory^, DataSize);
    if Assigned(PartData) then
      FreeAndNil(FPartData);
    if FindResourceTypeHandler(FResType, FResTypeStr, FPartData) then
    begin
      Log.AddToLog('Giving data to resourcehandler: %s', [FPartData.Classname]);
      FResourceData.Position := 0;
      PartData.LoadResourcePartData(Self, FResourceData);
    end;
  end;
end;

procedure TResourcePart.SaveToStream(aStream: TStream);
var
  TmpWord: Word;
begin
  if Assigned(PartData) then
  begin
    FResourceData.Position := 0;
    PartData.SaveResourcePartData(Self, FResourceData);
  end;
  if Assigned(FResourceData) then
    FDataSize := FResourceData.Size
  else
    FDataSize := 0;
  aStream.Write(FDatasize, 4);

  FHeaderSize := 5*4 + 2*2;
  if ResType > -1 then
    FHeaderSize := FHeaderSize + 4
  else
    FHeaderSize := FHeaderSize + (NextAlignBoundary(WideLength(FResTypeStr)));
  if ResName > -1 then
    FHeaderSize := FHeaderSize + 4
  else
    FHeaderSize := FHeaderSize + (NextAlignBoundary(WideLength(FResNameStr)));
  aStream.Write(FHeadersize, 4);

  if ResType > -1 then
  begin
    TmpWord := $ffff;
    aStream.Write(TmpWord, 2);
    TmpWord := ResType;
    aStream.Write(TmpWord, 2);
  end
  else
    WriteUniStringToStream(aStream, FResTypeStr, True);

  if ResName > -1 then
  begin
    TmpWord := $ffff;
    aStream.Write(TmpWord, 2);
    TmpWord := ResName;
    aStream.Write(TmpWord, 2);
  end
  else
    WriteUniStringToStream(aStream, FResNameStr, True);

  aStream.Write(FDataversion, 4);
  aStream.Write(FMemoryflags, 2);
  aStream.Write(FLanguageid, 2);
  aStream.Write(FVersion, 4);
  aStream.Write(FCharacteristics, 4);
  if Datasize > 0 then
    aStream.WriteBuffer(TMemoryStream(FResourceData).Memory^, DataSize);
end;

procedure TResourcePart.SetResName(Value: Integer);
begin
  // TODO -cMM: default body inserted
end;

procedure TResourcePart.SetResNameStr(Value: WideString);
begin
  // TODO -cMM: default body inserted
end;

procedure TResourcePart.SetResType(Value: Integer);
begin
  // TODO -cMM: default body inserted
end;

procedure TResourcePart.SetResTypeStr(Value: WideString);
begin
  // TODO -cMM: default body inserted
end;


{
******************************** TResourceFile *********************************
}
constructor TResourceFile.Create;
begin
  inherited;
  FLog := TLog.Create;
  FResourceParts := TResourceParts.Create(True);
end;

destructor TResourceFile.Destroy;
begin
  FLog.Free;
  FResourceParts.Free;
  inherited;
end;

procedure TResourceFile.LoadFromFile(const aFilename: string);
var
  F: TFileStream;
  ResPart: TResourcePart;
begin
  Log.AddToLog('Loading file: '+aFileName);
  F := TFileStream.Create(aFilename, fmOpenRead);
  with F do
  try
    FResourceParts.Clear;
    Log.AddToLog('Filesize: '+IntToStr(Size));
    while Position < Size do
    begin
      AlignStream(F);
      ResPart := TResourcePart.Create;
      try
        ResPart.Log := Log;
        ResPart.LoadFromStream(F);
        Log.AddToLog('Part added');
        FResourceParts.Add(ResPart);
      except
        ResPart.Free;
        raise;
      end;
    end;
  finally
    Free;
  end;
end;
{$DEFINE PROCS}
{$I CheckLicense3.inc}
{$UNDEF PROCS}

procedure TResourceFile.SaveToFile(const aFilename: string);
var
  F: TFileStream;
  i: Integer;
  s, t: string;
  Label CreateIt;
begin
  F := TFileStream.Create(aFilename, fmCreate);
  with F do
  try
    for i := 0 to ResourceParts.Count-1 do
    begin
      AlignStream(F, True);
      ResourceParts[i].SaveToStream(F);
    end;
    F.Size := F.Position;
    {$I CheckLicense3.inc}
  finally
    Free;
  end;
end;


{
***************************** TResourceTypeHandler *****************************
}
constructor TResourceTypeHandler.Create;
begin
  inherited;
end;

function TResourceTypeHandler.ExtractParamNameAndValue(const aParam: string; 
   out aName, aValue: string): Boolean;
var
  P: Integer;
begin
  P := AnsiPos('=', aParam);
  Result := P <> 0;
  if Result then
  begin
    aName := Copy(aParam, 1, P-1);
    aValue := Copy(aParam, P+1, MaxInt);
  end
  else
  begin
    aName := aParam;
    aValue := '';
  end;
end;

{
******************************** TResourceParts ********************************
}
function TResourceParts.GetItems(Index: Integer): TResourcePart;
begin
  Result := TResourcePart(inherited Items[Index]);
end;

procedure TResourceParts.SetItems(Index: Integer; Value: TResourcePart);
begin
  inherited Items[Index] := Value;
end;

procedure TLog.AddToLog(aLogging: string);
begin
  if Assigned(FLogging) then
    FLogging.Add(aLogging);
end;

procedure TLog.AddToLog(aLogging: string; const Args: array of const);
begin
  if Assigned(FLogging) then
    FLogging.Add(Format(StringReplace(aLogging, '%dx', '%d $%.4x', [rfReplaceAll]), Args));
end;











initialization
finalization
  FResTypeStrHandlers.Free;
  FResTypeHandlers.Free;
end.
