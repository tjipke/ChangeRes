unit VersionResources;

interface

uses
  Windows, Classes, Contnrs, ResFiles, IniFiles;

const
  rtVersionInfo = $10;

type
  TVersionResourceChild = class (TObject)
  private
    FDataType: Word;
    FEndPos: LongWord;
    FKey: WideString;
    FValueLen: Word;
  protected
    FChildLen: Word;
    function GetChildLen: Word; virtual;
    procedure SetChildLen(Value: Word); virtual;
  public
    procedure LoadFromStream(aStream: TStream); virtual;
    procedure SaveToStream(aStream: TStream); virtual;
    property ChildLen: Word read GetChildLen write SetChildLen;
    property DataType: Word read FDataType write FDataType;
    property Key: WideString read FKey write FKey;
    property ValueLen: Word read FValueLen write FValueLen;
  end;

  TVersionResourceChildren = class (TObjectList)
  private
    function GetItems(Index: Integer): TVersionResourceChild;
    procedure SetItems(Index: Integer; Value: TVersionResourceChild);
  public
    property Items[Index: Integer]: TVersionResourceChild read GetItems write 
       SetItems; default;
  end;
  
  TStringVarFileInfo = class (TVersionResourceChild)
  private
    FChildren: TVersionResourceChildren;
    FIsString: Boolean;
    FIsVar: Boolean;
  protected
    function GetChildLen: Word; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ChangeByParam(aName, aValue: string; aDelete: Boolean=False);
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    property Children: TVersionResourceChildren read FChildren write FChildren;
    property IsString: Boolean read FIsString write FIsString;
    property IsVar: Boolean read FIsVar write FIsVar;
  end;
  
  TStringVarFileInfos = class (TVersionResourceChildren)
  private
    function GetItems(Index: Integer): TStringVarFileInfo;
    procedure SetItems(Index: Integer; Value: TStringVarFileInfo);
  public
    property Items[Index: Integer]: TStringVarFileInfo read GetItems write
       SetItems; default;
  end;

  TVersionResourceInfo = class (TVersionResourceChild)
  protected
    function GetChildLen: Word; override;
  public
    constructor Create;
  end;

  TVersionResource = class (TResourceTypeHandler)
  private
    FFixedFileInfo: VS_FIXEDFILEINFO;
    FInfoType: Word;
    FStringFileInfos: TStringVarFileInfos;
    FVarFileInfos: TStringVarFileInfos;
    FVersionInfo: TVersionResourceInfo;
    function GetFixedFileInfo: PVSFixedFileInfo;
    function HandleChangeVersion(aOldVersion: Int64; aChangeValue: string;
       aVersionStr: string): Int64;
    function HandleChangeFileFlags(aOldFlags: DWORD; aChangeValue: string):
        LongWord;
  protected
    procedure ReadChildren(aData: TStream);
    procedure WriteChildren(aData: TStream);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ChangeByParam(aParam: string); override;
    procedure LoadResourcePartData(aSender: TResourcePart; aPart: TStream);
       override;
    procedure SaveResourcePartData(aSender: TResourcePart; aPart: TStream);
       override;
    procedure UpdateBorlandOptionFile(aIniFile: TCustomIniFile); override;
    property FixedFileInfo: PVSFixedFileInfo read GetFixedFileInfo;
    property InfoType: Word read FInfoType write FInfoType;
    property StringFileInfos: TStringVarFileInfos read FStringFileInfos write
       FStringFileInfos;
    property VarFileInfos: TStringVarFileInfos read FVarFileInfos write
       FVarFileInfos;
  end;

  TVarChild = class (TVersionResourceChild)
  private
    FValue: Pointer;
  protected
    function GetChildLen: Word; override;
  public
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    property Value: Pointer read FValue write FValue;
  end;

  TStringTable = class (TVersionResourceChild)
  private
    FStrings: TVersionResourceChildren;
  protected
    function GetChildLen: Word; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    procedure ChangeByParam(aName, aValue: string; aDelete: Boolean=False);
    property Strings: TVersionResourceChildren read FStrings write FStrings;
  end;

  TStringChild = class (TVersionResourceChild)
  private
    FValue: WideString;
    function GetValueLen: Integer;
  protected
    function GetChildLen: Word; override;
  public
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    property Value: WideString read FValue write FValue;
  end;

implementation

uses SysUtils, DateUtils;

{$I CheckLicenseChangeRes.inc}
var
  GStartTime: TDateTime; 

function Now:TDateTime;
begin
  raise Exception.Create('Don''t use now, but use GStartTime');
end;

procedure DateTimeToFileTime(const aDateTime: TDateTime; var aFileTime:TFileTime);
var
  SystemTime: TSystemTime;
begin
  DateTimeToSystemTime(aDateTime, SystemTime);
  SystemTimeToFileTime(SystemTime, aFileTime);
end;

function DecodeDateTime(const Value: string): TDateTime;
var
  W1, W2, W3: Word;
begin
  if (Length(Value) <> 8) and (Length(Value) <> 14) then
    raise Exception.Create('Invalid Datetime value: '+Value);
  try
    W1 := StrToInt(Copy(Value, 1, 4));
    W2 := StrToInt(Copy(Value, 5, 2));
    W3 := StrToInt(Copy(Value, 7, 2));
    Result := EncodeDate(W1, W2, W3);
    if Length(Value) > 8 then
    begin
      W1 := StrToInt(Copy(Value, 9, 2));
      W2 := StrToInt(Copy(Value, 11, 2));
      W3 := StrToInt(Copy(Value, 13, 2));
      Result := Result + EncodeTime(W1, W2, W3, 0);
    end;
  except
    on E: EConvertError do
      raise Exception.Create('Invalid Datetime value: '+Value);
  end;
end;
{
***************************** TStringVarFileInfos ******************************
}
function TStringVarFileInfos.GetItems(Index: Integer): TStringVarFileInfo;
begin
  Result := TStringVarFileInfo(inherited Items[Index]);
end;

procedure TStringVarFileInfos.SetItems(Index: Integer; Value: 
   TStringVarFileInfo);
begin
  inherited Items[Index] := Value;
end;

{
**************************** TVersionResourceChild *****************************
}
function TVersionResourceChild.GetChildLen: Word;
begin
  Result := NextAlignBoundary(6 + WideLength(FKey));
end;

procedure TVersionResourceChild.LoadFromStream(aStream: TStream);
begin
  aStream.Read(FChildLen, 2);
  FEndPos := aStream.Position + FChildLen - 3;
  aStream.Read(FValueLen, 2);
  aStream.Read(FDataType, 2);
  FKey := ReadUniStringFromStream(aStream, True);
end;

procedure TVersionResourceChild.SaveToStream(aStream: TStream);
begin
  ChildLen := ChildLen; // recalc
  aStream.Write(FChildLen, 2);
  FEndPos := aStream.Position + FChildLen - 3;
  aStream.Write(FValueLen, 2);
  aStream.Write(FDataType, 2);
  WriteUniStringToStream(aStream, FKey, True);
end;

procedure TVersionResourceChild.SetChildLen(Value: Word);
begin
  FChildLen := Value;
end;


{
*************************** TVersionResourceChildren ***************************
}
function TVersionResourceChildren.GetItems(Index: Integer): 
   TVersionResourceChild;
begin
  Result := TVersionResourceChild(inherited Items[Index]);
end;

procedure TVersionResourceChildren.SetItems(Index: Integer; Value: 
   TVersionResourceChild);
begin
  inherited Items[Index] := Value;
end;



{
******************************* TVersionResource *******************************
}
constructor TVersionResource.Create;
begin
  inherited;
  FVersionInfo := TVersionResourceInfo.Create;
  FVersionInfo.ValueLen := SizeOf(FFixedFileInfo);
  FStringFileInfos := TStringVarFileInfos.Create;
  FVarFileInfos := TStringVarFileInfos.Create;
end;

destructor TVersionResource.Destroy;
begin
  FStringFileInfos.Free;
  FVarFileInfos.Free;
  FVersionInfo.Free;
  inherited;
end;

procedure TVersionResource.ChangeByParam(aParam: string);
var
  Name: string;
  Value: string;
  CurValue: Int64;
  P,I: Integer;
  HasMinus: Boolean;
  FileDateTime: TFileTime;
  DT: TDateTime;
begin
  if not ExtractParamNameAndValue(aParam, Name, Value) then
  begin
    HasMinus := (Length(Name) > 0) and (Name[1] = '-');
    if HasMinus then
    begin
      Value := '';
      Delete(Name, 1, 1);
    end
    else
      raise Exception.Create('Unknown param: '+aParam);
  end
  else
    HasMinus := False;

  if Length(Name) = 0 then
    raise Exception.Create('Unknown param: '+aParam);

  if CompareText(Name, 'FileVersion')=0 then
  begin
    CurValue := (Int64(FFixedFileInfo.dwFileVersionMS) shl 32) + FFixedFileInfo.dwFileVersionLS;
    CurValue := HandleChangeVersion(CurValue, Value, Name);
    FFixedFileInfo.dwFileVersionLS := CurValue and $FFFFFFFF;
    FFixedFileInfo.dwFileVersionMS := CurValue shr 32;
  end
  else
  if CompareText(Name, 'ProductVersion')=0 then
  begin
    CurValue := (Int64(FFixedFileInfo.dwProductVersionMS) shl 32) + FFixedFileInfo.dwProductVersionLS;
    CurValue := HandleChangeVersion(CurValue, Value, Name);
    FFixedFileInfo.dwProductVersionLS := CurValue and $FFFFFFFF;
    FFixedFileInfo.dwProductVersionMS := CurValue shr 32;
  end
  else
  if CompareText(Name, 'FileFlags')=0 then
  begin
    FFixedFileInfo.dwFileFlags := HandleChangeFileFlags(FFixedFileInfo.dwFileFlags, Value);
    // now set all flags to valid in the mask
    FFixedFileInfo.dwFileFlagsMask := HandleChangeFileFlags(FFixedFileInfo.dwFileFlagsMask, StringReplace(Value, '-', '+', [rfReplaceAll]));
  end
  else
  if CompareText(Name, 'FileDate')=0 then
  begin
    if CompareText(Value, 'TODAY') = 0 then
      DT := Date
    else
    if CompareText(Value, 'NOW') = 0 then
      DT := GStartTime
    else
      DT := DecodeDateTime(Value);
    DateTimeToFileTime(DT, FileDateTime);
    FFixedFileInfo.dwFileDateMS := FileDateTime.dwHighDateTime;
    FFixedFileInfo.dwFileDateLS := FileDateTime.dwLowDateTime;
  end
  else
    // "\StringFileInfo\ or \VarFileInfo\
  if Name[1] in ['/','\'] then
  begin
    P := Pos(Name[1], Copy(Name, 2, MaxInt));
    if P = 0 then
      raise Exception.Create('Unknown param: '+aParam);
    if CompareText('StringFileInfo', Copy(Name, 2, P-1)) = 0 then
    begin
      for I := 0 to StringFileInfos.Count-1 do
        StringFileInfos[I].ChangeByParam(Copy(Name, P+1, MaxInt), Value, HasMinus);
    end
    else
    if CompareText('VarFileInfo', Copy(Name, 2, P-1)) = 0 then
    begin
      for I := 0 to VarFileInfos.Count-1 do
        VarFileInfos[I].ChangeByParam(Copy(Name, P+1, MaxInt), Value, HasMinus);
    end
    else
      raise Exception.Create('Unknown param: '+aParam);
  end
  else
    raise Exception.Create('Unknown param: '+aParam);
end;

function TVersionResource.GetFixedFileInfo: PVSFixedFileInfo;
begin
  Result := @FFixedFileInfo;
end;

function TVersionResource.HandleChangeVersion(aOldVersion: Int64; aChangeValue: 
   string; aVersionStr: string): Int64;
var
  P, I: Integer;
  S: string;

  function SetInt64Value(aOldValue: Int64; aWordPos: Integer; aValue: string):
     Int64;
  var
    sResult: string;
    iTemp: Integer;
    H,M,S, MS: Word;
  begin
    if (Length(aValue) > 0) and (aValue='*') then
    begin
      //do the .net way:  http://msdn2.microsoft.com/en-us/library/aa446487.aspx
      // The actual build number generated by the compiler just happens to be an amount of days since
      // midnight January 1, 2000, while the revision number is an amount of seconds (div 2!) since midnight
      // (all UTC => however investigating with VS & C#Builder shows that it is not using UTC!
      if aWordPos = 2 then
      begin
        Result := Trunc(GStartTime) - Trunc(EncodeDate(2000,1,1));
      end
      else
      if aWordPos = 3 then
      begin
        DecodeTime(GStartTime, H, M, S, MS);
        Result := ((H*3600 + M * 60) + S) div 2;
      end
      else // keep the old value
        Result := (aOldValue shr ((3-aWordPos) * 16)) and $FFFF;
    end
    else
    if (Length(aValue) > 0) and not TryStrToInt(aValue, iTemp) then
    begin // handle datetime setting of version fields (ddd = 3 number day of year)
      sResult := StringReplace(aValue, 'ddd', Format('%.3d', [DayOfTheYear(GStartTime)]), [rfIgnoreCase]);
      sResult := FormatDateTime(sResult, GStartTime);
      if not TryStrToInt(sResult, iTemp) then
        StrToInt(aValue); // raises exception
      Result := iTemp;
    end
    else
    begin
      // no + or - then replace the word else add to the word
      if (Length(aValue) > 0) and ((aValue[1]<>'+') and (aValue[1]<>'-')) then
        Result := 0
      else
        Result := (aOldValue shr ((3-aWordPos) * 16)) and $FFFF;
      Result := Result + StrToInt(aValue);
    end;
    if (Result < 0) then
      Result := 0
    else if Result > $FFFF then
      Result := $FFFF;
    Result := Result shl ((3-aWordPos) * 16);
    Result := (aOldValue and not(Int64($FFFF) shl ((3-aWordPos) * 16))) or Result;
  end;

  function FormatVersion(aValue: Int64): string;
  var I: Integer;
  begin
    Result := IntToStr(aValue and $FFFF);
    for I := 1 to 3 do
    begin
      aValue := aValue shr 16;
      Result := IntToStr(aValue and $FFFF)+'.'+Result;
    end;
  end;
  
begin
  P := 0;
  I := 1;
  S := '';
  Result := aOldVersion;
  while i <= Length(aChangeValue) do
  begin
    if aChangeValue[I] in ['.', ','] then
    begin
      if S <> '' then
        Result := SetInt64Value(Result, P, S);
      Inc(P);
      S := '';
    end
    else
    if aChangeValue[I] in ['/', '\'] then
    begin
      if S <> '' then
        Result := SetInt64Value(Result, P, S);
      S := Copy(aChangeValue, I+1, MaxInt);
      if CompareText('s', S) = 0 then
      begin
        ChangeByParam('\StringFileInfo\\'+aVersionStr+'='+FormatVersion(Result));
      end
      else
        raise Exception.Create('Unknown value: '+aChangeValue);
      S := '';
      Break;
    end
    else
      S := S + aChangeValue[I];
    Inc(I);
  end;
  if S <> '' then
    Result := SetInt64Value(Result, P, S);
end;

procedure TVersionResource.LoadResourcePartData(aSender: TResourcePart; aPart: 
   TStream);
begin
  {$I CheckLicense2.inc}
  inherited;
  // delegate loading to a TVersionResourceChild
  FVersionInfo.LoadFromStream(aPart);
  if WideCompareText('VS_VERSION_INFO', FVersionInfo.Key) <> 0 then
    raise Exception.Create('Unknown Version resource type');
  if FVersionInfo.ValueLen > 0 then
    aPart.ReadBuffer(FFixedFileInfo, SizeOf(FFixedFileInfo));
  AlignStream(aPart);
  ReadChildren(aPart);
end;

procedure TVersionResource.ReadChildren(aData: TStream);
var
  StringOrVar: TStringVarFileInfo;
begin
  while aData.Position < aData.Size do
  begin
    StringOrVar := TStringVarFileInfo.Create;
    try
      StringOrVar.LoadFromStream(aData);
      if StringOrVar.IsString then
        FStringFileInfos.Add(StringOrVar)
      else
        FVarFileInfos.Add(StringOrVar);
    except
      StringOrVar.Free;
    end;
  end;
end;

procedure TVersionResource.SaveResourcePartData(aSender: TResourcePart; aPart:
   TStream);
var
  P: Int64;
begin
  inherited;
  // delegate Saving to a TVersionResourceInfo
  P := aPart.Position;
  FVersionInfo.SaveToStream(aPart);
  if FVersionInfo.ValueLen > 0 then
    aPart.WriteBuffer(FFixedFileInfo, SizeOf(FFixedFileInfo));
  AlignStream(aPart, {aWrite=}True);
  WriteChildren(aPart);
  // now we know the length: write it
  FVersionInfo.ChildLen := aPart.Position-P;
  aPart.Position := P;
  FVersionInfo.SaveToStream(aPart);
  aPart.Position := P + FVersionInfo.ChildLen;
  aPart.Size := aPart.Position;
end;

procedure TVersionResource.WriteChildren(aData: TStream);
var
  I: Integer;
begin
  for I := 0 to FStringFileInfos.Count-1 do
    FStringFileInfos[I].SaveToStream(aData);
  for I := 0 to FVarFileInfos.Count-1 do
    FVarFileInfos[I].SaveToStream(aData);
end;

function TVersionResource.HandleChangeFileFlags(aOldFlags: DWORD; aChangeValue:
    string): LongWord;
var
  I: Integer;
  S: string;
  function SetFlag(aOldValue: DWORD; aValue: string): DWORD;
  var
    bAdd: Boolean;
    Flag: DWORD;
  begin
    if (aValue[1]='+') then
      bAdd := True
    else if (aValue[1]='-') then
      bAdd := False
    else
      raise Exception.Create('Invalid FileFlag: '+aValue);
    Delete(aValue, 1, 1);
    if CompareText('VS_FF_DEBUG', aValue) = 0 then
      Flag := VS_FF_DEBUG
    else
    if CompareText('VS_FF_PRERELEASE', aValue) = 0 then
      Flag := VS_FF_PRERELEASE
    else
    if CompareText('VS_FF_PATCHED', aValue) = 0 then
      Flag := VS_FF_PATCHED
    else
    if CompareText('VS_FF_PRIVATEBUILD', aValue) = 0 then
      Flag := VS_FF_PRIVATEBUILD
    else
    if CompareText('VS_FF_INFOINFERRED', aValue) = 0 then
      Flag := VS_FF_INFOINFERRED
    else
    if CompareText('VS_FF_SPECIALBUILD', aValue) = 0 then
      Flag := VS_FF_SPECIALBUILD
    else
      raise Exception.Create('Invalid FileFlag: '+aValue);
    if bAdd then
      Result := aOldValue or Flag
    else
      Result := aOldValue and not Flag;
  end;

begin
  if (Length(aChangeValue) = 0) or (aChangeValue[1] in ['+', '-']) then
  begin
    Result := aOldFlags;
    S := '';
  end
  else
  begin
    Result := 0;
    S := '+';
  end;

  I := 1;
  while I <= Length(aChangeValue) do
  begin
    if aChangeValue[I] in ['+', '-'] then
    begin
      if S <> '' then
        Result := SetFlag(Result, S);
      S := aChangeValue[I];
    end
    else
      S := S + aChangeValue[I];
    Inc(I);
  end;
  if S <> '' then
    Result := SetFlag(Result, S);
end;



{
********************************** TVarChild ***********************************
}
function TVarChild.GetChildLen: Word;
begin
  Result := inherited GetChildLen + ValueLen;
end;

procedure TVarChild.LoadFromStream(aStream: TStream);
begin
  inherited;
  if ValueLen > 0 then
  begin
    GetMem(FValue, ValueLen);
    aStream.Read(FValue^, ValueLen);
  end;
end;

procedure TVarChild.SaveToStream(aStream: TStream);
begin
  inherited;
  if ValueLen > 0 then
    aStream.Write(FValue^, ValueLen);
end;



{
********************************* TStringTable *********************************
}
constructor TStringTable.Create;
begin
  inherited;
  FStrings := TVersionResourceChildren.Create;
end;

destructor TStringTable.Destroy;
begin
  FStrings.Free;
  inherited;
end;

function TStringTable.GetChildLen: Word;
var
  I: Integer;
begin
  Result := inherited GetChildLen;
  for I := 0 to FStrings.Count-2 do
    Inc(Result, NextAlignBoundary(FStrings[I].ChildLen));
  if FStrings.Count > 0 then
    Inc(Result, FStrings[FStrings.Count-1].ChildLen);
  
end;

procedure TStringTable.LoadFromStream(aStream: TStream);
var
  Child: TStringChild;
begin
  inherited;
  while aStream.Position < FEndPos do
  begin
    Child := TStringChild.Create;
    try
      Child.LoadFromStream(aStream);
      FStrings.Add(Child)
    except
      Child.Free;
    end;
  end;
end;

procedure TStringTable.SaveToStream(aStream: TStream);
var
  I: Integer;
begin
  inherited;
  for I := 0 to FStrings.Count-1 do
    FStrings[I].SaveToStream(aStream);
end;

procedure TStringTable.ChangeByParam(aName, aValue: string; aDelete: Boolean=False);
var
  I: Integer;
  s: string;
  Child: TStringChild;
begin
  if aName[1] in ['/','\'] then
  begin
    s := Copy(aName, 2, MaxInt);
    for I := 0 to Strings.Count-1 do
    begin
      if CompareText(Strings[I].Key, s)=0 then
      begin
        if aDelete then
          Strings.Delete(I)
        else
          TStringChild(Strings[I]).Value := aValue;
        Exit;
      end;
    end;
    if not aDelete then
    begin
      Child := TStringChild.Create;
      try
        Child.Key := s;
        Child.Value := aValue;
        FStrings.Add(Child)
      except
        Child.Free;
      end;
    end;
  end;
end;


{
********************************* TStringChild *********************************
}
function TStringChild.GetChildLen: Word;
begin
  Result := inherited GetChildLen + (GetValueLen*2);
end;

function TStringChild.GetValueLen: Integer;
begin
  Result := Length(Value) + 1; // Don't use WideLength!!!
end;

procedure TStringChild.LoadFromStream(aStream: TStream);
begin
  inherited;
  if ValueLen > 0 then
    Value := ReadUniStringFromStream(aStream, True)
  else
    Value := '';
end;

procedure TStringChild.SaveToStream(aStream: TStream);
begin
  ValueLen := GetValueLen;
  inherited;
  if ValueLen = 1 then
    Value := #0;
  WriteUniStringToStream(aStream, Value, True);
end;



{
****************************** TStringVarFileInfo ******************************
}
constructor TStringVarFileInfo.Create;
begin
  inherited;
  FChildren := TVersionResourceChildren.Create;
end;

destructor TStringVarFileInfo.Destroy;
begin
  FChildren.Free;
  inherited;
end;

procedure TStringVarFileInfo.ChangeByParam(aName, aValue: string; aDelete: 
    Boolean=False);
var
  I, P: Integer;
  s: string;
begin
  if aName[1] in ['/','\'] then
  begin
    P := Pos(aName[1], Copy(aName, 2, MaxInt));
    if P = 0 then
      raise Exception.Create('Unknown param: '+aName);
    s := Copy(aName, 2, P-1);
    for I := 0 to Children.Count-1 do
    begin
      if IsString and ((s = '') or (CompareText(Children[I].Key, s)=0)) then
        TStringTable(Children[I]).ChangeByParam(Copy(aName, P+1, MaxInt), aValue, aDelete)
      else
      if IsVar and (CompareText(Children[I].Key, s)=0) then
        raise Exception.Create('Changing VarFileInfo is not supported');
//        TVarChild(Children[I]).Value := TVarChild(Children[I]).Value
    end;
  end
  else
    raise Exception.Create('Unknown param: '+aName);
end;

function TStringVarFileInfo.GetChildLen: Word;
var
  I: Integer;
begin
  Result := inherited GetChildLen;
  for I := 0 to FChildren.Count-2 do
    Inc(Result, NextAlignBoundary(FChildren[I].ChildLen));
  if FChildren.Count > 0 then
    Inc(Result, FChildren[FChildren.Count-1].ChildLen);
end;

procedure TStringVarFileInfo.LoadFromStream(aStream: TStream);
var
  Child: TVersionResourceChild;
begin
  inherited;
  if WideCompareText('VarFileInfo', Key)=0 then
    FIsVar := True
  else
  if WideCompareText('StringFileInfo', Key)=0 then
    FIsVar := False
  else
    raise Exception.Create('Unknown FileInfo type');
  FIsString := not FIsVar;
  while aStream.Position < FEndPos do
  begin
    if IsVar then
      Child := TVarChild.Create
    else
      Child := TStringTable.Create;
    try
      Child.LoadFromStream(aStream);
      FChildren.Add(Child)
    except
      Child.Free;
    end;
  end;
end;

procedure TStringVarFileInfo.SaveToStream(aStream: TStream);
var
  I: Integer;
begin
  if FIsVar then
    Key := 'VarFileInfo'
  else
    Key := 'StringFileInfo';
  ValueLen := 0;
  inherited;
  for I := 0 to FChildren.Count-1 do
    FChildren[I].SaveToStream(aStream);
end;


{
***************************** TVersionResourceInfo *****************************
}
constructor TVersionResourceInfo.Create;
begin
  inherited;
  FKey := 'VS_VERSION_INFO';
end;

function TVersionResourceInfo.GetChildLen: Word;
begin
  Result := FChildLen;
end;

procedure TVersionResource.UpdateBorlandOptionFile(aIniFile: TCustomIniFile);
var
  I, J: Integer;
  Key: string;
  Strings: TStringList;
begin
  with aIniFile do
  begin
    WriteInteger('Version Info', 'MajorVer', FFixedFileInfo.dwFileVersionMS shr 16);
    WriteInteger('Version Info', 'MinorVer', FFixedFileInfo.dwFileVersionMS and $FFFF);
    WriteInteger('Version Info', 'Release', FFixedFileInfo.dwFileVersionLS shr 16);
    WriteInteger('Version Info', 'Build', FFixedFileInfo.dwFileVersionLS and $FFFF);

    WriteBool('Version Info', 'Debug', (FFixedFileInfo.dwFileFlags and VS_FF_DEBUG) <> 0);
    WriteBool('Version Info', 'PreRelease', (FFixedFileInfo.dwFileFlags and VS_FF_PRERELEASE) <> 0);
    WriteBool('Version Info', 'Special', (FFixedFileInfo.dwFileFlags and VS_FF_SPECIALBUILD) <> 0);
    WriteBool('Version Info', 'Private', (FFixedFileInfo.dwFileFlags and VS_FF_PRIVATEBUILD) <> 0);

    if StringFileInfos.Count = 0 then Exit;
    if StringFileInfos[0].Children.Count = 0 then Exit;

    Strings := TStringList.Create;
    try
      ReadSectionValues('Version Info Keys', Strings);
      for I := 0 to TStringTable(StringFileInfos[0].Children[0]).Strings.Count-1 do
      begin
        Key := TStringTable(StringFileInfos[0].Children[0]).Strings[I].Key;
        J := Strings.IndexOfName(Key);
        if J <> -1 then
          Strings.Delete(J);
        WriteString('Version Info Keys', Key, TStringChild(TStringTable(StringFileInfos[0].Children[0]).Strings[I]).Value);
      end;
      for I := 0 to Strings.Count-1 do
        DeleteKey('Version Info Keys', Strings.Names[I]);
    finally
      Strings.Free;
    end;
  end;
end;

initialization
  GStartTime := SysUtils.Now; // to make sure that manipulating current datetime at different times all use the same time
  RegisterResourceTypeHandler(rtVersionInfo, '', TVersionResource);
finalization
  UnregisterResourceTypeHandler(rtVersionInfo, '', TVersionResource);
end.


