unit BdsProjSupport;

interface

uses IniFiles, LibXMLParser, Classes, ResFiles;

type
  TOptionFileUpdater = class
  public
    class procedure UpdateBorlandOptionFile(aResFile: TResourceFile; aFileName: string);
  end;

  // supports old bdsproj and new msbuild dproj files
  TBdsProjIniFile = class(TMemIniFile)
  private
    FMemStream: TMemoryStream;
    FXMLFile: string;
    procedure SetXMLFile(const Value: string);
  public
    destructor Destroy; override;
    procedure LoadVersionXML;
    function ReadStringToUTF(const Section, Ident, Default: string): string;
    procedure SaveVersionXML;
    procedure WriteBool(const Section, Ident: string; Value: Boolean); override;
    property XMLFile: string read FXMLFile write SetXMLFile;
  end;

implementation

uses SysUtils;

function SimpleHTMLEscape(const Str: string): string;
var
  i, l : integer;
begin
  l := Length(Str);
  Result := '';
  for i := 1 to l do
  begin
    case Str[i]  of
    '<' : Result := Result + '&lt;';    { Do not localize }
    '>' : Result := Result + '&gt;';    { Do not localize }
    '&' : Result := Result + '&amp;';   { Do not localize }
    '"' : Result := Result + '&quot;';  { Do not localize }
    #92 : Result := Result + '&#' + IntToStr(Ord(Str[ i ])) +';';  { Do not localize }
    else
      Result := Result + Str[i];
    end;
  end;
end;

procedure SaveMemIniFileWithoutEmptyLines(aMemIniFile:TMemIniFile);
var
  I: Integer;
  List: TStringList;
begin
  List := TStringList.Create;
  try
    aMemIniFile.GetStrings(List);
    for I := List.Count-1 downto 0 do
      if List[I] = '' then
        List.Delete(I);
    List.SaveToFile(aMemIniFile.FileName)
  finally
    List.Free;
  end;
end;

destructor TBdsProjIniFile.Destroy;
begin
  FMemStream.Free;
  inherited;
end;

procedure TBdsProjIniFile.LoadVersionXML;
var
  S: string;
  XmlParser: TXmlParser;
  InVersionInfo: Integer;
  InVersionInfoKeys: Integer;
  VersionInfoKeysName: string;
  VersionInfoName: string;
begin
  XmlParser := TXmlParser.Create;
  try
    XmlParser.LoadFromBuffer(PChar(FMemStream.Memory));
    XmlParser.StartScan;
    XmlParser.Normalize := FALSE;

    InversionInfo := 0;
    InversionInfoKeys := 0;
    WHILE XmlParser.Scan DO
    begin
      if (XmlParser.CurPartType = ptStartTag) then
      begin
        if XmlParser.CurName = 'VersionInfo' then
        begin
          Inc(InVersionInfo);
          if (InVersionInfo > 1) and (XmlParser.CurAttr.Count > 0) then
          begin
            VersionInfoName := XmlParser.CurAttr.Value('Name');
          end;
        end
        else
        if XmlParser.CurName = 'VersionInfoKeys' then
        begin
          Inc(InVersionInfoKeys);
          if (InVersionInfoKeys > 1) and (XmlParser.CurAttr.Count > 0) then
          begin
            VersionInfoKeysName := XmlParser.CurAttr.Value('Name');
          end;
        end;
      end
      else
      if (XmlParser.CurPartType = ptEndTag) then
      begin
        if XmlParser.CurName = 'VersionInfo' then
          Dec(InversionInfo)
        else
        if XmlParser.CurName = 'VersionInfoKeys' then
        begin
          Dec(InversionInfoKeys);
          if InVersionInfoKeys=0 then
            Break ; // fast way out
        end;
      end;

      if (XmlParser.CurPartType = ptContent) then
      begin
        if (VersionInfoName <> '') then
        begin
          SetLength(S, Cardinal(XmlParser.CurFinal)-Cardinal(XmlParser.CurStart)+1);
          StrLCopy(PChar(S), XmlParser.CurStart, Length(S));
          WriteString('Version Info', VersionInfoName, S);
          VersionInfoName := '';
        end
        else
        if (VersionInfoKeysName <> '') then
        begin
          SetLength(S, Cardinal(XmlParser.CurFinal)-Cardinal(XmlParser.CurStart)+1);
          StrLCopy(PChar(S), XmlParser.CurStart, Length(S));
          WriteString('Version Info Keys', VersionInfoKeysName, Utf8ToAnsi(S));
          VersionInfoKeysName := '';
        end;
      end;
    end;
  finally
    XmlParser.Free;
  end;
end;

function TBdsProjIniFile.ReadStringToUTF(const Section, Ident, Default:
    string): string;
begin
  if Self.ValueExists(Section, Ident) then
    Result := AnsiToUtf8(ReadString(Section, Ident, Default))
  else
    Result := Default;
  Result := SimpleHTMLEscape(Result);
end;

procedure TBdsProjIniFile.SaveVersionXML;
var
  LastContent: string;
  S: string;
  I: Integer;
  Strings: TStringList;
  VersionInfoKeysName: string;
  VersionInfoName: string;
  InVersionInfoKeys: Integer;
  InVersionInfo: Integer;
  XmlParser: TXmlParser;
  IndentContent: string;
  RemoveVersionInfoKeys: Boolean;
  DestStream: TFileStream;
  procedure VersionInfoStart;
  begin
    Inc(InVersionInfo);
    if (InVersionInfo > 1) and (XmlParser.CurAttr.Count > 0) then
    begin
      VersionInfoName := XmlParser.CurAttr.Value('Name');
      LastContent := ''; // so it gets replaced with real content of none if none
    end;
  end;
  procedure VersionInfoKeysStart(aIsEmptyTag: Boolean);
  begin
    Inc(InVersionInfoKeys);
    if (InVersionInfoKeys > 1) and (XmlParser.CurAttr.Count > 0) then
    begin
      VersionInfoKeysName := XmlParser.CurAttr.Value('Name');
      RemoveVersionInfoKeys := not ValueExists('Version Info Keys', VersionInfoKeysName);
      if RemoveVersionInfoKeys then // first remove the last content its the indention
      begin
        DestStream.Position := DestStream.Position-Length(LastContent);
        if aIsEmptyTag then
          XmlParser.CurPartType := ptEndTag; // fake endtag
      end;
      LastContent := ''; // so it gets replaced with real content or none if none
    end
    else
      RemoveVersionInfoKeys := False;
  end;

begin
  XmlParser := TXmlParser.Create;
  try
    XmlParser.LoadFromBuffer(PChar(FMemStream.Memory));
    XmlParser.StartScan;
    XmlParser.Normalize := FALSE;

    DestStream := TFileStream.Create(FXMLFile, fmOpenWrite or fmShareDenyWrite);
    try
      IndentContent := #13#10;
      InVersionInfo := 0;
      InVersionInfoKeys := 0;
      RemoveVersionInfoKeys := False;
      WHILE XmlParser.Scan DO
      begin
        if (XmlParser.CurPartType = ptStartTag) then
        begin
          if XmlParser.CurName = 'VersionInfo' then
          begin
            VersionInfoStart;
          end
          else
          if XmlParser.CurName = 'VersionInfoKeys' then
          begin
            VersionInfoKeysStart(False);
          end;
        end
        else
        if (XmlParser.CurPartType in [ptEndTag, ptEmptyTag]) then
        begin
          if XmlParser.CurName = 'VersionInfo' then
          begin
            if XmlParser.CurPartType = ptEmptyTag then
              VersionInfoStart;
            if (VersionInfoName <> '') then
            begin
              // remove last content
              DestStream.Position := DestStream.Position-Length(LastContent);
              // write new content
              LastContent := ReadString('Version Info', VersionInfoName, S);
              DestStream.Write(Pointer(LastContent)^, Length(LastContent));
              VersionInfoName := '';
            end;
            Dec(InversionInfo);
          end
          else
          if XmlParser.CurName = 'VersionInfoKeys' then
          begin
            if XmlParser.CurPartType = ptEmptyTag then
              VersionInfoKeysStart(True);
            if (VersionInfoKeysName <> '') then
            begin
              if not RemoveVersionInfoKeys then
              begin
                // remove last content
                DestStream.Position := DestStream.Position-Length(LastContent);
                // write new content
                LastContent := ReadStringToUTF('Version Info Keys', VersionInfoKeysName, S);
                DeleteKey('Version Info Keys', VersionInfoKeysName);
                if XmlParser.CurPartType = ptEmptyTag then
                begin
                  if LastContent <> '' then // turn emptytag into complete tag
                  begin
                    LastContent := Format('<VersionInfoKeys Name="%s">%s</VersionInfoKeys>', [VersionInfoKeysName, LastContent]);
                    DestStream.Write(Pointer(LastContent)^, Length(LastContent));
                    RemoveVersionInfoKeys := True; // remove the emptytag
                    XmlParser.CurPartType := ptEndTag; // fake endtag, otherwise RemoveVersionInfoKeys doesn't work
                  end
                  else
                  // empty tag can stay
                end
                else
                  DestStream.Write(Pointer(LastContent)^, Length(LastContent));
              end;
              VersionInfoKeysName := '';
            end;

            Dec(InversionInfoKeys);
            if InVersionInfoKeys=0 then
            begin
              // write out the stuff that wasn't written yet
              Strings := TStringList.Create;
              try
                ReadSection('Version Info Keys', Strings);
                if Strings.Count > 0 then
                begin
                  // remove last content it is the indention of /versioninfokeys
                  DestStream.Position := DestStream.Position-Length(LastContent);
                  for I := 0 to Strings.Count-1 do
                  begin
                    S := Format('%s<VersionInfoKeys Name="%s" Type="String">%s</VersionInfoKeys>', [IndentContent, Strings[I], ReadStringToUTF('Version Info Keys', Strings[I], '')]);
                    DestStream.Write(Pointer(S)^, Length(S))
                  end;
                  // now add the last content it is the indention of /versioninfokeys
                  DestStream.Write(Pointer(LastContent)^, Length(LastContent))
                end;
              finally
                Strings.Free;
              end;
            end;
          end;
          // we're finished with the end tag, make sure LastContent is empty again, we use LastContent to remove indention in VersionInfoKeysStart
          LastContent := '';
        end;

        SetLength(S, Cardinal(XmlParser.CurFinal)-Cardinal(XmlParser.CurStart)+1);
        StrLCopy(PChar(S), XmlParser.CurStart, Length(S));
        if (XmlParser.CurPartType = ptContent) then
        begin
          if (InVersionInfoKeys=1) and (IndentContent = #13#10) then
          begin
            IndentContent := S;
          end;
          LastContent := S;
        end;
        if (InVersionInfoKeys > 0) and RemoveVersionInfoKeys then
        begin
          if (XmlParser.CurPartType = ptEndTag) and (XmlParser.CurName = 'VersionInfoKeys') then
            RemoveVersionInfoKeys := False; // stop removing
          // nothing to write
        end
        else
          DestStream.Write(Pointer(S)^, Length(S))
      end;
      DestStream.Size := DestStream.Position;
    finally
      DestStream.Free;
    end;
  finally
    XmlParser.Free;
  end;
end;

procedure TBdsProjIniFile.SetXMLFile(const Value: string);
var
  C: Char;
begin
  FXMLFile := Value;
  if FMemStream = nil then
    FMemStream := TMemoryStream.Create
  else
    FMemStream.Clear;
  FMemStream.LoadFromFile(Value);
  FMemStream.Seek(0, soFromEnd);
  C := #0;
  FMemStream.Write(C, 1);
end;

procedure TBdsProjIniFile.WriteBool(const Section, Ident: string; Value:
    Boolean);
const
  Values: array[Boolean] of string = ('False', 'True');
begin
  WriteString(Section, Ident, Values[Value]);
end;

class procedure TOptionFileUpdater.UpdateBorlandOptionFile(aResFile: TResourceFile; aFileName: string);

  procedure UpdateResParts(aIniFile: TCustomIniFile);
  var
    I: Integer;
  begin
    for I := 0 to aResFile.ResourceParts.Count-1 do
      if aResFile.ResourceParts[I].PartData <> nil then
        aResFile.ResourceParts[I].PartData.UpdateBorlandOptionFile(aIniFile);
  end;
var
  I: Integer;
  FileName: string;
  TmpStrings, Options: TStringList;
  aIniFile: TCustomIniFile;
  IDEOptionsStart: Integer;
begin
  inherited;
  FileName := ChangeFileExt(aFileName, '.dof');
  if FileExists(FileName) then
  begin
    aIniFile := TMemIniFile.Create(FileName);
    try
      UpdateResParts(aIniFile);
      SaveMemIniFileWithoutEmptyLines(aIniFile as TMemIniFile);
    finally
      aIniFile.Free;
    end;
  end
  else
  begin
    FileName := ChangeFileExt(aFileName, '.bpk');
    if not FileExists(FileName) then
      FileName := ChangeFileExt(aFileName, '.bpr');
    if FileExists(FileName) then
    begin
      TmpStrings := nil;
      Options := TStringList.Create;
      try
        TmpStrings := TStringList.Create;
        TmpStrings.LoadFromFile(FileName);
        IDEOptionsStart := -1;
        i := 0;
        while i < TmpStrings.Count do
        begin
          if (IDEOptionsStart < 0) then
          begin
            if (CompareText(Trim(TmpStrings[I]), '!ifdef IDEOPTIONS') = 0) or
               (CompareText(Trim(TmpStrings[I]), '<IDEOPTIONS>') = 0) then
            begin
              Inc(i);
              while (i < TmpStrings.Count) and (Trim(TmpStrings[I])='') do
                Inc(i);
              IDEOptionsStart := I;
            end
            else
              Inc(i);
          end
          else
          begin
            if (CompareText(Trim(TmpStrings[I]), '!endif') = 0) or
               (CompareText(Trim(TmpStrings[I]), '</IDEOPTIONS>') = 0) then
              Break
            else
            begin
              Options.Add(TmpStrings[I]);
              TmpStrings.Delete(I);
            end;
          end;
        end;
        aIniFile := TMemIniFile.Create('');
        try
          TMemIniFile(aIniFile).SetStrings(Options);
          UpdateResParts(aIniFile);
          Options.Clear;
          TMemIniFile(aIniFile).GetStrings(Options);
        finally
          aIniFile.Free;
        end;
        for i := 0 to Options.Count-1 do
          TmpStrings.Insert(IDEOptionsStart+i, Options[I]);
        TmpStrings.SaveToFile(FileName);
      finally
        Options.Free;
        TmpStrings.Free;
      end;
    end
    else
    begin
      FileName := ChangeFileExt(aFileName, '.bdsproj');
      if not FileExists(FileName) then
        FileName := ChangeFileExt(aFileName, '.dproj');
      if not FileExists(FileName) then
        FileName := ChangeFileExt(aFileName, '.cbproj');
      if not FileExists(FileName) then
        raise Exception.Create('No Delphi/C++ option file found (.dof, .bdsproj, dproj, .cbproj, .bpk)');

      aIniFile := TBdsProjIniFile.Create('');
      try
        TBdsProjIniFile(aIniFile).XMLFile := FileName;
        TBdsProjIniFile(aIniFile).LoadVersionXML;
        UpdateResParts(aIniFile);
        TBdsProjIniFile(aIniFile).SaveVersionXML;
      finally
        aIniFile.Free;
      end;
    end;
  end;

end;

end.
