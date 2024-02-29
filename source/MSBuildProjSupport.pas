unit BdsProjSupport;

interface

uses IniFiles, LibXMLParser, classes;

type
  TMSBuildProjIniFile = class(TMemIniFile)
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

destructor TMSBuildProjIniFile.Destroy;
begin
  FMemStream.Free;
  inherited;
end;

procedure TMSBuildProjIniFile.LoadVersionXML;
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

function TMSBuildProjIniFile.ReadStringToUTF(const Section, Ident, Default:
    string): string;
begin
  if Self.ValueExists(Section, Ident) then
    Result := AnsiToUtf8(ReadString(Section, Ident, Default))
  else
    Result := Default;
end;

procedure TMSBuildProjIniFile.SaveVersionXML;
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
            Inc(InVersionInfo);
            if (InVersionInfo > 1) and (XmlParser.CurAttr.Count > 0) then
            begin
              VersionInfoName := XmlParser.CurAttr.Value('Name');
              LastContent := ''; // so it gets replaced with real content of none if none
            end;
          end
          else
          if XmlParser.CurName = 'VersionInfoKeys' then
          begin
            Inc(InVersionInfoKeys);
            if (InVersionInfoKeys > 1) and (XmlParser.CurAttr.Count > 0) then
            begin
              VersionInfoKeysName := XmlParser.CurAttr.Value('Name');
              RemoveVersionInfoKeys := not ValueExists('Version Info Keys', VersionInfoKeysName);
              if RemoveVersionInfoKeys then // first remove the last content its the indention
                DestStream.Position := DestStream.Position-Length(LastContent);
              LastContent := ''; // so it gets replaced with real content of none if none
            end
            else
              RemoveVersionInfoKeys := False;
          end;
        end
        else
        if (XmlParser.CurPartType = ptEndTag) then
        begin
          if XmlParser.CurName = 'VersionInfo' then
          begin
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
            if (VersionInfoKeysName <> '') then
            begin
              if not RemoveVersionInfoKeys then
              begin
                // remove last content
                DestStream.Position := DestStream.Position-Length(LastContent);
                // write new content
                LastContent := ReadStringToUTF('Version Info Keys', VersionInfoKeysName, S);
                DeleteKey('Version Info Keys', VersionInfoKeysName);
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

procedure TMSBuildProjIniFile.SetXMLFile(const Value: string);
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

procedure TMSBuildProjIniFile.WriteBool(const Section, Ident: string; Value:
    Boolean);
const
  Values: array[Boolean] of string = ('False', 'True');
begin
  WriteString(Section, Ident, Values[Value]);
end;

end.
