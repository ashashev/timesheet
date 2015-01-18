unit config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  TConfig = class(TObject)
  private
    IniFile: TIniFile;
    FDbPath: string;

    procedure ReadData;
    procedure WriteData;

  public
    constructor Create(const FileName: string);
    destructor Destroy(); override;

    property DbPath: string read FDbPath;
  end;

implementation

constructor TConfig.Create(const FileName: string);
begin
  inherited Create;
  if FileExists(FileName) then
  begin
    IniFile := TIniFile.Create(FileName);
    ReadData;
  end
  else
    raise Exception.Create('Can''t find file ' + FileName);
end;

destructor TConfig.Destroy;
begin
  WriteData;
  FreeAndNil(IniFile);
  inherited Destroy;
end;

procedure TConfig.ReadData;
begin
  FDbPath := IniFile.ReadString('DataBase', 'path', 'timesheet.db');
end;

procedure TConfig.WriteData;
begin
  if IniFile <> nil then
  begin
    IniFile.WriteString('DataBase', 'path', FDbPath);
  end;
end;

end.

