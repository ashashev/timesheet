unit versioninfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TVersionInfo=class
  public
    function Get: String;
  end;

var
  Version: TVersionInfo = nil;

implementation

{$I taggedversion.inc}

function TVersionInfo.Get: String;
begin
  Result := 'ver. ' + TaggedVersionStr;
end;

initialization
begin
  Version.Create;
end;

finalization
begin
  Version.Free;
  Version := nil;
end;

end.

