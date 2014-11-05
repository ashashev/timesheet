unit auxiliary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  days_per_week = 7;
  sunday = 1;
  monday = 2;
  empty_time_str = '  :  ';
  error_str_time_must_be = 'Hours must be less 24, minutes must be less 60, time must be less 24:00';

function minutes_to_string( minutes: Longint ): String;
function string_to_minutes( s_time: String ): Longint;
function validate_time_string( s_time: String ): Boolean;

implementation
  const min_per_hour = 60;

function minutes_to_string(minutes: Longint): String;
var
  hours: Longint;
  mins: Longint;
begin
  hours := minutes div min_per_hour;
  mins := minutes mod min_per_hour;
  if hours < 10 then
    Result := '0';
  Result := Result + IntToStr(hours) + ':';
  if mins < 10 then
    Result := Result + '0';
  Result := Result + IntToStr(mins);
end;

function string_to_minutes( s_time: String ): Longint;
var
  hours, minutes: Integer;
begin
  s_time := StringReplace( s_time, ' ', '0', [rfReplaceAll] );
  hours := StrToInt( Copy( s_time, 1, 2 ) );
  minutes := StrToInt( Copy( s_time, 4, 2 ) );
  Result := hours * min_per_hour + minutes;
end;

function validate_time_string( s_time: String ): Boolean;
var
  hours, minutes: Integer;
begin
  Result := true;
  if s_time <> empty_time_str then
  begin
    s_time := StringReplace( s_time, ' ', '0', [rfReplaceAll] );
    hours := StrToInt( Copy( s_time, 1, 2 ) );
    minutes := StrToInt( Copy( s_time, 4, 2 ) );
    if (hours > 24) or (minutes > 59) or ((hours = 24) and (minutes <> 0)) then
      Result := false;
  end;
end;

end.

