unit auxiliary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  daysPerWeek = 7;
  sunday = 1;
  monday = 2;
  emptyTimeStr = '  :  ';
  errorStrTimeMustBe = 'Hours must be less 24, minutes must be less 60, time must be less 24:00';

function minutesToString( minutes: Longint ): String;
function stringToMinutes( strTime: String ): Longint;
function stringToMinutesVariant( strTime: String ): Variant;
function minutesVariantToString( minutes: Variant; strNull: String = '' ): String;
function validateTimeString( strTime: String ): Boolean;
function firstDayOfWeek(date: TDateTime): TDateTime;
function lastDayOfWeek(date: TDateTime): TDateTime;

implementation
  const minPerHour = 60;

function minutesToString(minutes: Longint): String;
var
  hours: Longint;
  mins: Longint;
begin
  hours := abs(minutes) div minPerHour;
  mins := abs(minutes) mod minPerHour;
  if minutes < 0 then
    Result := '-'
  else
    Result := '';
  if hours < 10 then
    Result := Result + '0';
  Result := Result + IntToStr(hours) + ':';
  if mins < 10 then
    Result := Result + '0';
  Result := Result + IntToStr(mins);
end;

function stringToMinutes( strTime: String ): Longint;
var
  hours, minutes: Integer;
begin
  strTime := StringReplace( strTime, ' ', '0', [rfReplaceAll] );
  hours := StrToInt( Copy( strTime, 1, 2 ) );
  minutes := StrToInt( Copy( strTime, 4, 2 ) );
  Result := hours * minPerHour + minutes;
end;

function stringToMinutesVariant( strTime: String ): Variant;
begin
  if (strTime <> emptyTimeStr) and (strTime <> '') then
    Result := stringToMinutes( strTime )
  else
    Result := Null;
end;

function minutesVariantToString( minutes: Variant; strNull: String = '' ): String;
begin
  if minutes <> Null then
    Result := minutesToString( minutes )
  else
    Result := strNull;
end;

function validateTimeString( strTime: String ): Boolean;
var
  hours, minutes: Integer;
begin
  Result := true;
  if (strTime <> emptyTimeStr) and (strTime <> '') then
  begin
    strTime := StringReplace( strTime, ' ', '0', [rfReplaceAll] );
    hours := StrToInt( Copy( strTime, 1, 2 ) );
    minutes := StrToInt( Copy( strTime, 4, 2 ) );
    if (hours > 24) or (minutes > 59) or ((hours = 24) and (minutes <> 0)) then
      Result := false;
  end;
end;

function firstDayOfWeek(date: TDateTime): TDateTime;
var
  curDayOfWeek:Integer;
begin
  curDayOfWeek := DayOfWeek(date);
  if curDayOfWeek = sunday then
    Result := date - (daysPerWeek - 1)
  else
    Result := date - (curDayOfWeek - monday);
end;

function lastDayOfWeek(date: TDateTime): TDateTime;
begin
  Result := firstDayOfWeek(date) + (daysPerWeek - 1);
end;

end.

