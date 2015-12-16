unit popupcalendar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DateTimePicker, Forms, Controls, Graphics,
  Dialogs, ovccal;

type

  { TPopupCalendar }

  TPopupCalendar = class(TForm)
    calendar: TOvcCalendar;
    procedure calendarChange(Sender: TObject; Date: TDateTime);
    procedure calendarDblClick(Sender: TObject);
    procedure calendarDrawItem(Sender: TObject; ADate: TDateTime;
      const Rect: TRect);
    procedure FormDeactivate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure initialize(receiver: TDateTimePicker);
  private
    { private declarations }
    receiver: TDateTimePicker;
    lastDate: TDateTime;
  public
    { public declarations }
  end;

var
  PopupCalendarForm: TPopupCalendar;

implementation

uses {$IFNDEF LCL} Windows, Messages, {$ELSE} LclIntf, LclType {$ENDIF},
  dm, auxiliary;

{$R *.lfm}
procedure calcFirstAndLastDate(curDate: TDateTime; out first, last: TDateTime);
var
  day, month, year: word;
  sDate: String;
const
  fmt = '%.4d-%.2d-%.2d';
begin
  DecodeDate(curDate, year, month, day);
  sDate := format(fmt,[year, month, 1]);
  first := firstDayOfWeek(StrToDate(sDate, dbDateFormatStr, dbDateSeparator));
  if month = 12 then
    sDate := format(fmt,[year + 1, 1, 1])
  else
    sDate := format(fmt,[year, month + 1, 1]);
  last := lastDayOfWeek(StrToDate(sDate, dbDateFormatStr, dbDateSeparator) - 1);
end;

function recordExists(date: TDateTime): Boolean;
var
  sDate: String;
begin
  Result := False;
  Assert(dmMain.sqlDatesWithRecords.Active, 'dmMain.sqlDatesWithRecords is not opened!');
  sDate := FormatDateTime(dbDateFormatStr, date);
  if not dmMain.sqlDatesWithRecords.IsEmpty then
  begin
    dmMain.sqlDatesWithRecords.First;
    while not dmMain.sqlDatesWithRecords.EOF do
    begin
      if dmMain.sqlDatesWithRecords.FieldByName('date').AsString = sDate then
      begin
        Result := True;
        break;
      end;
      dmMain.sqlDatesWithRecords.Next;
    end;
  end;
end;

{ TPopupCalendar }

procedure TPopupCalendar.FormDeactivate(Sender: TObject);
begin
  Hide;
end;

procedure TPopupCalendar.calendarDblClick(Sender: TObject);
begin
  if Assigned(receiver) then
  begin
    receiver.Date := calendar.Date;
    if Assigned(receiver.OnEditingDone) then
      receiver.OnEditingDone(receiver);
    Hide;
  end;
end;

procedure TPopupCalendar.calendarDrawItem(Sender: TObject; ADate: TDateTime;
  const Rect: TRect);
var
  S: String;
  R: TRect;
  R1: TRect;
const
  fields = 3;
  radius = 6;
begin
  S := FormatDateTime('dd', ADate);
  R := Rect;
  if ADate = calendar.Date then
  begin
    calendar.Canvas.Pen.Color := clSilver;
    calendar.Canvas.Rectangle(R);
  end;

  if recordExists(ADate) then
  begin
    R1.Top := R.Top + fields;
    R1.Right := R.Right - fields;
    R1.Bottom := R1.Top + radius;
    R1.Left := R1.Right - radius;
    calendar.Canvas.Brush.Color := clLime;
    calendar.Canvas.Pen.Color := clLime;
    calendar.Canvas.Ellipse(R1);
  end;
  calendar.Canvas.Brush.Color := clNone;

  DrawText(calendar.Canvas.Handle, @S[1], Length(S), R, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
end;

procedure TPopupCalendar.calendarChange(Sender: TObject; Date: TDateTime);
var
  year1, month1, day1: word;
  year2, month2, day2: word;
  startMonth, endMonth: TDateTime;
begin
  DecodeDate(lastDate, year1, month1, day1);
  DecodeDate(Date, year2, month2, day2);
  if (year1 <> year2) or (month1 <> month2) then
  begin
    calcFirstAndLastDate(Date, startMonth, endMonth);
    with dmMain.sqlDatesWithRecords do
    begin
      Close;
      ParamByName('datefrom').Value := FormatDateTime(dbDateFormatStr, startMonth);
      ParamByName('dateto').Value := FormatDateTime(dbDateFormatStr, endMonth);
      Open;
    end;
    lastDate := Date;
  end;
end;

procedure TPopupCalendar.FormHide(Sender: TObject);
begin
  receiver := nil;
end;

procedure TPopupCalendar.initialize(receiver: TDateTimePicker);
var
  point: TPoint;
  bounds: TRect;
begin
  Assert(Assigned(receiver), 'Receiver mustn''t be nil!');
  if not Assigned(receiver) then
    Exit;

  Self.receiver := receiver;
  if receiver.Date = NullDate then
    calendar.Date := SysUtils.Date
  else
    calendar.Date := receiver.Date;

  point := receiver.ControlToScreen(Classes.Point(0,receiver.Height));
  bounds := Screen.MonitorFromPoint(point).BoundsRect;
  if point.X + Width > bounds.Right then
    Left := bounds.Right - Width
  else
    Left := point.X;
  if point.Y + Height > bounds.Bottom then
    Top := bounds.Bottom - Height
  else
    Top := point.Y;

  calendarChange(Self, calendar.Date);

  Show;
  calendar.SetFocus;
end;

end.

