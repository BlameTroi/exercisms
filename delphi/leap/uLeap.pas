{ uLeap.pas -- is a given year a leap year? }

unit uLeap;

interface

type
  TYear = class(TObject)
  public
    class function IsLeap(year: integer): boolean;
  end;

implementation

{ on every year that is evenly divisible by 4
  except every year that is evenly divisible by 100
  unless the year is also evenly divisible by 400 }
class function TYear.IsLeap(year: integer): boolean;
begin
  if (year mod 400) = 0 then
    IsLeap := true
  else if (year mod 100) = 0 then
    IsLeap := false
  else
    IsLeap := (year mod 4) = 0
end;

end.
