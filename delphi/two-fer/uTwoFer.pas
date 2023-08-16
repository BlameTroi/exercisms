{ uTwoFer.pas }

unit uTwoFer;

interface

function twoFer: string; overload;
function twoFer(name: string): string; overload;

implementation

function twoFer: string;
begin
  result := 'One for you, one for me.';
end;

function twoFer(name: string): string;
begin
  result := 'One for ' + name + ', one for me.';
end;

end.
