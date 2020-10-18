unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, BCListBoxEx,
  BCTrackbarupdown;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCListBoxEx1: TBCListBoxEx;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  with BCListBoxEx1 do
  begin
    items.Add('Argentina');
    items.Add('Brasil');
    items.Add('Paraguay');
    items.Add('Uruguay');
    items.Add('Chile');
    items.Add('Bolivia');
    items.Add('Peru');
    items.Add('Ecuador');
  end;
end;

end.

