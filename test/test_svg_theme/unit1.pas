unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, BGRASVGImageList,
  BGRAThemeButton, BGRASVGTheme;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRASVGImageList1: TBGRASVGImageList;
    BGRASVGTheme1: TBGRASVGTheme;
    BGRAThemeButton1: TBGRAThemeButton;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

