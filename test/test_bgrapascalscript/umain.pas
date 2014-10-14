unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterPas, Forms, Controls,
  Graphics, Dialogs, StdCtrls, BGRAPascalScript, uPSI_BGRAPascalScript,
  BGRAVirtualScreen, uPSComponent, uPSComponent_Default, BGRABitmap, BCTypes,
  BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    Button1: TButton;
    Button2: TButton;
    PSImport_Classes1: TPSImport_Classes;
    SynEdit1: TSynEdit;
    PSImport_BGRAPascalScript1: TPSImport_BGRAPascalScript;
    PSScript1: TPSScript;
    SynPasSyn1: TSynPasSyn;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
  s: String;
begin
  PSScript1.Script := SynEdit1.Lines;
  if PSScript1.Compile then
  begin
    PSScript1.Execute;
    Button2.Enabled := True;
  end else
  begin
     s := 'Compile error.' + LineEnding;
     for i := 0 to PSScript1.CompilerMessageCount-1 do
       with PSScript1.CompilerMessages[i] do
         s += '('+inttostr(Row)+','+inttostr(Col)+') '+MessageToString+LineEnding;
     ShowMessage(s);
  end;
end;

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  if Button2.Enabled then
    Bitmap.StretchPutImage(Rect(0, 0, 32, 32), BitmapArray[0], dmSet);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  BGRAVirtualScreen1.DiscardBitmap;
end;

end.
