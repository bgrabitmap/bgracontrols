unit umain;

{
 How to write:
  - Separate commands by line (only one command in one line)
  - Separate parameters by single space ' ' or comma ','
  - All parameters are obligatory
  - Put strings with double quotes "text"
  - Put float numbers as strings "1,5"
  - Comments are threated as the first parameter
  - Comment with '// comment' and '{ comment }' or '//,comment' and '{,comment}'
  - Multi line comments not allowed

 Error handling:
  - If one line fails and program not crash it will continue running other lines
  - Empty lines will not run and will not be printed in debug
  - Wrong number of parameters will not run and will show an error in debug
  - Wrong command name will not run and will show an error in debug
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynCompletion, SynHighlighterAny, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, BGRAGraphicControl, BGRABitmap,
  BCTypes, BGRAScript;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRAGraphicControl1: TBGRAGraphicControl;
    Splitter1: TSplitter;
    SynAnySyn1: TSynAnySyn;
    SynCompletion1: TSynCompletion;
    SynEdit1: TSynEdit;
    procedure BGRAGraphicControl1Click(Sender: TObject);
    procedure BGRAGraphicControl1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
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

procedure TForm1.BGRAGraphicControl1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  BGRAScript.ScriptCommandList(SynEdit1.Lines, Bitmap);
end;

procedure TForm1.BGRAGraphicControl1Click(Sender: TObject);
begin
  BGRAGraphicControl1.DiscardBitmap;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BGRAScript.SynCompletionList(SynCompletion1.ItemList);
end;

end.


