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
  - Use 'let' to store values: "let a 100" "let key value"

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
  Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, BGRAGraphicControl,
  BGRABitmap, BCTypes, BGRAScript, BGRAVirtualScreen, BCButton, bgrabitmaptypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCButton1: TBCButton;
    BGRAGraphicControl1: TBGRAGraphicControl;
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    ListBox1: TListBox;
    Splitter1: TSplitter;
    SynAnySyn1: TSynAnySyn;
    SynCompletion1: TSynCompletion;
    SynEdit1: TSynEdit;
    procedure BCButton1Click(Sender: TObject);
    procedure BGRAGraphicControl1Click(Sender: TObject);
    procedure BGRAGraphicControl1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
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

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  Bitmap.DrawHorizLine(0,Bitmap.Height-3,Bitmap.Width-1,BGRA(215,215,215,255));
  Bitmap.DrawHorizLine(0,Bitmap.Height-2,Bitmap.Width-1,BGRA(235,235,235,255));
  Bitmap.DrawHorizLine(0,Bitmap.Height-1,Bitmap.Width-1,BGRA(240,240,240,255));
end;

procedure TForm1.BGRAGraphicControl1Click(Sender: TObject);
begin
  BGRAGraphicControl1.DiscardBitmap;
end;

procedure TForm1.BCButton1Click(Sender: TObject);
begin
  ListBox1.Visible := not ListBox1.Visible;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BGRAScript.SynCompletionList(SynCompletion1.ItemList);
  BGRAScript.SynCompletionList(ListBox1.Items);
end;

procedure TForm1.ListBox1DblClick(Sender: TObject);
begin
  SynEdit1.Lines.Add(ListBox1.GetSelectedText);
end;

end.


