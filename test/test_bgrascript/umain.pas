unit umain;

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


