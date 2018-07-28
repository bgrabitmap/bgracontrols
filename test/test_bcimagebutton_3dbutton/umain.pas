unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  BCImageButton;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    BCImageButton1: TBCImageButton;
    BCImageButton2: TBCImageButton;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  BCImageButton1.LoadFromBitmapResource('boton3d');
  BCImageButton2.LoadFromBitmapResource('boton3d');
end;

end.

