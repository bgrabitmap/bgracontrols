unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  BGRAGraphicControl, BGRABitmap, BCTypes, BCTrackbarUpdown, BCImageButton,
  BGRABitmapTypes;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    BCXButton1: TBCXButton;
    procedure BCXButton1RenderControl(Sender: TObject; Bitmap: TBGRABitmap;
      State: TBCGraphicButtonState);
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
  bgramacosdraw;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.BCXButton1RenderControl(Sender: TObject;
  Bitmap: TBGRABitmap; State: TBCGraphicButtonState);
var
  r: TRect;
begin
  r := Rect(0, 0, Bitmap.Width, Bitmap.Height);
  Bitmap.FontHeight := 12;
  Bitmap.FontQuality := fqSystemClearType;
  case State of
    gbsNormal:
    begin
      TBGRAMacOs.Button(Bitmap, r);
      Bitmap.TextRect(r, BCXButton1.Caption, taCenter, tlCenter, BGRABlack);
    end;
    gbsHover:
    begin
      TBGRAMacOs.ButtonActive(Bitmap, r);
      Bitmap.TextRect(r, BCXButton1.Caption, taCenter, tlCenter, BGRAWhite);
    end;
    gbsActive:
    begin
      TBGRAMacOs.ButtonPressed(Bitmap, r);
      Bitmap.TextRect(r, BCXButton1.Caption, taCenter, tlCenter, BGRA(224, 230, 243));
    end;
  end;
end;

end.

