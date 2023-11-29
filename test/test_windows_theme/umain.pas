unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, BCPanel, BCLabel;

type

  { TfrmWindowsTheme }

  TfrmWindowsTheme = class(TForm)
    BCLabel1: TBCLabel;
    BCLabel2: TBCLabel;
    BCPanel1: TBCPanel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmWindowsTheme: TfrmWindowsTheme;

implementation

{$R *.lfm}

{ TfrmWindowsTheme }

procedure TfrmWindowsTheme.FormCreate(Sender: TObject);
begin
  BCLabel1.FontEx.Height := ScaleY(BCLabel1.FontEx.Height, 96);
  BCLabel2.FontEx.Height := ScaleY(BCLabel2.FontEx.Height, 96);
  BCPanel1.Border.Width := ScaleY(BCPanel1.Border.Width, 96);
end;

end.

