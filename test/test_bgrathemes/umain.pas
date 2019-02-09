unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, BGRATheme,
  BGRAThemeButton, BGRAColorTheme, BGRAImageTheme;

type

  { TfrmBGRAThemesButton }

  TfrmBGRAThemesButton = class(TForm)
    BGRAColorTheme1: TBGRAColorTheme;
    BGRAImageTheme1: TBGRAImageTheme;
    BGRATheme1: TBGRATheme;
    BGRAThemeButton1: TBGRAThemeButton;
    BGRAThemeButton2: TBGRAThemeButton;
    BGRAThemeButton3: TBGRAThemeButton;
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
  private

  public

  end;

var
  frmBGRAThemesButton: TfrmBGRAThemesButton;

implementation

{$R *.lfm}

{ TfrmBGRAThemesButton }

procedure TfrmBGRAThemesButton.FormCreate(Sender: TObject);
begin
  BGRAImageTheme1.LoadResources('theme.ini');
end;

procedure TfrmBGRAThemesButton.ListBox1SelectionChange(Sender: TObject;
  User: boolean);
begin
  case ListBox1.ItemIndex of
    0: begin
      BGRAThemeButton1.Theme := BGRATheme1;
      BGRAThemeButton2.Theme := BGRATheme1;
      BGRAThemeButton3.Theme := BGRATheme1;
    end;
    1: begin
      BGRAThemeButton1.Theme := BGRAColorTheme1;
      BGRAThemeButton2.Theme := BGRAColorTheme1;
      BGRAThemeButton3.Theme := BGRAColorTheme1;
    end;
    2: begin
      BGRAThemeButton1.Theme := BGRAImageTheme1;
      BGRAThemeButton2.Theme := BGRAImageTheme1;
      BGRAThemeButton3.Theme := BGRAImageTheme1;
    end;
  end;
end;

end.

