unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, BGRATheme,
  BGRAThemeButton, BGRAColorTheme, BGRAImageTheme, BGRAThemeRadioButton,
  BCListBox;

type

  { TfrmBGRAThemesButton }

  TfrmBGRAThemesButton = class(TForm)
    BGRAColorTheme1: TBGRAColorTheme;
    BGRAImageTheme1: TBGRAImageTheme;
    BGRATheme1: TBGRATheme;
    BGRAThemeButton1: TBGRAThemeButton;
    BGRAThemeButton2: TBGRAThemeButton;
    BGRAThemeRadioButton1: TBGRAThemeRadioButton;
    BGRAThemeRadioButton2: TBGRAThemeRadioButton;
    BGRAThemeRadioButton3: TBGRAThemeRadioButton;
    ListBox1: TBCPaperListBox;
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
  case ListBox1.ListBox.ItemIndex of
    0: begin
      Self.Color := clWhite;
      BGRADefaultTheme := BGRATheme1;
    end;
    1: begin
      Self.Color := clBlack;
      BGRADefaultTheme := BGRAColorTheme1;
    end;
    2: begin
      Self.Color := clWhite;
      BGRADefaultTheme := BGRAImageTheme1;
    end;
  end;
  Invalidate;
end;

end.

