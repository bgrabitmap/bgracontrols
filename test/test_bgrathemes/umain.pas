unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, BGRATheme,
  BGRAThemeButton, BGRAColorTheme, BGRAImageTheme, BGRAThemeRadioButton,
  BCListBox, BGRAThemeCheckBox, BGRASVGTheme, BGRASVGImageList;

type

  { TfrmBGRAThemesButton }

  TfrmBGRAThemesButton = class(TForm)
    BGRAColorTheme1: TBGRAColorTheme;
    BGRAImageTheme1: TBGRAImageTheme;
    BGRASVGImageList1: TBGRASVGImageList;
    BGRASVGTheme1: TBGRASVGTheme;
    BGRATheme1: TBGRATheme;
    BGRAThemeButton1: TBGRAThemeButton;
    BGRAThemeButton2: TBGRAThemeButton;
    BGRAThemeCheckBox1: TBGRAThemeCheckBox;
    BGRAThemeCheckBox2: TBGRAThemeCheckBox;
    BGRAThemeCheckBox3: TBGRAThemeCheckBox;
    BGRAThemeRadioButton1: TBGRAThemeRadioButton;
    BGRAThemeRadioButton2: TBGRAThemeRadioButton;
    BGRAThemeRadioButton3: TBGRAThemeRadioButton;
    ListBox1: TBCPaperListBox;
    procedure BGRAThemeCheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
  private

  public
    procedure InvalidateAll;
  end;

var
  frmBGRAThemesButton: TfrmBGRAThemesButton;

implementation

{$R *.lfm}

{ TfrmBGRAThemesButton }

procedure TfrmBGRAThemesButton.FormCreate(Sender: TObject);
begin
  BGRAImageTheme1.LoadResources('theme.ini');
  BGRAThemeButton1.Caption := 'This button is clickable';
  BGRAThemeButton2.Caption := 'This one may be disabled';
end;

procedure TfrmBGRAThemesButton.BGRAThemeCheckBox1Change(Sender: TObject);
begin
  BGRAThemeButton2.Enabled:= BGRAThemeCheckBox1.Checked;
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
    3: begin
      Self.Color := clWhite;
      BGRADefaultTheme := BGRASVGTheme1;
    end;
  end;
  InvalidateAll;
end;

procedure TfrmBGRAThemesButton.InvalidateAll;
  procedure InvalidateRec(AControl: TControl);
  var
    i: Integer;
  begin
    AControl.Invalidate;
    if AControl is TWinControl then
    with TWinControl(AControl) do
    begin
      for i := 0 to ControlCount-1 do
        InvalidateRec(Controls[i]);
    end;
  end;

begin
  InvalidateRec(self);
end;

end.

