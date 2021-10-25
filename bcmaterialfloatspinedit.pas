unit BCMaterialFloatSpinEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin;

type

  { TBCMaterialFloatSpinEdit }

  TBCMaterialFloatSpinEdit = class(TCustomPanel)
  private
    FAccentColor: TColor;
    FDisabledColor: TColor;
    Flbl: TLabel;
    Fedt: TFloatSpinEdit;
    Ffocused: boolean;
    FOnChange: TNotifyEvent;
    FTexto: string;
    procedure ChangeEdit(Sender: TObject);
    procedure EnterEdit(Sender: TObject);
    procedure ExitEdit(Sender: TObject);
    procedure SetTexto(AValue: string);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Color;
    property Text: string read FTexto write SetTexto;
    property Edit: TFloatSpinEdit read Fedt;
    property Title: TLabel read Flbl;
    property DisabledColor: TColor read FDisabledColor write FDisabledColor;
    property AccentColor: TColor read FAccentColor write FAccentColor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCMaterialFloatSpinEdit]);
end;

{ TBCMaterialFloatSpinEdit }

procedure TBCMaterialFloatSpinEdit.EnterEdit(Sender: TObject);
begin
  Ffocused := True;
  Invalidate;
  Flbl.Font.Color := AccentColor;
end;

procedure TBCMaterialFloatSpinEdit.ChangeEdit(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCMaterialFloatSpinEdit.ExitEdit(Sender: TObject);
begin
  Ffocused := False;
  Invalidate;
  Flbl.Font.Color := DisabledColor;
end;

procedure TBCMaterialFloatSpinEdit.SetTexto(AValue: string);
begin
  if FTexto = AValue then
    Exit;
  FTexto := AValue;
  Flbl.Caption := FTexto;
  //Fedt.TextHint := FTexto;
end;

procedure TBCMaterialFloatSpinEdit.Paint;
begin
  inherited Paint;
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Color;
  Canvas.Rectangle(0, 0, Width, Height);
  if (fFocused) then
  begin
    Canvas.Pen.Color := AccentColor;
    Canvas.Line(0, Height - 2, Width, Height - 2);
    Canvas.Line(0, Height - 1, Width, Height - 1);
  end
  else
  begin
    Canvas.Pen.Color := DisabledColor;
    Canvas.Line(0, Height - 1, Width, Height - 1);
  end;
end;

constructor TBCMaterialFloatSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.BevelOuter := bvNone;
  Self.Color := clWhite;
  AccentColor := clHighlight;
  DisabledColor := $00B8AFA8;
  Flbl := TLabel.Create(Self);
  Flbl.Align := alTop;
  Flbl.Caption := 'Buscar';
  Flbl.BorderSpacing.Around := 4;
  Flbl.Font.Style := [fsBold];
  Flbl.Font.Color := $00B8AFA8;
  Flbl.Parent := Self;
  Fedt := TFloatSpinEdit.Create(Self);
  Fedt.Color := Color;
  Fedt.Font.Color := clBlack;
  Fedt.OnEnter := @EnterEdit;
  Fedt.OnExit := @ExitEdit;
  Fedt.OnChange:=@ChangeEdit;
  Fedt.Align := alClient;
  Fedt.BorderStyle := bsNone;
  //Fedt.TextHint := 'Buscar';
  Fedt.BorderSpacing.Around := 4;
  Fedt.Parent := Self;
  Fedt.MinValue := 0;
  Fedt.MaxValue := MaxInt;
end;

end.
