unit umain;

{$mode objfpc}{$H+}

interface

uses
  Forms, Controls, Graphics, ExtCtrls, ComCtrls, Types,
  { BGRABitmap }
  BGRABitmap, BGRABitmapTypes,
  { BGRAControls }
  BGRACustomDrawn, BCPanel, BCToolBar, Classes;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    BCDButton1: TBCDButton;
    BCDButton2: TBCDButton;
    BCDCheckBox3: TBCDCheckBox;
    BCDCheckBox4: TBCDCheckBox;
    BCDEdit1: TBCDEdit;
    BCDEdit2: TBCDEdit;
    BCDProgressBar1: TBCDProgressBar;
    BCDRadioButton1: TBCDRadioButton;
    BCDRadioButton2: TBCDRadioButton;
    BCDRadioButton3: TBCDRadioButton;
    BCDRadioButton4: TBCDRadioButton;
    BCDSpinEdit1: TBCDSpinEdit;
    BCDSpinEdit2: TBCDSpinEdit;
    BCDStaticText1: TBCDStaticText;
    BCDStaticText2: TBCDStaticText;
    BCPanel1: TBCPanel;
    BCToolBar1: TBCToolBar;
    BCDCheckBox1: TBCDCheckBox;
    BCDCheckBox2: TBCDCheckBox;
    Timer1: TTimer;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure BCDButton1Click(Sender: TObject);
    procedure BCToolBar1PaintButton(Sender: TToolButton; State: integer);
    procedure BCToolBar1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure AutoAdjustLayout(AMode: TLayoutAdjustmentPolicy;
      const AFromDPI, AToDPI, AOldFormWidth, ANewFormWidth: integer); override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Self.AutoAdjustLayout(lapAutoAdjustForDPI, Self.DesignTimeDPI,
    Screen.PixelsPerInch, Self.Width, ScaleX(Self.Width, Self.DesignTimeDPI));
end;

procedure TfrmMain.BCToolBar1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  Bitmap.Rectangle(0, 0, Bitmap.Width, Bitmap.Height, BGRA(83, 83, 83),
    BGRA(83, 83, 83), dmSet);
  Bitmap.SetHorizLine(0, Bitmap.Height - 2, Bitmap.Width - 1, BGRA(106, 106, 106));
  Bitmap.SetHorizLine(0, Bitmap.Height - 1, Bitmap.Width - 1, BGRA(40, 40, 40));
end;

procedure TfrmMain.BCToolBar1PaintButton(Sender: TToolButton; State: integer);
var
  Bitmap: TBGRABitmap;
  ts: TSize;
begin
  Bitmap := TBGRABitmap.Create(Sender.Width, Sender.Height);

  if Sender.Style = tbsButton then
  begin
    if Sender.Enabled then
    begin
      if State = 3 then
      begin
        { Button Down }
        Bitmap.Rectangle(0, 0, Sender.Width, Sender.Height - 1, BGRA(48, 48, 48),
          BGRA(61, 61, 61), dmSet);
        Bitmap.Rectangle(1, 1, Sender.Width - 1, Sender.Height - 2, BGRA(55, 55, 55),
          BGRA(61, 61, 61), dmSet);
        Bitmap.SetHorizLine(0, Sender.Height - 1, Sender.Width - 1, BGRA(83, 83, 83));
      end
      else
      begin
        if State = 2 then
        begin
          { Button Hovered }
          Bitmap.GradientFill(0, 0, Sender.Width, Sender.Height, BGRA(132, 132, 132),
            BGRA(109, 109, 109), gtLinear, PointF(0, 0),
            PointF(0, Sender.Height), dmSet);
          Bitmap.Rectangle(0, 0, Sender.Width, Sender.Height - 1,
            BGRA(48, 48, 48), dmSet);
          Bitmap.SetHorizLine(1, 1, Sender.Width - 2, BGRA(160, 160, 160));
          Bitmap.SetHorizLine(0, Sender.Height - 1, Sender.Width - 1, BGRA(83, 83, 83));
        end
        else
          { Button Normal }
          Bitmap.Fill(BGRA(83, 83, 83));
      end;
    end
    else
    begin
      { Button Disabled }
      Bitmap.Rectangle(0, 0, Sender.Width, Sender.Height - 1, BGRA(66, 66, 66),
        BGRA(71, 71, 71), dmSet);
      Bitmap.SetHorizLine(0, Sender.Height - 1, Sender.Width - 1, BGRA(83, 83, 83));
    end;

    Bitmap.FontName := Sender.Font.Name;
    Bitmap.FontStyle := Sender.Font.Style;
    Bitmap.FontHeight := Sender.Font.Height;
    Bitmap.FontQuality := fqSystemClearType;
    ts := Bitmap.TextSize(Sender.Caption);

    if Sender.Enabled then
    begin
      { Text Enabled }
      Bitmap.TextOut((Sender.Width - ts.cx) div 2, ((Sender.Height - ts.cy) div 2) -
        1, Sender.Caption, BGRA(47, 47, 47));
      Bitmap.TextOut((Sender.Width - ts.cx) div 2, (Sender.Height - ts.cy) div 2,
        Sender.Caption, BGRA(229, 229, 229));
    end
    else
      { Text Disabled }
      Bitmap.TextOut((Sender.Width - ts.cx) div 2, (Sender.Height - ts.cy) div 2,
        Sender.Caption, BGRA(170, 170, 170));
  end;

  Bitmap.Draw(Sender.Canvas, 0, 0, True);
  Bitmap.Free;
end;

procedure TfrmMain.BCDButton1Click(Sender: TObject);
begin
  BCDButton2.Enabled := not BCDButton2.Enabled;
  BCDEdit2.Enabled := not BCDEdit2.Enabled;
  BCDSpinEdit2.Enabled := not BCDSpinEdit2.Enabled;
  BCDStaticText2.Enabled := not BCDStaticText2.Enabled;
  BCDCheckBox3.Enabled := not BCDCheckBox3.Enabled;
  BCDCheckBox4.Enabled := not BCDCheckBox4.Enabled;
  BCDRadioButton3.Enabled := not BCDRadioButton3.Enabled;
  BCDRadioButton4.Enabled := not BCDRadioButton4.Enabled;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  if BCDProgressBar1.Position <> BCDProgressBar1.Max then
    BCDProgressBar1.Position := BCDProgressBar1.Position + 1
  else
    Timer1.Enabled := False;
end;

procedure TfrmMain.AutoAdjustLayout(AMode: TLayoutAdjustmentPolicy;
  const AFromDPI, AToDPI, AOldFormWidth, ANewFormWidth: integer);
begin
  inherited AutoAdjustLayout(AMode, AFromDPI, AToDPI, AOldFormWidth,
    ANewFormWidth);

  BCToolBar1.ButtonWidth := ScaleX(BCToolBar1.ButtonWidth, Self.DesignTimeDPI);
  BCToolBar1.ButtonHeight := ScaleY(BCToolBar1.ButtonHeight, Self.DesignTimeDPI);
  BCToolBar1.Height := BCToolBar1.ButtonHeight + 4;
end;

end.
