unit utest;

{$mode objfpc}{$H+}

interface

uses
  Forms, SysUtils,
  { Custom Drawn }
  customdrawncontrols, customdrawndrawers, StdCtrls,
  { BGRABitmap }
  BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes,
  { Custom Drawn + BGRABitmap }
  customdrawn_windows7;

type

  { TfrmTest }

  TfrmTest = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    cbd_a_c: TCDCheckBox;
    cbd_a_g: TCDCheckBox;
    cbd_a_u: TCDCheckBox;
    cbd_c: TCDCheckBox;
    cbd_g: TCDCheckBox;
    cbd_u: TCDCheckBox;
    cb_a_c: TCDCheckBox;
    cb_a_g: TCDCheckBox;
    cb_a_u: TCDCheckBox;
    cb_c: TCDCheckBox;
    cb_g: TCDCheckBox;
    cb_u: TCDCheckBox;
    CDButton1: TCDButton;
    CDButton10: TCDButton;
    CDButton2: TCDButton;
    CDButton3: TCDButton;
    CDButton4: TCDButton;
    CDButton5: TCDButton;
    CDButton6: TCDButton;
    CDButton7: TCDButton;
    CDButton8: TCDButton;
    CDButton9: TCDButton;
    CDComboBox1: TCDComboBox;
    CDEdit1: TCDEdit;
    CDGroupBox1: TCDGroupBox;
    CDGroupBox2: TCDGroupBox;
    CDGroupBox3: TCDGroupBox;
    CDGroupBox4: TCDGroupBox;
    CDListView1: TCDListView;
    CDPageControl1: TCDPageControl;
    CDPageControl2: TCDPageControl;
    CDProgressBar1: TCDProgressBar;
    CDProgressBar2: TCDProgressBar;
    CDProgressBar3: TCDProgressBar;
    CDProgressBar4: TCDProgressBar;
    CDRadioButton1: TCDRadioButton;
    CDRadioButton2: TCDRadioButton;
    CDRadioButton3: TCDRadioButton;
    CDRadioButton4: TCDRadioButton;
    CDScrollBar1: TCDScrollBar;
    CDScrollBar2: TCDScrollBar;
    CDSpinEdit1: TCDSpinEdit;
    CDStaticText1: TCDStaticText;
    CDTabSheet1: TCDTabSheet;
    CDTabSheet2: TCDTabSheet;
    CDTrackBar1: TCDTrackBar;
    CDTrackBar2: TCDTrackBar;
    CDTrackBar3: TCDTrackBar;
    CDTrackBar4: TCDTrackBar;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure CDButton10Click(Sender: TObject);
    procedure CDButton1Click(Sender: TObject);
    procedure CDButton2Click(Sender: TObject);
    procedure CDButton4Click(Sender: TObject);
    procedure CDButton9Click(Sender: TObject);
    procedure CDScrollBar1Change(Sender: TObject);
    procedure CDTrackBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmTest: TfrmTest;

implementation

{$R *.lfm}

{ TfrmTest }

procedure TfrmTest.FormCreate(Sender: TObject);
begin
  { This code set the dsWindows7 style
  that is registered in the unit customdrawn_windows7 }
  DefaultStyle := dsWindows7;

  { Set the form color }
  Color := WIN7_FORM_COLOR;
  BGRAVirtualScreen1.Color := WIN7_FORM_COLOR;

end;

{ The code below is how to configure this drawer }

procedure TfrmTest.CDButton1Click(Sender: TObject);
begin
  { This code is for changing DPI }
  if win7.dpi = 96 then
    win7.dpi := 120
  else if win7.dpi = 120 then
    win7.dpi := 144
  else if win7.dpi = 144 then
    win7.dpi := 96
  else
    win7.dpi := 96;

  { This code is for scaling extra1 buttons }
  // extra1 'arrowleft' in caption
  CDButton5.Width := win7.ArrowLeft.SliceScalingArray[0].BitmapWidth;
  CDButton5.Height := win7.ArrowLeft.SliceScalingArray[0].BitmapHeight;
  // extra1 'arrowright' in caption
  CDButton6.Width := win7.ArrowRight.SliceScalingArray[0].BitmapWidth;
  CDButton6.Height := win7.ArrowRight.SliceScalingArray[0].BitmapHeight;
  // extra1 'arrow' in caption
  CDButton7.Width := win7.Arrow.SliceScalingArray[0].BitmapWidth;
  CDButton7.Height := win7.Arrow.SliceScalingArray[0].BitmapHeight;
  // extra1 'closebutton' in caption
  CDButton8.Width := win7.CloseButton.SliceScalingArray[0].BitmapWidth;
  CDButton8.Height := win7.CloseButton.SliceScalingArray[0].BitmapHeight;

  { Do changes visible }
  Invalidate;
end;

procedure TfrmTest.CDButton2Click(Sender: TObject);
begin
  { This code is for using the extra tickmark }
  win7.tickmark := not win7.tickmark;

  { Do changes visible }
  Invalidate;
end;

procedure TfrmTest.CDButton4Click(Sender: TObject);
begin
  { This code is for changing the folder }
  if win7.folder = 'aero.zip' then
    win7.folder := 'aerow8.zip'
  else if win7.folder = 'aerow8.zip' then
    win7.folder := 'luna.zip'
  else if win7.folder = 'luna.zip' then
    win7.folder := 'extra.zip'
  else if win7.folder = 'extra.zip' then
    win7.folder := 'aero.zip'
  else
    win7.folder := 'aero.zip';

  { Do changes visible }
  Invalidate;
end;

procedure TfrmTest.CDButton9Click(Sender: TObject);
begin
  { This show / hide debug lines }
  win7.debug := not win7.debug;

  { Do changes visible }
  Invalidate;
end;

{ The code below is just for testing purposes }

procedure TfrmTest.CDButton10Click(Sender: TObject);
begin
  CDButton5.Enabled := not CDButton5.Enabled;
  CDButton6.Enabled := not CDButton6.Enabled;
  CDButton7.Enabled := not CDButton7.Enabled;
  CDButton8.Enabled := not CDButton8.Enabled;
  CDScrollBar1.Enabled := not CDScrollBar1.Enabled;
  CDScrollBar2.Enabled := not CDScrollBar2.Enabled;
  CDComboBox1.Enabled := not CDComboBox1.Enabled;
  CDStaticText1.Enabled := not CDStaticText1.Enabled;
  CDSpinEdit1.Enabled := not CDSpinEdit1.Enabled;
  CDListView1.Enabled := not CDListView1.Enabled;
  CDButton3.Enabled := not CDButton3.Enabled;
  CDEdit1.Enabled := not CDEdit1.Enabled;
  CDPageControl1.Enabled := not CDPageControl1.Enabled;
  CDPageControl2.Enabled := not CDPageControl2.Enabled;
end;

procedure TfrmTest.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);

  procedure DrawTileBackground(ABitmap: TBGRABitmap; Multiply: integer);
  var
    temp: TBGRABitmap;
  begin
    temp := TBGRABitmap.Create(2, 2, BGRAWhite);
    temp.SetPixel(0, 1, BGRA(220, 220, 220));
    temp.SetPixel(1, 0, BGRA(220, 220, 220));
    BGRAReplace(temp, temp.Resample(2 * Multiply, 2 * Multiply, rmSimpleStretch));
    ABitmap.Fill(temp, dmSet);
    temp.Free;
  end;

begin
  //DrawTileBackground(Bitmap, 8);
  //DrawIOSBackground(Bitmap);
end;

procedure TfrmTest.CDScrollBar1Change(Sender: TObject);
begin
  CDProgressBar1.Position := TCDScrollBar(Sender).Position;
  CDProgressBar2.Position := TCDScrollBar(Sender).Position;
  CDProgressBar3.Position := TCDScrollBar(Sender).Position;
  CDProgressBar4.Position := TCDScrollBar(Sender).Position;
end;

procedure TfrmTest.CDTrackBar1Change(Sender: TObject);
begin
  CDProgressBar1.Position := TCDTrackBar(Sender).Position;
  CDProgressBar2.Position := TCDTrackBar(Sender).Position;
  CDProgressBar3.Position := TCDTrackBar(Sender).Position;
  CDProgressBar4.Position := TCDTrackBar(Sender).Position;
  CDTrackBar1.Position := CDProgressBar1.Position;
  CDTrackBar2.Position := CDProgressBar1.Position;
  CDTrackBar3.Position := CDProgressBar1.Position;
  CDTrackBar4.Position := CDProgressBar1.Position;
end;

end.


