{
  Material Design color pallete from
  https://material.google.com/style/color.html#color-color-palette
}

unit MaterialColors;

{$mode objfpc}{$H+}{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, FGL;

type

  { TMaterialColors }

  TMaterialColors = record
    M50: TBGRAPixel;
    M100: TBGRAPixel;
    M200: TBGRAPixel;
    M300: TBGRAPixel;
    M400: TBGRAPixel;
    M500: TBGRAPixel;
    M600: TBGRAPixel;
    M700: TBGRAPixel;
    M800: TBGRAPixel;
    M900: TBGRAPixel;
    A100: TBGRAPixel;
    A200: TBGRAPixel;
    A400: TBGRAPixel;
    A700: TBGRAPixel;
    procedure Create(aM50, aM100, aM200, aM300, aM400, aM500, aM600,
        aM700, aM800, aM900, aA100, aA200, aA400, aA700: string);
  end;

  TMaterialColorsList = specialize TFPGMap<string, TMaterialColors>;

procedure MaterialColorsListStr(AList: TStrings);

var
  MaterialBlack: TBGRAPixel;
  MaterialWhite: TBGRAPixel;
  MaterialRed, MaterialPink, MaterialPurple, MaterialDeepPurple,
  MaterialIndigo, MaterialBlue, MaterialLightBlue, MaterialCyan,
  MaterialTeal, MaterialGreen, MaterialLightGreen, MaterialLime,
  MaterialYellow, MaterialAmber, MaterialOrange, MaterialDeepOrange,
  MaterialBrown, MaterialGrey, MaterialBlueGrey: TMaterialColors;
  MaterialColorsList: TMaterialColorsList;

implementation

procedure MaterialColorsListStr(AList: TStrings);
begin
  with AList do
  begin
    Add('Red');
    Add('Pink');
    Add('Purple');
    Add('Deep Purple');
    Add('Indigo');
    Add('Blue');
    Add('Light Blue');
    Add('Cyan');
    Add('Teal');
    Add('Green');
    Add('Light Green');
    Add('Lime');
    Add('Yellow');
    Add('Amber');
    Add('Orange');
    Add('Deep Orange');
    Add('Brown');
    Add('Grey');
    Add('Blue Grey');
  end;
end;

{ TMaterialColors }

procedure TMaterialColors.Create(aM50, aM100, aM200, aM300, aM400,
  aM500, aM600, aM700, aM800, aM900, aA100, aA200, aA400, aA700: string);
begin
  self.M50 := StrToBGRA(aM50);
  self.M100 := StrToBGRA(aM100);
  self.M200 := StrToBGRA(aM200);
  self.M300 := StrToBGRA(aM300);
  self.M400 := StrToBGRA(aM400);
  self.M500 := StrToBGRA(aM500);
  self.M600 := StrToBGRA(aM600);
  self.M700 := StrToBGRA(aM700);
  self.M800 := StrToBGRA(aM800);
  self.M900 := StrToBGRA(aM900);
  self.A100 := StrToBGRA(aA100);
  self.A200 := StrToBGRA(aA200);
  self.A400 := StrToBGRA(aA400);
  self.A700 := StrToBGRA(aA700);
end;

initialization

  MaterialBlack := BGRABlack;
  MaterialWhite := BGRAWhite;

  MaterialRed.Create('#FFEBEE', '#FFCDD2', '#EF9A9A', '#E57373', '#EF5350',
    '#F44336', '#E53935', '#D32F2F', '#C62828', '#B71C1C', '#FF8A80',
    '#FF5252', '#FF1744', '#D50000');
  MaterialPink.Create('#FCE4EC', '#F8BBD0', '#F48FB1', '#F06292',
    '#EC407A', '#E91E63', '#D81B60', '#C2185B', '#AD1457', '#880E4F',
    '#FF80AB', '#FF4081', '#F50057', '#C51162');
  MaterialPurple.Create('#F3E5F5', '#E1BEE7', '#CE93D8', '#BA68C8', '#AB47BC',
    '#9C27B0', '#8E24AA', '#7B1FA2', '#6A1B9A', '#4A148C',
    '#EA80FC', '#E040FB', '#D500F9', '#AA00FF');
  MaterialDeepPurple.Create('#EDE7F6', '#D1C4E9', '#B39DDB', '#9575CD', '#7E57C2',
    '#673AB7', '#5E35B1', '#512DA8', '#4527A0',
    '#311B92', '#B388FF', '#7C4DFF', '#651FFF', '#6200EA');
  MaterialIndigo.Create('#E8EAF6', '#C5CAE9', '#9FA8DA', '#7986CB', '#5C6BC0',
    '#3F51B5', '#3949AB', '#303F9F', '#283593', '#1A237E',
    '#8C9EFF', '#536DFE', '#3D5AFE', '#304FFE');
  MaterialBlue.Create('#E3F2FD', '#BBDEFB', '#90CAF9', '#64B5F6', '#42A5F5',
    '#2196F3', '#1E88E5', '#1976D2', '#1565C0', '#0D47A1',
    '#82B1FF', '#448AFF', '#2979FF', '#2962FF');
  MaterialLightBlue.Create('#E1F5FE', '#B3E5FC', '#81D4FA', '#4FC3F7', '#29B6F6',
    '#03A9F4', '#039BE5', '#0288D1', '#0277BD', '#01579B',
    '#80D8FF', '#40C4FF', '#00B0FF', '#0091EA');
  MaterialCyan.Create('#E0F7FA', '#B2EBF2', '#80DEEA', '#4DD0E1', '#26C6DA',
    '#00BCD4', '#00ACC1', '#0097A7', '#00838F', '#006064',
    '#84FFFF', '#18FFFF', '#00E5FF', '#00B8D4');
  MaterialTeal.Create('#E0F2F1', '#B2DFDB', '#80CBC4', '#4DB6AC', '#26A69A',
    '#009688', '#00897B', '#00796B', '#00695C', '#004D40',
    '#A7FFEB', '#64FFDA', '#1DE9B6', '#00BFA5');
  MaterialGreen.Create('#E8F5E9', '#C8E6C9', '#A5D6A7', '#81C784', '#66BB6A',
    '#4CAF50', '#43A047', '#388E3C', '#2E7D32', '#1B5E20',
    '#B9F6CA', '#69F0AE', '#00E676', '#00C853');
  MaterialLightGreen.Create('#F1F8E9', '#DCEDC8', '#C5E1A5', '#AED581', '#9CCC65',
    '#8BC34A', '#7CB342', '#689F38', '#558B2F', '#33691E',
    '#CCFF90', '#B2FF59', '#76FF03', '#64DD17');
  MaterialLime.Create('#F9FBE7', '#F0F4C3', '#E6EE9C', '#DCE775', '#D4E157',
    '#CDDC39', '#C0CA33', '#AFB42B', '#9E9D24', '#827717',
    '#F4FF81', '#EEFF41', '#C6FF00', '#AEEA00');
  MaterialYellow.Create('#FFFDE7', '#FFF9C4', '#FFF59D', '#FFF176', '#FFEE58',
    '#FFEB3B', '#FDD835', '#FBC02D', '#F9A825', '#F57F17',
    '#FFFF8D', '#FFFF00', '#FFEA00', '#FFD600');
  MaterialAmber.Create('#FFF8E1', '#FFECB3', '#FFE082', '#FFD54F', '#FFCA28',
    '#FFC107', '#FFB300', '#FFA000', '#FF8F00', '#FF6F00',
    '#FFE57F', '#FFD740', '#FFC400', '#FFAB00');
  MaterialOrange.Create('#FFF3E0', '#FFE0B2', '#FFCC80', '#FFB74D', '#FFA726',
    '#FF9800', '#FB8C00', '#F57C00', '#EF6C00', '#E65100',
    '#FFD180', '#FFAB40', '#FF9100', '#FF6D00');
  MaterialDeepOrange.Create('#FBE9E7', '#FFCCBC', '#FFAB91', '#FF8A65', '#FF7043',
    '#FF5722', '#F4511E', '#E64A19', '#D84315', '#BF360C',
    '#FF9E80', '#FF6E40', '#FF3D00', '#DD2C00');
  MaterialBrown.Create('#EFEBE9', '#D7CCC8', '#BCAAA4', '#A1887F', '#8D6E63',
    '#795548', '#6D4C41', '#5D4037', '#4E342E', '#3E2723',
    '', '', '', '');
  MaterialGrey.Create('#FAFAFA', '#F5F5F5', '#EEEEEE', '#E0E0E0', '#BDBDBD',
    '#9E9E9E', '#757575', '#616161', '#424242', '#212121',
    '', '', '', '');
  MaterialBlueGrey.Create('#ECEFF1', '#CFD8DC', '#B0BEC5', '#90A4AE', '#78909C',
    '#607D8B', '#546E7A', '#455A64', '#37474F', '#263238',
    '', '', '', '');

  MaterialColorsList := TMaterialColorsList.Create;
  with MaterialColorsList do
  begin
    Add('Red', MaterialRed);
    Add('Pink', MaterialPink);
    Add('Purple', MaterialPurple);
    Add('Deep Purple', MaterialDeepPurple);
    Add('Indigo', MaterialIndigo);
    Add('Blue', MaterialBlue);
    Add('Light Blue', MaterialLightBlue);
    Add('Cyan', MaterialCyan);
    Add('Teal', MaterialTeal);
    Add('Green', MaterialGreen);
    Add('Light Green', MaterialLightGreen);
    Add('Lime', MaterialLime);
    Add('Yellow', MaterialYellow);
    Add('Amber', MaterialAmber);
    Add('Orange', MaterialOrange);
    Add('Deep Orange', MaterialDeepOrange);
    Add('Brown', MaterialBrown);
    Add('Grey', MaterialGrey);
    Add('Blue Grey', MaterialBlueGrey);
  end;

finalization
  MaterialColorsList.Free;

end.
