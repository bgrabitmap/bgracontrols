{ A Graphic Button Control that uses SVG images as the button states
  for Normal,Hover and DOWN states.

  Copyright (C) 2018 User Josh on Lazarus Forum.

  You can use the SVGDOWNXML property to enter the SVG XML code to create the
  image or You can enter the full svg image file and pathname into the properties
  FileNameDown; it will then read in the File Information and place it in the
  SVGDownXML Property.

  This Component uses the BGRABITMAP and BGRACONTROLS Framework to implement
  the Button's Functionality

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit BCSVGButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, BCSVGViewer,LResources,lazutils;

type

  SVGButtonState = (MouseIn, MouseOut, Pressed);

  TBCSVGButton = class(TBCSVGViewer)
  private
    fsvgnormal:tstrings;
    fsvghover:tstrings;
    fsvgdown:tstrings;
    fdown:boolean;
    FState:SVGButtonState;
    FOwner: TComponent;
    FFileNameHover: String;
    FFileNameNormal: String;
    FFileNameDown: String;
    FPosition: Integer;
    FMax: Integer;
    FInfo1: String;
    FInfo2: String;
  //  property OnPositionChange;
    procedure setdown(AValue: boolean);
    procedure ReadSVGFileAndSetString(fn:String;itm:Integer);
    procedure GenerateCompletedSVGImage(AValue: string);
  protected
    FOnPositionChange: TNotifyEvent;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      MX, MY: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; MX, MY: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure setsvghoverxml(const AValue: tstrings);
    procedure setsvgnormalxml(const AValue: tstrings);
    procedure setsvgdownxml(const AValue: tstrings);
    procedure setFFileNameDown(const AValue: string);
    procedure setFFileNameHover(const AValue: string);
    procedure setFFileNameNormal(const AValue: string);
    procedure SetInfo1(const AValue:String);
    procedure SetInfo2(const AValue:String);
    procedure Setposition(const AValue:Integer);
    procedure SetMax(const AValue:Integer);
    procedure RedrawBitmapContent; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure paint; override;
  published
    Property FileNameDown : String Read FFileNameDown Write setFFileNameDown;
    Property FileNameHover : String Read FFileNameHover Write setFFileNameHover;
    Property FileNameNormal : String Read FFileNameNormal Write setFFileNameNormal;
    property SVGNormalXML:tstrings read fsvgnormal write setsvgnormalxml;
    property SVGHoverXML:tstrings read fsvghover write setsvghoverxml;
    property SVGDownXML:tstrings read fsvgdown write setsvgdownxml;
    property Down:boolean read fdown write setdown default false;
    property Information1:string read FInfo1 write SetInfo1;
    property Information2:string read FInfo2 write SetInfo2;
    property Position:integer read fposition write SetPosition;
    property Maximum:integer read fmax write SetMax;
    property OnPositionChange: TNotifyEvent read FOnPositionChange write FOnPositionChange;

  end;

procedure Register;

implementation

procedure TBCSVGButton.Paint;
begin
  inherited Paint;
end;

constructor TBCSVGButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
  fsvgnormal :=  TStringList.Create;
  fsvgnormal.Add('<?xml version="1.0" encoding="UTF-8" standalone="no"?><svg xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:cc="http://creativecommons.org/ns#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:svg="http://www.w3.org/2000/svg" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" id="svg2" viewBox="0 0 256 256" height="256" width="256"><defs id="defs4"><linearGradient id="linearGradient33100"><stop style="stop-color:#5888d3;stop-opacity:1" offset="0" id="stop33102" /><stop style="stop-color:#ffffff;stop-opacity:1" offset="1" id="stop33104" /></linearGradient><linearGradient id="linearGradient32954"><stop id="stop32956" offset="0" style="stop-color:#a9c2e9;stop-opacity:1" /><stop id="stop32958" offset="1" style="stop-color:#6e98d9;stop-opacity:0" /></linearGradient><radialGradient gradientUnits="userSpaceOnUse" gradientTransform="matrix(0.43020472,1.0087229,-1.3026574,0.55556329,407.35604,-355.15804)" r="136.11285" fy="252.45506" fx="492.90915" cy="252.45506" cx="492.90915" id="radialGradient32960" xlink:href="#linearGradient33100" /><radialGradient r="136.11285" fy="256.02982" fx="493.7803" cy="256.02982" cx="493.7803" gradientTransform="matrix(0.176207,-0.98435313,1.4397582,0.25772811,-170.13397,703.38364)" gradientUnits="userSpaceOnUse" id="radialGradient33090" xlink:href="#linearGradient32954" /></defs><metadata id="metadata7"><rdf:RDF><cc:Work rdf:about=""><dc:format>image/svg+xml</dc:format><dc:type rdf:resource="http://purl.org/dc/dcmitype/StillImage" /><dc:title></dc:title></cc:Work></rdf:RDF></metadata><g transform="translate(22.3428,-828.73785)" id="layer1"><g transform="matrix(0.81004176,0,0,0.81004176,-126.53159,724.48161)" id="g33106"><path style="opacity:1;fill:#787878;fill-opacity:1;stroke:none;stroke-width:7;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1" d="m 302.53806,149.63097 c 6.25469,0.0842 11.86303,1.35643 17.18164,3.02344 6.93279,2.17295 14.83357,6.08052 14.36719,17.98047 -1.27819,32.61398 23.10806,43.50122 46.64062,50.90039 11.19738,3.52071 26.30827,-8.87017 37.08454,-5.44029 12.89135,4.10307 15.69011,21.41531 20.12891,33.35491 2.64996,7.12795 -5.09188,16.0695 -11.16657,18.72796 -26.42574,11.56468 -29.98438,37.39026 -18.61496,62.05943 4.59111,9.96168 14.78447,15.80179 18.53516,25.3697 3.52351,8.98836 -6.01863,23.08073 -11.33622,30.51032 -5.0328,5.93692 -16.16861,17.10298 -24.76758,11.81836 -28.00539,-17.21112 -50.11272,-10.38901 -70.8291,8.48672 -7.37915,6.72351 -7.011,15.55432 -10.71387,28.31016 -1.67807,5.7807 -7.00484,8.41883 -12.42246,9.19062 -11.36714,1.61935 -24.08831,1.12785 -36.10766,-4.2668 -6.52006,-2.9264 -13.58092,-9.23237 -13.21847,-15.10239 1.70363,-27.59117 -19.52246,-48.3543 -44.44115,-52.59474 -9.70834,-1.65208 -21.79969,8.66252 -30.94752,5.9569 -10.24192,-3.02923 -13.9108,-14.70954 -17.03265,-23.48828 -2.36153,-6.64069 -5.99624,-19.05949 1.84375,-22.99609 25.25922,-12.68318 35.48899,-41.03813 30.03321,-61.75893 -4.46638,-16.96309 -15.5773,-23.40024 -18.31,-26.15347 -5.05904,-5.09705 -6.24524,-11.18085 -3.94977,-17.45479 3.83592,-10.48429 10.15916,-19.72865 18.71094,-27.17969 5.44765,-4.74644 14.30066,-3.18606 19.6582,0.77148 25.05079,18.50463 69.75358,0.25355 75.30859,-30.70703 2.18203,-12.1614 3.95511,-16.10791 12.33008,-18.01367 4.29575,-0.97752 8.28234,-1.35523 12.03515,-1.30469 z" id="path32877-6" /><path style="opacity:1;fill:#264769;fill-opacity:1;stroke:none;stroke-width:7;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1" d="m 288.4894,128.7048 c -41.44264,-0.035 -8.79945,31.42306 -48.31055,51.24414 -45.86653,23.00934 -47.15906,-35.3288 -82.51758,13.125 -36.37323,49.84431 19.91339,33.25642 14.10743,78.65039 -6.86179,53.649 -56.12207,22.73363 -28.76563,78.09375 27.06104,54.76233 34.31821,-2.74181 76.30469,26.45117 43.66879,30.36268 -11.29747,55.19521 50.74805,61.99414 61.14363,6.70011 19.59248,-30.50298 62.64453,-50.59375 48.07345,-22.43407 46.68137,37.20731 82.54882,-13.77734 35.39947,-50.31943 -18.19229,-26.95692 -14.6875,-79.00782 3.33203,-49.48516 54.63695,-20.2052 28.40821,-76.84375 -23.80472,-51.4041 -37.31346,0.53372 -75.87305,-26.33203 -41.14069,-28.66409 9.70026,-55.88176 -52.38672,-62.29101 -4.66524,-0.4816 -8.70861,-0.70993 -12.2207,-0.71289 z" id="path33060" /><path style="opacity:1;fill:#ffffff;fill-opacity:1;stroke:none;stroke-width:7;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1" d="m 293.95815,135.97823 c 6.25469,0.0842 11.86303,1.35643 17.18164,3.02344 6.93279,2.17295 14.83357,6.08052 14.36719,17.98047 -1.27819,32.61398 31.7155,49.47578 45.63047,50.90039 16.00769,1.63886 25.53271,-7.4416 36.30898,-4.01172 12.89135,4.10307 15.69011,20.70103 20.12891,32.64063 2.64996,7.12795 -3.30617,15.35521 -9.38086,18.01367 -26.42574,11.56468 -32.12724,43.46169 -20.75782,68.13086 4.59111,9.96168 14.78447,14.01607 18.53516,23.58398 3.52351,8.98836 -3.87577,18.79502 -9.19336,26.22461 -5.0328,5.93692 -16.16861,17.10298 -24.76758,11.81836 -28.00539,-17.21112 -50.11272,-10.38901 -70.8291,8.48672 -7.37915,6.72351 -7.011,15.55432 -10.71387,28.31016 -1.67807,5.7807 -7.00484,8.41883 -12.42246,9.19062 -11.36715,1.61935 -22.65975,1.12785 -34.6791,-4.2668 -6.52006,-2.9264 -8.22378,-10.3038 -7.86133,-16.17382 1.70363,-27.59117 -23.80817,-45.49716 -48.72686,-49.7376 -9.70834,-1.65208 -20.01398,6.87681 -29.16181,4.17119 -10.24192,-3.02923 -18.19651,-14.70954 -21.31836,-23.48828 -2.36153,-6.64069 -5.99624,-19.05949 1.84375,-22.99609 25.25922,-12.68318 35.48899,-41.03813 30.03321,-61.75893 -4.46638,-16.96309 -15.5773,-23.40024 -18.31,-26.15347 -5.05904,-5.09705 -6.24524,-11.18085 -3.94977,-17.45479 3.83592,-10.48429 10.15916,-19.72865 18.71094,-27.17969 5.44765,-4.74644 14.30066,-3.18606 19.6582,0.77148 25.05079,18.50463 69.75358,0.25355 75.30859,-30.70703 2.18203,-12.1614 3.95511,-16.10791 12.33008,-18.01367 4.29576,-0.97752 8.28235,-1.35523 12.03516,-1.30469 z" id="path32877" /><path style="opacity:1;fill:url(#radialGradient32960);fill-opacity:1;stroke:none;stroke-width:7;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1" d="m 418.39844,228.06055 -21.64258,15.75781 c -1.68438,1.22636 -3.44292,2.31313 -5.26758,3.2793 -1.82465,0.96616 -3.7166,1.81063 -5.66406,2.55078 -1.94747,0.74015 -3.9511,1.37746 -6.00391,1.92578 -2.0528,0.54832 -4.15424,1.00776 -6.29492,1.39844 -4.28136,0.78135 -8.72,1.28281 -13.24805,1.63867 -4.52804,0.35585 -9.14626,0.56767 -13.78125,0.77148 -4.63498,0.20382 -9.28844,0.39937 -13.89062,0.72461 -4.60218,0.32524 -9.15241,0.77988 -13.58203,1.5 -2.21482,0.36006 -4.39945,0.78682 -6.54492,1.29688 -2.14548,0.51005 -4.25189,1.10264 -6.31055,1.79687 -2.05867,0.69423 -4.06905,1.48976 -6.02344,2.40235 -1.95439,0.91258 -3.8529,1.94229 -5.68555,3.10742 -1.74401,1.10878 -3.38132,2.34412 -4.92968,3.68554 -1.54837,1.34143 -3.00778,2.7883 -4.39063,4.32227 -1.38284,1.53397 -2.69005,3.1554 -3.9375,4.8418 -1.24745,1.6864 -2.43398,3.4376 -3.57617,5.23633 -2.28438,3.59745 -4.3895,7.37942 -6.43359,11.18554 -2.0441,3.80613 -4.02743,7.63809 -6.07227,11.33203 -2.04484,3.69395 -4.15089,7.2508 -6.4375,10.51172 -1.1433,1.63046 -2.33114,3.1864 -3.58008,4.64844 -1.24894,1.46204 -2.55865,2.83046 -3.94336,4.08398 -1.3847,1.25352 -2.84589,2.3916 -4.39648,3.39649 -1.5506,1.00489 -3.19089,1.87564 -4.9375,2.5918 -1.74662,0.71615 -3.5995,1.2787 -5.57227,1.66601 -1.97276,0.38731 -4.06588,0.60078 -6.29492,0.61914 -1.72197,0.0142 -3.53595,0.20039 -5.42578,0.53906 -1.88983,0.33867 -3.85404,0.82945 -5.875,1.44922 -2.02096,0.61977 -4.09949,1.36908 -6.21484,2.22657 -2.11536,0.85748 -4.26839,1.82317 -6.44141,2.875 -4.34605,2.10364 -8.77063,4.55314 -13.12695,7.17382 -3.32,1.99725 -6.51582,4.132 -9.69141,6.25196 18.1271,-5.92018 37.76161,4.94633 51.65625,17.03906 11.33352,9.86375 17.86693,23.92223 16.62891,39.02148 -0.46311,5.64825 4.52238,10.30014 9.97265,11.5625 10.56799,2.4477 25.52882,6.47695 31.67383,-2.85351 5.72566,-8.69374 2.03596,-19.83286 13.97852,-31.43946 16.46278,-15.99966 38.89779,-27.1805 63.5,-17.23828 6.17011,2.49346 12.38557,8.86345 19.02343,6.87891 9.85288,-2.94574 15.96302,-13.08166 20.11133,-22 3.03589,-6.52676 -1.61574,-13.22925 -6.85937,-17.63281 -18.48175,-15.52084 -21.21093,-44.76288 -11.12891,-66.79688 4.83363,-10.56376 14.14755,-18.21905 24.4961,-23.27539 5.63724,-2.75438 6.6455,-9.72555 3.92968,-15.66992 -0.58978,-1.29092 -1.18062,-2.87761 -1.76562,-4.38281 z" id="path33088" /><path style="opacity:1;fill:url(#radialGradient33090);fill-opacity:1;stroke:none;stroke-width:7;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1" d="m 288.375,141.16016 c -6.46238,-0.0631 -11.06569,4.84019 -11.9082,12.02929 -4.1113,35.08163 -51.67578,59.94587 -80.57813,40 -4.78551,-3.30254 -10.58975,-7.47031 -16.60547,-3.7207 -7.53578,4.69707 -11.63701,12.522 -16.1914,19.85937 -4.8151,7.7574 -4.02472,12.14429 4.44336,18.80469 12.05643,9.48275 18.77127,26.19227 17.60937,41.97266 -2.24528,30.49423 -9.56954,36.4356 -33.55273,53.57031 -6.2013,4.43049 2.09404,18.68947 7.18945,26.37695 4.55744,6.87583 13.92777,8.81795 22.03906,5.47071 0.77861,-0.32131 1.57642,-0.41944 2.36133,-0.67578 3.17559,-2.11996 6.37141,-4.25471 9.69141,-6.25196 4.35632,-2.62068 8.7809,-5.07018 13.12695,-7.17382 2.17302,-1.05183 4.32605,-2.01752 6.44141,-2.875 2.11535,-0.85749 4.19388,-1.6068 6.21484,-2.22657 2.02096,-0.61977 3.98517,-1.11055 5.875,-1.44922 1.88983,-0.33867 3.70381,-0.52487 5.42578,-0.53906 2.22904,-0.0184 4.32216,-0.23183 6.29492,-0.61914 1.97277,-0.38731 3.82565,-0.94986 5.57227,-1.66601 1.74661,-0.71616 3.3869,-1.58691 4.9375,-2.5918 1.55059,-1.00489 3.01178,-2.14297 4.39648,-3.39649 1.38471,-1.25352 2.69442,-2.62194 3.94336,-4.08398 1.24894,-1.46204 2.43678,-3.01798 3.58008,-4.64844 2.28661,-3.26092 4.39266,-6.81777 6.4375,-10.51172 2.04484,-3.69394 4.02817,-7.5259 6.07227,-11.33203 2.04409,-3.80612 4.14921,-7.58809 6.43359,-11.18554 1.14219,-1.79873 2.32872,-3.54993 3.57617,-5.23633 1.24745,-1.6864 2.55466,-3.30783 3.9375,-4.8418 1.38285,-1.53397 2.84226,-2.98084 4.39063,-4.32227 1.54836,-1.34142 3.18567,-2.57676 4.92968,-3.68554 1.83265,-1.16513 3.73116,-2.19484 5.68555,-3.10742 1.95439,-0.91259 3.96477,-1.70812 6.02344,-2.40235 2.05866,-0.69423 4.16507,-1.28682 6.31055,-1.79687 2.14547,-0.51006 4.3301,-0.93682 6.54492,-1.29688 4.42962,-0.72012 8.97985,-1.17476 13.58203,-1.5 4.60218,-0.32524 9.25564,-0.52079 13.89062,-0.72461 4.63499,-0.20381 9.25321,-0.41563 13.78125,-0.77148 4.52805,-0.35586 8.96669,-0.85732 13.24805,-1.63867 2.14068,-0.39068 4.24212,-0.85012 6.29492,-1.39844 2.05281,-0.54832 4.05644,-1.18563 6.00391,-1.92578 1.94746,-0.74015 3.83941,-1.58462 5.66406,-2.55078 1.82466,-0.96617 3.5832,-2.05294 5.26758,-3.2793 l 21.64258,-15.75781 c -3.25317,-8.37034 -6.87832,-18.78138 -16.0586,-19.08399 -9.25314,-0.305 -17.20047,7.00655 -31.19922,5.13867 -15.76886,-2.10406 -36.33689,-15.31395 -45.75,-31.25976 -5.07506,-8.59716 -5.45982,-18.68922 -5.72656,-28.42969 -0.21535,-7.86385 -8.91018,-11.28757 -15.9375,-12.1875 -5.13553,-0.65766 -10.19168,-1.02774 -15.35156,-1.07812 z" id="path32877-5-9" /><path style="fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" d="m 271.69643,303.38899 c 6.43057,0.31186 6.30992,3.70513 18.70535,3.92857 7.02812,0.1267 14.10004,-4.58309 20.84822,-3.16964 4.93244,1.03313 4.95896,4.32362 8.03571,5.08928 0.75313,0.18742 1.67397,0.24935 2.32143,-0.17857 0.69963,-0.46241 0.60092,-2.85658 1.16072,-2.23214 1.5155,1.6905 0.51799,4.62359 0.0893,6.87499 -0.46936,2.46491 -1.30281,4.99449 -2.85714,6.9643 -2.27175,2.879 -6.13478,4.0528 -8.92857,6.42857 -3.07813,2.61756 -5.54089,5.89891 -8.57143,8.57143 -1.89317,1.66951 -4.30042,2.76053 -5.98214,4.64285 -0.95221,1.06578 -2.34001,2.2356 -2.23214,3.66072 0.0733,0.96894 2.41529,1.42449 1.87499,2.23214 -0.71177,1.06399 -2.49331,-1.68592 -3.66071,-1.16072 -0.80194,0.36079 -1.29907,1.56133 -1.07143,2.41072 0.74777,2.79018 3.27554,-0.36959 3.48214,0.80357 0.22764,1.29267 -2.00217,2.31373 -3.30356,2.14286 -1.44311,-0.18949 -1.94973,-2.17356 -2.9257,-3.24136 -0.70889,-0.7756 -0.96141,0.33005 -0.96141,0.33005 -0.094,0.96133 1.64856,2.09135 0.93526,2.74264 -0.5462,0.49871 -1.45234,-0.43745 -2.02649,-0.90369 -0.63076,-0.51221 -1.36908,-1.16477 -1.42857,-1.97512 -0.0783,-1.06621 1.47487,-1.77631 1.47993,-2.84538 0.006,-1.22102 -0.89169,-2.3162 -1.65944,-3.26566 -0.56687,-0.70104 -1.35015,-1.20431 -2.10138,-1.70277 -2.28537,-1.51642 -5.18222,-2.08382 -7.25737,-3.8772 -2.87165,-2.48171 -4.03731,-6.51933 -6.76404,-9.15945 -3.08207,-2.98416 -8.48768,-3.30662 -10.59437,-7.30742 -1.24702,-2.36819 -2.26747,-6.60091 -1.07143,-12.27679 0.34064,-1.61655 2.24979,3.92348 4.15179,3.48215 2.1938,-0.50905 6.42633,-7.1974 10.3125,-7.00893 z" id="path32029" /><path style="fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" d="m 225.22015,211.08287 c -0.57285,0.39256 -0.53269,1.73203 -1.2182,1.84291 -3.25454,0.5264 -13.46027,-0.40138 -18.3231,0.55162 -6.46924,1.26782 -14.35901,2.28119 -15.85994,3.75726 0,0 -1.96979,5.17764 -2.4361,6.5505 -0.29918,0.88081 2.87255,-0.01 3.76144,-0.28376 1.77135,-0.54648 2.15258,-2.22527 3.93583,-2.73159 1.79766,-0.51041 3.91281,-1.22303 5.28683,0.0435 2.65852,2.45053 3.85869,7.48218 6.96707,10.29408 3.69587,3.34336 7.06713,5.85563 11.512,8.10966 4.46115,2.26228 10.90095,2.28373 14.49972,3.75948 0.73492,0.30137 0.64601,2.02842 1.44029,2.02149 0.98173,-0.009 0.42398,-2.16463 1.33706,-2.52539 0.5698,-0.22513 1.25477,0.16865 1.76757,0.5039 2.10214,1.37428 4.42989,3.2983 4.3524,5.8086 -0.27537,8.92135 0.70967,18.90607 -0.0594,21.9707 -0.83498,3.32711 -5.47535,12.95979 -9.09179,19.19336 -2.50073,4.31044 -4.62084,5.08172 -7.66128,10.79771 -0.7291,1.3707 -4.45758,2.17047 -4.45758,2.17047 -1.51561,0.56489 1.66262,1.24338 1.82477,2.85269 0.35037,3.47729 -1.15492,6.03248 -1.78125,9.32785 -0.67754,3.56478 -0.98867,2.66667 -0.93638,11.39508 0.007,1.09976 2.41601,-2.29361 5.95508,-1.76757 1.66737,0.24784 2.32759,-3.08812 2.12695,-5.05079 -0.25408,-2.48543 0.8882,-4.2578 3.32672,-7.29465 0.85808,-1.06861 0.0461,-2.12541 0.84711,-3.23743 0.50642,-0.70303 1.43321,-1.02292 1.46791,-1.88867 0.0364,-0.90914 -1.85243,-0.25418 -1.73577,-1.15654 0.35892,-2.77624 0.30178,-4.79085 1.3351,-6.71511 3.44478,-6.41493 10.14183,-13.46706 13.38477,-21.46485 1.34274,-3.31148 1.96302,-8.4225 2.72064,-10.19026 0.75761,-1.76777 -0.56062,-13.39659 -1.26445,-19.95176 -0.79695,-7.42252 0.0185,-15.3008 -2.77734,-22.22266 -1.26113,-3.12224 -3.85638,-5.20165 -6.50698,-7.27846 -2.87992,-2.25649 -4.41798,-2.49006 -8.31584,-3.93583 -1.07322,-0.39806 -2.07016,-1.1519 -3.1345,-1.5731 -0.63147,-0.2499 -1.72912,-2.06636 -2.28933,-1.68247 z" id="path33094" /><path style="fill:#ffffff;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" d="m 222.73828,218.73438 c 1.83375,0.13012 3.39882,0.44843 5.11333,0.88427 2.06324,0.5245 2.90747,2.28848 1.11018,2.26631 -4.66944,-0.0576 -5.01664,-1.03415 -6.33485,-0.95681 -1.58938,0.0933 0.58729,2.39318 1.57499,3.37645 0.85406,0.85023 2.50805,1.66654 3.71301,1.6476 1.31789,-0.0207 1.54254,-0.78337 2.47465,-1.71527 0.9317,-0.93149 0.28309,-1.76821 1.18853,-2.72524 0.46634,-0.49291 1.0924,-1.07744 1.76758,-1.00977 2.67092,0.26769 4.81762,2.92354 6.06055,5.30274 0.70582,1.35107 1.79654,3.73393 0.50586,4.54492 -2.01658,1.2671 -5.75627,3.46069 -8.58789,6.31445 -0.96485,0.97239 -2.68,0.92934 -4.03906,0.75782 -4.14758,-0.52345 -7.9158,-2.94101 -11.36524,-5.30274 -2.30889,-1.58083 -4.68772,-3.26754 -5.34625,-5.98716 -0.74329,-3.06965 -1.96807,-6.69163 0.11802,-6.9841 4.16566,-0.58403 7.62385,-0.42938 12.04659,-0.41347 z" id="path32831" /><path style="fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" d="m 350.4605,215.51489 c -1.54393,-0.67663 -2.1935,0.27351 -3.78809,0.82016 -1.94396,0.66642 -2.79115,2.30053 -4.73511,2.96695 -1.59459,0.54665 -4.13926,-0.56346 -5.49272,0.44136 -1.57774,1.17132 -6.3467,5.4348 -6.97025,7.89587 -1.6929,6.68167 0.0874,14.76899 -0.4173,22.22078 -0.44947,6.63667 -0.62686,13.68259 0.69526,20.20175 1.09673,5.40779 3.07684,9.2749 5.55547,14.20475 2.36088,4.69567 4.87151,7.68392 7.63951,14.26865 2.95714,7.03466 1.48452,10.61583 2.58774,14.07944 1.13269,3.55612 3.25152,6.94436 3.22085,10.92251 -0.0145,1.88249 1.32503,1.37824 1.00976,3.53633 -0.0852,0.583 -0.5302,2.04563 -0.94682,2.46225 -1.07125,1.07125 0.55104,3.06351 -0.12627,4.41865 -0.18815,0.37643 -1.48975,0.0801 -1.70425,0.44214 -0.30339,0.51212 0.64385,1.42468 0.37745,1.95698 -0.21304,0.42567 -1.61445,0.90339 -1.19918,1.13604 2.32421,1.30212 7.67754,6.34198 8.27066,5.36685 0.64313,-1.05735 0.6794,-4.34238 1.45209,-5.68272 0.26065,-0.45212 -1.56773,-1.35017 -1.50586,-2.48821 0.0357,-0.65639 1.03288,-1.35512 1.19098,-1.99318 0.74446,-3.00447 -2.72418,-3.33114 -2.96891,-6.56563 -0.0648,-0.85607 1.85414,0.33869 2.20972,-0.44273 0.28104,-0.61762 -0.77188,-1.82657 -1.00976,-2.46206 -1.20377,-3.21591 -1.47533,-7.00712 -1.07251,-10.41723 0.1414,-1.19706 1.43588,-2.51233 1.07368,-3.66201 -0.35146,-1.1156 -2.05799,0.032 -2.58931,-1.00997 -0.7686,-1.50737 -1.04032,-3.93162 -1.51444,-5.55585 -1.93954,-6.64444 -6.70408,-11.71704 -9.28101,-18.18223 -2.00758,-5.03676 -5.67561,-9.40929 -7.07227,-14.64844 -1.52171,-5.70822 -2.37018,-11.87509 -1.26172,-17.67773 0.47332,-2.47775 1.31624,-5.23902 3.2832,-6.81836 1.14627,-0.92038 2.51018,-1.38237 3.91416,-0.94664 1.19505,0.37089 1.34183,2.37922 2.58853,2.27228 1.09336,-0.0938 0.3548,-2.22035 1.38877,-2.58795 3.60963,-1.28328 7.92584,-1.16471 11.36446,-2.52597 3.75826,-1.4878 6.81513,-3.82046 9.73934,-6.61099 2.71953,-2.5952 4.20311,-6.18952 6.9841,-8.71875 0.78024,-0.7096 1.99106,-2.01821 3.0452,-2.05135 1.39241,-0.0438 2.57355,0.61188 3.60854,1.54437 4.49588,4.05065 4.65527,3.60515 6.863,5.23047 1.07219,0.78934 1.99462,0.97064 3.07477,0.19224 1.02467,-0.73842 -0.15446,-3.63983 -1.11607,-4.4587 -2.57129,-2.18963 -2.99209,-5.70694 -5.94828,-6.21274 -4.07413,-0.69707 -9.12441,-1.48761 -14.87649,-2.94909 -3.91165,-0.99387 -8.37931,-0.50131 -11.4998,-2.03973 -0.67558,-0.33307 -0.96857,1.79542 -1.70581,1.64111 -0.64292,-0.13457 -1.73339,-1.25001 -2.33501,-1.51367 z" id="path33098" /><path style="fill:#ffffff;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" d="m 348.75586,223.02734 c 3.36588,-0.44879 5.72424,-0.5786 7.82812,0.25196 1.6067,0.63429 3.21943,2.09079 3.53516,3.78906 0.44872,2.41359 0.11237,4.28703 -2.3354,6.69209 -2.22898,2.1901 -5.63671,4.20384 -9.09101,4.98706 -3.82312,0.86685 -6.94109,0.72778 -8.61814,-0.0562 -1.63603,-0.7648 -1.91186,-2.18637 -3.54706,-3.58355 -1.90942,-1.63149 -6.3251,-4.55889 -5.44999,-5.704 1.06051,-1.38771 2.18475,0.21221 2.39893,-0.44214 0.14114,-0.43121 -0.93896,-0.68694 -0.88467,-1.1374 0.0867,-0.71907 0.74316,-1.64276 1.26367,-2.14639 0.87255,-0.84425 2.27946,-2.79492 3.15752,-1.95639 3.36014,3.20886 4.27778,5.10634 7.19599,5.1145 1.7047,0.005 2.84841,-1.06456 3.72476,-2.52676 0.52092,-0.86916 -0.1823,-3.14792 0.82212,-3.28184 z" id="path32838" /><path style="fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" d="m 287.3106,188.31666 c -0.0497,6.44431 0.53446,12.59505 -0.18755,22.53031 -0.0867,1.19327 0.3768,6.23099 0.48341,7.42264 0.40436,4.51983 0.0225,9.56045 1.57651,13.82397 0.35069,0.96217 -0.0136,-4.30465 0.20735,-9.3693 0.21568,-4.94355 0.22446,-10.00596 0.31567,-13.10904 0.0221,-0.75259 -0.0859,-3.91233 0.52488,-4.35257 0.77261,-0.55687 3.72582,-0.0269 3.04655,-0.69447 -2.10027,-2.06408 -2.87164,-2.45839 -3.42536,-4.51474 -0.99245,-3.68567 -0.56426,-7.15839 -0.96681,-11.4917 -0.83339,-8.97111 -1.52626,-6.52084 -1.57465,-0.2451 z" id="path32843" /><path style="fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" d="m 218.25762,339.54653 c 0.76163,-2.57809 1.68346,-4.73518 3.06635,-7.12119 0.8769,-1.51298 0.91001,-1.91838 4.12695,-4.39481 0.71842,-0.55305 0.38819,1.43655 0.4961,2.33675 0.14169,1.18201 1.4012,1.28137 1.76405,2.41521 0.56827,1.77569 2.47757,2.04004 3.40183,3.57143 0.34434,0.57053 -0.45635,3.08329 -0.82075,3.53181 -3.30361,4.06625 -3.06594,3.68083 -7.62441,9.21886 -0.5909,0.71788 -1.58655,-0.70804 -1.59631,-1.63778 -0.008,-0.76224 1.31703,-1.06458 1.68335,-1.96428 0.73415,-1.80312 -3.58325,0.56281 -1.74145,-1.49527 1.29243,-1.44421 0.80284,-0.82404 1.86363,-2.01209 0,0 -0.0498,-0.66107 -0.6493,-1.54495 -0.38963,-0.5745 -1.34465,1.54108 -2.03825,1.51338 -1.02328,-0.0409 -2.22194,-1.43494 -1.93179,-2.41707 z" id="path32845" /><path style="fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" d="m 224.41964,350.57648 c 1.23122,-2.31118 2.83221,-3.91985 5.13393,-6.24999 0.58898,-0.59625 2.38439,-1.54555 3.21429,-1.42858 1.64323,0.23161 2.30741,1.6833 3.83928,2.32143 1.12641,0.46924 2.35905,-0.10607 2.8125,1.02678 0.39877,0.99624 -0.54916,1.46419 -1.25,2.27679 -1.10093,1.27652 -2.35397,2.31892 -3.48214,3.57143 -1.28285,1.42425 -0.67443,2.58082 -3.30357,4.19643 -1.90681,1.17173 2.76103,-3.97305 2.14285,-5.625 -0.13916,-0.37188 -0.19719,-0.32742 -0.58035,-0.22322 -2.21433,0.60222 -2.32397,2.98849 -4.77679,4.68751 -0.93002,0.64421 -0.73049,-2.48889 -0.40177,-3.57143 0.38867,-1.27992 1.63168,-1.41794 1.33928,-2.72322 -0.0848,-0.37866 -0.23772,-0.7791 -0.625,-0.80357 -2.19014,-0.13836 -2.7599,3.54308 -4.0625,2.90179 -0.10681,-0.0526 -0.056,-0.25207 -1e-5,-0.35715 z" id="path32847" /><path style="fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" d="m 216.47611,335.84248 c -0.32667,0.24136 -0.2699,1.13947 0.12982,1.21157 0.34913,0.063 0.59799,-0.61883 0.47597,-0.95195 -0.0756,-0.20629 -0.42909,-0.39017 -0.60579,-0.25962 z" id="path32849" /><path style="fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" d="m 233.125,357.71935 c -1.01876,1.08836 1.12384,-2.78658 1.96429,-4.01786 0.84313,-1.23519 1.68913,-2.10444 3.03571,-3.30357 0.79675,-0.70951 -0.68788,2.03965 -1.25,2.94643 -1.012,1.63251 -1.52014,1.9928 -3.75,4.375 z" id="path32851" /><path style="fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" d="m 240.04464,353.12113 c -1.20273,2.05917 -2.11637,3.56934 -2.58929,5.22321 -0.34317,1.20013 -0.32753,2.22385 -1.40625,2.79018 -0.90134,0.47321 -1.22599,-0.91658 -0.82589,-1.85267 0.63033,-1.47476 1.16209,-2.77718 2.27679,-4.0625 1.01885,-1.1748 1.58342,-1.91666 2.56696,-3.05804 0.16713,-0.19395 1.11934,-0.9948 -0.0223,0.95982 z" id="path32853" /><path style="fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" d="m 238.01339,363.21041 c -0.55574,-0.41273 -0.44772,0.72286 -0.55803,1.40625 -0.0765,0.47377 -0.21021,0.74664 0.067,1.1384 0.19219,0.27164 0.35906,1.2042 0.55803,0.93749 0.29467,-0.39499 0.54553,-0.91444 0.46876,-1.69642 -0.0646,-0.65766 0.34561,-1.13118 -0.53572,-1.78572 z" id="path32855" /><path style="fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" d="m 336.37375,356.26205 c 0.0107,0.2459 0.38227,0.32766 0.61436,0.40958 0.17774,0.0627 0.44194,-0.0932 0.56316,0.0512 0.13216,0.15737 -0.12185,0.42141 -0.0512,0.61437 0.0996,0.27194 0.32715,0.65152 0.61437,0.61436 0.23512,-0.0304 0.33397,-0.37854 0.35838,-0.61436 0.0389,-0.37576 -0.0913,-0.80802 -0.35838,-1.07514 -0.30528,-0.30528 -0.8037,-0.48537 -1.22873,-0.40957 -0.21516,0.0384 -0.52147,0.19124 -0.51199,0.40959 z" id="path32857" /><path style="fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" d="m 340.25345,360.59473 c -0.15347,0.072 -0.1348,0.34319 -0.0773,0.50269 0.0526,0.14603 0.19408,0.32919 0.34802,0.30936 0.18437,-0.0238 0.35014,-0.28266 0.30935,-0.46403 -0.0495,-0.21998 -0.37592,-0.44383 -0.58003,-0.34802 z" id="path32859" /><path style="fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" d="m 338.4789,355.10441 c 1.07624,1.03389 1.36667,1.65024 2.0203,2.67332 0.72946,1.14174 2.28495,2.66399 2.61785,2.269 0.35003,-0.41531 -0.94173,-1.97032 -0.80161,-3.11142 0.0573,-0.46707 1.05339,-0.14452 1.12877,-0.60903 0.0953,-0.58755 -0.77102,-0.80928 -1.12877,-1.28501 -0.41893,-0.55708 -0.48534,-1.96672 -1.01015,-1.8309 -0.54565,0.14121 -1.14963,0.21788 -1.40362,0.75444 -0.26862,0.56746 -1.87553,0.70466 -1.42277,1.1396 z" id="path32861" /><path style="fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" d="m 342.82052,351.94768 c -0.26871,0.14868 3.21484,4.70931 4.04061,4.73509 0.30604,0.01 0.86154,-1.85175 0.63135,-1.70463 -0.18825,0.12031 -4.27435,-3.25046 -4.67196,-3.03046 z" id="path32863" /><path style="fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" d="m 339.94789,347.02319 c 0.97393,1.27929 1.81957,2.5391 3.3777,2.93576 2.57135,0.6546 6.07642,4.38129 6.40816,3.7565 0.18185,-0.34249 -1.76142,-2.26215 -2.52538,-3.4724 -0.44736,-0.70872 -1.18954,-1.02127 -0.66385,-1.674 0.42611,-0.52908 0.76219,-0.13135 1.42146,0.0325 1.10552,0.27476 2.25604,1.01956 3.3777,0.82075 0.70728,-0.12537 1.03844,-0.82529 1.48366,-1.38897 0.495,-0.62669 0.73264,-1.49867 0.53665,-2.27284 -0.33981,-1.34231 -1.36873,-1.70755 -2.46225,-2.55694 -1.3601,-1.05647 -2.4033,-1.48597 -3.88277,-2.36755 -0.55839,-0.33274 -1.17147,-1.0479 -1.76778,-0.78918 -0.7164,0.31082 0.13829,1.56646 -0.47351,2.05187 -1.05523,0.83724 -1.40417,0.90777 -3.50397,0.82071 -0.96078,-0.0398 -1.69329,0.24889 -2.14657,1.09697 -0.0911,0.17038 -0.21075,0.95233 -0.18937,1.14435 0.0738,0.66238 0.42325,1.07218 1.01012,1.86246 z" id="path32865" /></g></g></svg>');
  fsvghover :=  TStringList.Create;
  fsvgdown :=  TStringList.Create;
  FState := MouseOut;
end;

destructor TBCSVGButton.Destroy;
begin
  fsvghover.Free;
  fsvghover := nil;
  fsvgnormal.Free;
  fsvgnormal := nil;
  fsvgdown.Free;
  fsvgdown := nil;
  inherited Destroy;
end;

//FSVG.CreateFromString(fsvgnormal.Text);
procedure TBCSVGButton.GenerateCompletedSVGImage(AValue: string);
begin
  FSVG.CreateFromString(AValue);
end;

procedure TBCSVGButton.ReadSVGFileAndSetString(fn:String;itm:Integer);
var li,st:ansistring;
    F:Text;

begin
  li:='';
  st:='';
  if  fileexists(fn) then
  begin
    AssignFile(F,fn);
    {$I-}
    Reset(F);
    {$I+}
    If (IoResult = 0) Then
    Begin
      While Not(EoF(F)) Do
      Begin
        ReadLn(F,Li);
        st:=st+li;
        If (EoF(F)) Then Break;
      End;
    End;
    CloseFile(F);
  end else showmessage('File Not Found');
  case itm of
    0:begin
        if st<>'' then fsvgNormal.Text:=st;
        FFileNameNormal:='';
      end;
    1:Begin
        if st<>'' then fsvgHover.Text:=st;
        FFileNameHover:='';
      End;
    2:Begin
        if st<>'' then fsvgDown.Text:=st;
        FFileNameDown:='';
      ENd;
  end;
  if st<>'' then RedrawBitmap;
End;

procedure TBCSVGButton.SetInfo1(const AValue: string);
begin
  If AValue<>'' then FInfo1:=AValue;
end;

procedure TBCSVGButton.SetInfo2(const AValue: string);
begin
  If AValue<>'' then FInfo2:=AValue;
end;

procedure TBCSVGButton.setposition(const AValue: Integer);
begin
  If AValue<>FPosition then
  begin
    FPosition:=AValue;
    if assigned(FOnPositionChange) then FOnPositionChange(self);
  end;
end;

procedure TBCSVGButton.setmax(const AValue: Integer);
begin
  If AValue<>Fmax then Fmax:=AValue;
end;

procedure TBCSVGButton.setFFileNameNormal(const AValue: string);
begin
  If AValue<>'' then ReadSVGFileAndSetString(AValue,0);
end;

procedure TBCSVGButton.setFFileNameHover(const AValue: string);
begin
  If AValue<>'' then ReadSVGFileAndSetString(Avalue,1);
end;

procedure TBCSVGButton.setFFileNameDown(const AValue: string);
begin
  If AValue<>'' then ReadSVGFileAndSetString(Avalue,2);
End;

procedure TBCSVGButton.setsvgnormalxml(const AValue: tstrings);
begin
  if fsvgnormal.Text = AValue.Text then
    Exit;
  fsvgnormal.Assign(AValue);
  DiscardBitmap;
  if FDown=false then if fsvgnormal.Text<>'' then GenerateCompletedSVGImage(fsvgnormal.Text);
  RedrawBitmap;
 // if not fdown then RedrawBitmap;
end;

procedure TBCSVGButton.setsvghoverxml(const AValue: tstrings);
begin
  if fsvghover.Text = AValue.Text then
    Exit;
  fsvghover.Assign(AValue);
  DiscardBitmap;
end;

procedure TBCSVGButton.setsvgdownxml(const AValue: tstrings);
begin
  if fsvgdown.Text = AValue.Text then
    Exit;
  fsvgdown.Assign(AValue);
  DiscardBitmap;
  if FDown then
  begin
    if fsvgdown.Text<>'' then GenerateCompletedSVGImage(fsvgdown.Text);
    RedrawBitmap;
  end;
end;

procedure TBCSVGButton.setdown(AValue: boolean);
begin
  if fdown = AValue then
    Exit;
  fdown := AValue;
  if fdown=false then Fstate:=MouseOut;
  DiscardBitmap;
  if FDown then
  begin
    if fsvgdown.Text<>'' then GenerateCompletedSVGImage(fsvgdown.Text);
  end
  else
  begin
    if fsvgnormal.Text<>'' then GenerateCompletedSVGImage(fsvgnormal.Text);
  end;
  RedrawBitmap;
end;

procedure TBCSVGButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  MX, MY: integer);
begin
  inherited MouseDown(Button, Shift, MX, MY);
  if csDesigning in ComponentState then
    exit;

  if (Button = mbLeft) and Enabled then
  begin
    FState := Pressed;
    if fsvgdown.Text<>'' then GenerateCompletedSVGImage(fsvgdown.Text);
//    RedrawBitmapContent;
    RedrawBitmap;
  end;
end;

procedure TBCSVGButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  MX, MY: integer);
begin
  inherited MouseUp(Button, Shift, MX, MY);

  if csDesigning in ComponentState then exit;

  if (Button = mbLeft) and Enabled then
  begin
    if FDown then
    begin
      if fsvgdown.Text<>'' then GenerateCompletedSVGImage(fsvgdown.Text)
    end
    else
    begin
      if fsvghover.Text<>'' then GenerateCompletedSVGImage(fsvghover.Text);
    end;
    FState := MouseIn;
 //   RedrawBitmapContent;
    RedrawBitmap;
  end;
end;

procedure TBCSVGButton.MouseEnter;
begin
  if csDesigning in ComponentState then exit;

  inherited MouseEnter;

  if fsvghover.Text<>'' then GenerateCompletedSVGImage(fsvghover.Text);
  FState := MouseIn;
 // RedrawBitmapContent;
  RedrawBitmap;

end;

procedure TBCSVGButton.MouseLeave;
begin
  inherited MouseLeave;
  if csDesigning in ComponentState then
    exit;
  if FDown then
  begin
    if fsvgdown.Text<>'' then GenerateCompletedSVGImage(fsvgdown.Text)
  end
  else
  begin
    if fsvgnormal.Text<>'' then GenerateCompletedSVGImage(fsvgnormal.Text);
  end;
  FState := MouseOut;
//  RedrawBitmapContent;
  RedrawBitmap;

end;

procedure TBCSVGButton.RedrawBitmapContent;
begin
  if FDown then
  begin
    if fsvgdown.Text<>'' then GenerateCompletedSVGImage(fsvgdown.Text)
  end
  else
  begin
    case fstate of
      mousein :if fsvghover.Text<>'' then GenerateCompletedSVGImage(fsvghover.Text);
      mouseout:if fsvgnormal.Text<>'' then GenerateCompletedSVGImage(fsvgnormal.Text);
    end;
  end;
  inherited RedrawBitmapContent;
end;

procedure Register;
begin
  {$I icons\bcsvgbutton.lrs}
  RegisterComponents('BGRA Button Controls',[TBCSVGButton]);
end;



end.
