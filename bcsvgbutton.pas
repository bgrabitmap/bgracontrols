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
var li,st:ansistring;
    F:Text;
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
  RegisterComponents('BGRA Controls',[TBCSVGButton]);
end;



end.
