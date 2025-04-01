// SPDX-License-Identifier: LGPL-3.0-linking-exception
{*******************************************************************************

 (c) 2025 - Massimo Magnano

********************************************************************************

 Form that contains the various UI of the graphic formats inside panels.

 When it is executed calling Execute ONLY the panel of the selected format will be
 visible and the form will be resized accordingly.

 Another way to use it is to call the GetUI method to take the panel of the
 selected format, so that you can change its parent and use it in another form.
 In this case the user is responsible for releasing the TBGRAFormatUIContainer class.
}

unit BGRAFormatUI;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Buttons,
  TypInfo, Rtti, FpImage,
  BCPanel, BCTrackbarUpdown, BCFluentSlider,
  BGRABitmapTypes;

type
  //To implement a new panel of a format, which I will probably do all of them myself :-) :
  //
  //The container panel (which must be a TBCPanel) must be named like the Format
  //enum TBGRAImageFormat, so for example the panel for the Jpeg format will be named ifJpeg.
  //The position at design time does not matter because the position is changed at runtime.
  //
  //If you want to use autofill to and from the UI
  // - the Writer class must have the properties of interest declared as published
  // - the names of the Panel sub controls must be panelname_propertyname
  //
  //   for example we have the ifJpeg Writer TBGRAWriterJPEG
  //   published
  //     property ProgressiveEncoding;
  //     property GrayScale;
  //     property CompressionQuality;
  //
  //   the corresponding UI will be
  //     ifJpeg : TBCPanel
  //       ifJpeg_ProgressiveEncoding: TCheckBox;
  //       ifJpeg_GrayScale: TCheckBox;
  //       ifJpeg_CompressionQuality: TBCTrackbarUpdown;
  //
  // - for controls that have a list of Items, like ComboBox or RadioGroup, there are 3 ways to use them:
  //     Tag=0 the value will be taken/set directly from ItemIndex;
  //     Tag=1 the value will be taken/set from the corresponding Objects[ItemIndex]
  //           (it is the UI creator's responsibility to fill Objects in the Create event;
  //     Tag=2 same as Tag=1 but Items are filled automatically with Enum Names;

  { TBGRAFormatUIContainer }

  TBGRAFormatUIContainer = class(TForm)
    ifWebP: TBCPanel;
    ifWebP_QualityPercent: TBCTrackbarUpdown;
    ifWebP_Lossless: TCheckBox;
    ifTiff_Compression: TCheckBox;
    ifLazPaint_Caption: TEdit;
    ifPcx: TBCPanel;
    ifBmp_BitsPerPixel: TComboBox;
    ifBmp_GrayScale: TCheckBox;
    ifPcx_Compressed: TCheckBox;
    ifPng_CompressionLevel: TBCFluentSlider;
    ifPng: TBCPanel;
    ifPng_GrayScale: TCheckBox;
    ifPng_WordSized: TCheckBox;
    ifBmp: TBCPanel;
    ifBmp_RLECompress: TCheckBox;
    ifLazPaint: TBCPanel;
    ifTiff_SaveCMYKAsRGB: TCheckBox;
    ifTiff_PremultiplyRGB: TCheckBox;
    ifTiff: TBCPanel;
    btCancel: TBitBtn;
    btOk: TBitBtn;
    ifJpeg_CompressionQuality: TBCTrackbarUpdown;
    ifJpeg_GrayScale: TCheckBox;
    ifJpeg: TBCPanel;
    ifLazPaint_IncludeThumbnail: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    panelButtons: TPanel;
    ifJpeg_ProgressiveEncoding: TCheckBox;
    ifLazPaint_Compression: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure ifBmp_BitsPerPixelChange(Sender: TObject);
    procedure ifBmp_GrayScaleChange(Sender: TObject);
  private
    curFormat: TBGRAImageFormat;
    curWriter: TFPCustomImageWriter;
    rPanelFormat: TBCPanel;

    //some var for format specific UI
    oldBmp_BitsPerPixel: Integer;

    function AdjustPanels: Boolean;
    function SelectPanel: TBCPanel;

    function SetControlValue(const AValue: TValue; const AControl: TControl): Boolean;
    function GetControlValue(var AValue: TValue; const AControl: TControl): Boolean;

    //Copy Properties form TFPCustomImageWriter to UI
    procedure GetWriterProperties;

  public
    class function Execute(const AFormat: TBGRAImageFormat;
                           var AWriter: TFPCustomImageWriter): Boolean;

    class function GetUI(const AFormat: TBGRAImageFormat;
                         var AWriter: TFPCustomImageWriter;
                         var APanel: TBCPanel): Boolean;

    class function BuildSaveFormats(const AControl: TControl;
                                    const ASelectFormat: TBGRAImageFormat=ifUnknown): Integer;

    //Set TFPCustomImageWriter Properties from UI
    procedure SetWriterProperties(var AWriter: TFPCustomImageWriter);

    property PanelFormat: TBCPanel read rPanelFormat;
  end;

var
  BGRAFormatUIContainer: TBGRAFormatUIContainer = nil;


implementation

{$R *.lfm}

uses BCComboBox;

const
  BMP_BitsValidValues: array[0..6] of Integer = (1,4,8,15,16,24,32);

type
  TRttiPropertyArray = specialize TArray<TRttiProperty>;

{ TBGRAFormatUIContainer }

class function TBGRAFormatUIContainer.Execute(const AFormat: TBGRAImageFormat;
                                              var AWriter: TFPCustomImageWriter): Boolean;
begin
  Result:= False;
  if (AFormat = ifUnknown) or
     ((AWriter = nil) and (DefaultBGRAImageWriter[AFormat] = nil))
  then exit;

  if (BGRAFormatUIContainer = nil)
  then BGRAFormatUIContainer :=TBGRAFormatUIContainer.Create(nil);

  if (BGRAFormatUIContainer <> nil) then
  with BGRAFormatUIContainer do
  try
     if (AWriter = nil) then AWriter:= CreateBGRAImageWriter(AFormat, True);

     curFormat:= AFormat;
     curWriter:= AWriter;

     AdjustPanels;
     GetWriterProperties;

     if (rPanelFormat <> nil) and (ShowModal = mrOk) then
     begin
       SetWriterProperties(AWriter);
       Result:= True;
     end;

  finally
     BGRAFormatUIContainer.Free; BGRAFormatUIContainer:= nil;
  end;
end;

class function TBGRAFormatUIContainer.GetUI(const AFormat: TBGRAImageFormat;
                                            var AWriter: TFPCustomImageWriter;
                                            var APanel: TBCPanel): Boolean;
begin
  Result:= False;
  if (AFormat = ifUnknown) or
     ((AWriter = nil) and (DefaultBGRAImageWriter[AFormat] = nil))
  then exit;

  if (BGRAFormatUIContainer = nil)
  then BGRAFormatUIContainer :=TBGRAFormatUIContainer.Create(nil);

  if (BGRAFormatUIContainer <> nil) then
  with BGRAFormatUIContainer do
  try
     if (AWriter = nil) then AWriter:= CreateBGRAImageWriter(AFormat, True);

     curFormat:= AFormat;
     curWriter:= AWriter;

     APanel:= SelectPanel;
     GetWriterProperties;

  finally
  end;
end;

class function TBGRAFormatUIContainer.BuildSaveFormats(const AControl: TControl;
                                                       const ASelectFormat: TBGRAImageFormat): Integer;
var
  iFormat: TBGRAImageFormat;
  aItems: TStrings;

  procedure SetItemIndex(AValue: Integer);
  begin
    if (AControl is TComboBox)
    then TComboBox(AControl).ItemIndex:= AValue
    else
    if (AControl is TBCComboBox)
    then TBCComboBox(AControl).ItemIndex:= AValue
    else
    if (AControl is TRadioGroup)
    then TRadioGroup(AControl).ItemIndex:= AValue;
  end;

begin
  Result:= 0;

  if (AControl is TComboBox)
  then with TComboBox(AControl) do
       begin
         Clear;
         aItems:= Items;
       end
  else
  if (AControl is TBCComboBox)
  then with TBCComboBox(AControl) do
       begin
         Clear;
         aItems:= Items;
       end
  else
  if (AControl is TRadioGroup)
  then with TRadioGroup(AControl) do
       begin
         aItems:= Items;
         aItems.Clear;
       end
  else exit;

  for iFormat:=Low(TBGRAImageFormat) to High(TBGRAImageFormat) do
    if (iFormat <> ifUnknown) and (DefaultBGRAImageWriter[iFormat] <> nil) then
      aItems.AddObject(BGRAImageFormat[iFormat].TypeName+' (.'+SuggestImageExtension(iFormat)+')',
                                TObject(PTRUInt(iFormat)));

  Result:= aItems.Count;
  if (Result > 0)
  then if (ASelectFormat = ifUnknown)
       then SetItemIndex(0)
       else SetItemIndex(aItems.IndexOfObject(TObject(PTRUInt(ASelectFormat))));
end;

function TBGRAFormatUIContainer.SetControlValue(const AValue: TValue; const AControl: TControl): Boolean;
var
   minVal, maxVal, intVal,
   iIndex: Integer;

   function SetControlItems(AItems: TStrings): Boolean;
   var
      PS: PShortString;
      i: Integer;

   begin
     Result:= False;

     Case AControl.Tag of
       0: begin
            iIndex:= intVal;
            Result:= True;
       end;
       1, -1: begin
            iIndex:= AItems.IndexOfObject(TObject(PtrUInt(intVal)));
            Result := (iIndex > -1);
       end;
       2: if (AValue.Kind = tkEnumeration)
          then begin
                 Result:= False;
                 AControl.Tag:= -1; //Avoid Re-Fill already filled Items
                 AItems.Clear;
                 PS:= @AValue.TypeData^.NameList;
                 for i:=AValue.TypeData^.MinValue to AValue.TypeData^.MaxValue do
                 begin
                   if PS=nil then break;

                   AItems.AddObject(PS^, TObject(PtrUInt(i)));
                   if (i = intVal) then
                   begin
                     iIndex:= intVal;
                     Result:= True;
                   end;
                   PS:=PShortString(pointer(PS)+PByte(PS)^+1);
                 end;
                end
           else begin
                  iIndex:= AItems.IndexOfObject(TObject(PtrUInt(intVal)));
                  Result := (iIndex > -1);
                end;
     end;
   end;

begin
  Result:= False;

  //If we are here the corresponding property is present
  AControl.Visible:= True;

  if AControl.Enabled then
  try
     {#to-do Set with TCheckGroup}
     Case AValue.Kind of
       tkInteger: intVal:= AValue.AsInteger;
       tkEnumeration: begin
         minVal:= AValue.TypeData^.MinValue;
         maxVal:= AValue.TypeData^.MaxValue;
         intVal:= AValue.AsOrdinal;
       end;
       tkFloat: intVal:= Round(AValue.AsExtended);
     end;

     //Types will be added as we use them,
     //it is the responsibility of the UI creator not to put in crap like
     //a checkbox that takes the value from a string, etc...

     if (AControl is TEdit)
     then TEdit(AControl).Caption:= AValue.AsString
     else
     if (AControl is TCheckBox)
     then TCheckBox(AControl).Checked:= AValue.AsBoolean
     else
     if (AControl is TBCTrackbarUpdown)
     then with TBCTrackbarUpdown(AControl) do
          begin
            if (AValue.Kind = tkEnumeration) then
            begin
              MinValue:= minVal;
              MaxValue:= maxVal;
            end;
            Value:= intVal;
          end
     else
     (*if (AControl is TTrackbar)
     then with TTrackbar(AControl) do
          begin
            if (AValue.Kind = tkEnumeration) then
            begin
              Min:= minVal;
              Max:= maxVal;
            end;
            Position:= intVal;
          end
     else*)
     if (AControl is TComboBox)
     then with TComboBox(AControl) do
          begin
            if SetControlItems(Items) then ItemIndex:= iIndex;
          end
     else
     if (AControl is TRadioGroup)
     then with TRadioGroup(AControl) do
          begin
            if SetControlItems(Items) then ItemIndex:= iIndex;
          end
     else
     if (AControl is TBCFluentSlider)
     then with TBCFluentSlider(AControl) do
          begin
            if (AValue.Kind = tkEnumeration) then
            begin
              MinValue:= minVal;
              MaxValue:= maxVal;
            end;
            Value:= intVal;
          end;

    Result:= True;
  except
    Result:= False;
  end;
end;

function TBGRAFormatUIContainer.GetControlValue(var AValue: TValue; const AControl: TControl): Boolean;
begin
  Result:= False;

  if AControl.Visible and AControl.Enabled then
  try
     //Types will be added as we use them,
     //it is the responsibility of the UI creator not to put in crap like
     //a Boolean that takes the value from an Trackbar, etc...

     {#to-do Set with TCheckGroup}

     if (AControl is TEdit)
     then AValue:= TEdit(AControl).Caption
     else
     if (AControl is TCheckBox)
     then AValue:= TCheckBox(AControl).Checked
     else
     if (AControl is TBCTrackbarUpdown)
     then AValue:= TBCTrackbarUpdown(AControl).Value
     else
     (*if (AControl is TTrackbar)
     then AValue:= TTrackbar(AControl).Position
     else*)
     if (AControl is TComboBox)
     then with TComboBox(AControl) do
          begin
            if (Tag = 0)
            then AValue:= ItemIndex
            else if (ItemIndex > -1) then AValue:= Integer(PtrUInt(Items.Objects[ItemIndex]));
          end
     else
     if (AControl is TRadioGroup)
     then with TRadioGroup(AControl) do
          begin
            if (Tag = 0)
            then AValue:= ItemIndex
            else if (ItemIndex > -1) then AValue:= Integer(PtrUInt(Items.Objects[ItemIndex]));
          end
     else
     if (AControl is TBCFluentSlider)
     then AValue:= TBCFluentSlider(AControl).Value;

    Result:= True;
  except
    Result:= False;
  end;
end;

//Set Writer Properties from UI
procedure TBGRAFormatUIContainer.SetWriterProperties(var AWriter: TFPCustomImageWriter);
var
  LContext: TRttiContext;

  procedure SetClassValues(const subPath: String; aInstance: TObject);
  var
    i: Integer;
    LType: TRttiType;
    PropList: TRttiPropertyArray;
    aValue: TValue;
    curControl: TControl;

  begin
    try
       LType:= LContext.GetType(aInstance.ClassType);

       //Read properties list
       PropList := LType.GetProperties;

       for i:= 0 to length(PropList)-1 do
         if PropList[i].IsReadable and PropList[i].IsWritable then
         begin
           aValue:= PropList[i].GetValue(aInstance);

           if aValue.IsObject
           then begin
                  //Call recursively passing the object
                  if (aValue.AsObject <> nil)
                  then SetClassValues(subPath+'_'+PropList[i].Name, aValue.AsObject);
                end
           else if not(aValue.Kind = tkMethod) then
                begin
                  //Find corresponding Control if any and Set Property value from it's Value
                  curControl:= rPanelFormat.FindChildControl(subPath+'_'+PropList[i].Name);
                  if (curControl <> nil) and
                     GetControlValue(aValue, curControl)
                  then PropList[i].SetValue(aInstance, aValue);
                end;
         end;

    finally
       PropList:=nil;
    end;
  end;

begin
  if (curWriter <> nil) and (rPanelFormat <> nil) then
  try
     LContext:= TRttiContext.Create;
     SetClassValues(rPanelFormat.Name, curWriter);

  finally
    LContext.Free;
  end;
end;

//Set UI Control Values from Writer Properties
procedure TBGRAFormatUIContainer.GetWriterProperties;
var
  LContext: TRttiContext;

  procedure GetClassValues(const subPath: String; aInstance: TObject);
  var
    i: Integer;
    LType: TRttiType;
    PropList: TRttiPropertyArray;
    aValue: TValue;
    curControl: TControl;

  begin
    try
       LType:= LContext.GetType(aInstance.ClassType);

       //Read properties list
       PropList := LType.GetProperties;

       for i:= 0 to length(PropList)-1 do
         if PropList[i].IsReadable then
         begin
           aValue:= PropList[i].GetValue(aInstance);

           if aValue.IsObject
           then begin
                  //Call recursively passing the object
                  if (aValue.AsObject <> nil)
                  then GetClassValues(subPath+'_'+PropList[i].Name, aValue.AsObject);
                end
           else if not(aValue.Kind = tkMethod) then
                begin
                  //Find corresponding Control if any and Set it's value
                  curControl:= rPanelFormat.FindChildControl(subPath+'_'+PropList[i].Name);
                  if (curControl <> nil) then SetControlValue(aValue, curControl);
                end;
         end;

    finally
       PropList:=nil;
    end;
  end;

begin
  if (curWriter <> nil) and (rPanelFormat <> nil) then
  try
     LContext:= TRttiContext.Create;
     GetClassValues(rPanelFormat.Name, curWriter);

  finally
    LContext.Free;
  end;
end;

procedure TBGRAFormatUIContainer.FormCreate(Sender: TObject);
var
   i: Integer;

begin
  //Bitmap Format
  oldBmp_BitsPerPixel:= -1;

  //Fill Bits x Pixels Objects Values
  for i:=0 to ifBmp_BitsPerPixel.Items.Count-1 do
    ifBmp_BitsPerPixel.Items.Objects[i]:= TObject(PtrUInt(BMP_BitsValidValues[i]));
end;

procedure TBGRAFormatUIContainer.ifBmp_GrayScaleChange(Sender: TObject);
begin
  if ifBmp_GrayScale.Checked
  then begin
         oldBmp_BitsPerPixel:= ifBmp_BitsPerPixel.ItemIndex;
         ifBmp_BitsPerPixel.ItemIndex:= 2; //GrayScale
       end
  else if (oldBmp_BitsPerPixel > -1)
       then ifBmp_BitsPerPixel.ItemIndex:= oldBmp_BitsPerPixel;

  ifBmp_RLECompress.Enabled:= ifBmp_GrayScale.Checked;
  ifBmp_BitsPerPixel.Enabled:= not(ifBmp_GrayScale.Checked);
end;

procedure TBGRAFormatUIContainer.ifBmp_BitsPerPixelChange(Sender: TObject);
begin
  ifBmp_RLECompress.Enabled:= (ifBmp_BitsPerPixel.ItemIndex in [1,2]);
  ifBmp_GrayScale.Enabled:= (ifBmp_BitsPerPixel.ItemIndex = 2);
end;

function TBGRAFormatUIContainer.AdjustPanels: Boolean;
var
   pName: String;
   curControl: TControl;
   i: Integer;

begin
  rPanelFormat:= nil;
  Result:= False;

  pName:= GetEnumName(TypeInfo(TBGRAImageFormat), Integer(curFormat));

  for i:=0 to ControlCount-1 do
  begin
    curControl:= Controls[i];

    if (curControl <> nil) and
       (curControl is TBCPanel) and
       (curControl.Enabled) then
    begin
      if (CompareText(curControl.Name, pName) = 0) then
      begin
        rPanelFormat:= TBCPanel(curControl);
        Result:= True;
      end;

      curControl.Visible:= False;
    end;
  end;

  if Result then
  begin
    rPanelFormat.Top:= 0; rPanelFormat.Left:= 0;
    rPanelFormat.BevelInner:= bvNone;
    rPanelFormat.BevelOuter:= bvNone;
    rPanelFormat.Caption:='';
    Self.Width:= rPanelFormat.Width;
    Self.Height:= rPanelFormat.Height+panelButtons.Height;

    rPanelFormat.Visible:= True;
  end;
end;

function TBGRAFormatUIContainer.SelectPanel: TBCPanel;
var
   pName: String;
   curControl: TControl;
   i: Integer;

begin
  rPanelFormat:= nil;
  Result:= nil;

  pName:= GetEnumName(TypeInfo(TBGRAImageFormat), Integer(curFormat));

  //I use Components because when the Panels parent is changed they are removed from Controls
  for i:=0 to ComponentCount-1 do
    if (Components[i] is TControl) then
    begin
      curControl:= TControl(Components[i]);

      if (curControl <> nil) and
         (curControl is TBCPanel) and
         (curControl.Enabled) and
         (CompareText(curControl.Name, pName) = 0) then
      begin
        Result:= TBCPanel(curControl);
        break;
      end;
    end;

  if (Result <> nil) then
  begin
    Result.Top:= 0; Result.Left:= 0;
    Result.BevelInner:= bvNone;
    Result.BevelOuter:= bvNone;
    Result.Caption:='';
  end;

  rPanelFormat:= Result;
end;

end.

