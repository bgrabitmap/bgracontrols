{ Styles form manager

  ------------------------------------------------------------------------------
  Copyright (C) 2012 Krzysztof Dibowski dibowski at interia.pl

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
unit BCStylesForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ActnList, ComCtrls, Buttons, ComponentEditors, PropEdits,
  bcbasectrls;

type

  { TBCfrmStyle }

  TBCfrmStyle = class(TForm)
    ActionRefresh: TAction;
    ActionNewFromFile: TAction;
    ActionDelete: TAction;
    ActionNewFromCtrl: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    gboxPreview: TGroupBox;
    gboxStyles: TGroupBox;
    lvFiles: TListView;
    memoLogs: TMemo;
    OpenDialog1: TOpenDialog;
    pnlBottom: TPanel;
    Splitter1: TSplitter;
    sptrLog: TSplitter;
    ToolBar1: TToolBar;
    btnDelete: TToolButton;
    btnNewFromCtrl: TToolButton;
    ToolButton1: TToolButton;
    btnNewFromFile: TToolButton;
    btnRefresh: TToolButton;
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionNewFromCtrlExecute(Sender: TObject);
    procedure ActionNewFromFileExecute(Sender: TObject);
    procedure ActionRefreshExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure lvFilesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    { private declarations }
    FControl: TControl;
    FPreviewControl: TControl;
    FStyleExt: String;
    procedure AddLog(const AText: String; AClear: Boolean = True);
    procedure CreatePreviewControl;
    function GetFileName: String;
    function GetStylesDir: String;
  public
    { public declarations }
    constructor Create(AControl: TControl; const AFileExt: String);

    property FileName: String read GetFileName;
  end;

  { TBCStyleComponentEditor }

  TBCStyleComponentEditor = class(TComponentEditor)
  protected
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetStyleExtension: String;
    procedure DoShowEditor;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function  GetVerb(Index: Integer): String; override;
    function  GetVerbCount: Integer; override;
  end;

  { TBCSylePropertyEditor }

  TBCSylePropertyEditor = class(TClassPropertyEditor)
  private
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetStyleExtension: String;
    procedure DoShowEditor;
  public
    procedure Edit; Override;
    function  GetAttributes: TPropertyAttributes; Override;
  end;

implementation

uses MacroIntf, BCRTTI, IDEImagesIntf;

{ TBCSylePropertyEditor }

procedure TBCSylePropertyEditor.BeginUpdate;
begin
  if GetComponent(0).InheritsFrom(TBCStyleGraphicControl) then
    TBCStyleGraphicControl(GetComponent(0)).BeginUpdate
  else
  if GetComponent(0).InheritsFrom(TBCStyleCustomControl) then
    TBCStyleCustomControl(GetComponent(0)).BeginUpdate;
end;

procedure TBCSylePropertyEditor.EndUpdate;
begin
  if GetComponent(0).InheritsFrom(TBCStyleGraphicControl) then
    TBCStyleGraphicControl(GetComponent(0)).EndUpdate
  else
  if GetComponent(0).InheritsFrom(TBCStyleCustomControl) then
    TBCStyleCustomControl(GetComponent(0)).EndUpdate;
end;

function TBCSylePropertyEditor.GetStyleExtension: String;
begin
  if GetComponent(0).InheritsFrom(TBCStyleGraphicControl) then
    Result := TBCStyleGraphicControl(GetComponent(0)).StyleExtension
  else
  if GetComponent(0).InheritsFrom(TBCStyleCustomControl) then
    Result := TBCStyleCustomControl(GetComponent(0)).StyleExtension
  else
    Result := '';
end;

procedure TBCSylePropertyEditor.DoShowEditor;
var f: TBCfrmStyle;
begin
  if GetStyleExtension='' then
  begin
    MessageDlg('Empty ext', Format('Class %s has empty style extension',
      [GetComponent(0).ClassName]),mtError,[mbOK],0);
    Exit;
  end;

  f := TBCfrmStyle.Create(TControl(GetComponent(0)),GetStyleExtension);
  try
    if (f.ShowModal=mrOK) and FileExists(f.FileName) then
    begin
      try
        BeginUpdate;
        LoadStyle(GetComponent(0),f.FileName);
      finally
        EndUpdate;
      end;
    end;
  finally
    f.Free;
  end;
end;

procedure TBCSylePropertyEditor.Edit;
begin
  DoShowEditor;
end;

function TBCSylePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{ TBCfrmStyle }

procedure TBCfrmStyle.ActionNewFromCtrlExecute(Sender: TObject);
var
  sName: String;
  sl: TStrings;
begin
  sName := 'My new style';
  if InputQuery('Create new style', 'Style name', sName) then
  begin
    if Trim(sName)='' then
      raise Exception.Create('Name can not be empty');
    sName := IncludeTrailingBackslash(GetStylesDir) + sName+'.'+FStyleExt;
    if FileExists(sName) then
      raise Exception.Create('Style with this name already exists!');
    sl := TStringList.Create;
    try
      SaveStyle(FControl,'Me','',sl);
      sl.SaveToFile(sName);
      ActionRefresh.Execute;
    finally
      sl.Free;
    end;
  end;
end;

procedure TBCfrmStyle.ActionNewFromFileExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    if FileExists(IncludeTrailingBackslash(GetStylesDir)+ExtractFileName(OpenDialog1.FileName)) then
      raise Exception.Create('This style already exists');
    CopyFile(OpenDialog1.FileName,IncludeTrailingBackslash(GetStylesDir)+ExtractFileName(OpenDialog1.FileName));
    ActionRefresh.Execute;
  end;
end;

procedure TBCfrmStyle.ActionRefreshExecute(Sender: TObject);
var
  sl: TStrings;
  i: Integer;
  it: TListItem;
  h: TBCStyleHeader;
begin
  sl := FindAllFiles(GetStylesDir,'*.'+FStyleExt,False);
  try
    lvFiles.ItemIndex := -1;
    lvFiles.Selected := nil;
    lvFiles.Clear;
    if (sl<>nil) and (sl.Count>0) then
    begin
      lvFiles.BeginUpdate;
      try
        for i:=0 to Pred(sl.Count) do
        begin
          it := lvFiles.Items.Add;
          it.Caption := ExtractFileName(sl.Strings[i]);
          GetStyleHeader(sl.Strings[i],@h);
          it.SubItems.Add(h.Author); // Author
          it.SubItems.Add(h.Description); // Description
        end;
        lvFiles.ItemIndex := 0;
        lvFiles.Selected := lvFiles.Items.Item[0];
        // I noticed that OnSelect event is not called when we change
        // selected index manually, so we must call it manually
        lvFilesSelectItem(lvFiles,lvFiles.Selected,True);
        ActionDelete.Enabled := True;
      finally
        lvFiles.EndUpdate;
      end;
    end else
    begin
      memoLogs.Clear;
      memoLogs.Visible        := False;
      sptrLog.Visible         := False;
      FPreviewControl.Visible := False;
      ActionDelete.Enabled    := False;
    end;
  finally
    if sl<>nil then sl.Free;
  end;
end;

procedure TBCfrmStyle.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if (ModalResult=mrOK) and (lvFiles.ItemIndex=-1) then
  begin
    MessageDlg('Assign file', 'No style selected', mtError, [mbOK], 0);
    CanClose := False;
  end
  else
    CanClose := True;
end;

procedure TBCfrmStyle.ActionDeleteExecute(Sender: TObject);
begin
  if (lvFiles.SelCount=0) or
     (MessageDlg('Deleting style', 'Do you really want to delete selected style? '+
                 'This action delete file: '+IncludeTrailingBackslash(GetStylesDir)+lvFiles.Selected.Caption,
                 mtConfirmation,mbYesNo,0)=mrNo)
  then
    Exit;

  DeleteFile(IncludeTrailingBackslash(GetStylesDir)+lvFiles.Selected.Caption);
  ActionRefresh.Execute;
end;

procedure TBCfrmStyle.lvFilesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  sl_logs: TStrings;
  i: Integer;
begin
  if Selected and (Item<>nil) then
  begin
    memoLogs.Visible        := False;
    sptrLog.Visible         := False;
    memoLogs.Clear;
    FPreviewControl.Visible := True;
    ActionDelete.Enabled    := True;

    sl_logs := TStringList.Create;
    try
      if not FileExists(IncludeTrailingBackslash(GetStylesDir)+Item.Caption) then
        Exit;

      LoadStyle(FPreviewControl,IncludeTrailingBackslash(GetStylesDir)+Item.Caption,
        sl_logs);
      // Because load style override it
      FPreviewControl.Constraints.MinWidth := 100;
      FPreviewControl.Constraints.MinHeight := 100;
      // Logs
      for i:=0 to Pred(sl_logs.Count) do
        AddLog(sl_logs.Strings[i],False);
    finally
      sl_logs.Free;
    end;
  end;
end;

procedure TBCfrmStyle.AddLog(const AText: String; AClear: Boolean = True);
begin
  if AClear then memoLogs.Clear;
  if not memoLogs.Visible then
  begin
    memoLogs.Visible := True;
    sptrLog.Visible  := True;
    sptrLog.Top      := memoLogs.Top - 1;
  end;
  memoLogs.Lines.Add(AText);
end;

function TBCfrmStyle.GetStylesDir: String;
begin
  Result := '$PkgDir(bgracontrols)';
  IDEMacros.SubstituteMacros(Result);
  Result := IncludeTrailingBackslash(Result)+'styles';
end;

procedure TBCfrmStyle.CreatePreviewControl;
begin
  FPreviewControl := TControlClass(FControl.ClassType).Create(Self);
  FPreviewControl.Constraints.MinWidth := 100;
  FPreviewControl.Constraints.MinHeight := 100;
  FPreviewControl.Parent := gboxPreview;
  FPreviewControl.Caption := FControl.Caption;
  if Trim(FPreviewControl.Caption) = '' then
    FPreviewControl.Caption := 'Demo';
  FPreviewControl.Visible := False;
end;

function TBCfrmStyle.GetFileName: String;
begin
  if lvFiles.ItemIndex=-1 then
    Result := ''
  else
    Result := IncludeTrailingBackslash(GetStylesDir)+lvFiles.Selected.Caption;
end;

constructor TBCfrmStyle.Create(AControl: TControl;
  const AFileExt: String);

  // It seems that method LoadImage load icon on each call. Others lazarus
  // component editors doesn't check if icon exist but I will do. Small memory leak
  // reduction :P
  function _LoadImage(AIdx: Integer; const AName: String): Integer;
  begin
    Result := IDEImages.GetImageIndex(AIdx,AName);
    if Result=-1 then
      Result := IDEImages.LoadImage(AIdx,AName);
  end;
begin
  inherited Create(Application);

  FControl  := AControl;
  FStyleExt := AFileExt;

  CreatePreviewControl;
  ActionRefresh.Execute;

  ToolBar1.Images               := IDEImages.Images_16;
  ActionList1.Images            := ToolBar1.Images;
  ActionDelete.ImageIndex       := _LoadImage(16,'laz_delete');
  ActionNewFromCtrl.ImageIndex  := _LoadImage(16,'laz_add');
  ActionNewFromFile.ImageIndex  := _LoadImage(16,'laz_open');
  ActionRefresh.ImageIndex      := _LoadImage(16,'laz_refresh');

  ActionDelete.Enabled    := False;

  OpenDialog1.Filter     := 'BC Style|*.'+FStyleExt;
  OpenDialog1.DefaultExt := FStyleExt;
  OpenDialog1.InitialDir := GetStylesDir;
end;

{$R *.lfm}

{ TBCStyleComponentEditor }

procedure TBCStyleComponentEditor.BeginUpdate;
begin
  if Component.InheritsFrom(TBCStyleGraphicControl) then
    TBCStyleGraphicControl(Component).BeginUpdate
  else
  if Component.InheritsFrom(TBCStyleCustomControl) then
    TBCStyleCustomControl(Component).BeginUpdate;
end;

procedure TBCStyleComponentEditor.EndUpdate;
begin
  if Component.InheritsFrom(TBCStyleGraphicControl) then
    TBCStyleGraphicControl(Component).EndUpdate
  else
  if Component.InheritsFrom(TBCStyleCustomControl) then
    TBCStyleCustomControl(Component).EndUpdate;
end;

function TBCStyleComponentEditor.GetStyleExtension: String;
begin
  if Component.InheritsFrom(TBCStyleGraphicControl) then
    Result := TBCStyleGraphicControl(Component).StyleExtension
  else
  if Component.InheritsFrom(TBCStyleCustomControl) then
    Result := TBCStyleCustomControl(Component).StyleExtension
  else
    Result := '';
end;

procedure TBCStyleComponentEditor.DoShowEditor;
var f: TBCfrmStyle;
begin
  if GetStyleExtension='' then
  begin
    MessageDlg('Empty ext', Format('Class %s has empty style extension',
      [Component.ClassName]),mtError,[mbOK],0);
    Exit;
  end;

  f := TBCfrmStyle.Create(TControl(Component),GetStyleExtension);
  try
    if (f.ShowModal=mrOK) and FileExists(f.FileName) then
    begin
      try
        BeginUpdate;
        LoadStyle(Component,f.FileName);
      finally
        EndUpdate;
      end;
    end;
  finally
    f.Free;
  end;
end;

procedure TBCStyleComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: DoShowEditor;
  end;
end;

function TBCStyleComponentEditor.GetVerb(Index: Integer): String;
begin
  Result := 'Assign style';
end;

function TBCStyleComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

initialization
  RegisterComponentEditor([TBCStyleGraphicControl, TBCStyleCustomControl], TBCStyleComponentEditor);
  RegisterPropertyEditor(ClassTypeInfo(TBCStyleDummyProperty),nil,'',TBCSylePropertyEditor);

end.

