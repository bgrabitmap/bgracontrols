// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Styles form manager

  ------------------------------------------------------------------------------
  originally written in 2012 by Krzysztof Dibowski dibowski at interia.pl
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCStylesForm;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils,
  {$IFDEF FPC}
  FileUtil, ComponentEditors, PropEdits, LazVersion,
  {$ELSE}
  Windows, DesignIntf, DesignEditors, PropertyCategories,
  ToolIntf, ExptIntf, DesignWindows,
  {$ENDIF}
  Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ActnList, ComCtrls, Buttons,
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
    procedure ActionDeleteExecute({%H-}Sender: TObject);
    procedure ActionNewFromCtrlExecute({%H-}Sender: TObject);
    procedure ActionNewFromFileExecute({%H-}Sender: TObject);
    procedure ActionRefreshExecute({%H-}Sender: TObject);
    procedure FormCloseQuery({%H-}Sender: TObject; var CanClose: boolean);
    procedure lvFilesSelectItem({%H-}Sender: TObject; Item: TListItem;
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
    constructor {%H-}Create(AControl: TControl; const AFileExt: String);

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
    function  GetVerb({%H-}Index: Integer): String; override;
    function  GetVerbCount: Integer; override;
  end;

  { TBCSylePropertyEditor }

  TBCSylePropertyEditor = class({$IFDEF FPC}TClassPropertyEditor{$ELSE}TPropertyEditor{$ENDIF})
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

{$IFDEF FPC}
uses MacroIntf, BCRTTI, IDEImagesIntf;
{$ELSE}
uses BCRTTI;
{$ENDIF}

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
    {$IFDEF FPC}
    MessageDlg('Empty ext', Format('Class %s has empty style extension',
      [GetComponent(0).ClassName]),mtError,[mbOK],0);
    {$ELSE}
    MessageDlg('Empty ext' + #10#13 + Format('Class %s has empty style extension',
      [GetComponent(0).ClassName]),mtError,[mbOK],0);
    {$ENDIF}
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
    {$IFDEF FPC}
    CopyFile(OpenDialog1.FileName,IncludeTrailingBackslash(GetStylesDir)+ExtractFileName(OpenDialog1.FileName));
    {$ELSE}
    CopyFile(PWidechar(OpenDialog1.FileName),PWidechar(IncludeTrailingBackslash(GetStylesDir)+ExtractFileName(OpenDialog1.FileName)),False);
    {$ENDIF}
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
  {$IFDEF FPC}//#
  sl := FindAllFiles(GetStylesDir,'*.'+FStyleExt,False);
  {$ENDIF}
  try
    lvFiles.ItemIndex := -1;
    lvFiles.Selected := nil;
    lvFiles.Clear;
    if (sl<>nil) and (sl.Count>0) then
    begin
      lvFiles.{$IFNDEF FPC}Items.{$ENDIF}BeginUpdate;
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
        lvFiles.{$IFNDEF FPC}Items.{$ENDIF}EndUpdate;
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
    {$IFDEF FPC}
    MessageDlg('Assign file', 'No style selected', mtError, [mbOK], 0);
    {$ELSE}
    MessageDlg('Assign file' + #10#13 + 'No style selected', mtError, [mbOK], 0);
    {$ENDIF}
    CanClose := False;
  end
  else
    CanClose := True;
end;

procedure TBCfrmStyle.ActionDeleteExecute(Sender: TObject);
begin
  if (lvFiles.SelCount=0) or
    {$IFDEF FPC}
     (MessageDlg('Deleting style', 'Do you really want to delete selected style? '+
                 'This action delete file: '+IncludeTrailingBackslash(GetStylesDir)+lvFiles.Selected.Caption,
                 mtConfirmation,mbYesNo,0)=mrNo)
    {$ELSE}
     (MessageDlg('Deleting style' + #10#13 + 'Do you really want to delete selected style? '+
                 'This action delete file: '+ IncludeTrailingBackslash(GetStylesDir) + lvFiles.Selected.Caption,
                 mtConfirmation,mbYesNo,0)=mrNo)
    {$ENDIF}
  then
    Exit;

  {$IFDEF FPC}
  DeleteFile(IncludeTrailingBackslash(GetStylesDir)+lvFiles.Selected.Caption);
  {$ELSE}
  DeleteFile(PWideChar(IncludeTrailingBackslash(GetStylesDir)+lvFiles.Selected.Caption));
  {$ENDIF}
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
  {$IFDEF FPC}
  IDEMacros.SubstituteMacros(Result);
  {$ENDIF}
  Result := IncludeTrailingBackslash(Result)+'styles';
end;

procedure TBCfrmStyle.CreatePreviewControl;
begin
  FPreviewControl := TControlClass(FControl.ClassType).Create(Self);
  FPreviewControl.Constraints.MinWidth := 100;
  FPreviewControl.Constraints.MinHeight := 100;
  FPreviewControl.Parent := gboxPreview;
  {$IFDEF FPC}//#
  FPreviewControl.Caption := FControl.Caption;
  if Trim(FPreviewControl.Caption) = '' then
    FPreviewControl.Caption := 'Demo';
  {$ENDIF}
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
  {$IFDEF FPC}//#
  function _LoadImage(AIdx: Integer; const AName: String): Integer;
  begin
    {$if laz_fullversion<4990000}
    Result := IDEImages.GetImageIndex(AIdx,AName);
    if Result=-1 then
      Result := IDEImages.LoadImage(AIdx,AName);
    {$else}
    Result := IDEImages.GetImageIndex(AName,AIdx);
    if Result=-1 then
      Result := IDEImages.LoadImage(AName,AIdx);
    {$endif}
  end;
  {$ENDIF}

begin
  inherited Create(Application);

  FControl  := AControl;
  FStyleExt := AFileExt;

  CreatePreviewControl;
  ActionRefresh.Execute;

  {$IFDEF FPC}//#
  ToolBar1.Images               := IDEImages.Images_16;
  ActionList1.Images            := ToolBar1.Images;
  ActionDelete.ImageIndex       := _LoadImage(16,'laz_delete');
  ActionNewFromCtrl.ImageIndex  := _LoadImage(16,'laz_add');
  ActionNewFromFile.ImageIndex  := _LoadImage(16,'laz_open');
  ActionRefresh.ImageIndex      := _LoadImage(16,'laz_refresh');
  {$ENDIF}

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
    {$IFDEF FPC}
    MessageDlg('Empty ext', Format('Class %s has empty style extension',
      [Component.ClassName]),mtError,[mbOK],0);
    {$ELSE}
    MessageDlg('Empty ext' + #10#13 + Format('Class %s has empty style extension',
      [Component.ClassName]),mtError,[mbOK],0);
    {$ENDIF}
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
  RegisterComponentEditor(TBCStyleGraphicControl, TBCStyleComponentEditor);
  RegisterComponentEditor(TBCStyleCustomControl, TBCStyleComponentEditor);
  {$IFDEF FPC}
  RegisterPropertyEditor(ClassTypeInfo(TBCStyleDummyProperty),nil,'',TBCSylePropertyEditor);
  {$ELSE}
  RegisterPropertyEditor(TypeInfo(TBCStyleDummyProperty),nil,'',TBCSylePropertyEditor);
  {$ENDIF}

end.

