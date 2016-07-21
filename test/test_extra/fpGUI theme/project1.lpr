program project1;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_button,
  fpg_checkbox,
  fpg_combobox,
  fpg_edit,
  fpg_form,
  fpg_gauge,
  fpg_grid,
  fpg_hyperlink,
  fpg_label,
  fpg_listbox,
  fpg_listview,
  fpg_memo,
  fpg_panel,
  fpg_popupcalendar,
  fpg_progressbar,
  fpg_radiobutton,
  fpg_trackbar,
  fpg_splitter,
  fpg_ColorWheel,
  fpg_editbtn,
  fpg_menu,
  fpg_stylemanager,
  mystyle,
  fpg_widget {$IFDEF WINDOWS},
  Windows {$ENDIF};

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}

    Label1: TfpgLabel;
    Edit1: TfpgEdit;
    Memo1: TfpgMemo;
    Button1: TfpgButton;
    CheckBox1: TfpgCheckBox;
    RadioButton1: TfpgRadioButton;
    ComboBox1: TfpgComboBox;
    CalendarCombo1: TfpgCalendarCombo;
    CalendarCombo2: TfpgCalendarCheckCombo;
    ListBox1: TfpgListBox;
    ColorListBox1: TfpgColorListBox;
    Grid1: TfpgStringGrid;
    Bevel1: TfpgBevel;
    Panel1: TfpgPanel;
    GroupBox1: TfpgGroupBox;
    ProgressBar1: TfpgProgressBar;
    TrackBar1: TfpgTrackBar;
    ListView1: TfpgListView;
    Gauge1: TfpgGauge;
    EditInteger1: TfpgEditInteger;
    EditFloat1: TfpgEditFloat;
    EditCurrency1: TfpgEditCurrency;
    FilenameEdit1: TfpgFileNameEdit;
    DirectoryEdit1: TfpgDirectoryEdit;
    DirectoryEdit2: TfpgFontEdit;
    ColorWheel1: TfpgColorWheel;
    ValueBar1: TfpgValueBar;
    //Splitter1: TfpgSplitter;
    Hyperlink1: TfpgHyperlink;
    //Toggle1: TfpgToggle;
    {@VFD_HEAD_END: MainForm}
  public
    procedure AfterCreate; override;
  end;

  {@VFD_NEWFORM_DECL}

  {$IFDEF WINDOWS}
var
  hPixelsPerInch: integer;
  vPixelsPerInch: integer;

  function ScaleX(const SizeX, FromDPI: integer): integer;
  begin
    Result := MulDiv(SizeX, hPixelsPerInch, FromDPI);
  end;

  function ScaleY(const SizeY, FromDPI: integer): integer;
  begin
    Result := MulDiv(SizeY, vPixelsPerInch, FromDPI);
  end;

{$ENDIF}

  procedure TMainForm.AfterCreate;
  var
    i: integer;
    {$IFDEF WINDOWS}
    screen: hdc;
    {$ENDIF}
  begin
    {%region 'Auto-generated GUI code' -fold}
    {@VFD_BODY_BEGIN: MainForm}
    Name := 'MainForm';
    SetPosition(316, 186, 300, 250);
    WindowTitle := 'MainForm';

    {%region 'Auto-generated GUI code' -fold}
    {@VFD_BODY_BEGIN: Test}
    Name := 'Test';
    SetPosition(469, 323, 981, 466);
    WindowTitle := 'Test';
    Hint := '';
    IconName := '';

    Label1 := TfpgLabel.Create(self);
    with Label1 do
    begin
      Name := 'Label1';
      SetPosition(8, 8, 68, 16);
      FontDesc := '#Label1';
      Hint := '';
      Text := 'Label';
    end;

    Edit1 := TfpgEdit.Create(self);
    with Edit1 do
    begin
      Name := 'Edit1';
      SetPosition(8, 28, 120, 24);
      ExtraHint := '';
      FontDesc := '#Edit1';
      Hint := '';
      TabOrder := 2;
      Text := '';
    end;

    Memo1 := TfpgMemo.Create(self);
    with Memo1 do
    begin
      Name := 'Memo1';
      SetPosition(8, 60, 120, 52);
      FontDesc := '#Edit1';
      Hint := '';
      TabOrder := 3;
    end;

    Button1 := TfpgButton.Create(self);
    with Button1 do
    begin
      Name := 'Button1';
      SetPosition(8, 120, 80, 24);
      Text := 'Button';
      FontDesc := '#Label1';
      Hint := '';
      ImageName := '';
      TabOrder := 4;
      Flat := False;
    end;

    CheckBox1 := TfpgCheckBox.Create(self);
    with CheckBox1 do
    begin
      Name := 'CheckBox1';
      SetPosition(8, 152, 120, 20);
      FontDesc := '#Label1';
      Hint := '';
      TabOrder := 5;
      Text := 'CheckBox';
    end;

    RadioButton1 := TfpgRadioButton.Create(self);
    with RadioButton1 do
    begin
      Name := 'RadioButton1';
      SetPosition(8, 176, 120, 20);
      FontDesc := '#Label1';
      GroupIndex := 0;
      Hint := '';
      TabOrder := 6;
      Text := 'RadioButton';
    end;

    ComboBox1 := TfpgComboBox.Create(self);
    with ComboBox1 do
    begin
      Name := 'ComboBox1';
      SetPosition(8, 204, 120, 24);
      ExtraHint := '';
      FontDesc := '#List';
      Hint := '';
      Items.Add('Line 1');
      Items.Add('Line 1');
      Items.Add('Line 1');
      Items.Add('Line 1');
      Items.Add('Line 1');
      Items.Add('Line 1');
      Items.Add('Line 1');
      Items.Add('Line 1');
      FocusItem := -1;
      TabOrder := 7;
    end;

    CalendarCombo1 := TfpgCalendarCombo.Create(self);
    with CalendarCombo1 do
    begin
      Name := 'CalendarCombo1';
      SetPosition(8, 236, 120, 24);
      BackgroundColor := TfpgColor($80000002);
      DateFormat := 'dd/MM/yyyy';
      DayColor := TfpgColor($000000);
      FontDesc := '#List';
      Hint := '';
      HolidayColor := TfpgColor($000000);
      SelectedColor := TfpgColor($000000);
      SelectedColor := TfpgColor($000000);
      TabOrder := 8;
    end;

    CalendarCombo2 := TfpgCalendarCheckCombo.Create(self);
    with CalendarCombo2 do
    begin
      Name := 'CalendarCombo2';
      SetPosition(8, 264, 120, 24);
      BackgroundColor := TfpgColor($80000002);
      Checked := True;
      DateFormat := 'dd/MM/yyyy';
      DayColor := TfpgColor($000000);
      FontDesc := '#List';
      Hint := '';
      HolidayColor := TfpgColor($000000);
      SelectedColor := TfpgColor($000000);
      SelectedColor := TfpgColor($000000);
      TabOrder := 9;
    end;

    ListBox1 := TfpgListBox.Create(self);
    with ListBox1 do
    begin
      Name := 'ListBox1';
      SetPosition(8, 296, 116, 104);
      FontDesc := '#List';
      Hint := '';
      Items.Add('Line 1');
      Items.Add('Line 1');
      Items.Add('Line 1');
      Items.Add('Line 1');
      Items.Add('Line 1');
      Items.Add('Line 1');
      Items.Add('Line 1');
      Items.Add('Line 1');
      TabOrder := 10;
    end;

    ColorListBox1 := TfpgColorListBox.Create(self);
    with ColorListBox1 do
    begin
      Name := 'ColorListBox1';
      SetPosition(140, 28, 168, 84);
      Color := TfpgColor($FF00FFFF);
      FontDesc := '#List';
      Hint := '';
      TabOrder := 11;
    end;

    Grid1 := TfpgStringGrid.Create(self);
    with Grid1 do
    begin
      Name := 'Grid1';
      SetPosition(140, 124, 180, 112);
      BackgroundColor := TfpgColor($80000002);
      AddColumn('New', 50, taLeftJustify);
      AddColumn('New', 50, taLeftJustify);
      AddColumn('New', 50, taLeftJustify);
      FontDesc := '#Grid';
      HeaderFontDesc := '#GridHeader';
      Hint := '';
      RowCount := 10;
      RowSelect := False;
      TabOrder := 12;
    end;

    Bevel1 := TfpgBevel.Create(self);
    with Bevel1 do
    begin
      Name := 'Bevel1';
      SetPosition(140, 244, 180, 120);
      Hint := '';
    end;

    Panel1 := TfpgPanel.Create(self);
    with Panel1 do
    begin
      Name := 'Panel1';
      SetPosition(140, 372, 180, 88);
      FontDesc := '#Label1';
      Hint := '';
      Text := 'Panel';
    end;

    GroupBox1 := TfpgGroupBox.Create(self);
    with GroupBox1 do
    begin
      Name := 'GroupBox1';
      SetPosition(328, 32, 200, 80);
      FontDesc := '#Label1';
      Hint := '';
      Text := 'Group box';
    end;

    ProgressBar1 := TfpgProgressBar.Create(self);
    with ProgressBar1 do
    begin
      Name := 'ProgressBar1';
      SetPosition(328, 124, 150, 22);
      Hint := '';
      Position := 75;
    end;

    TrackBar1 := TfpgTrackBar.Create(self);
    with TrackBar1 do
    begin
      Name := 'TrackBar1';
      SetPosition(328, 160, 148, 30);
      Hint := '';
      Position := 20;
      Position := 20;
      TabOrder := 17;
    end;

    ListView1 := TfpgListView.Create(self);
    with ListView1 do
    begin
      Name := 'ListView1';
      SetPosition(328, 244, 184, 120);
      Hint := '';
      MultiSelect := False;
      ShowHeaders := True;
      ShowHeaders := True;
      TabOrder := 18;
    end;

    Gauge1 := TfpgGauge.Create(self);
    with Gauge1 do
    begin
      Name := 'Gauge1';
      SetPosition(332, 376, 180, 25);
      Hint := '';
      Progress := 50;
    end;

    EditInteger1 := TfpgEditInteger.Create(self);
    with EditInteger1 do
    begin
      Name := 'EditInteger1';
      SetPosition(544, 40, 120, 24);
      FontDesc := '#Edit1';
      Hint := '';
      MaxValue := 0;
      MinValue := 0;
      TabOrder := 21;
      Value := 50;
    end;

    EditFloat1 := TfpgEditFloat.Create(self);
    with EditFloat1 do
    begin
      Name := 'EditFloat1';
      SetPosition(544, 76, 120, 24);
      FontDesc := '#Edit1';
      Hint := '';
      MaxValue := 0;
      MinValue := 0;
      TabOrder := 22;
      Value := 50.75;
    end;

    EditCurrency1 := TfpgEditCurrency.Create(self);
    with EditCurrency1 do
    begin
      Name := 'EditCurrency1';
      SetPosition(544, 112, 120, 24);
      FontDesc := '#Edit1';
      Hint := '';
      TabOrder := 23;
      Value := 100;
    end;

    FilenameEdit1 := TfpgFileNameEdit.Create(self);
    with FilenameEdit1 do
    begin
      Name := 'FilenameEdit1';
      SetPosition(544, 152, 140, 24);
      ExtraHint := '';
      FileName := '';
      Filter := '';
      InitialDir := '';
      TabOrder := 24;
    end;

    DirectoryEdit1 := TfpgDirectoryEdit.Create(self);
    with DirectoryEdit1 do
    begin
      Name := 'DirectoryEdit1';
      SetPosition(544, 188, 140, 24);
      Directory := '';
      ExtraHint := '';
      RootDirectory := '';
      TabOrder := 25;
    end;

    DirectoryEdit2 := TfpgFontEdit.Create(self);
    with DirectoryEdit2 do
    begin
      Name := 'DirectoryEdit2';
      SetPosition(544, 220, 140, 24);
      ExtraHint := '';
      FontDesc := '';
      TabOrder := 26;
    end;

    ColorWheel1 := TfpgColorWheel.Create(self);
    with ColorWheel1 do
    begin
      Name := 'ColorWheel1';
      SetPosition(544, 260, 116, 120);
    end;

    ValueBar1 := TfpgValueBar.Create(self);
    with ValueBar1 do
    begin
      Name := 'ValueBar1';
      SetPosition(700, 36, 80, 160);
      Value := 1;
    end;

  {Splitter1 := TfpgSplitter.Create(self);
  with Splitter1 do
  begin
    Name := 'Splitter1';
    SetPosition(776, 0, 14, 466);
    Align := alLeft;
  end;}

    Hyperlink1 := TfpgHyperlink.Create(self);
    with Hyperlink1 do
    begin
      Name := 'Hyperlink1';
      SetPosition(796, 36, 120, 16);
      FontDesc := 'Arial-8:antialias=true:underline';
      Hint := '';
      HotTrackFont := 'Arial-8:antialias=true:underline:bold';
      Text := 'fpGUI website';
      URL := 'http://fpgui.sourceforge.net/';
    end;

  {Toggle1 := TfpgToggle.Create(self);
  with Toggle1 do
  begin
    Name := 'Toggle1';
    SetPosition(796, 60, 120, 20);
    CheckedCaption := 'ON';
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 31;
    Text := 'ToggleBox';
    UnCheckedCaption := 'OFF';
    UseAnimation := True;
  end;}

  {$IFDEF WINDOWS}
    screen := GetDC(0);
    hPixelsPerInch := GetDeviceCaps(screen, LOGPIXELSX);
    vPixelsPerInch := GetDeviceCaps(screen, LOGPIXELSX);
    ReleaseDC(0, screen);

    Self.Width := ScaleX(Width, 96);
    Self.Height := ScaleY(Height, 96);

    //fpgApplication.Screen_dpi

    for i := 0 to Self.ComponentCount - 1 do
    begin
      if Self.Components[i] is TFPGWidget then
      begin
        with TFPGWidget(Self.Components[i]) do
        begin
          Left := ScaleX(Left, 96);
          Top := ScaleY(Top, 96);
          Width := ScaleX(Width, 96);
          Height := ScaleY(Height, 96);
        end;
      end;
    end;
  {$ENDIF}

    {@VFD_BODY_END: MainForm}
    {%endregion}
  end;


  procedure MainProc;
  var
    frm: TMainForm;
  begin
    fpgApplication.Initialize;

    if fpgStyleManager.SetStyle('Demo Style') then
      fpgStyle := fpgStyleManager.Style;

    fpgApplication.CreateForm(TMainForm, frm);
    try
      frm.Show;
      fpgApplication.Run;
    finally
      frm.Free;
    end;
  end;

{$R *.res}

begin
  MainProc;
end.
