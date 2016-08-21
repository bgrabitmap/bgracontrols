unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, strings, Forms, Controls, Graphics, Dialogs, StdCtrls,
  BGRABitmap, BGRABitmapTypes, BGRAVirtualScreen, dynlibs;

const
  {$IFDEF WINDOWS}
  LIBRARYEXT = '*.dll';
  {$ENDIF}
  {$IFDEF LINUX}
  LIBRARYEXT = '*.so';
  {$ENDIF}
  {$IFDEF DARWIN}
  LIBRARYEXT = '*.dylib';
  {$ENDIF}

type
  TFilterName = procedure(s: PChar); cdecl;
  TApplyFilter = procedure(BGRA: TBGRABitmap); cdecl;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    ComboBox1: TComboBox;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure ComboBox1Select(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    DLLnames: TStringList;
    CurrentFilter: TApplyFilter;
    dll: TLibHandle;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  Bitmap.Fill(BGRA(255, 0, 0));
  if Assigned(CurrentFilter) then
    CurrentFilter(Bitmap);
end;

procedure TForm1.ComboBox1Select(Sender: TObject);
begin
  if dll <> dynlibs.NilHandle then
    if FreeLibrary(dll) then
      dll := dynlibs.NilHandle;
  dll := LoadLibrary(DLLnames[ComboBox1.ItemIndex]);
  if dll <> dynlibs.NilHandle then
  begin
    CurrentFilter := TApplyFilter(GetProcAddress(dll, 'ApplyFilter'));
  end;
  BGRAVirtualScreen1.DiscardBitmap;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
  GetName: TFilterName;
  s: PChar;
begin
  DLLnames := FindAllFiles(ProgramDirectory, LIBRARYEXT, False);
  for i := 0 to DLLnames.Count - 1 do
  begin
    DLLnames[i] := ExtractFileName(DLLnames[i]);
  end;

  s := stralloc(50);
  for i := 0 to DLLnames.Count - 1 do
  begin
    dll := LoadLibrary(DLLnames[i]);
    if dll <> dynlibs.NilHandle then
    begin
      GetName := TFilterName(GetProcAddress(dll, 'FilterName'));
      if Assigned(GetName) then
      begin
        GetName(s);
        ComboBox1.Items.Add(string(s));
      end;
      if FreeLibrary(dll) then
        dll := dynlibs.NilHandle;
    end;
  end;
  strdispose(s);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DLLnames.Free;
end;

end.

