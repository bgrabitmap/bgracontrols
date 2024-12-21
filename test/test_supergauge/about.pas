unit about;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, SuperGauge;

type

  { TAboutFrm }

  TAboutFrm = class(TForm)
    CloseBtn: TBitBtn;
    Image1: TImage;
    AuthorLbl: TLabel;
    Memo1: TMemo;
    NameLbl: TLabel;
    NameLbl1: TLabel;
    Panel1: TPanel;
    SGVersionLbl: TLabel;
    SGTestVersionLbl: TLabel;
    procedure CloseBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  AboutFrm: TAboutFrm;

implementation

{$R *.lfm}

{ TAboutFrm }

procedure TAboutFrm.FormCreate(Sender: TObject);
begin
  SGTestVersionLbl.Caption := VERSIONSTR;
  SGVersionLbl.Caption := SuperGauge.VERSIONSTR;
end;

procedure TAboutFrm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

end.

