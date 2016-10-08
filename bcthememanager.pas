unit BCThemeManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type
  TBCThemeManager = class(TComponent)
  private

  protected

  public
    procedure Apply(AControl: TWinControl); virtual abstract;
    procedure Apply(); virtual abstract;
  published

  end;

implementation

end.
