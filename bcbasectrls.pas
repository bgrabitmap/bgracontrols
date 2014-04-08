{ Base framework classes

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

unit BCBaseCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, BGRABitmap, BGRABitmapTypes;

type

  TOnBCPropertyChange = procedure(ASender: TObject; AData: PtrInt) of object;

  { TBCProperty
    Base BC Property with OnChange event support
  }

  TBCProperty = class(TPersistent)
  private
    FOnChange: TOnBCPropertyChange;
  protected
    FControl: TControl;
    procedure Change(AData: PtrInt = 0); virtual;
  public
    constructor Create(AControl: TControl); virtual;
  public
    property Control: TControl read FControl;
    property OnChange: TOnBCPropertyChange read FOnChange write FOnChange;
  end;

  { TBGRABitmapEx
    Some BGRABitmap descendant which can store custom data and has NeedRender flag
  }

  TBGRABitmapEx = class(TBGRABitmap)
  private
    FCustomData: PtrInt;
    FNeedRender: Boolean;
  protected
    procedure Init; override;
  public
    property NeedRender: Boolean read FNeedRender write FNeedRender;
    property CustomData: PtrInt read FCustomData write FCustomData;
  end;

  { TBCGraphicControl
    BC graphic control with some basic functionality like begin/end update and
    debug functions
  }

  TBCGraphicControl = class(TGraphicControl)
  private
    {$IFDEF DEBUG}
    FPaintCount: Integer;
    {$ENDIF}
    FUpdateCount: Integer;
  protected
    procedure DoOnResize; override;
  protected
    {$IFDEF DEBUG}
    function GetDebugText: String; virtual;
    {$ENDIF}
    procedure Paint; override; // do not override in descendants!
    // All descendants should use DrawControl method instead of Paint.
    // DrawControl is not called between BeginUpdate and EndUpdate
    procedure DrawControl; virtual;
    // This method is called when control should be rendered (when some
    // general action occur which change "body" e.g. resize)
    procedure RenderControl; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    // This disable DrawControl method
    procedure BeginUpdate; virtual;
    // This enable DrawControl method
    procedure EndUpdate; virtual;
    // Called on EndUpdate if FUpdateCount is 0
    procedure UpdateControl; virtual;
    // Check if BeginUpdate was called
    function IsUpdating: Boolean;
  end;

  { TBCStyleDummyProperty
    This is only dummy property type for access to style editor from
    object inspector
  }

  TBCStyleDummyProperty = class(TBCProperty)

  end;

  { TBCStyleGraphicControl
    All descendants of this class have support for saving and loading styles and
    access to style editor from object inspector or component context menu
  }

  TBCStyleGraphicControl = class(TBCGraphicControl)
  private
    FAssignStyle: TBCStyleDummyProperty;
  protected
    function GetStyleExtension: String; virtual; abstract;
    // Dummy property for access to style editor dialog
    property AssignStyle: TBCStyleDummyProperty read FAssignStyle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property StyleExtension: String read GetStyleExtension;
  end;

  { TBCCustomControl
    BC custom control with some basic functionality like begin/end update and
    debug functions
  }

  TBCCustomControl = class(TCustomControl)
  private
    {$IFDEF DEBUG}
    FPaintCount: Integer;
    {$ENDIF}
    FUpdateCount: Integer;
  protected
    procedure DoOnResize; override;
  protected
    {$IFDEF DEBUG}
    function GetDebugText: String; virtual;
    {$ENDIF}
    procedure Paint; override; // do not override in descendants!
    // All descendants should use DrawControl method instead of Paint.
    // DrawControl is not called between BeginUpdate and EndUpdate
    procedure DrawControl; virtual;
    // This method is called when control should be rendered (when some
    // general action occur which change "body" e.g. resize)
    procedure RenderControl; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    // This disable DrawControl method
    procedure BeginUpdate; virtual;
    // This enable DrawControl method
    procedure EndUpdate; virtual;
    // Called on EndUpdate if FUpdateCount is 0
    procedure UpdateControl; virtual;
    // Check if BeginUpdate was called
    function IsUpdating: Boolean;
  end;

  { TBCStyleCustomControl
    All descendants of this class have support for saving and loading styles and
    access to style editor from object inspector or component context menu
  }

  TBCStyleCustomControl = class(TBCCustomControl)
  private
    FAssignStyle: TBCStyleDummyProperty;
  protected
    function GetStyleExtension: String; virtual; abstract;
    // Dummy property for access to style editor dialog
    property AssignStyle: TBCStyleDummyProperty read FAssignStyle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property StyleExtension: String read GetStyleExtension;
  end;

procedure Register;

implementation

{$IFDEF DEBUG} uses Graphics; {$ENDIF}

procedure Register;
begin
  RegisterNoIcon([TBCCustomControl]);
end;

{ TBCStyleCustomControl }

constructor TBCStyleCustomControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAssignStyle := TBCStyleDummyProperty.Create(Self);
end;

destructor TBCStyleCustomControl.Destroy;
begin
  FAssignStyle.Free;
  inherited Destroy;
end;

{ TBCStyleGraphicControl }

constructor TBCStyleGraphicControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAssignStyle := TBCStyleDummyProperty.Create(Self);
end;

destructor TBCStyleGraphicControl.Destroy;
begin
  FAssignStyle.Free;
  inherited Destroy;
end;

{ TBCCustomControl }

procedure TBCCustomControl.DoOnResize;
begin
  inherited DoOnResize;
  RenderControl;
end;

{$IFDEF DEBUG}
function TBCCustomControl.GetDebugText: String;
begin
  Result := EmptyStr;
end;
{$ENDIF}

procedure TBCCustomControl.Paint;
begin
  if (csCreating in FControlState) or IsUpdating then
    Exit;

  DrawControl;
  {$IFDEF DEBUG}
  FPaintCount += 1;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clBlack;
  Canvas.Font.Color := clWhite;
  Canvas.TextOut(1,1,'P: '+IntToStr(FPaintCount)+' '+GetDebugText);
  {$ENDIF}
  inherited Paint;
end;

procedure TBCCustomControl.DrawControl;
begin

end;

procedure TBCCustomControl.RenderControl;
begin

end;

constructor TBCCustomControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF DEBUG}
  FPaintCount := 0;
  {$ENDIF}
end;

procedure TBCCustomControl.BeginUpdate;
begin
  FUpdateCount += 1;
end;

procedure TBCCustomControl.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    FUpdateCount -= 1;
    if FUpdateCount=0 then
      UpdateControl;
  end;
end;

procedure TBCCustomControl.UpdateControl;
begin
  Self.Invalidate;
end;

function TBCCustomControl.IsUpdating: Boolean;
begin
  Result := FUpdateCount>0;
end;

{ TBCProperty }

procedure TBCProperty.Change(AData: PtrInt);
begin
  if Assigned(FOnChange) then
    FOnChange(Self,AData);
end;

constructor TBCProperty.Create(AControl: TControl);
begin
  FControl := AControl;

  inherited Create;
end;

{ TBCGraphicControl }

procedure TBCGraphicControl.DoOnResize;
begin
  inherited DoOnResize;
  RenderControl;
end;

{$IFDEF DEBUG}
function TBCGraphicControl.GetDebugText: String;
begin
  Result := EmptyStr;
end;
{$ENDIF}

procedure TBCGraphicControl.Paint;
begin
  //inherited Paint;
  if (csCreating in FControlState) or IsUpdating then
    Exit;
  DrawControl;
  {$IFDEF DEBUG}
  FPaintCount += 1;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clBlack;
  Canvas.Font.Color := clWhite;
  Canvas.TextOut(1,1,'P: '+IntToStr(FPaintCount)+' '+GetDebugText);
  {$ENDIF}
end;

procedure TBCGraphicControl.DrawControl;
begin

end;

procedure TBCGraphicControl.RenderControl;
begin

end;

constructor TBCGraphicControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF DEBUG}
  FPaintCount := 0;
  {$ENDIF}
end;

procedure TBCGraphicControl.BeginUpdate;
begin
  FUpdateCount += 1;
end;

procedure TBCGraphicControl.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    FUpdateCount -= 1;
    if FUpdateCount=0 then
      UpdateControl;
  end;
end;

procedure TBCGraphicControl.UpdateControl;
begin
  Invalidate;
end;

function TBCGraphicControl.IsUpdating: Boolean;
begin
  Result := FUpdateCount>0;
end;

{ TBGRABitmapEx }

procedure TBGRABitmapEx.Init;
begin
  inherited Init;
  FNeedRender := True;
  FCustomData := 0;
end;

end.

