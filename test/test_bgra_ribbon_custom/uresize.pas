unit uResize;
{
  Copyright 2014 Michael Thomas Greer
  Distributed under the Boost Software License, Version 1.0
  (See accompanying file LICENSE.txt or copy
   at http://www.boost.org/LICENSE_1_0.txt )
}

//////////////////////////////////////////////////////////////////////////////
interface
//////////////////////////////////////////////////////////////////////////////

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Windows;

const
  ResizeMaskLeft   = $1;
  ResizeMaskTop    = $2;
  ResizeMaskWidth  = $4;
  ResizeMaskHeight = $8;

type

  { TResizeForm }

  TResizeForm = class( TForm )
    procedure FormMouseMove( Sender: TObject;      Shift: TShiftState; X, Y: Integer );
    procedure FormMouseUp(   Sender: TObject;
                             Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
  private
    anchor_g: TRect;
    anchor_c: TPoint;
    form_ref: TForm;
    resize_m: cardinal;

  public
    procedure SetMouseDown( AForm: TForm; ResizeMask: cardinal );
  end;

var
  ResizeForm: TResizeForm;


//////////////////////////////////////////////////////////////////////////////
implementation
//////////////////////////////////////////////////////////////////////////////

{$R *.lfm}

//----------------------------------------------------------------------------
procedure TResizeForm.SetMouseDown( AForm: TForm; ResizeMask: cardinal );
  begin
  anchor_g.Left   := AForm.Left;
  anchor_g.Top    := AForm.Top;
  anchor_g.Right  := AForm.Width;
  anchor_g.Bottom := AForm.Height;
  anchor_c        := Mouse.CursorPos;
  form_ref        := AForm;
  resize_m        := ResizeMask;
  SetCapture( Handle )
  end;

//----------------------------------------------------------------------------
procedure TResizeForm.FormMouseMove(
  Sender: TObject;
  Shift:  TShiftState;
  X, Y:   Integer
  );
  var
    p: TPoint;
    r: TRect;
  begin
  if Assigned( form_ref ) and (ssLeft in Shift)
    then begin
         p := Mouse.CursorPos;
         Dec( p.x, anchor_c.x );
         Dec( p.y, anchor_c.y );

         r.Left   := form_ref.Left;
         r.Top    := form_ref.Top;
         r.Right  := form_ref.Width;
         r.Bottom := form_ref.Height;

         if (resize_m and ResizeMaskLeft)   <> 0 then begin r.Left   := anchor_g.Left   + p.x;  p.x := -p.x end;
         if (resize_m and ResizeMaskTop)    <> 0 then begin r.Top    := anchor_g.Top    + p.y;  p.y := -p.y end;
         if (resize_m and ResizeMaskWidth)  <> 0 then       r.Right  := anchor_g.Right  + p.x;
         if (resize_m and ResizeMaskHeight) <> 0 then       r.Bottom := anchor_g.Bottom + p.y;

         with r do form_ref.SetBounds( Left, Top, Right, Bottom )
         end
  end;

//----------------------------------------------------------------------------
procedure TResizeForm.FormMouseUp(
  Sender: TObject;
  Button: TMouseButton;
  Shift:  TShiftState;
  X, Y:   Integer
  );
  begin
  ReleaseCapture;
  form_ref := nil
  end;

end.
