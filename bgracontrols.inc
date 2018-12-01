{$IFDEF FPC}
  {.$B-} // Boolean short-circuit evaluation ON
  {$H+} // Long strings ON
  {.$T-} // Type-checked pointers OFF
  {.$X+} // Extended syntax ON

  {$IFNDEF RO_FPC_MODE_SET}
    {$MODE Delphi}
  {$ENDIF}
  {.$DEFINE INDEBUG}
{$ELSE}
  {.$DEFINE INDEBUG}
{$ENDIF}
