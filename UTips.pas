/////////////////////////////////////////////////////////
//                                                     //
//         Classe desenvolvida em Delphi 10.1 Berlin.  //
//                                                     //
//        Nesta Classe tem contida funções de ajuda    //
//   desenvolvidas por mim e ainda em desenvolvimen-   //
//   to, assim como desenvolvida por terceiros.        //
//                                                     //
//        Em caso de melhorias na classe por favor me  //
//   comuniquem pelo e-mail "ramonrxc@gmail.com" ou    //
//   pelo skype ramon.ruan2.                           //
//                                                     //
//        Classe desenvolvida por Ramon Ruan.          //
//                                                     //
/////////////////////////////////////////////////////////

unit UTips;

interface

uses
  Controls, CommCtrl, Graphics, Windows, Messages, SysUtils, Variants, Classes, Forms,
  Dialogs, StdCtrls, ExtCtrls;

{$SCOPEDENUMS ON}

type
  TIconKind = (None = TTI_NONE, Info = TTI_INFO, Warning = TTI_WARNING, Error = TTI_ERROR, Info_Large = TTI_INFO_LARGE, Warning_Large = TTI_WARNING_LARGE, Eror_Large = TTI_ERROR_LARGE);

  TTip = class helper for TWinControl
  public
    procedure ShowBalloonTip(Icon: TIconKind; const Title, Text: string);
    procedure SetBorder(AColor: TColor);
  end;

implementation

{ TBalloonTip }

procedure TTip.SetBorder(AColor: TColor);
var
  Canvas: TCanvas;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := GetWindowDC(Handle);
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := AColor;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(0, 0, Width, Height);
  finally
    ReleaseDC(Handle, Canvas.Handle);
    Canvas.Free;
  end;
end;

procedure TTip.ShowBalloonTip(Icon: TIconKind; const Title, Text: string);
var
  hWndTip: THandle;
  ToolInfo: TToolInfo;
  BodyText: pWideChar;
begin
  hWndTip := CreateWindow(TOOLTIPS_CLASS, nil, WS_POPUP or TTS_CLOSE or TTS_NOPREFIX or TTS_BALLOON or TTS_ALWAYSTIP, 0, 0, 0, 0, Handle, 0, HInstance, nil);

  if hWndTip = 0 then
    exit;

  GetMem(BodyText, 2 * 256);

  try
    ToolInfo.cbSize := SizeOf(TToolInfo);
    ToolInfo.uFlags := TTF_CENTERTIP or TTF_TRANSPARENT or TTF_SUBCLASS;
    ToolInfo.hWnd := Handle;
    ToolInfo.lpszText := StringToWideChar(Text, BodyText, 2 * 356);
    SetWindowPos(hWndTip, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
    ToolInfo.Rect := GetClientRect;

    SendMessage(hWndTip, TTM_ADDTOOL, 1, integer(@ToolInfo));
    SendMessage(hWndTip, TTM_SETTITLE, integer(Icon), integer(PChar(Title)));
    SendMessage(hWndTip, TTM_TRACKACTIVATE, integer(true), integer(@ToolInfo));
  finally
    FreeMem(BodyText);
  end;
end;

end.
