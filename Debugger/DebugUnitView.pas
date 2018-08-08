unit DebugUnitView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, SynEdit, Vcl.ComCtrls, SynEditHighlighter, SynHighlighterGeneral, SynHighlighterPas, VMDebuggerCore;

type
  TfrmDebugUnitView = class(TForm)
    SynPasSyn1: TSynPasSyn;
    edSource: TSynEdit;
    procedure edSourceGutterClick(Sender: TObject; Button: TMouseButton; X, Y, Line: Integer; Mark: TSynEditMark);
  private
    FUnitID: Integer;
    FDebugger: TVMDebugger;
    { Private declarations }
  public
    { Public declarations }
    property UnitID: Integer read FUnitID write FUnitID;
    property Debugger: TVMDebugger read FDebugger write FDebugger;
    procedure MoveToPosition(Line: Integer);
  end;

implementation

{$R *.dfm}

procedure TfrmDebugUnitView.edSourceGutterClick(Sender: TObject; Button: TMouseButton; X, Y, Line: Integer; Mark: TSynEditMark);
begin
  if FDebugger.BreakPointExist(Line) then
    FDebugger.DeleteBreakPoint(Line)
  else begin
    FDebugger.AddBreakPoint(Line);
  end;
end;

procedure TfrmDebugUnitView.MoveToPosition(Line: Integer);
var
  RowCol: TBufferCoord;
begin
  RowCol.Char := 1;
  RowCol.Line := Line;
  edSource.SelStart := edSource.RowColToCharIndex(RowCol);
  edSource.SelEnd := edSource.SelStart;
end;

end.
