unit DebugMasterView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ExtCtrls, TB2Dock, TB2Toolbar, SpTBXItem, SpTBXDkPanels, SynEdit, VirtualTrees, System.Actions, Vcl.ActnList,
  TB2Item, NPCompiler, IL2VMTranslator, VM.Core, NPCompiler.Classes, NPCompiler.Utils, VMDebuggerCore, SynEditHighlighter,
  SynHighlighterGeneral, System.ImageList, Generics.Collections, Vcl.ImgList, Vcl.StdCtrls, Vcl.ComCtrls, DebugUnitView,
  System.UITypes;

type

  TUnitViewInfo = record
    TabSheet: TTabSheet;
    AUnit: TNPUnit;
    UnitView: TfrmDebugUnitView;
  end;

  TUnitViews = array of TUnitViewInfo;

  TfrmDebugMasterView = class(TForm)
    SpTBXDock1: TSpTBXDock;
    tbMain: TSpTBXToolbar;
    pnlClient: TPanel;
    SpTBXSplitter1: TSpTBXSplitter;
    pnlRight: TPanel;
    pnlGlobalVars: TPanel;
    pnlRegs: TPanel;
    SpTBXSplitter2: TSpTBXSplitter;
    vtRegisters: TVirtualStringTree;
    vtGloabalVars: TVirtualStringTree;
    SpTBXStatusBar1: TSpTBXStatusBar;
    ActionList1: TActionList;
    actRun: TAction;
    actStop: TAction;
    actStepInto: TAction;
    actStepOver: TAction;
    actRunUntilRet: TAction;
    SpTBXItem1: TSpTBXItem;
    SpTBXItem2: TSpTBXItem;
    SpTBXItem3: TSpTBXItem;
    SpTBXItem4: TSpTBXItem;
    SpTBXItem5: TSpTBXItem;
    SpTBXSplitter3: TSpTBXSplitter;
    pnlLocalVars: TPanel;
    vtLocalVars: TVirtualStringTree;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SynHL: TSynGeneralSyn;
    pnlCallStack: TPanel;
    vtCallStack: TVirtualStringTree;
    Panel5: TPanel;
    SpTBXSplitter4: TSpTBXSplitter;
    ImageList1: TImageList;
    ImageList2: TImageList;
    SpTBXItem7: TSpTBXItem;
    actStartTrace: TAction;
    pcUnits: TPageControl;
    tsASMView: TTabSheet;
    edVMCode: TSynEdit;
    pnlWhatchView: TPanel;
    VirtualStringTree1: TVirtualStringTree;
    Panel7: TPanel;
    pnlLeft: TPanel;
    SpTBXSplitter5: TSpTBXSplitter;
    procedure FormDestroy(Sender: TObject);
    procedure actStepIntoExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actStopExecute(Sender: TObject);
    procedure actRunExecute(Sender: TObject);
    procedure vtRegistersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtRegistersGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure actRunUpdate(Sender: TObject);
    procedure actStopUpdate(Sender: TObject);
    procedure actStepIntoUpdate(Sender: TObject);
    procedure vtRegistersDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vtGloabalVarsGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure vtGloabalVarsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtCallStackGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure vtCallStackGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtCallStackDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure actRunUntilRetExecute(Sender: TObject);
    procedure actStepOverExecute(Sender: TObject);
    procedure SpTBXItem6Click(Sender: TObject);
    procedure edVMCodeGutterClick(Sender: TObject; Button: TMouseButton; X, Y, Line: Integer; Mark: TSynEditMark);
    procedure actStartTraceExecute(Sender: TObject);
    procedure actStartTraceUpdate(Sender: TObject);
    procedure edVMCodeSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
    procedure vtCallStackFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VirtualStringTree1FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtRegistersFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtLocalVarsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    { Private declarations }
    FDebugger: TVMDebugger;
    FPackage: INPPackage;
    FUnitViews: TUnitViews;
    FUnitsSearchPath: string;
    procedure RefreshALL;
    function GetStepType: TStepType;
    procedure RefreshRegisters;
    procedure RefreshGlobalVars;
    procedure RefreshLocalVars;
    procedure RefreshCallStack;
    procedure RefreshActiveUnitView;
    procedure RefreshLinePosition;
    procedure ShowRegister(const Name, Value: string);
    procedure ShowVariable(const V: PVMDebugVar);
    procedure ShowLocalVariable(const V: PVMDebugVar);
    procedure ShowCSProc(const V: TVMDebugProc);
    procedure ShowBreakPoints;
    procedure PlaceMark(Line: Integer);
    procedure OnVMChange(var Message: TMessage); message WM_VMCHANGE;
    procedure ShowALLUnits;
    procedure edSourceSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
  public
    { Public declarations }
    procedure Debug(const Source: string; RTTICharsert: TRTTICharset);
    property UnitsSearchPath: string read FUnitsSearchPath write FUnitsSearchPath;
  end;

implementation

{$R *.dfm}

uses SystemUnit, IL.TypeInfo, NPCompiler.DataTypes, NPCompiler.Package;


type
  TBaseNodeData = object
    Modified: Boolean;
  end;
  PBaseNodeData = ^TBaseNodeData;

  TVMRegNodeData = object(TBaseNodeData)
    Name: string;
    Value: string;

  end;
  PVMRegNodeData = ^TVMRegNodeData;

  TVMVarNodeData = object(TBaseNodeData)
    Variable: PVMDebugVar;
    Value: string;
  end;
  PVMVarNodeData = ^TVMVarNodeData;

  TVMCSData = record
    ProcName: string;
    Vars: TVMDebugVars;
  end;
  PVMCSData = ^TVMCSData;

{ TfrmDebug }

procedure TfrmDebugMasterView.actRunExecute(Sender: TObject);
begin
  FDebugger.RunToBreakPoint;
end;

procedure TfrmDebugMasterView.actRunUntilRetExecute(Sender: TObject);
begin
  FDebugger.RunUntilRet;
end;

procedure TfrmDebugMasterView.actRunUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FDebugger.State = dsStopped) or (FDebugger.State = dsWaiting);
end;

procedure TfrmDebugMasterView.actStartTraceExecute(Sender: TObject);
begin
  FDebugger.Run;
end;

procedure TfrmDebugMasterView.actStartTraceUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FDebugger.State = dsStopped);
end;

procedure TfrmDebugMasterView.actStepIntoExecute(Sender: TObject);
begin
  FDebugger.StepInto(GetStepType);
  Sleep(50);
  Caption := 'ASM: ' + IntToStr(FDebugger.ASMTextLine) + ' SRC: ' + IntToStr(FDebugger.SRCTextLine);
end;

procedure TfrmDebugMasterView.actStepIntoUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FDebugger.State = dsWaiting);
end;

procedure TfrmDebugMasterView.actStepOverExecute(Sender: TObject);
begin
  FDebugger.StepOver(GetStepType);
  Sleep(50);
  Caption := 'ASM: ' + IntToStr(FDebugger.ASMTextLine) + ' SRC: ' + IntToStr(FDebugger.SRCTextLine);
end;

procedure TfrmDebugMasterView.actStopExecute(Sender: TObject);
begin
  FDebugger.Stop;
  edVMCode.SelEnd := edVMCode.SelStart;
  vtCallStack.Clear;
end;

procedure TfrmDebugMasterView.actStopUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FDebugger.State <> dsStopped);
end;

procedure TfrmDebugMasterView.RefreshLinePosition;
var
  RowCol: TBufferCoord;
begin
  if FDebugger.State = dsStopped then
    Exit;
  RowCol.Char := 1;
  RowCol.Line := FDebugger.ASMTextLine;
  edVMCode.SelStart := edVMCode.RowColToCharIndex(RowCol);
  edVMCode.SelEnd := edVMCode.SelStart;
end;

procedure TfrmDebugMasterView.Debug(const Source: string; RTTICharsert: TRTTICharset);
var
  UN: TNPUnit;
  Msg: TStrings;
  CResult: TCompilerResult;
begin
  FPackage := TNPPackage.Create('test');
  FPackage.AddUnitSearchPath(FUnitsSearchPath);
  FPackage.IncludeDebugInfo := True;
  FPackage.RTTICharset := RTTICharsert;
  FPackage.InitUnits;

  Msg := TStringList.Create;
  UN := TNPUnit.Create(FPackage, Source);
  try
    FPackage.AddUnit(SystemUnit.SYSUnit, nil);
    FPackage.AddUnit(UN, nil);
    Msg.Add('===================================================================');
    Msg.Add('Test: custom');

    CResult := FPackage.Compile();

    if CResult <> CompileSuccess then
      Exit;

    FDebugger := TVMDebugger.Create(FPackage, Self);

    ShowALLUnits;

    FDebugger.Run;
    edVMCode.Text := FDebugger.ASMText;
    ShowBreakPoints;
  finally
    Msg.Free;
  end;
end;

procedure TfrmDebugMasterView.edVMCodeGutterClick(Sender: TObject; Button: TMouseButton; X, Y, Line: Integer; Mark: TSynEditMark);
begin
  if FDebugger.BreakPointExist(Line) then
    FDebugger.DeleteBreakPoint(Line)
  else begin
    FDebugger.AddBreakPoint(Line);
  end;
  ShowBreakPoints;
end;

procedure TfrmDebugMasterView.edVMCodeSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
begin
  if Line = FDebugger.ASMTextLine then
  begin
    Special := True;
    BG := clRed;
  end;
end;

procedure TfrmDebugMasterView.edSourceSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
var
  View: TfrmDebugUnitView;
begin
  View := TControl(Sender).Parent as TfrmDebugUnitView;

  if (View.UnitID = FDebugger.CurrUnitID) and (Line = FDebugger.SRCTextLine) then
  begin
    Special := True;
    BG := clRed;
  end;
end;

procedure TfrmDebugMasterView.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FDebugger.Stop;
end;

procedure TfrmDebugMasterView.FormDestroy(Sender: TObject);
begin
  FDebugger.Free;
end;

procedure TfrmDebugMasterView.OnVMChange(var Message: TMessage);
begin
  RefreshALL;
end;

procedure TfrmDebugMasterView.PlaceMark(Line: Integer);
var
  Mark: TSynEditMark;
begin
  edVMCode.Marks.ClearLine(Line);
  Mark := TSynEditMark.Create(edVMCode);
  Mark.Line := Line;
  Mark.Char := 1;
  Mark.ImageIndex := 1;
  Mark.Visible := TRUE;
  Mark.InternalImage := False;
  edVMCode.Marks.Place(Mark);
end;

procedure TfrmDebugMasterView.RefreshActiveUnitView;
var
  View: TfrmDebugUnitView;
begin
  View := FUnitViews[FDebugger.CurrUnitID].UnitView;
  View.edSource.Repaint;
  View.MoveToPosition(FDebugger.SRCTextLine);
end;

procedure TfrmDebugMasterView.RefreshALL;
begin
  RefreshRegisters;
  RefreshGlobalVars;
  RefreshLocalVars;
  RefreshCallStack;
  RefreshLinePosition;
  edVMCode.Repaint;
  tbMain.Repaint;
  RefreshActiveUnitView;
end;

procedure TfrmDebugMasterView.RefreshCallStack;
var
  i: Integer;
  P: TVMDebugProc;
begin
  if FDebugger.State = dsStopped then
    Exit;

  vtCallStack.Clear;
  for i := FDebugger.CallStack.Count - 1 downto 0 do
  begin
    P := FDebugger.CallStack[i];
    ShowCSProc(P);
  end;
  vtCallStack.Repaint;
end;

procedure TfrmDebugMasterView.RefreshGlobalVars;
var
  i: Integer;
  V: PVMDebugVar;
begin
  if FDebugger.State = dsStopped then
    Exit;

  for i := 0 to Length(FDebugger.GlobalVars) - 1 do
  begin
    V := addr(FDebugger.GlobalVars[i]);
    ShowVariable(V);
  end;
  vtGloabalVars.Repaint;
end;

procedure TfrmDebugMasterView.RefreshLocalVars;
var
  i: Integer;
  V: PVMDebugVar;
  //Vars: TVMVariables;
  P: TVMDebugProc;
begin
  if FDebugger.State = dsStopped then
    Exit;

  vtLocalVars.Clear;

  P := FDebugger.CurrentProc;
  if P.Name = '' then
    Exit;

  for i := 0 to Length(P.Vars) - 1 do
  begin
    V := addr(P.Vars[i]);
    ShowLocalVariable(V);
  end;
  vtLocalVars.Repaint;
end;

procedure TfrmDebugMasterView.RefreshRegisters;
var
  i: Integer;
  Reg: PVMReg;
begin
  if FDebugger.State = dsStopped then
    Exit;

  for i := 0 to Length(FDebugger.Registers^) - 1 do
  begin
    Reg := addr(FDebugger.Registers[i]);
    ShowRegister('R' + IntToStr(i), IntToStr(Reg.I64) + ' ($' + IntToHex(Reg.I64, 8) + ')');
  end;
  vtRegisters.Repaint;
end;

procedure TfrmDebugMasterView.ShowALLUnits;
var
  i: Integer;
  VI: ^TUnitViewInfo;
begin
  SetLength(FUnitViews, FPackage.UnitsCount);
  for i := 0 to FPackage.UnitsCount - 1 do
  begin
    VI := addr(FUnitViews[i]);
    VI.AUnit := FPackage.Units[i] as TNPUnit;
    VI.TabSheet := TTabSheet.Create(Self);
    VI.TabSheet.Caption := VI.AUnit.Name;
    VI.TabSheet.PageControl := pcUnits;
    VI.UnitView := TfrmDebugUnitView.Create(Self);
    VI.UnitView.Parent := VI.TabSheet;
    VI.UnitView.Align := alClient;
    VI.UnitView.Visible := True;
    VI.UnitView.edSource.Text := VI.AUnit.Source;
    VI.UnitView.UnitID := i;
    VI.UnitView.Debugger := FDebugger;
    VI.UnitView.edSource.OnSpecialLineColors := edSourceSpecialLineColors;
  end;
end;

procedure TfrmDebugMasterView.ShowBreakPoints;
var
  i: Integer;
  BP: TVMDBreakPoint;
begin
  edVMCode.Marks.Clear;
  for i := 0 to FDebugger.BreakPoints.Count - 1 do
  begin
    BP := FDebugger.BreakPoints[i];
    PlaceMark(BP.VMSrcLine);
  end;
end;

procedure TfrmDebugMasterView.ShowCSProc(const V: TVMDebugProc);
var
  Node: PVirtualNode;
  NData: PVMCSData;
begin
  Node := vtCallStack.AddChild(nil);
  NData := vtCallStack.GetNodeData(Node);
  NData.ProcName := V.Name;
  NData.Vars := V.Vars;
end;

procedure TfrmDebugMasterView.ShowLocalVariable(const V: PVMDebugVar);
var
  Node: PVirtualNode;
  NData: PVMVarNodeData;
  NewValue: string;
begin
  Node := vtLocalVars.GetFirst;
  NewValue := FDebugger.GetLocalVarValue(V);
  while Assigned(Node) do begin
    NData := vtLocalVars.GetNodeData(Node);
    if NData.Variable = V then
    begin
      NData.Modified := (NewValue <> NData.Value);
      NData.Value := NewValue;
      Exit;
    end;
    Node := vtLocalVars.GetNext(Node);
  end;
  Node := vtLocalVars.AddChild(nil);
  NData := vtLocalVars.GetNodeData(Node);
  NData.Variable := V;
  NData.Value := NewValue;
  NData.Modified := False;
end;

procedure TfrmDebugMasterView.ShowRegister(const Name, Value: string);
var
  Node: PVirtualNode;
  NData: PVMRegNodeData;
begin
  Node := vtRegisters.GetFirst;
  while Assigned(Node) do begin
    NData := vtRegisters.GetNodeData(Node);
    if NData.Name = Name then
    begin
      NData.Modified := (Value <> NData.Value);
      NData.Value := Value;
      Exit;
    end;
    Node := vtRegisters.GetNext(Node);
  end;
  Node := vtRegisters.AddChild(nil);
  NData := vtRegisters.GetNodeData(Node);
  NData.Name := Name;
  NData.Value := Value;
  NData.Modified := False;
end;

procedure TfrmDebugMasterView.ShowVariable(const V: PVMDebugVar);
var
  Node: PVirtualNode;
  NData: PVMVarNodeData;
  NewValue: string;
begin
  Node := vtGloabalVars.GetFirst;
  NewValue := FDebugger.GetGlobalVarValue(V);
  while Assigned(Node) do begin
    NData := vtGloabalVars.GetNodeData(Node);
    if NData.Variable = V then
    begin
      NData.Modified := (NewValue <> NData.Value);
      NData.Value := NewValue;
      Exit;
    end;
    Node := vtGloabalVars.GetNext(Node);
  end;
  Node := vtGloabalVars.AddChild(nil);
  NData := vtGloabalVars.GetNodeData(Node);
  NData.Variable := V;
  NData.Value := NewValue;
  NData.Modified := False;
end;

procedure TfrmDebugMasterView.SpTBXItem6Click(Sender: TObject);
begin
  FDebugger.RunToBreakPoint;
  Sleep(50);
  RefreshALL;
end;

procedure TfrmDebugMasterView.VirtualStringTree1FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  NData: PVMVarNodeData;
begin
  NData := Sender.GetNodeData(Node);
  Finalize(NData^);
end;

function TfrmDebugMasterView.GetStepType: TStepType;
begin
  if pcUnits.ActivePage = tsASMView then
    Result := StepASM
  else
    Result := StepSRC;
end;

procedure TfrmDebugMasterView.vtCallStackDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  case Column of
    0: if Node.Index = 0 then
         TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold]
       else
         TargetCanvas.Font.Style := TargetCanvas.Font.Style - [fsBold];
  end;
end;

procedure TfrmDebugMasterView.vtCallStackFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  NData: PVMCSData;
begin
  NData := Sender.GetNodeData(Node);
  Finalize(NData^);
end;

procedure TfrmDebugMasterView.vtCallStackGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TVMCSData);
end;

procedure TfrmDebugMasterView.vtCallStackGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  NData: PVMCSData;
begin
  NData := Sender.GetNodeData(Node);
  case Column of
    0: CellText := NData.ProcName;
  end;
end;

procedure TfrmDebugMasterView.vtGloabalVarsGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TVMVarNodeData);
end;

procedure TfrmDebugMasterView.vtGloabalVarsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  NData: PVMVarNodeData;
begin
  NData := Sender.GetNodeData(Node);
  case Column of
    0: CellText := NData.Variable.Name;
    1: CellText := NData.Value;
    2: CellText := FDebugger.VM.GetTypeName(NData.Variable.TypeInfo);
  end;
end;

procedure TfrmDebugMasterView.vtLocalVarsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  NData: PVMVarNodeData;
begin
  NData := Sender.GetNodeData(Node);
  Finalize(NData^);
end;

procedure TfrmDebugMasterView.vtRegistersDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  NData: PBaseNodeData;
begin
  NData := Sender.GetNodeData(Node);
  case Column of
    1: if NData.Modified then
         TargetCanvas.Font.Color := clRed
       else
         TargetCanvas.Font.Color := clBlack;
  end;
end;

procedure TfrmDebugMasterView.vtRegistersFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  NData: PVMRegNodeData;
begin
  NData := Sender.GetNodeData(Node);
  Finalize(NData^);
end;

procedure TfrmDebugMasterView.vtRegistersGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TVMRegNodeData);
end;

procedure TfrmDebugMasterView.vtRegistersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  NData: PVMRegNodeData;
begin
  NData := Sender.GetNodeData(Node);
  case Column of
    0: CellText := NData.Name;
    1: CellText := NData.Value;
  end;
end;

end.
