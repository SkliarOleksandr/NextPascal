unit CATMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, System.IOUtils, System.Types,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, SynEdit, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ImgList,
  Generics.Collections, SynEditHighlighter, SynHighlighterPas, SynHighlighterGeneral, Vcl.Buttons, System.Actions,
  Vcl.ActnList, SynEditTypes, System.UITypes, IL2VMTranslator, VM.Core, NPCompiler.DataTypes, IL.TypeInfo, SystemUnit,
  IniFiles, Vcl.Menus, Vcl.ToolWin, System.ImageList, NPCompiler, RTTI, NPCompiler.Classes,
  NPCompiler.Utils, VM.Invoke, SynEditMiscClasses, SynEditSearch, DebugMasterView, Math, SynCompletionProposal, SpTBXItem,
  SpTBXControls, SynEditCodeFolding, IL2LLVMTranslator; //system

type

  TNodeType = (ntFolder, ntTest);
  TTestResult = (trNone, trPass, trFail, trSkip);
  TVMRunResult = (VMTranslationFail, VMRunFail, VMRunPass);
  TNodeData = record
  private
    FSource: string;
    //FOut: string;
    function GetSource: string;
    function GetChecked: Boolean;
  public
    NodeType: TNodeType;
    Caption: string;
    FilePath: string;
    Result: TTestResult;
    Modified: Boolean;
    Node: PVirtualNode;
    Package: INPPackage;
    property Source: string read GetSource;
    property Checked: Boolean read GetChecked;
  end;
  PNodeData = ^TNodeData;



  TfrmCATMain = class(TForm)
    Panel1: TPanel;
    vtTests: TVirtualStringTree;
    Splitter1: TSplitter;
    PageControl1: TPageControl;
    tsMessages: TTabSheet;
    edMessages: TSynEdit;
    Panel2: TPanel;
    tsSrcEditor: TTabSheet;
    ImageList1: TImageList;
    tmLoader: TTimer;
    SynPasSyn1: TSynPasSyn;
    edSource: TSynEdit;
    Panel3: TPanel;
    btnSaveSrc: TBitBtn;
    ActionList1: TActionList;
    actRunAllTests: TAction;
    BitBtn1: TBitBtn;
    actSaveSrc: TAction;
    BitBtn2: TBitBtn;
    actRunTest: TAction;
    edSrcMessages: TSynEdit;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    PageControl2: TPageControl;
    tsILCode: TTabSheet;
    tsVMCode: TTabSheet;
    edVMCode: TSynEdit;
    tsVMOut: TTabSheet;
    edVMOut: TSynEdit;
    stBar: TStatusBar;
    BitBtn3: TBitBtn;
    actRunAllTest100: TAction;
    actRunTest100Times: TAction;
    BitBtn4: TBitBtn;
    actCompileTest: TAction;
    BitBtn5: TBitBtn;
    pmTests: TPopupMenu;
    actAddTest: TAction;
    Addnewtest1: TMenuItem;
    Button1: TButton;
    pmExtraSpeedTests: TPopupMenu;
    Totalspeedtest1: TMenuItem;
    Expressionstest1: TMenuItem;
    Boolexpressionsspeedtest1: TMenuItem;
    Procinlinespeedtest1: TMenuItem;
    Procoverloadspeedtest1: TMenuItem;
    actExpandAll: TAction;
    actColapseAll: TAction;
    N1: TMenuItem;
    Expandall1: TMenuItem;
    Colapseall1: TMenuItem;
    cbReportMemLeaks: TCheckBox;
    actDelete: TAction;
    N2: TMenuItem;
    Deletetest1: TMenuItem;
    actCreateDir: TAction;
    Createdirectory1: TMenuItem;
    actRename: TAction;
    Rename1: TMenuItem;
    actSavePrepared: TAction;
    Button2: TButton;
    chkIncludeRTTI: TCheckBox;
    edILCode: TSynEdit;
    cbRTTICharset: TComboBox;
    Label1: TLabel;
    chkbStopOnAssert: TCheckBox;
    SynHL: TSynGeneralSyn;
    btnParseOnly: TButton;
    SynEditSearch1: TSynEditSearch;
    FindDialog1: TFindDialog;
    pmSource: TPopupMenu;
    Find1: TMenuItem;
    pmILCode: TPopupMenu;
    MenuItem1: TMenuItem;
    pmVMCode: TPopupMenu;
    MenuItem2: TMenuItem;
    FindDialog2: TFindDialog;
    FindDialog3: TFindDialog;
    BitBtn6: TBitBtn;
    actDebug: TAction;
    chkRunCppVMTest: TCheckBox;
    tsConsole: TTabSheet;
    edConsole: TSynEdit;
    cbShwoOutMode: TComboBox;
    actCheckAll: TAction;
    actUncheckAll: TAction;
    CheckAll1: TMenuItem;
    UncheckAll1: TMenuItem;
    N3: TMenuItem;
    SynCompletionProposal1: TSynCompletionProposal;
    chkbReduceTMPVars: TCheckBox;
    chkEleminateUnusedVars: TCheckBox;
    tsCanvas: TTabSheet;
    Image1: TImage;
    btnToWASM: TButton;
    btnToJS: TButton;
    procedure vtTestsGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure tmLoaderTimer(Sender: TObject);
    procedure vtTestsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vtTestsDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vtTestsNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure actRunAllTestsExecute(Sender: TObject);
    procedure vtTestsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure edSourceChange(Sender: TObject);
    procedure actSaveSrcUpdate(Sender: TObject);
    procedure actSaveSrcExecute(Sender: TObject);
    procedure actRunTestUpdate(Sender: TObject);
    procedure actRunTestExecute(Sender: TObject);
    procedure vtTestsChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
    procedure actRunAllTest100Execute(Sender: TObject);
    procedure actRunTest100TimesExecute(Sender: TObject);
    procedure actCompileTestExecute(Sender: TObject);
    procedure actAddTestUpdate(Sender: TObject);
    procedure actAddTestExecute(Sender: TObject);
    procedure Totalspeedtest1Click(Sender: TObject);
    procedure Expressionstest1Click(Sender: TObject);
    procedure Boolexpressionsspeedtest1Click(Sender: TObject);
    procedure Procinlinespeedtest1Click(Sender: TObject);
    procedure Procoverloadspeedtest1Click(Sender: TObject);
    procedure actExpandAllExecute(Sender: TObject);
    procedure actColapseAllExecute(Sender: TObject);
    procedure cbReportMemLeaksClick(Sender: TObject);
    procedure actCreateDirExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actDeleteUpdate(Sender: TObject);
    procedure actRenameExecute(Sender: TObject);
    procedure actSavePreparedExecute(Sender: TObject);
    procedure cbRTTICharsetChange(Sender: TObject);
    procedure btnParseOnlyClick(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure FindDialog2Find(Sender: TObject);
    procedure FindDialog3Find(Sender: TObject);
    procedure actDebugExecute(Sender: TObject);
    procedure actCheckAllExecute(Sender: TObject);
    procedure actUncheckAllExecute(Sender: TObject);
    procedure vtTestsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure SynCompletionProposal1Execute(Kind: SynCompletionType; Sender: TObject; var CurrentInput: string; var x, y: Integer;
      var CanExecute: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnToWASMClick(Sender: TObject);
    procedure btnToJSClick(Sender: TObject);
  private
    { Private declarations }
    FTests: TList<PNodeData>;
    FILStream: TMemoryStream;
    FSelected: PNodeData;
    FIniFile: TIniFile;
    FRootPath: string;
    FLastSelectedTest: string;
    FVMStream: TMemoryStream;
    FRTTICharset: TRTTICharset;
    FUnitsPath: string;
    FNodePath: string;
  public
    { Public declarations }
    procedure LoadTests(RootNode: PVirtualNode; const RootPath: string);
    procedure ShowVMOut(VM: TILMachine);
    procedure SelectTest(NodeData: PNodeData);
    procedure RunCPPVMTest(const BinPath: string; Custom: Boolean);
    function RunTest(NodeData: PNodeData; Custom: Boolean = False; RunVMCount: Integer = 0; TranslateOnly: Boolean = False): TTestResult; overload;
    function RunTest(const Src: string; RunVMCount: Integer = 0): TTestResult; overload;
    function RunVMTest(NodeData: PNodeData; RunCount: Integer; ILStream: TStream;
                       Custom: Boolean; TranslateOnly: Boolean; out ErrorText: string; out RunTime: TDateTime): TVMRunResult;
    function AddNode(Parent: PVirtualNode; NodeType: TNodeType; const Caption, FilePath: string): PVirtualNode;
    procedure CWriteStr(const Str: string);
    procedure CWriteInt(const Str: Integer);
    procedure CWriteInt64(const Str: Int64);
    procedure BuildLLVM(const Src: string);
    function RunJSScript(const FileName: string): TTestResult;
  end;

var
  frmCATMain: TfrmCATMain;

implementation

{$R *.dfm}

uses NPCompiler.Intf,
     vm.sys.console,
     VM_System,
     VM_SysUtils,
     VM_DateUtils,
     VM_Forms,
     VM_Variants,
     NPCompiler.Package,
     VM_INTF, NPCompiler.Parser, iDStringParser, VM_Canvas, IL2WASMTranslator, IL2JSTranslator;

var
  _CurKey: Integer = 0;

type
  TVTEnumerateProc = reference to procedure(const Node: PVirtualNode);

procedure Enumerate(VT: TVirtualStringTree; const Proc: TVTEnumerateProc);
var
  Node: PVirtualNode;
begin
  Node := VT.GetFirst;
  while Assigned(Node) do
  begin
    Proc(Node);
    Node := VT.GetNext(Node);
  end;
end;


{ TfrmCATMain }

procedure TfrmCATMain.actRenameExecute(Sender: TObject);
var
  Path, NewName: string;
  NodeData: PNodeData;
begin
  NodeData := vtTests.GetNodeData(vtTests.FocusedNode);

  NewName := NodeData.Caption;
  if not InputQuery('Rename', 'Specify a name', NewName) then
    Exit;

  Path := ExtractFilePath(NodeData.FilePath);
  if NodeData.NodeType = ntFolder then begin
    if not RenameFile(NodeData.FilePath, Path + NewName) then
      RaiseLastOSError;
  end
  else begin
    if not RenameFile(NodeData.FilePath, Path + NewName + '.pas') then
      RaiseLastOSError;
  end;
  NodeData.Caption := NewName;
  NodeData.FilePath := Path + NewName + '.pas';

  if FSelected = NodeData then
    FIniFile.WriteString('COMMON', 'LAST_SELECTED', NodeData.FilePath);
end;

procedure TfrmCATMain.actRunAllTest100Execute(Sender: TObject);
var
  i, j: Integer;
  NodeData: PNodeData;
begin
  for j := 0 to 100 - 1 do begin
    edMessages.Clear;
    PageControl1.ActivePage := tsMessages;
    for i := 0 to FTests.Count - 1 do
    begin
      NodeData := PNodeData(FTests[i]);
      NodeData.Result := RunTest(NodeData);
      vtTests.Repaint;
    end;
  end;
end;

procedure TfrmCATMain.actRunAllTestsExecute(Sender: TObject);
var
  i, PassCnt, FailCnt, SkipCnt: Integer;
  NodeData: PNodeData;
  dt: TDateTime;
begin
  PassCnt := 0;
  FailCnt := 0;
  SkipCnt := 0;
  edMessages.Clear;
  PageControl1.ActivePage := tsMessages;
  for i := 0 to FTests.Count - 1 do
  begin
    NodeData := PNodeData(FTests[i]);
    NodeData.Result := trNone;
  end;
  vtTests.Repaint;
  dt := Now;
  for i := 0 to FTests.Count - 1 do
  begin
    NodeData := PNodeData(FTests[i]);
    if not NodeData.Checked then
      continue;
    stBar.Panels[4].Text := 'Process test: ' + NodeData.FilePath;
    stBar.Repaint;
    try
      try
        NodeData.Result := RunTest(NodeData, False, 1);
      except
        NodeData.Result := trFail;
        Exit;
      end;
    finally
      vtTests.Repaint;
    end;
    if NodeData.Result = trPass then
      Inc(PassCnt)
    else
    if NodeData.Result = trFail then
    begin
      Inc(FailCnt);
      if chkbStopOnAssert.Checked then
        Exit;
    end;
    if NodeData.Result = trSkip then
      Inc(SkipCnt);
    vtTests.Repaint;
    stBar.Panels[1].Text := format('Passed: %d', [PassCnt]);
    stBar.Panels[2].Text := format('Failed: %d', [FailCnt]);
    stBar.Panels[3].Text := format('Skiped: %d', [SkipCnt]);
    stBar.Repaint;
  end;
  stBar.Panels[4].Text := 'Process test: Done at ' + FormatDateTime('HH:NN:SS.ZZZ', Now - dt);
  TPooledObject.ClearPool;
end;

procedure TfrmCATMain.actRunTest100TimesExecute(Sender: TObject);
begin
  FSelected.FSource := edSource.Text;
  RunTest(FSelected, True, 1000);
end;

procedure TfrmCATMain.actAddTestExecute(Sender: TObject);
var
  s, path: string;
  ParentNode, Node: PVirtualNode;
  Data: PNodeData;
  Stream: TStringStream;
begin
  s := 'New_test';
  if not InputQuery('New test', 'Test name', s) then
    Exit;

  ParentNode := vtTests.FocusedNode;
  if PNodeData(vtTests.GetNodeData(ParentNode)).NodeType = ntTest then
    ParentNode := ParentNode.Parent;

  Data := vtTests.GetNodeData(ParentNode);

  path := Data.FilePath + '\' + s + '.pas';

  Stream := TStringStream.Create;
  try
    Stream.WriteString('unit ' + s + ';'#10);
    Stream.WriteString(#10);
    Stream.WriteString('interface'#10);
    Stream.WriteString(#10);
    Stream.WriteString('implementation'#10);
    Stream.WriteString(#10);
    Stream.WriteString('procedure Test;'#10);
    Stream.WriteString('begin'#10);
    Stream.WriteString(#10);
    Stream.WriteString('end;'#10);
    Stream.WriteString(#10);
    Stream.WriteString('initialization'#10);
    Stream.WriteString('  Test();'#10);
    Stream.WriteString(#10);
    Stream.WriteString('finalization'#10);
    Stream.WriteString(#10);
    Stream.WriteString('end.');
    Stream.SaveToFile(path);
  finally
    Stream.Free;
  end;

  Node := AddNode(ParentNode, ntTest, s, path);

  vtTests.FocusedNode := Node;
  vtTests.Selected[Node] := True;

  Data := vtTests.GetNodeData(Node);
  FTests.Add(Data);
  SelectTest(Data);
  FIniFile.WriteString('COMMON', 'LAST_SELECTED', ExtractRelativePath(Application.ExeName, Data.FilePath));

  stBar.Panels[0].Text := format('Total tests: %d', [FTests.Count]);
end;

procedure TfrmCATMain.actAddTestUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(vtTests.FocusedNode);
end;

procedure TfrmCATMain.actCheckAllExecute(Sender: TObject);
begin
  Enumerate(vtTests, procedure(const Node: PVirtualNode)
                     begin
                       Node.CheckState := csCheckedNormal;
                     end);
  vtTests.Repaint;
end;

procedure TfrmCATMain.actUncheckAllExecute(Sender: TObject);
begin
  Enumerate(vtTests, procedure(const Node: PVirtualNode)
                     begin
                       Node.CheckState := csUncheckedNormal;
                     end);
  vtTests.Repaint;
end;

procedure TfrmCATMain.actColapseAllExecute(Sender: TObject);
begin
  vtTests.FullCollapse();
end;

procedure TfrmCATMain.actCompileTestExecute(Sender: TObject);
begin
  RunTest(edSource.Text, 0);
end;

procedure TfrmCATMain.actCreateDirExecute(Sender: TObject);
var
  Path, DirName: string;
  NodeData: PNodeData;
begin
  NodeData := vtTests.GetNodeData(vtTests.FocusedNode);

  if Assigned(NodeData) then begin
    if NodeData.NodeType = ntFolder then
      Path := NodeData.FilePath
    else
      Path := ExtractFilePath(NodeData.FilePath);
  end else
    Path := FRootPath;

  DirName := 'New';
  if not InputQuery('New directory', 'Specify a directory name', DirName) then
    Exit;

  Path := Path + '\' + DirName;

  if not ForceDirectories(Path) then
    RaiseLastOSError;

  AddNode(vtTests.FocusedNode, ntFolder, DirName, Path);
end;

procedure TfrmCATMain.actDebugExecute(Sender: TObject);
var
  frm: TfrmDebugMasterView;
begin
  frm := TfrmDebugMasterView.Create(Self);
  try
    frm.UnitsSearchPath := FUnitsPath;
    frm.Debug(edSource.Text, FRTTICharset);
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TfrmCATMain.actDeleteExecute(Sender: TObject);
var
  i: Integer;
  Data: PNodeData;
begin
  Data := vtTests.GetNodeData(vtTests.FocusedNode);
  if MessageDlg(Format('Delete test: %s', [Data.Caption]), mtConfirmation, [mbYes, mbCancel], 0) <> mrYes then
    Exit;
  if Data.NodeType = ntTest then
  begin
    i := FTests.IndexOf(Data);
    if i <> - 1 then
      FTests.Delete(i);
    if not DeleteFile(Data.FilePath) then
      RaiseLastOSError;
  end else
    TDirectory.Delete(Data.FilePath);
  vtTests.DeleteNode(vtTests.FocusedNode);
  SelectTest(nil);
  stBar.Panels[0].Text := format('Total tests: %d', [FTests.Count]);
end;

procedure TfrmCATMain.actDeleteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(vtTests.FocusedNode);
end;

procedure TfrmCATMain.actExpandAllExecute(Sender: TObject);
begin
  vtTests.FullExpand();
end;

procedure TfrmCATMain.actFindExecute(Sender: TObject);
begin
  FindDialog1.Execute(Self.Handle);
end;

procedure TfrmCATMain.actRunTestExecute(Sender: TObject);
begin
  FSelected.FSource := edSource.Text;
  RunTest(FSelected, True, 1);
end;

procedure TfrmCATMain.actRunTestUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(FSelected);
end;

procedure TfrmCATMain.actSavePreparedExecute(Sender: TObject);
begin
  FSelected.FSource := edSource.Text;
  RunTest(FSelected, True, 1, True);
  FVMStream.SaveToFile(ChangeFileExt(FSelected.FilePath, '.bin'));
end;

procedure TfrmCATMain.actSaveSrcExecute(Sender: TObject);
begin
  edSource.Lines.SaveToFile(FSelected.FilePath);
  FSelected.FSource := edSource.Text;
  FSelected.Modified := False;
end;

procedure TfrmCATMain.actSaveSrcUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(FSelected) and FSelected.Modified;
end;

function TfrmCATMain.AddNode(Parent: PVirtualNode; NodeType: TNodeType; const Caption, FilePath: string): PVirtualNode;
var
  NodeData: PNodeData;
begin
  Result := vtTests.AddChild(Parent);
  Result.CheckType := ctCheckBox;
  Result.CheckState := csCheckedNormal;
  NodeData := vtTests.GetNodeData(Result);
  NodeData.NodeType := NodeType;
  NodeData.Caption := Caption;
  NodeData.FilePath := FilePath;
  NodeData.Modified := False;
  NodeData.Node := Result;
end;

procedure TfrmCATMain.Boolexpressionsspeedtest1Click(Sender: TObject);
var
  i, c: Integer;
begin
  PageControl1.ActivePage := tsSrcEditor;
  with edSource do begin
    Lines.BeginUpdate;
    try
      clear;
      Lines.Add('unit bool_expression_speed_test;');
      Lines.Add('interface');
      Lines.Add('procedure Test; export;');
      Lines.Add('implementation');
      Lines.Add('procedure Test;');
      Lines.Add('var');
      Lines.Add('  a, b, c, d: Int32;');
      Lines.Add('  bl1: Boolean;');
      Lines.Add('begin');
      c := 200000;
      //Lines.Add('  a := 0; b := 0; c := 0; d := 0;');
      //Lines.Add('  bl1 := FALSE;');
      for i := 1 to c do begin
        Lines.Add('  if ((a = 1 or a = 2) and (b = 1 or b = 2)) or (c = 1 or (a = b and c = d)) then');
        //Lines.Add('  if (((a = 1) or (a = 2)) and ((b = 1) or (b = 2))) or ((c = 1) or ((a = b) and (c = d))) then');
        Lines.Add('    bl1 := true');
        Lines.Add('  else ');
        Lines.Add('    bl1 := false; ');
      end;
      //Lines.Add('  //if (a > 0) and (b > 0) and (c > 0) and (d > 0) then;');
      Lines.Add('end;');
      Lines.Add('end.');
    finally
      Lines.EndUpdate;
    end;
  end;
end;

procedure TfrmCATMain.btnParseOnlyClick(Sender: TObject);
var
  P: TNPParser;
  dt: TDateTime;
begin
  p := TNPParser.Create(edSource.Text);
  try
    p.First;
    dt := Now;
    while P.NextToken <> token_eof do;
    edSrcMessages.Text := 'Parse complete at: ' + FormatDateTime('HH:NN:SS.ZZZ', now - dt);
  finally
    P.Free;
  end;
end;

procedure SaveWASMHTML(const FileName: string; Stream: TMemoryStream);
var
  HTML, Bytes: AnsiString;
  FStream: TFileStream;
  Ptr: PByte;
  i: Integer;
begin
  ////////////////////////
  Ptr := Stream.Memory;
  for i := 0 to Stream.Size - 1 do
  begin
    Bytes := AddStringSegment(Bytes, AnsiString(IntToStr(Ptr^)), AnsiString(','));
    Inc(Ptr);
  end;

  HTML :=
  '<!DOCTYPE html>'#13#10 +
  '<html>'#13#10 +
  '<body>'#13#10 +
  ''#13#10 +
  '<canvas id="myCanvas" width="1024" height="1024">'#13#10 +
  ''#13#10 +
  '<script>'#13#10 +
  ''#13#10 +
  'const wasmImports = {'#13#10 +
  '  env: {'#13#10 +
  '    xlog: console.log'#13#10 +
  '  }'#13#10 +
  '};'#13#10 +
  'var wasmCode = new Uint8Array('#13#10 +
    '[' + Bytes + ']'#13#10 +
  ');'#13#10 +
  'var wasmModule = new WebAssembly.Module(wasmCode);'#13#10 +
  'var wasmInstance = new WebAssembly.Instance(wasmModule, wasmImports);'#13#10 +
  'console.log("wasm test...");'#13#10 +
  'console.log(wasmInstance.exports.Test(12));'#13#10 +
//  ''#13#10 +
  '</script>'#13#10 +
//  ''#13#10 +
  '</body>'#13#10 +
  '</html>'#13#10;

  FStream := TFileStream.Create(FileName, fmCreate);
  try
    FStream.WriteBuffer(HTML[1], Length(HTML));
  finally
    FStream.Free;
  end;
end;

procedure SaveJSHTML(const FileName: string; Stream: TMemoryStream);
var
  HTML: AnsiString;
  FStream: TFileStream;
  Str: TStringStream;
begin

  Str := TStringStream.Create;
  try
    Stream.Position := 0;
    Str.CopyFrom(Stream, Stream.Size);
    HTML :=
    '<!DOCTYPE html>'#13#10 +
    '<html>'#13#10 +
    '<body>'#13#10 +
    ''#13#10 +
    '<script>'#13#10 +
    AnsiString(Str.DataString) +
    '</script>'#13#10 +
    '</body>'#13#10 +
    '</html>'#13#10;
  finally
    Str.Free;
  end;

  FStream := TFileStream.Create(FileName, fmCreate);
  try
    FStream.WriteBuffer(HTML[1], Length(HTML));
  finally
    FStream.Free;
  end;
end;

procedure SaveJSScript(const FileName: string; Stream: TMemoryStream);
var
  FStream: TFileStream;
begin
  FStream := TFileStream.Create(FileName, fmCreate);
  try
    Stream.Position := 0;
    FStream.CopyFrom(Stream, Stream.Size);
  finally
    FStream.Free;
  end;
end;

procedure TfrmCATMain.btnToJSClick(Sender: TObject);
var
  JS: TIL2JSTranslator;
  Stream: TMemoryStream;
  Path, FileName: string;
begin
  FSelected.FSource := edSource.Text;
  RunTest(FSelected, True, 0, True);

  JS := TIL2JSTranslator.Create;
  try
    FILStream.Position := 0;
    JS.LoadILCode(FILStream);
    Stream := TMemoryStream.Create;
    try
      JS.SaveTargetCode(Stream);
      Path := 'D:\Softbuild\TPC\OUT\Bin\JS-Frontend\';
      ForceDirectories(Path);
      FileName := Path + ExtractFileName(FSelected.FilePath) + '.html';
      SaveJSHTML(FileName, Stream);

      Path := 'D:\Softbuild\TPC\OUT\Bin\JS-Backend\';
      ForceDirectories(Path);
      FileName := Path + ExtractFileName(FSelected.FilePath) + '.js';
      SaveJSScript(FileName, Stream);

      RunJSScript(FileName);
    finally
      Stream.Free;
    end;
  finally
    JS.Free;
  end;
end;

procedure TfrmCATMain.btnToWASMClick(Sender: TObject);
var
  WT: TWASMTranslator;
  Stream: TMemoryStream;
begin
  FSelected.FSource := edSource.Text;
  RunTest(FSelected, True, 0, True);

  WT := TWASMTranslator.Create;
  try
    FILStream.Position := 0;
    WT.LoadILCode(FILStream);
    Stream := TMemoryStream.Create;
    try
      WT.SaveTargetCode(Stream);
      SaveWASMHTML('D:\Softbuild\TPC\OUT\Bin\' + ExtractFileName(FSelected.FilePath) + '.html', Stream);
    finally
      Stream.Free;
    end;
  finally
    WT.Free;
  end;
end;

procedure TfrmCATMain.BuildLLVM(const Src: string);
var
  TR: TLLVMTranslator;
begin
 { TR := TLLVMTranslator.Create;
  try

  finally
    TR.Free;
  end;}
end;

procedure TfrmCATMain.cbReportMemLeaksClick(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := cbReportMemLeaks.Checked;
end;

procedure TfrmCATMain.cbRTTICharsetChange(Sender: TObject);
begin
  FRTTICharset := TRTTICharset(cbRTTICharset.ItemIndex);
end;

procedure TfrmCATMain.edSourceChange(Sender: TObject);
begin
  if Assigned(FSelected) then
    FSelected.Modified := True;
end;

procedure TfrmCATMain.Expressionstest1Click(Sender: TObject);
var
   i, c: Integer;
begin
  PageControl1.ActivePage := tsSrcEditor;
  with edSource do begin
    Lines.BeginUpdate;
    try
      Clear;
      Lines.Add('unit expression_speed_test;');
      Lines.Add('interface');
      Lines.Add('procedure Test; export;');
      Lines.Add('implementation');
      Lines.Add('procedure Test;');
      Lines.Add('var');
      Lines.Add('  a, b, c, d: int32;');
      Lines.Add('begin');
      c := 500000;
      Lines.Add('  a, b, c, d := 1;');
      for i := 1 to c do
       // Lines.Add('  a := -((a + b)/c - (a - (b - (c + d)*a)*a) + (c + d)/a)*-b;');
        Lines.Add('  a := - a + b + c - d + a + c*d - a div b + c;');
        //Lines.Add('  a := - a + a + a - a + a + a*a - a div a + a;');
      Lines.Add('  if (a > b) or (b > c) then a := 10;');
      Lines.Add('end;');
      Lines.Add('end.');
    finally
      Lines.EndUpdate;
    end;
  end;
end;

function VCLFindOptionsToSynSearchOptions(const FindOptions: TFindOptions): TSynSearchOptions;
begin
  Result := [];
  if not (TFindOption.frDown in FindOptions) then
    Include(Result, TSynSearchOption.ssoBackwards);

  if TFindOption.frMatchCase in FindOptions then
    Include(Result, TSynSearchOption.ssoMatchCase);

  if TFindOption.frWholeWord in FindOptions then
    Include(Result, TSynSearchOption.ssoWholeWord);

  if TFindOption.frReplace in FindOptions then
    Include(Result, TSynSearchOption.ssoReplace);

  if TFindOption.frReplaceAll in FindOptions then
    Include(Result, TSynSearchOption.ssoReplaceAll);
end;

procedure TfrmCATMain.Find1Click(Sender: TObject);
begin
  FindDialog1.Execute(Self.Handle);
end;

procedure TfrmCATMain.FindDialog1Find(Sender: TObject);
begin
  edSource.SearchReplace(FindDialog1.FindText, '', VCLFindOptionsToSynSearchOptions(FindDialog1.Options));
end;

procedure TfrmCATMain.FindDialog2Find(Sender: TObject);
begin
  edILCode.SearchReplace(FindDialog2.FindText, '', VCLFindOptionsToSynSearchOptions(FindDialog2.Options));
end;

procedure TfrmCATMain.FindDialog3Find(Sender: TObject);
begin
  edVMCode.SearchReplace(FindDialog3.FindText, '', VCLFindOptionsToSynSearchOptions(FindDialog3.Options));
end;

procedure TfrmCATMain.FormCreate(Sender: TObject);
begin
  FTests := TList<PNodeData>.Create;
  PageControl1.ActivePage := tsSrcEditor;
  PageControl2.ActivePage := tsILCode;
  FILStream := TMemoryStream.Create;

  FIniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'CAT.ini');

  FRootPath := FIniFile.ReadString('COMMON', 'TESTS_PATH', 'Tests\');
  if IsRelativePath(FRootPath) then
    FRootPath := (TPath.GetFullPath(ExtractFilePath(Application.ExeName) + FRootPath));

  FUnitsPath := FIniFile.ReadString('COMMON', 'UNITS_PATH', '..\..\Units');
  if IsRelativePath(FUnitsPath) then
    FUnitsPath := IncludeTrailingPathDelimiter(TPath.GetFullPath(ExtractFilePath(Application.ExeName) + FUnitsPath));

  FNodePath := FIniFile.ReadString('COMMON', 'NODEJS_PATH', 'c:\Program Files\nodejs\node.exe');

  FLastSelectedTest := FIniFile.ReadString('COMMON', 'LAST_SELECTED', '');
  if IsRelativePath(FLastSelectedTest) then
    FLastSelectedTest := TPath.GetFullPath(ExtractFilePath(Application.ExeName) + FLastSelectedTest);

  FVMStream := TMemoryStream.Create;
  cbRTTICharset.OnChange(cbRTTICharset);

  {$IFDEF CPUX64}
  Caption := Caption + ' X64';
  {$ENDIF}
  _Canvas := Image1.Canvas;
end;

procedure TfrmCATMain.FormDestroy(Sender: TObject);
begin
  FIniFile.Free;
  FTests.Free;
  FILStream.Free;
  FVMStream.Free;
end;

procedure TfrmCATMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  _CurKey := Key;
end;

procedure TfrmCATMain.LoadTests(RootNode: PVirtualNode; const RootPath: string);
var
  Files: TStringDynArray;
  i: Integer;
  NodeCaption, s: string;
  Node: PVirtualNode;
  NodeData: PNodeData;
begin
  Files := TDirectory.GetFileSystemEntries(RootPath);
  for i := 0 to Length(Files) - 1 do
  begin
    s := Files[i];
    if DirectoryExists(s) then
    begin
      NodeCaption := StringSegment(s, StringSegCount(s, PathDelim), PathDelim);
      Node := AddNode(RootNode, ntFolder, NodeCaption, s);
      LoadTests(Node, s);
    end else begin
      if ExtractFileExt(s) <> '.pas' then
        continue;
      NodeCaption := ExtractFileName(s);
      NodeCaption := StringSegment(NodeCaption, 1, '.');
      Node := AddNode(RootNode, ntTest, NodeCaption, s);
      NodeData := vtTests.GetNodeData(Node);
      FTests.Add(NodeData);

      if NodeData.FilePath = FLastSelectedTest then
      begin
        SelectTest(NodeData);
        vtTests.Selected[Node] := True;
      end;
    end;
  end;
end;

procedure TfrmCATMain.MenuItem1Click(Sender: TObject);
begin
  FindDialog2.Execute(Self.Handle);
end;

procedure TfrmCATMain.MenuItem2Click(Sender: TObject);
begin
  FindDialog3.Execute(Self.Handle);
end;

procedure TfrmCATMain.Procinlinespeedtest1Click(Sender: TObject);
var
  i, c: Integer;
begin
  PageControl1.ActivePage := tsSrcEditor;
  with edSource do begin
    Lines.BeginUpdate;
    try
      clear;
      Lines.Add('unit inline_speed_test;');
      Lines.Add('interface');
      Lines.Add('procedure Test; export;');
      Lines.Add('implementation');
      Lines.Add('var');
      Lines.Add(' G: Int32;');
      Lines.Add('function InlineFunc1(a, b, c: int32): int32; inline;');
      Lines.Add('begin');
      Lines.Add('  Result := a - b + c;');
      Lines.Add('end;');
      Lines.Add('procedure Test;');
      Lines.Add('var');
      Lines.Add(' a, b: Int32;');
      Lines.Add('begin');
      Lines.Add('  a, b := 1;');
      c := 500000;
      for i := 1 to c do begin
        Lines.Add('  G := InlineFunc1(a, b, a + b);');
      end;
      Lines.Add('end;');
      Lines.Add('end.');
    finally
      Lines.EndUpdate;
    end;
  end;
end;

procedure TfrmCATMain.Procoverloadspeedtest1Click(Sender: TObject);
var
  i, c: Integer;
begin
  PageControl1.ActivePage := tsSrcEditor;
  with edSource do begin
    Lines.BeginUpdate;
    try
      Clear;
      Lines.Add('unit overload_speed_test;');
      Lines.Add('interface');
      Lines.Add('function funcA1(a: int16; b: int8; c: int8): string; overload;');
      Lines.Add('function funcA1(a: int16; b: int8; c: int16): string; overload;');
      Lines.Add('function funcA1(a: int16; b: int8; c: int32): string; overload;');
      Lines.Add('function funcA1(a: Variant; b: int8; c: int64): string; overload;');
      Lines.Add('implementation');
      Lines.Add('function funcA1(a: int16; b: int8; c: int8): string;');
      Lines.Add('begin');
      Lines.Add('end;');
      Lines.Add('function funcA1(a: int16; b: int8; c: int16): string;');
      Lines.Add('begin');
      Lines.Add('end;');
      Lines.Add('function funcA1(a: int16; b: int8; c: int32): string;');
      Lines.Add('begin');
      Lines.Add('end;');
      Lines.Add('function funcA1(a: Variant; b: int8; c: int64): string;');
      Lines.Add('begin');
      Lines.Add('end;');
      Lines.Add('procedure Test;');
      Lines.Add('var a: Int16; b: Int32; c: Int64; s: string;');
      Lines.Add('begin');
      c := 1000000;
      for i := 1 to c do
        Lines.Add('  s := funcA1(a, b, c);');
      Lines.Add('end;');

      Lines.Add('end.');
    finally
      Lines.EndUpdate;
    end;
  end;
end;

procedure CompilerMessagesToSynEdit(const Messages: ICompilerMessages; Strings: TStrings);
var
  I: Integer;
  Msg: TCompilerMessage;
begin
  for i := 0 to Messages.Count - 1 do
  begin
    Msg := Messages[i];
    Strings.Add(Msg.AsString);
  end;
end;

procedure ShowCompilerError(const Messages: ICompilerMessages; SynEdit: TSynEdit);
var
  I: Integer;
  Msg: TCompilerMessage;
begin
  SynEdit.BeginUpdate;
  try
    for i := 0 to Messages.Count - 1 do
    begin
      Msg := Messages[i];
      if Msg.MessageType >= cmtError then
      begin
        SynEdit.CaretY := Msg.Row;
        SynEdit.CaretX := Msg.Col;
        SynEdit.SetFocus;
        Exit;
      end;
    end;
  finally
    SynEdit.EndUpdate;
  end;
end;

function TfrmCATMain.RunJSScript(const FileName: string): TTestResult;
var
  RunResult: Boolean;
  NodePath, StdErrorFile: string;
begin
  NodePath := FNodePath;
  StdErrorFile := ExtractFilePath(FileName) + 'stderror.txt';
  RunResult := OpenExe(NodePath, FileName, '', StdErrorFile);
  if RunResult then
    Result := trPass
  else
    Result := trFail;
end;

function TfrmCATMain.RunTest(NodeData: PNodeData; Custom: Boolean = False; RunVMCount: Integer = 0; TranslateOnly: Boolean = False): TTestResult;
var
  UN: TNPUnit;
  i: Integer;
  VMErrorText: string;
  VMRunResult: TVMRunResult;
  Msg: TStrings;
  dt: TDateTime;
  VMRunTime: TDateTime;
  PKG: INPPackage;
begin
  if Assigned(FSelected.Package) then
    FSelected.Package.Clear;

  FreeAndNil(SYSUnit);
  PKG := TNPPackage.Create('test');
  NodeData.Package := PKG;

  PKG.AddUnitSearchPath(FUnitsPath);
  PKG.IncludeDebugInfo := True;
  PKG.RTTICharset := FRTTICharset;
  PKG.InitUnits;

  Msg := TStringList.Create;
  UN := TNPUnit.Create(PKG, NodeData.Source);
  try
    UN.Options.OPT_REUSE_TMP_VARS := chkbReduceTMPVars.Checked;
    UN.Options.OPT_ELEMINATE_UNUSED_LOCAL_VARS := chkEleminateUnusedVars.Checked;
    PKG.AddUnit(SystemUnit.SYSUnit, nil);
    PKG.AddUnit(UN, nil);
    Msg.Add('===================================================================');
    Msg.Add('Test: ' + NodeData.Caption);

    case PKG.Compile() of
      CompileFail: begin
        Msg.Add('Compile: ' + 'FAIL');
        Result := trFail;
      end;
      CompileSkip: begin
        Msg.Add('Compile: ' + 'SKIP');
        Result := trSkip;
      end;
    else
      Msg.Add('Compile: ' + 'PASS');
      FILStream.Size := 0;
      try
        PKG.SaveToStream(FILStream);
      except
        on e: exception do
        begin
          if Custom then
            edSrcMessages.Lines.Add('Save IL to stream ERROR: ' + e.Message)
          else
            edMessages.Lines.Add('Save IL to stream ERROR: ' + e.Message);
          Exit(trFail);
        end;
      end;

      if Custom then
      begin
        edILCode.Clear;
        for i := 0 to PKG.UnitsCount - 1 do
        begin
          edILCode.Lines.Add('UNIT: ' + (PKG.Units[i] as TNPUnit).Name + #10);
          edILCode.Text := edILCode.Text + (PKG.Units[i] as TNPUnit).GetILText ;
        end;
      end;

      if RunVMCount > 0 then
      begin
        if Custom then
          edConsole.Clear;
        dt := Now;
        VMRunResult := VMRunFail;
        try
          VMRunResult := RunVMTest(NodeData, RunVMCount, FILStream, Custom, TranslateOnly, VMErrorText, VMRunTime);

        finally
          case VMRunResult of
            VMTranslationFail: begin
              Msg.Add('VM: TRANSLATION FAIL');
              Msg.Add('VM Error: ' + VMErrorText);
              Result := trFail;
            end;
            VMRunFail: begin
              Msg.Add('VM: RUN FAIL');
              Msg.Add('VM Error: ' + VMErrorText);
              Result := trFail;
            end;
          else
            Msg.Add('VM: PASS at: ' + FormatDateTime('NN:SS.ZZZ', Now - dt));
            Msg.Add('VM: PURE run time: ' + FormatDateTime('NN:SS.ZZZ', VMRunTime));
            Result := trPass;
          end;

          if PKG.Messages.Count > 0 then
          begin
            Msg.Add('Messages: ');
            CompilerMessagesToSynEdit(PKG.Messages, Msg);
            if Custom then
              ShowCompilerError(PKG.Messages, edSource);
          end else
            CompilerMessagesToSynEdit(PKG.Messages, Msg);

          if Custom then begin
            edSrcMessages.Clear;
            edSrcMessages.Lines.AddStrings(Msg);
          end else
            edMessages.Lines.AddStrings(Msg);
        end;
      end else
        Result := trPass;
    end;

  finally
    Msg.Free;
    //Package.Clear;
    //FreeAndNil(SYSUnit);
  end;
end;

procedure TfrmCATMain.RunCPPVMTest(const BinPath: string; Custom: Boolean);
var
  RootPath, TestAppPath, cmd, OutFile: string;
  StartupInfo : TStartupInfo;
  ProcessInfo : TProcessInformation;
  Stream: TFileStream;
  NeedDelete: Boolean;
begin
  NeedDelete := False;
  RootPath := ExtractFilePath(Application.ExeName);

  if not Custom then begin
    OutFile := RootPath + '\CRVMTestOut\' + ChangeFileExt(ExtractFileName(BinPath), '.txt');
    Stream := TFileStream.Create(OutFile, fmCreate);
    SetHandleInformation(Stream.Handle, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT);
  end else begin
    OutFile := '';
    Stream := nil;
  end;

  try
    GetStartupInfo(StartupInfo);
    if not Custom then
    begin
      StartupInfo.wShowWindow := SW_HIDE; // не показывать окно
      StartupInfo.dwFlags := StartupInfo.dwFlags or STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      StartupInfo.hStdOutput  := Stream.Handle; // присваиваем рукоятку на свой файл
    end;

    TestAppPath := RootPath + 'CRVMTest.exe';

    cmd := TestAppPath + ' "' + BinPath + '" s';
    if CreateProcess(nil, PChar(cmd), nil, nil, True, NORMAL_PRIORITY_CLASS, nil, PChar(RootPath), StartupInfo, ProcessInfo) then
      WaitforSingleObject(ProcessInfo.hProcess, 1000)
    else
      RaiseLastOSError;
    CloseHandle(ProcessInfo.hProcess);
    if not Custom then
      NeedDelete := (Stream.Size = 0);
  finally
    Stream.Free;
  end;
  if NeedDelete then
    DeleteFile(OutFile);
end;

function TfrmCATMain.RunTest(const Src: string; RunVMCount: Integer): TTestResult;
var
  i: Integer;
  UN: TNPUnit;
  Msg: TStrings;
  PKG: INPPackage;
  StartDt: TDateTime;
begin
  if Assigned(FSelected.Package) then
    FSelected.Package.Clear;

  FreeAndNil(SYSUnit);

  PKG := TNPPackage.Create('test');
  FSelected.Package := PKG;
  PKG.AddUnitSearchPath(FUnitsPath);
  PKG.IncludeDebugInfo := True;
  PKG.RTTICharset := FRTTICharset;
  PKG.InitUnits;

  Msg := TStringList.Create;
  UN := TNPUnit.Create(FSelected.Package, Src);
  try
    UN.Options.OPT_REUSE_TMP_VARS := chkbReduceTMPVars.Checked;
    UN.Options.OPT_ELEMINATE_UNUSED_LOCAL_VARS := chkEleminateUnusedVars.Checked;
    PKG.AddUnit(SystemUnit.SYSUnit, nil);
    PKG.AddUnit(UN, nil);
    Msg.Add('===================================================================');
    Msg.Add('Test: custom');
    StartDt := Now;

    case PKG.Compile() of
      CompileSuccess: begin
        Msg.Add('Compile: ' + 'PASS');
        Result := trPass;
      end;
      CompileFail: begin
        Msg.Add('Compile: ' + 'FAIL');
        Result := trFail;
      end;
      CompileSkip: begin
        Msg.Add('Compile: ' + 'SKIP');
        Result := trSkip;
      end;
    else
      Result := trFail;
    end;

    BuildLLVM('');

    Msg.Add('Compile time: ' + FormatDateTime('HH:NN:SS.ZZZ', Now - StartDt));

    edILCode.Clear;
    for i := 0 to PKG.UnitsCount - 1 do
    begin
      edILCode.Lines.Add('UNIT: ' + (PKG.Units[i] as TNPUnit).Name + #10);
      edILCode.Text := edILCode.Text + (PKG.Units[i] as TNPUnit).GetILText;
    end;

    if PKG.Messages.Count > 0 then
    begin
      Msg.Add('Messages: ');
      CompilerMessagesToSynEdit(PKG.Messages, Msg);
      ShowCompilerError(PKG.Messages, edSource);
    end;
    edSrcMessages.Clear;
    edSrcMessages.Lines.AddStrings(Msg);
  finally
    Msg.Free;
  end;
end;

function TfrmCATMain.RunVMTest(NodeData: PNodeData; RunCount: Integer; ILStream: TStream;
                       Custom: Boolean; TranslateOnly: Boolean; out ErrorText: string; out RunTime: TDateTime): TVMRunResult;
var
  i: Integer;
  M: TILMachine;
  MT: TVMTranslator;
  dt: TDateTime;
  BinPath: string;
begin
  MT := TVMTranslator.Create;
  try
    MT.IncludeRTTI := chkIncludeRTTI.Checked;
    MT.RTTICharset := FRTTICharset;
    try
      ILStream.Position := 0;
      try
        MT.LoadILCode(ILStream);
      finally
        if Custom then
          edVMCode.Text := MT.VMCodeAsString;
      end;
      FVMStream.Size := 0;
      MT.SaveTargetCode(FVMStream);
      if Custom then
        edVMCode.Text := edVMCode.Text + '-------------------------'#10'IMG size: ' + IntToStr(FVMStream.Size);
    except
      on e: exception do begin
        Result := VMTranslationFail;
        ErrorText := e.Message;
        Exit;
      end;
    end;
  finally
    MT.Free;
  end;

  if chkRunCppVMTest.Checked then
  begin
    BinPath := ChangeFileExt(NodeData.FilePath, '.bin');
    FVMStream.SaveToFile(BinPath);
    RunCPPVMTest(BinPath, Custom);
  end;

  M := TILMachine.Create();
  try
    FVMStream.Position := 0;
    try
      M.LoadVMImage(FVMStream);
      dt := Now();
      if not TranslateOnly then
      for i := 1 to RunCount do
      begin
        if cbShwoOutMode.ItemIndex = 0 then
        begin
          M.RunInitSections;
          if Custom then
            ShowVMOut(M);
          M.RunFinalSections;
        end else begin
          M.Run;
          if Custom then
            ShowVMOut(M);
        end;
      end;
      RunTime := Now - dt;
      Result := VMRunPass;
      ErrorText := '';
    except
      on e: exception do
      begin
        Result := VMRunFail;
        ErrorText := e.Message;
        if Custom then
          ShowVMOut(M);
      end;
    end;
  finally
    M.Free;
  end;
end;

procedure TfrmCATMain.SelectTest(NodeData: PNodeData);
begin
  PageControl1.ActivePage := tsSrcEditor;
  if Assigned(NodeData) then
    edSource.Text := NodeData.Source
  else
    edSource.Text := '';
  edILCode.Clear;
  edVMCode.Clear;
  edVMOut.Clear;
  FSelected := NodeData;
end;

procedure TfrmCATMain.ShowVMOut(VM: TILMachine);
var
  i, j: Integer;
  pUnit: PVMUnit;
  pVar: PVMVariable;
begin
  with edVMOut.Lines do
  begin
    Clear;
    for i := 0 to VM.UnitsCount - 1 do
    begin
      pUnit := addr(VM.Units[i]);
      if pUnit.VarsCount > -0 then
      begin
        Add('------------------');
        Add('UNIT: ' + VM.GetUnitName(pUnit));
        Add('------------------');
        Add('GLOBAL VARS:');
        Add('');
        for j := 0 to pUnit.VarsCount - 1 do
        begin
          pVar := @pUnit.Variables[j];
          Add(format('%d: %s: %s = %s', [j, VM.GetVarName(pVar), VM.GetTypeName(pVar.DataType), VM.ReadVarValue(pVar)]));
        end;
      end;
    end;
  end;
end;

procedure TfrmCATMain.SynCompletionProposal1Execute(Kind: SynCompletionType; Sender: TObject; var CurrentInput: string; var x, y: Integer;
  var CanExecute: Boolean);
var
  Pos: TTextPosition;
  Items: TIDDeclarationList;
  Decl: TIDDeclaration;
  i: Integer;
  Prefix, DataTypeName, UName: string;
  UN: TNPUnit;
begin
  if not Assigned(FSelected.Package) then
    RunTest(FSelected, True);

  Pos.Row := edSource.CaretY;
  Pos.Col := edSource.CaretX;
  Items := TIDDeclarationList.Create;
  try
    UName := FSelected.Caption;
    UN := TNPUnit(FSelected.Package.GetUnit(UName));
    if not Assigned(UN) then
      Exit;
    UN.GetSRCDeclarations(Pos, Items);
    SynCompletionProposal1.ItemList.Clear;
    SynCompletionProposal1.InsertList.Clear;
    for i := 0 to Items.Count - 1 do
    begin
      Decl := Items[i];
      if (Decl.ItemType in [itVar, itConst]) and (Decl.DataType <> nil) then
        DataTypeName := Decl.DataType.Name
      else
        DataTypeName := '';
      case Decl.ItemType of
        itVar: Prefix := 'var';
        itProcedure: Prefix := 'proc';
        itConst: Prefix := 'const';
        itType: Prefix := 'type';
        itUnit: Prefix := 'unit';
      end;
      SynCompletionProposal1.ItemList.Add(Prefix + '\column{}\style{+B}' + Decl.Name + '\style{-B}: ' + DataTypeName);
      SynCompletionProposal1.InsertList.Add(Decl.Name);
    end;
    CanExecute := True;
  finally
    Items.Free;
  end;
end;

procedure TfrmCATMain.tmLoaderTimer(Sender: TObject);
begin
  tmLoader.Enabled := False;
  ForceDirectories(FRootPath);
  LoadTests(vtTests.RootNode, FRootPath);
  stBar.Panels[0].Text := format('Total tests: %d', [FTests.Count]);
end;

procedure TfrmCATMain.Totalspeedtest1Click(Sender: TObject);
var
  i, c: Integer;
begin
  PageControl1.ActivePage := tsSrcEditor;
  with edSource do begin
    Lines.BeginUpdate;
    try
      clear;
      Lines.Add('unit compile_speed_test;');
      Lines.Add('interface');
      c := 100000;
      for i := 1 to c do begin
        Lines.Add(format('function func%d(a, b, c: Int32; d, e, f: Boolean; g: Char = ''0''): String;', [i]));
      end;
      Lines.Add('implementation');
      for i := 1 to c do begin
        Lines.Add(format('function func%d(a, b, c: Int32; d, e, f: Boolean; g: Char = ''0''): String;', [i]));
        Lines.Add('begin');
        Lines.Add(format('  if d or (g = ''0'') then', []));
        Lines.Add(format('    Result := func%d(a, b, c, d, e, f)', [c - i + 1]));
        Lines.Add(format('  else', []));
        Lines.Add(format('    Result := func%d(a, b, c, d, e, f);', [c - i + 1]));
        Lines.Add('end;');
      end;
      Lines.Add('end.');
    finally
      Lines.EndUpdate;
    end;
  end;
end;

procedure SetNodeChecked(Sender: TBaseVirtualTree; Node: PVirtualNode; CheckState: TCheckState);
var
  CNode: PVirtualNode;
begin
  CNode := Sender.GetFirstChild(Node);
  while Assigned(CNode) do
  begin
    CNode.CheckState := CheckState;
    SetNodeChecked(Sender, CNode, CheckState);
    CNode := Sender.GetNextSibling(CNode);
  end;
end;

procedure TfrmCATMain.vtTestsChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
begin
  Allowed := True;
  SetNodeChecked(Sender, Node, NewState);
  Sender.Repaint;
end;

procedure TfrmCATMain.vtTestsDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string;
  const CellRect: TRect; var DefaultDraw: Boolean);
var
  NodeData: PNodeData;
begin
  NodeData := Sender.GetNodeData(node);
  if Column = 1 then
  case NodeData.Result of
    trNone: TargetCanvas.Font.Color := clWindowText;
    trPass: TargetCanvas.Font.Color := clGreen;
    trFail: TargetCanvas.Font.Color := clRed;
    trSkip: TargetCanvas.Font.Color := clGray;
  end;
end;

procedure TfrmCATMain.vtTestsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  NodeData: PNodeData;
begin
  NodeData := Sender.GetNodeData(node);
  NodeData.Caption := '';
  NodeData.FilePath := '';
  NodeData.FSource := '';
  if Assigned(NodeData.Package) then
  begin
    NodeData.Package.Clear;
    NodeData.Package := nil;
  end;
end;

procedure TfrmCATMain.vtTestsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
                                           var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  NodeData: PNodeData;
begin
  if (Kind <> ikNormal) and (Kind <> ikSelected) then
    Exit;
  NodeData := Sender.GetNodeData(node);
  if Column = 0 then
    case NodeData.NodeType of
      ntFolder: ImageIndex := 0;
      ntTest: ImageIndex := 1;
    end
  else
    ImageIndex := -1;
end;

procedure TfrmCATMain.vtTestsGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNodeData);
end;

procedure TfrmCATMain.vtTestsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  NodeData: PNodeData;
begin
  NodeData := Sender.GetNodeData(node);
  case Column of
    0: CellText := NodeData.Caption;
    1: case NodeData.Result of
         trNone: CellText := '';
         trPass: CellText := 'pass';
         trFail: CellText := 'fail';
         trSkip: CellText := 'skip';
       end;
  end;
end;

procedure TfrmCATMain.vtTestsNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
var
  NodeData: PNodeData;
begin
  NodeData := Sender.GetNodeData(HitInfo.HitNode);
  if NodeData.NodeType = ntTest then
  begin
    SelectTest(NodeData);
    FIniFile.WriteString('COMMON', 'LAST_SELECTED', NodeData.FilePath);
  end;
end;

procedure TfrmCATMain.CWriteStr(const Str: string);
var
 S: TStrings;
begin
  S := TStringList.Create;
  try
    S.Text := Str;
    edConsole.Lines.AddStrings(S);
  finally
    S.Free;
  end;
  PageControl1.ActivePage := tsConsole;
end;

procedure TfrmCATMain.CWriteInt(const Str: Integer);
begin
  edConsole.Lines.Add(IntToStr(Str));
  PageControl1.ActivePage := tsConsole;
end;


procedure TfrmCATMain.CWriteInt64(const Str: Int64);
begin
  edConsole.Lines.Add(IntToStr(Str));
  PageControl1.ActivePage := tsConsole;
end;

{ TNodeData }

function TNodeData.GetChecked: Boolean;
begin
  Result := (Node.CheckState = csCheckedNormal);
end;

function TNodeData.GetSource: string;
var
  Stream: TFileStream;
begin
  if FSource = '' then
  begin
    Stream := TFileStream.Create(FilePath, fmOpenRead);
    try
      Stream.Position := 0;
      FSource := String(Stream.AsAnsiString());
    finally
      Stream.Free;
    end;
  end;
  Result := FSource;
end;

procedure CWriteStr(const Str: string);
begin
  frmCATMain.CWriteStr(Str);
end;

procedure CWriteInt(const Str: Integer);
begin
  frmCATMain.CWriteInt(Str);
end;

procedure CWriteInt64(const Str: Int64);
begin
  frmCATMain.CWriteInt64(Str);
end;

function _GetKey: Integer;
begin
  Result := _CurKey;
  _CurKey := 0;
end;

procedure Test;
{var
  x, y, z, a, b, c, g: Int32;}
begin
{  x := -1;
  y := -2;
  z := -3;
  a := x + y;
  b := x + z;
  c := a + b;
  g := x*a + y*b + z*c;}
end;

var G: Integer;
    p: TProc;

PROCEDURE ProcX;
var
  x, y, r: Integer;
begin
  x := 1;
  y := 2;
  r := x + y;
  p := procedure begin
         r := x + y;
       end;
  p();
  G := r + x + y;
end;

type
  III = interface
   ['{00000000-0000-0000-C000-000000000046}']
  end;

{var
  I, J: IInterface;
  R: Int32;

var
  OBJ: TMY;

procedure SetProc(proc: procedure of object);
var
  x: int32;
  p: proc;
begin
  p := ()->writeln(x);
  p := ()[x]->writeln(x);
  p := ()[var x]->writeln(x);
  p := (a, b: int32)->a - b;

  p := [::]writeln(x);
  p := [:x:]writeln(x);
  p := [:var x:]writeln(x);
  p := [::](a, b: int32)a - b;

  p := [::](a, b: int32)[a - b];


  p := (x, y) => x = y;

end;}


type
  TIntArray = array of Integer;

function SearchInRange(const Values: TIntArray; const Value: Integer; out FoundIndex: Integer): Boolean;
var
  L, H: Integer;
  mid, cmp: Integer;
  LastState: (ssNone, sLess, sGreater);
begin
  Result := False;
  L := 0;
  H := Length(Values) - 1;
  LastState := ssNone;
  while L <= H do
  begin
    mid := L + (H - L) shr 1;
    cmp := Values[mid] - Value;
    if cmp < 0 then
    begin
      if L = H then
      begin
        FoundIndex := L;
        Exit;
      end;
      L := mid + 1;
      LastState := sGreater;
    end else
    if cmp = 0 then
    begin
      FoundIndex := mid;
      Result := True;
      Exit;
    end else
    begin
      if (L = H) and (LastState = sGreater) then
      begin
        FoundIndex := L - 1;
        Exit;
      end;
      H := mid - 1;
      LastState := sLess;
    end;
  end;
  FoundIndex := H;
end;

var
  A: TIntArray;
  I: Integer;
  B: Boolean;

type TTa = (tta1, tta2);

var Arr: array [0..1] of array [0..3] of Int32 =
  (
    (1, 2, 3, 4),
    (1, 2, 3, 4)
  );

var ax: TTa;

procedure Test1;
begin
  A := [];
  B := SearchInRange(A, 5, I);
  Assert(I = -1);
end;

procedure Test2;
begin
  A := [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
  B := SearchInRange(A, 5, I);
  Assert(I = 5);
end;

procedure Test3;
begin
  A := [0, 2, 4, 6, 8, 10];
  B := SearchInRange(A, 5, I);
  Assert(I = 2);
end;

procedure SetVar(const V: Variant);
begin

end;

procedure Test4;
var
  v: Variant;
begin
  v := 5;
  SetVar(v);
end;

procedure RegisterVMRTL;
begin
  vm.sys.console.SetLogProc(CWriteStr);
end;

function GetBool1(Cond: Boolean): Boolean;
begin
  Result := Cond;
end;

function GetBool2(Cond: Boolean): Boolean;
begin
  Result := Cond;
end;

function GetBool3(Cond: Boolean): Boolean;
begin
  Result := Cond;
end;

procedure Test5;
begin
  B := GetBool1(True) or GetBool2(False) and GetBool3(True);
end;

initialization
  Test5();
  Test1();
  Test2();
  Test3();
  Test4();
  RegisterVMRTL();
  RegisterProc('console', 'CWriteStr', @CWriteStr);
  RegisterProc('console', 'CWriteInt', @CWriteInt);
  RegisterProc('console', 'CWriteInt64', @CWriteInt64);
  RegisterProc('sys', 'GetKey', @_GetKey);


finalization
  FreeAndNil(SYSUnit);

end.
