﻿unit NPCompiler.Operators;

interface

{$I compilers.inc}

uses SysUtils, Classes;

type

  TOperatorID = (
    // not oveloaded: //////////////////
    opNone,
    opOpenRound,
    opCloseRound,
    opSubExpression,
    opCall,
    opDereference,
    opPeriod,
    opAddr,
    opIs,
    opAs,
    // oveloaded: //////////////////////
    opAssignment,
    // unar:
    opImplicit,
    opExplicit,
    opNegative,
    opPositive,
    opPostInc,
    opPostDec,
    opNot,
    // binar:
    opIn,
    opEqual,
    opNotEqual,
    opGreater,
    opGreaterOrEqual,
    opLess,
    opLessOrEqual,
    opAdd,
    opSubtract,
    opMultiply,
    opDivide,
    opIntDiv,
    opModDiv,
    opShiftLeft,
    opShiftRight,
    opAnd,
    opOr,
    opXor
  );


  TOperatorType =
  (
    opSpecial,
    opBinary,
    opUnarPrefix,
    opUnarSufix

  );

{ signatures of operators:
|--------------------|------------|------------------------------------------------|-------------------|
|  Operator          | Category	  | Declaration Signature	                         | Symbol Mapping    |
|--------------------|------------|------------------------------------------------|-------------------|
| Implicit           | Conversion | Implicit(a : type) : resultType;               | implicit typecast |
| Explicit           | Conversion | Explicit(a: type) : resultType;                | explicit typecast |
| Negative           | Unary      | Negative(a: type) : resultType;                | -                 |
| Positive           | Unary      | Positive(a: type): resultType;                 | +                 |
| LogicalNot         | Unary      | LogicalNot(a: type): resultType;               | not               |
| In                 | Set        | In(a: type; b: type) : Boolean;                | in                |
| Eq                 | Comparison | Equal(a: type; b: type) : Boolean;             | =                 |
| NotEqual           | Comparison | NotEqual(a: type; b: type): Boolean;           | <>                |
| GreaterThan        | Comparison | GreaterThan(a: type; b: type) Boolean;         | >                 |
| GreaterThanOrEqual | Comparison | GreaterThanOrEqual(a: type; b: type): Boolean; | >=                |
| LessThan           | Comparison | LessThan(a: type; b: type): Boolean;           | <                 |
| LessThanOrEqual    | Comparison | LessThanOrEqual(a: type; b: type): Boolean;    | <=                |
| Add                | Binary     | Add(a: type; b: type): resultType;             | +                 |
| Subtract           | Binary     | Subtract(a: type; b: type) : resultType;       | -                 |
| Multiply           | Binary     | Multiply(a: type; b: type) : resultType;       | *                 |
| Divide             | Binary     | Divide(a: type; b: type) : resultType;         | /                 |
| IntDivide          | Binary     | IntDivide(a: type; b: type): resultType;       | div               |
| Modulus            | Binary     | Modulus(a: type; b: type): resultType;         | mod               |
| LeftShift          | Binary     | LeftShift(a: type; b: type): resultType;       | shl               |
| RightShift         | Binary     | RightShift(a: type; b: type): resultType;      | shr               |
| LogicalAnd         | Binary     | LogicalAnd(a: type; b: type): resultType;      | and               |
| LogicalOr          | Binary     | LogicalOr(a: type; b: type): resultType;       | or                |
| LogicalXor         | Binary     | LogicalXor(a: type; b: type): resultType;      | xor               |
| BitwiseAnd         | Binary     | BitwiseAnd(a: type; b: type): resultType;      | and               |
| BitwiseOr          | Binary     | BitwiseOr(a: type; b: type): resultType;       | or                |
| BitwiseXor         | Binary     | BitwiseXor(a: type; b: type): resultType;      | xor               |
|--------------------|------------|------------------------------------------------|-------------------|}


function OperatorFullName(&Operator: TOperatorID): string;
function OperatorShortName(&Operator: TOperatorID): string;
function GetOperatorID(const Name: string): TOperatorID; inline;





const
  // Приоритеты операций (-1 - низший приоритет, 10 - высшый приоритет)
  cOperatorPriorities: array [TOperatorID] of Integer = (

  {opNone}                -1,
  {opOpenRound}           10,
  {opCloseRound}          10,
  {opSubExpression}       -1,
  {opCall}		             9,
  {opDereference}          8,
  {opPeriod}               2,
  {opAddr}                 8,
  {opIS}                   8,
  {opAS}                   8,

  {opAssign}               0,
  {opImplicit}            -1,
  {opExplicit}            -1,
  {opNegative}             8,
  {opPositive}             2,
  {opPostInc}              7,
  {opPostDec}              7,
  {opLogicalNot}           1,

  {opIn}                   1,
  {opEqual}                5,
  {opNotEqual}             5,
  {opGreaterThan}          5,
  {opGreaterThanOrEqual}   5,
  {opLessThan}             5,
  {opLessThanOrEqual}      5,
  {opAdd}                  6,
  {opSubtract}             6,
  {opMultiply}             7,
  {opDivide}               7,
  {opIntDiv}               7,
  {opModDiv}               7,
  {opLeftShift}            7,
  {opRightShift}           7,
  {opLogicalAnd}           3,
  {opLogicalOr}            2,
  {opLogicalXor}           2
  );




const
  // Приоритеты операций (-1 - низший приоритет, 10 - высшый приоритет)
  cOperatorTypes: array [TOperatorID] of TOperatorType = (

  {opNone}                opSpecial,
  {opOpenRound}           opSpecial,
  {opCloseRound}          opSpecial,
  {opSubExpression}       opSpecial,
  {opCall}		            opSpecial,
  {opDereference}         opUnarSufix,
  {opPeriod}              opBinary,
  {opAddr}                opUnarPrefix,
  {opIS}                  opBinary,
  {opAS}                  opBinary,

  {opAssign}              opBinary,
  {opImplicit}            opSpecial,
  {opExplicit}            opSpecial,
  {opNegative}            opUnarPrefix,
  {opPositive}            opUnarPrefix,
  {opPostInc}             opUnarSufix,
  {opPostDec}             opUnarSufix,
  {opLogicalNot}          opUnarPrefix,

  {opIn}                   opBinary,
  {opEqual}                opBinary,
  {opNotEqual}             opBinary,
  {opGreaterThan}          opBinary,
  {opGreaterThanOrEqual}   opBinary,
  {opLessThan}             opBinary,
  {opLessThanOrEqual}      opBinary,
  {opAdd}                  opBinary,
  {opSubtract}             opBinary,
  {opMultiply}             opBinary,
  {opDivide}               opBinary,
  {opIntDiv}               opBinary,
  {opModDiv}               opBinary,
  {opLeftShift}            opBinary,
  {opRightShift}           opBinary,
  {opLogicalAnd}           opBinary,
  {opLogicalOr}            opBinary,
  {opLogicalXor}           opBinary
  );


var
  _operators: TStringList = nil;

implementation


const
  cOpFullNames: array [TOperatorID] of string = (

  {opNone}            '',
  {opOpenRound}       '(',
  {opCloseRound}      ')',
  {opSubExpression}   '',
  {opCall}            'call',
  {opDereference}     'Dereference',
  {opPeriod}          'period',
  {opAddr}            'Addr',
  {opAddr}            'IS',
  {opAddr}            'AS',
  {opAssign}          'Assign',
  {opImplicit}        'Implicit',
  {opExplicit}        'Explicit',
  {opNegative}        'Negative',
  {opPositive}        'Positive',
  {opPostInc}         'PostInc',
  {opPostDec}         'PostDec',
  {opLogicalNot}      'Not',

  {opIn}              'In',
  {opEqual}           'Equal',
  {opNotEqual}        'NotEqual',
  {opGreater}         'GreaterThan',
  {opGreaterOrEqual}  'GreaterThanOrEqual',
  {opLess}            'LessThan',
  {opLessOrEqual}     'LessThanOrEqual',
  {opAdd}             'Add',
  {opSubtract}        'Subtract',
  {opMultiply}        'Multiply',
  {opDivide}          'Divide',
  {opIntDiv}          'IntDivide',
  {opModDiv}          'Modulus',
  {opLeftShift}       'LeftShift',
  {opRightShift}      'RightShift',
  {opLogicalAnd}      'And',
  {opLogicalOr}       'Or',
  {opLogicalXor}      'Xor'
  );


  cOpShortNames: array [TOperatorID] of string = (

  {opNone}            '',
  {opOpenRound}       '(',
  {opCloseRound}      ')',
  {opSubExpression}   '',
  {opCall}            'call',
  {opPeriod}          '..',
  {opAddr}            '@',
  {opIs}              'is',
  {opAs}              'as',
  {opDereference}     '^',
  {opAssign}          ':=',
  {opImplicit}        'Implicit',
  {opExplicit}        'Explicit',
  {opNegative}        'Negative',
  {opPositive}        'Positive',
  {opPostInc}         '++',
  {opPostDec}         '--',
  {opLogicalNot}      'not',

  {opIn}              'in',
  {opEqual}           '=',
  {opNotEqual}        '<>',
  {opGreater}         '>',
  {opGreaterOrEqual}  '>=',
  {opLess}            '<',
  {opLessOrEqual}     '<=',
  {opAdd}             '+',
  {opSubtract}        '-',
  {opMultiply}        '*',
  {opDivide}          '/',
  {opIntDiv}          'div',
  {opModDiv}          'mod',
  {opLeftShift}       'shl',
  {opRightShift}      'shr',
  {opLogicalAnd}      'and',
  {opLogicalOr}       'or',
  {opLogicalXor}      'xor'
  );


function OperatorFullName(&Operator: TOperatorID): string;
begin
  Result := cOpFullNames[&Operator];
end;

function OperatorShortName(&Operator: TOperatorID): string;
begin
  Result := cOpShortNames[&Operator];
end;

function GetOperatorID(const Name: string): TOperatorID; inline;
var
  idx: Integer;
begin
  idx := _operators.IndexOf(Name);
  if idx >= 0 then
    Result := TOperatorID(_operators.Objects[idx])
  else
    Result := opNone;
end;

initialization
  _operators := TStringList.Create;
  _operators.AddObject('Assignment', TObject(opAssignment));
  _operators.AddObject('Implicit', TObject(opImplicit));
  _operators.AddObject('Explicit', TObject(opExplicit));
  _operators.AddObject('Negative', TObject(opNegative));
  _operators.AddObject('Positive', TObject(opPositive));
  _operators.AddObject('BitwiceNot', TObject(opNot));
  _operators.AddObject('In', TObject(opIn));
  _operators.AddObject('Equal', TObject(opEqual));
  _operators.AddObject('NotEqual', TObject(opNotEqual));
  _operators.AddObject('Greater', TObject(opGreater));
  _operators.AddObject('GreaterOrEqual', TObject(opGreaterOrEqual));
  _operators.AddObject('Less', TObject(opLess));
  _operators.AddObject('LessOrEqual', TObject(opLessOrEqual));
  _operators.AddObject('Add', TObject(opAdd));
  _operators.AddObject('Subtract', TObject(opSubtract));
  _operators.AddObject('Multiply', TObject(opMultiply));
  _operators.AddObject('Divide', TObject(opDivide));
  _operators.AddObject('IntDiv', TObject(opIntDiv));
  _operators.AddObject('ModDiv', TObject(opModDiv));
  _operators.AddObject('ShiftLeft', TObject(opShiftLeft));
  _operators.AddObject('ShiftRight', TObject(opShiftRight));
  _operators.AddObject('BitwiceAnd', TObject(opAnd));
  _operators.AddObject('BitwiceOr', TObject(opOr));
  _operators.AddObject('BitwiceXor', TObject(opXor));
  _operators.Sort;


finalization
  _operators.Free;

end.

