unit proc_ccreg_set64;

interface             

implementation

type
  TEnum64 = (_a00,_a01,_a02,_a03,_a04,_a05,_a06,_a07,_a08,_a09,_a10,_a11,_a12,_a13,_a14,_a15,_a16,_a17,_a18,_a19,_a20,_a21,_a22,_a23,_a24,_a25,_a26,_a27,_a28,_a29,_a30,_a31,
             _a32,_a33,_a34,_a35,_a36,_a37,_a38,_a39,_a40,_a41,_a42,_a43,_a44,_a45,_a46,_a47,_a48,_a49,_a50,_a51,_a52,_a53,_a54,_a55,_a56,_a57,_a58,_a59,_a60,_a61,_a62,_a63);

  TSet64 = packed set of TEnum64;	

procedure ExtProc(a: TSet64); external 'CAT' name 'proc_ccreg_set64'; 

procedure Test;
var
  s: TSet64;
begin
  s := [_a08];
  ExtProc(s);
end;

initialization
  Test();

finalization

end.