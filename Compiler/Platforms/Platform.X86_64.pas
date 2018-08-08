unit Platform.X86_64;

interface

implementation

uses CompilerClasses, HWPlatforms;

var
  Platform: TIDPlatform;

initialization
  Platform := TIDPlatform.Create('X86_64');
  HWPlatforms.RegisterPlatform(Platform);

finalization
  Platform.Free;
end.
