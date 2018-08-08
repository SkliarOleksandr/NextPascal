unit sys.console;

#ifdef target_js
  #set VariantExplicitConvert=False;
#end

interface
  procedure Log(const Msg: Variant); external 'sys.console';

end.