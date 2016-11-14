{%MainUnit fileutil.pas}


function ExtractShortPathNameUTF8(const FileName: String): String;
begin
  Result:=SysToUTF8(SysUtils.ExtractShortPathName(UTF8ToSys(FileName)));
end;

