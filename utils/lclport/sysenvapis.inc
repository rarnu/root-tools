{%MainUnit ../lclintf.pas}

// Locates the default browser associated in the system
function FindDefaultBrowser(out ABrowser, AParams: String): Boolean;

  function Find(const ShortFilename: String; out ABrowser: String): Boolean; inline;
  begin
    ABrowser := SearchFileInPath(ShortFilename + GetExeExt, '',
                      GetEnvironmentVariableUTF8('PATH'), PathSeparator,
                      [sffDontSearchInBasePath]);
    Result := ABrowser <> '';
  end;

begin
  {$IFDEF MSWindows}
  Find('rundll32', ABrowser);
  AParams := 'url.dll,FileProtocolHandler %s';
  {$ELSE}
    {$IFDEF DARWIN}
    // open command launches url in the appropriate browser under Mac OS X
    Find('open', ABrowser);
    AParams := '"%s"';
    {$ELSE}
      ABrowser := '';
    {$ENDIF}
  {$ENDIF}
  if ABrowser = '' then
  begin
    AParams := '"%s"';
    // Then search in path. Prefer open source ;)
    if Find('xdg-open', ABrowser)  // Portland OSDL/FreeDesktop standard on Linux
    or Find('htmlview', ABrowser)  // some redhat systems
    or Find('firefox', ABrowser)
    or Find('mozilla', ABrowser)
    or Find('galeon', ABrowser)
    or Find('konqueror', ABrowser)
    or Find('safari', ABrowser)
    or Find('netscape', ABrowser)
    or Find('opera', ABrowser)
    or Find('iexplore', ABrowser) then ;// some windows systems
  end;
  Result := ABrowser <> '';
end;
