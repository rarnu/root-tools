
uses winsock;

Type
  PHostEntry = PHostEnt;
  PNetEntry = PNetEnt;
  PServEntry = PServEnt;

Const
  DNSErr : Integer = 0;

function getnetbyname ( Name : pchar) : PNetEntry;

begin
  DNSErr:=-1;
  Result:=Nil;
end;

function getnetbyaddr ( Net : Longint; nettype : Longint) : PNetEntry;

begin
  DNSErr:=-2;
  Result:=Nil;
end;

Function GetDNSError : Longint;

begin
  If DNSErr<>0 then
    begin
    GetDNSError:=-DNSErr;
    DNSErr:=0;
    end
  else
    GetDNSError:=0; // Must get win32 error ?
end;

Function InitResolve : Boolean;

var
   wsaData : TWsaData;

begin
  Result:=WSAStartup(1,wsaData)=0;
end;

Function FinalResolve : Boolean;

begin
  Result:=True;
end;
