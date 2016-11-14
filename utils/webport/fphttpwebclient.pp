{ **********************************************************************
  This file is part of the Free Component Library (FCL)
  Copyright (c) 2015 by the Free Pascal development team
        
  FPHTTPClient implementation of TFPWebclient.
            
  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.
                   
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  **********************************************************************}
                                 
unit fphttpwebclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpwebclient, fphttpclient;

Type

  { TFPHTTPRequest }

  TFPHTTPRequest = Class(TWebClientRequest)
  Private
    FHTTP : TFPHTTPClient;
  Public
    function GetHeaders: TStrings;override;
    Constructor Create(AHTTP : TFPHTTPClient);
    Destructor Destroy; override;
  end;

  { TFPHTTPRequest }

  TFPHTTPResponse = Class(TWebClientResponse)
  Private
    FHTTP : TFPHTTPClient;
  Protected
    function GetHeaders: TStrings;override;
    Function GetStatusCode : Integer; override;
    Function GetStatusText : String; override;
  Public
    Constructor Create(AHTTP : TFPHTTPRequest);
  end;

  { TFPHTTPWebClient }

  TFPHTTPWebClient = Class(TAbstractWebClient)
  Protected
    Function DoCreateRequest: TWebClientRequest; override;
    Function DoHTTPMethod(Const AMethod,AURL : String; ARequest : TWebClientRequest) : TWebClientResponse; override;
  end;

implementation

uses dateutils;

{ TFPHTTPRequest }

function TFPHTTPRequest.GetHeaders: TStrings;
begin
  Result:=FHTTP.RequestHeaders;
end;

constructor TFPHTTPRequest.Create(AHTTP: TFPHTTPClient);
begin
  FHTTP:=AHTTP;
end;

destructor TFPHTTPRequest.Destroy;
begin
  FreeAndNil(FHTTP);
  inherited Destroy;
end;

{ TFPHTTPResponse }

function TFPHTTPResponse.GetHeaders: TStrings;
begin
  if Assigned(FHTTP) then
    Result:=FHTTP.ResponseHeaders
  else
    Result:=Inherited GetHeaders;
end;

Function TFPHTTPResponse.GetStatusCode: Integer;
begin
  if Assigned(FHTTP) then
    Result:=FHTTP.ResponseStatusCode
  else
    Result:=0;
end;

Function TFPHTTPResponse.GetStatusText: String;
begin
  if Assigned(FHTTP) then
    Result:=FHTTP.ResponseStatusText
  else
    Result:='';
end;

Constructor TFPHTTPResponse.Create(AHTTP: TFPHTTPRequest);
begin
  Inherited Create(AHTTP);
  FHTTP:=AHTTP.FHTTP;
end;


{ TFPHTTPWebClient }

Function TFPHTTPWebClient.DoCreateRequest: TWebClientRequest;
begin
  Result:=TFPHTTPRequest.Create(TFPHTTPClient.Create(Self));
  Result.Headers.NameValueSeparator:=':';
end;

Function TFPHTTPWebClient.DoHTTPMethod(Const AMethod, AURL: String;
  ARequest: TWebClientRequest): TWebClientResponse;

Var
  U,S : String;
  h : TFPHTTPClient;
  Res : Boolean;

begin
  U:=AURL;
  H:=TFPHTTPRequest(ARequest).FHTTP;
  S:=ARequest.ParamsAsQuery;
  if (S<>'') then
    begin
    if Pos('?',U)=0 then
      U:=U+'?';
    U:=U+S;
    end;
  Result:=TFPHTTPResponse.Create(ARequest as TFPHTTPRequest);
  try
    if Assigned(ARequest.Content) and (ARequest.Headers.IndexOfName('Content-length')<0) then
      H.AddHeader('Content-length',IntToStr(ARequest.Content.size));
    if ARequest.Content.Size>0 then
      begin
      H.RequestBody:=ARequest.Content;
      H.RequestBody.Position:=0;
      end;
    H.HTTPMethod(AMethod,U,Result.Content,[]); // Will rais an exception
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

end.

