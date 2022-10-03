program project1;

uses
  IdHTTPServer,
  IdContext,
  IdGlobal,
  IdCustomHTTPServer,
  SysUtils,
  Classes,
  Windows;

type

  { TMySSEServer }

  TMySSEServer = class(TIdHTTPServer)
  protected
    procedure DoCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); override;
  end;

  { TMySSEServer }

  procedure TMySSEServer.DoCommandGet(AContext: TIdContext;
    ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  var
    S: TStream;
    C: string;
  begin
    if ARequestInfo.Document = '/sse' then
    begin
       AResponseInfo.ContentType := 'text/event-stream';
       AResponseInfo.CacheControl := 'no-store';
       AResponseInfo.CharSet := 'UTF-8';

       C := C + 'event: ping' + #13
         + Format('data: {"time": "%d"}', [GetTickCount])
         + #13#13;

       AResponseInfo.ContentText := C;
       AResponseInfo.ResponseNo := 200;
    end
    else
    begin
      S := TFileStream.Create('index.html', fmOpenRead);
      AResponseInfo.ContentType := 'text/html';
      AResponseInfo.ContentStream := S;
      AResponseInfo.ResponseNo := 200;
    end;
  end;

var
  Server: TMySSEServer;

  procedure Test;
  begin
    Server := TMySSEServer.Create;
    try
      Server.KeepAlive := True;
      Server.Startup;
      ReadLn;
    finally
      Server.Free;
    end;
  end;

begin
  Test;
end.
