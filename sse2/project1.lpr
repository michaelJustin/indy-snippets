program project1;

uses
  IdHTTPServer,
  IdContext,
  IdGlobal,
  IdCustomHTTPServer,
  SysUtils,
  DateUtils,
  Classes,
  Windows;

type

  { TMySSEServer }

  TMySSEServer = class(TIdHTTPServer)
  private
    function BuildContentText(AContext:
      TIdContext): string;
  protected
    procedure DoCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); override;
  end;

  { TMySSEServer }

  function TMySSEServer.BuildContentText(
    AContext: TIdContext): string;
  begin
    Result := '';
    repeat
      Result := Result + 'event: ping' + #13 +
        Format('data: {"time": "%s", "peer": "%s:%d"}',
        [DateToISO8601(Now, False),
        AContext.Binding.PeerIP,
        AContext.Binding.PeerPort]) + #13#13;
      Sleep(100);
    until Random < 0.8;
  end;

  procedure TMySSEServer.DoCommandGet(AContext: TIdContext;
    ARequestInfo: TIdHTTPRequestInfo;
    AResponseInfo: TIdHTTPResponseInfo);
  begin
    if ARequestInfo.Document = '/sse' then
    begin
      AResponseInfo.ContentType := 'text/event-stream';
      AResponseInfo.CacheControl := 'no-store';
      AResponseInfo.ContentText :=
        BuildContentText(AContext);
    end
    else
    begin
      AResponseInfo.ContentType := 'text/html';
      AResponseInfo.ContentStream := TFileStream.Create('index.html', fmOpenRead);
    end;
    AResponseInfo.CharSet := 'UTF-8';
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
