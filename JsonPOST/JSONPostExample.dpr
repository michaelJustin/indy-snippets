(*
   Copyright 2016 Michael Justin

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

program JSONPostExample;

{$APPTYPE CONSOLE}

uses
  IdHTTP, IdGlobal,
  SysUtils, Classes;

var
  HTTP: TIdHTTP;
  RequestBody: TStream;
  ResponseBody: string;
begin
  HTTP := TIdHTTP.Create;
  try
    try
      RequestBody := TStringStream.Create('{"日本語":42}',
        TEncoding.UTF8);
      try
        HTTP.Request.Accept := 'application/json';
        HTTP.Request.ContentType := 'application/json';
        ResponseBody := HTTP.Post('http://httpbin.org/post', RequestBody);
        WriteLn(ResponseBody);
        WriteLn(HTTP.ResponseText);
      finally
        RequestBody.Free;
      end;
    except
      on E: EIdHTTPProtocolException do
      begin
        WriteLn(E.Message);
        WriteLn(E.ErrorMessage);
      end;
      on E: Exception do
      begin
        WriteLn(E.Message);
      end;
    end;
  finally
    HTTP.Free;
  end;
  ReadLn;
  ReportMemoryLeaksOnShutdown := True;
end.
