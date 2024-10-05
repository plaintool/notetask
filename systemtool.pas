unit systemtool;

{$mode ObjFPC}{$H+}

interface

uses
  Forms,
  Classes,
  SysUtils,
  gettext,
  DefaultTranslator,
  Translations,
  LResources,
  LCLTranslator,
  LCLType,
  {$IF DEFINED(Win64) OR DEFINED(Win32)}
  Windows
  {$ELSE}
  Unix
  {$IFDEF LCLCarbon}
  MacOSAll
  {$ENDIF}
  {$ENDIF}
  ;

function GetOSLanguage: string;
function ApplicationTranslate(const Language: string): boolean;

implementation

function GetOSLanguage: string;
  {platform-independent method to read the language of the user interface}
var
  l, fbl: string;
  {$IFDEF LCLCarbon}
  theLocaleRef: CFLocaleRef;
  locale: CFStringRef;
  buffer: StringPtr;
  bufferSize: CFIndex;
  encoding: CFStringEncoding;
  success: boolean;
  {$ENDIF}
begin
  {$IFDEF LCLCarbon}
  theLocaleRef := CFLocaleCopyCurrent;
  locale := CFLocaleGetIdentifier(theLocaleRef);
  encoding := 0;
  bufferSize := 256;
  buffer := new(StringPtr);
  success := CFStringGetPascalString(locale, buffer, bufferSize, encoding);
  if success then
    l := string(buffer^)
  else
    l := '';
  fbl := Copy(l, 1, 2);
  dispose(buffer);
  {$ELSE}
  {$IFDEF LINUX}
  fbl := Copy(GetEnvironmentVariable('LC_CTYPE'), 1, 2);
  {$ELSE}
  GetLanguageIDs(l, fbl);
  {$ENDIF}
  {$ENDIF}
  Result := fbl;
end;

function ApplicationTranslate(const Language: string): boolean;
var
  Res: TResourceStream;
  PoStringStream: TStringStream;
  PoFile: TPOFile;
  LocalTranslator: TUpdateTranslator;
  i: integer;
begin
  Result := False;

  // Оборачиваем в try-finally, чтобы гарантировать освобождение ресурсов
  Res := nil;
  PoStringStream := nil;
  PoFile := nil;
  LocalTranslator := nil;

  try
    try
      // Загружаем ресурсный файл
      Res := TResourceStream.Create(HInstance, 'notetask.' + Language, RT_RCDATA);
      PoStringStream := TStringStream.Create('');

      // Сохраняем ресурс в строковый поток
      Res.SaveToStream(PoStringStream);

      // Читаем строки из файла
      PoFile := TPOFile.Create(False);
      PoFile.ReadPOText(PoStringStream.DataString);

      // Переводим строки ресурсов (это сработает для messagestring и resourcestring)
      Result := TranslateResourceStrings(PoFile);

      if Result then
      begin
        // Создаем локального переводчика для формы
        LocalTranslator := TPOTranslator.Create(PoFile);
        LRSTranslator := LocalTranslator;

        // Обновляем перевод для всех форм
        for i := 0 to Screen.CustomFormCount - 1 do
          LocalTranslator.UpdateTranslation(Screen.CustomForms[i]);

        // Обновляем перевод для всех модулей данных
        for i := 0 to Screen.DataModuleCount - 1 do
          LocalTranslator.UpdateTranslation(Screen.DataModules[i]);
      end;

    except
      on E: Exception do
      begin
        // Обрабатываем ошибку перевода и выводим сообщение
        WriteLn('Error during translation: ', E.Message);
        Result := False; // Возвращаем False в случае ошибки
      end;
    end;

  finally
    // Освобождаем все используемые ресурсы
    if Assigned(LocalTranslator) then
    begin
      LRSTranslator := nil;
      LocalTranslator.Free;
    end
    else
    if Assigned(PoFile) then
      PoFile.Free;

    if Assigned(PoStringStream) then
      PoStringStream.Free;

    if Assigned(Res) then
      Res.Free;
  end;
end;

end.
